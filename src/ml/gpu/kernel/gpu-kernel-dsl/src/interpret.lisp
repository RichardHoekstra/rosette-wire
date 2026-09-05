;;;; interpret.lisp --- CPU interpreter for the rosette-gpu-kernel-dsl DSL.
;;;;
;;;; The base system lowers a KERNEL-SPEC to CUDA-C source; the optional
;;;; /bridge system compiles + launches it via NVRTC.  This /cpu system adds
;;;; the third corner: a pure-Lisp interpreter that *executes* the very same
;;;; KERNEL-SPEC on the host with no CUDA toolchain.  One `defkernel` source
;;;; therefore has two executable backends — the algorithm/schedule split that
;;;; rosette-kernel-backend dispatches over.
;;;;
;;;; The parallel construct is the grid-stride loop: on the device every
;;;; thread strides the index space; on the CPU we run the equivalent serial
;;;; sweep i = 0 .. n-1.  CUDA built-ins reduce to a single (0,0,1,1) thread.
;;;; Grammar coverage mirrors VALID-DSL-STMT-P: setq, for-grid-stride,
;;;; for-range, progn, let, if, cond, case, atomic-add, atomic-cas, and the
;;;; full expression sub-grammar (arithmetic, comparison, indexed load, call).

(defpackage #:rosette-gpu-kernel-dsl/cpu
  (:nicknames #:rosette-kdsl-cpu)
  (:use #:cl)
  (:import-from #:rosette-gpu-kernel-dsl
                #:kernel-spec
                #:kernel-spec-p
                #:kernel-spec-name
                #:kernel-spec-params
                #:kernel-spec-body
                #:dsl-syntax-error)
  (:export
   #:interpret-kernel
   #:interpret-kernel-spec
   #:half-bits-to-single
   #:single-to-half-bits))

(in-package #:rosette-gpu-kernel-dsl/cpu)

;;; Reach into the base lib's (non-exported) lexer helpers — single source of
;;; truth for symbol mangling and indexed-symbol decoding, shared with codegen.
(defun %cname (sym) (rosette-gpu-kernel-dsl::%symbol-c-name sym))
(defun %indexed-p (sym) (rosette-gpu-kernel-dsl::%indexed-symbol-p sym))
(defun %decode (sym) (rosette-gpu-kernel-dsl::%decode-indexed-symbol sym))
(defun %name= (a b) (rosette-gpu-kernel-dsl::%sym-name= a b))

;;; --- environment ----------------------------------------------------------
;;; A flat hash-table keyed by C-name string.  Scalars map to numbers,
;;; pointer params to Lisp vectors.  let / loop variables save+restore.

(defun %env-get (env key)
  (multiple-value-bind (v present) (gethash key env)
    (unless present
      (error 'dsl-syntax-error :form key :reason "unbound DSL identifier"))
    v))

(defun %env-set (env key value) (setf (gethash key env) value))

(defmacro %with-binding ((env key value) &body body)
  "Dynamically bind KEY to VALUE in ENV for the extent of BODY, restoring
the prior binding (or absence) afterwards."
  (let ((k (gensym)) (e (gensym)) (had (gensym)) (old (gensym)))
    `(let* ((,e ,env) (,k ,key))
       (multiple-value-bind (,old ,had) (gethash ,k ,e)
         (setf (gethash ,k ,e) ,value)
         (unwind-protect (progn ,@body)
           (if ,had (setf (gethash ,k ,e) ,old) (remhash ,k ,e)))))))

;;; --- named C functions reachable from (call fn ...) -----------------------

(defparameter *call-table*
  (list (cons "sqrtf" #'sqrt) (cons "sqrt" #'sqrt)
        (cons "expf" #'exp)   (cons "exp" #'exp)
        (cons "logf" #'log)   (cons "log" #'log)
        (cons "sinf" #'sin)   (cons "cosf" #'cos)
        (cons "tanhf" #'tanh) (cons "tanh" #'tanh)
        (cons "fabsf" #'abs)  (cons "fabs" #'abs)   (cons "abs" #'abs)
        (cons "fmaxf" #'max)  (cons "fminf" #'min)
        (cons "fmax" #'max)   (cons "fmin" #'min)
        (cons "powf" #'expt)  (cons "pow" #'expt)
        (cons "floorf" #'ffloor) (cons "ceilf" #'fceiling)
        ;; round-half-to-even to integral value (matches NVPTX cvt.rni / FROUND)
        (cons "rintf" #'fround) (cons "rint" #'fround)
        (cons "roundevenf" #'fround) (cons "roundeven" #'fround)))

(defun %call-fn (name)
  (or (cdr (assoc (%cname name) *call-table* :test #'string=))
      (error 'dsl-syntax-error :form name
             :reason "named C function not in CPU interpreter call table")))

(defun %cdiv (a b)
  "C-style division: integer truncation when both operands are integers."
  (if (and (integerp a) (integerp b)) (truncate a b) (/ a b)))

(defun %mod32 (a b)
  "C-style remainder: division by zero is an error."
  (if (zerop b)
      (error 'dsl-syntax-error :form '(%) :reason "division by zero in modulo")
      (rem a b)))

(defun %eval-sum (args env)
  (let ((sum 0))
    (dolist (arg args sum)
      (incf sum (eval-expr arg env)))))

(defun %eval-product (args env)
  (let ((product 1))
    (dolist (arg args product)
      (setf product (* product (eval-expr arg env))))))

(defun %eval-difference (args env)
  (let ((first-value (eval-expr (first args) env)))
    (if (null (rest args))
        (- first-value)
        (let ((difference first-value))
          (dolist (arg (rest args) difference)
            (decf difference (eval-expr arg env)))))))

(defun %eval-quotient (args env)
  (let ((quotient (eval-expr (first args) env)))
    (dolist (arg (rest args) quotient)
      (setf quotient (%cdiv quotient (eval-expr arg env))))))

(defun %eval-mod (args env)
  (unless (= 2 (length args))
    (error 'dsl-syntax-error :form (cons '% args)
           :reason "modulo requires exactly two operands"))
  (let ((lhs (eval-expr (first args) env))
        (rhs (eval-expr (second args) env)))
    (%mod32 lhs rhs)))

(defun %eval-mulmod (args env)
  "(mulmod a b p) = (a*b) mod p with an EXACT (non-truncating) product --
models the device mul.wide.u32 + rem.u64.  Operands are read as unsigned
32-bit; the result is in [0,p)."
  (unless (= 3 (length args))
    (error 'dsl-syntax-error :form (cons 'mulmod args)
           :reason "mulmod requires exactly three operands"))
  (let ((a (logand (eval-expr (first args) env)  #xffffffff))
        (b (logand (eval-expr (second args) env) #xffffffff))
        (p (logand (eval-expr (third args) env)  #xffffffff)))
    (when (zerop p)
      (error 'dsl-syntax-error :form '(mulmod) :reason "division by zero in mulmod"))
    (mod (* a b) p)))

(defun %eval-call-args (args env)
  (let ((values nil))
    (dolist (arg args (nreverse values))
      (push (eval-expr arg env) values))))

;;; --- expression evaluation ------------------------------------------------

(defparameter *builtin-values*
  ;; single-thread (0,0,1,1) model: one thread covers the whole index space
  ;; through the serial grid-stride sweep.
  '(("threadidx.x" . 0) ("threadidx.y" . 0) ("threadidx.z" . 0)
    ("blockidx.x" . 0)  ("blockidx.y" . 0)  ("blockidx.z" . 0)
    ("blockdim.x" . 1)  ("blockdim.y" . 1)  ("blockdim.z" . 1)
    ("griddim.x" . 1)   ("griddim.y" . 1)   ("griddim.z" . 1)))

(defun %builtin-value (head)
  (cdr (assoc (rosette-gpu-kernel-dsl::%builtin-c-string head)
              *builtin-values* :test #'string-equal)))

(defun eval-expr (form env)
  (cond
    ((integerp form) form)
    ((floatp form) form)
    ((and (symbolp form) (%indexed-p form))
     (multiple-value-bind (base idx) (%decode form)
       (aref (%env-get env base) (truncate (eval-expr idx env)))))
    ((symbolp form) (%env-get env (%cname form)))
    ((not (consp form))
     (error 'dsl-syntax-error :form form :reason "unrecognised expression atom"))
    (t
     (let ((head (first form))
           (args (rest form)))
       (cond
         ((member head rosette-gpu-kernel-dsl::*dsl-cuda-builtins* :test #'%name=)
          (%builtin-value head))
         ((%name= head '+) (%eval-sum args env))
         ((%name= head '*) (%eval-product args env))
         ((%name= head '-) (%eval-difference args env))
         ((%name= head '/) (%eval-quotient args env))
         ((%name= head '%) (%eval-mod args env))
         ((%name= head 'mulmod) (%eval-mulmod args env))
         ((%name= head 'bor)  (logior (logand (truncate (eval-expr (first args) env)) #xffffffff)
                                      (logand (truncate (eval-expr (second args) env)) #xffffffff)))
         ((%name= head 'band) (logand (truncate (eval-expr (first args) env))
                                      (truncate (eval-expr (second args) env)) #xffffffff))
         ((%name= head 'bxor) (logxor (logand (truncate (eval-expr (first args) env)) #xffffffff)
                                      (logand (truncate (eval-expr (second args) env)) #xffffffff)))
         ((%name= head 'shl)  (logand (ash (truncate (eval-expr (first args) env))
                                           (truncate (eval-expr (second args) env))) #xffffffff))
         ((%name= head 'shr)  (ash (logand (truncate (eval-expr (first args) env)) #xffffffff)
                                   (- (truncate (eval-expr (second args) env)))))
         ((%name= head '<)  (< (eval-expr (first args) env) (eval-expr (second args) env)))
         ((%name= head '>)  (> (eval-expr (first args) env) (eval-expr (second args) env)))
         ((%name= head '<=) (<= (eval-expr (first args) env) (eval-expr (second args) env)))
         ((%name= head '>=) (>= (eval-expr (first args) env) (eval-expr (second args) env)))
         ((%name= head '=)  (= (eval-expr (first args) env) (eval-expr (second args) env)))
         ((%name= head '/=) (/= (eval-expr (first args) env) (eval-expr (second args) env)))
         ((%name= head 'half-to-float)
          (%fp16-bits-to-single (logand (truncate (eval-expr (first args) env)) #xffff)))
         ((%name= head 'float-to-half)
          (%single-to-fp16-bits (eval-expr (first args) env)))
         ((%name= head 'call)
          (apply (%call-fn (first args))
                 (%eval-call-args (rest args) env)))
         ;; FETCH-AND-ADD: in expression position atomic-add returns the OLD
         ;; value, then performs the add -- the sequential serialisation the
         ;; device atomic realises in some order.
         ((%name= head 'atomic-add)
          (let ((old (eval-expr (first args) env)))
            (%assign (first args) (+ old (eval-expr (second args) env)) env)
            old))
         (t (error 'dsl-syntax-error :form form :reason "expression head not in DSL catalog")))))))

(defun %fp16-bits-to-single (bits)
  "Decode an IEEE-754 half (BITS, low 16) into a single-float.  Models the device
@llvm.convert.from.fp16 / cvt.f32.f16 -- the quantized-scale primitive."
  (let* ((bits (logand bits #xffff))
         (sign (if (logbitp 15 bits) -1f0 1f0))
         (exp  (ldb (byte 5 10) bits))
         (mant (ldb (byte 10 0) bits)))
    (cond
      ((= exp 0) (* sign (* (/ mant 1024f0) (expt 2f0 -14))))      ; zero / subnormal
      ((= exp 31) (* sign 1f30))                                    ; inf/nan stand-in
      (t (* sign (* (+ 1f0 (/ mant 1024f0)) (expt 2f0 (- exp 15))))))))

(defun %single-to-fp16-bits (value)
  "Round VALUE to IEEE binary16 bits with round-to-nearest-even.

The computation uses the exact rational represented by the input single, so
the interpreter is an independent oracle for LLVM's convert.to.fp16 intrinsic."
  (let ((x (coerce value 'single-float)))
    (cond
      ;; Canonical quiet NaN.  Payload preservation is intentionally outside
      ;; the scalar DSL contract; class and sign-independent quietness remain.
      ((not (= x x)) #x7e00)
      ((> (abs x) most-positive-single-float)
       (logior (if (minusp x) #x8000 0) #x7c00))
      (t
       (let* ((sign (if (minusp (float-sign x)) #x8000 0))
              (magnitude (abs (rational x))))
         (cond
           ((zerop magnitude) sign)
           ;; 65520 is the exact midpoint between max finite half and the
           ;; exponent-overflow result; ties-to-even selects infinity.
           ((>= magnitude 65520) (logior sign #x7c00))
           (t
            (multiple-value-bind (significand exponent ignored-sign)
                (integer-decode-float (abs x))
              (declare (ignore ignored-sign))
              (let ((e (+ exponent (1- (integer-length significand)))))
                (if (>= e -14)
                    (let* ((scaled (* (/ magnitude (expt 2 e)) 1024))
                           (rounded (round scaled))
                           (mantissa (- rounded 1024))
                           (half-exponent (+ e 15)))
                      (when (= mantissa 1024)
                        (setf mantissa 0)
                        (incf half-exponent))
                      (if (>= half-exponent 31)
                          (logior sign #x7c00)
                          (logior sign (ash half-exponent 10) mantissa)))
                    (let ((mantissa (round (* magnitude (expt 2 24)))))
                      (if (>= mantissa 1024)
                          (logior sign #x0400)
                          (logior sign mantissa)))))))))))))

(declaim (inline half-bits-to-single single-to-half-bits))

(defun half-bits-to-single (bits)
  "Decode an IEEE-754 binary16 bit pattern into a single-float.

This is the public host-side semantic twin of the DSL HALF-TO-FLOAT operation,
intended for CPU-side codecs that share the GPU format contract."
  (%fp16-bits-to-single bits))

(defun single-to-half-bits (value)
  "Round VALUE to IEEE-754 binary16 bits using the DSL's RNE oracle."
  (%single-to-fp16-bits value))

;;; --- l-value assignment ---------------------------------------------------

(defun %assign (lval value env)
  (cond
    ((and (symbolp lval) (%indexed-p lval))
     (multiple-value-bind (base idx) (%decode lval)
       (let ((array (%env-get env base)))
         (setf (aref array (truncate (eval-expr idx env)))
               (if (subtypep (array-element-type array) '(unsigned-byte 8))
                   (logand (truncate value) #xff)
                   value)))))
    ((symbolp lval) (%env-set env (%cname lval) value))
    (t (error 'dsl-syntax-error :form lval :reason "invalid l-value"))))

;;; --- statement execution --------------------------------------------------

(defun exec-body (forms env) (dolist (f forms) (exec-stmt f env)))

(defun %exec-loop (header body env)
  (let ((var (%cname (first header)))
        (limit (truncate (eval-expr (second header) env))))
    (dotimes (i limit)
      (%with-binding (env var i)
        (exec-body body env)))))

(defun exec-stmt (form env)
  (cond
    ((not (consp form)) (eval-expr form env))   ; expression-statement
    (t
     (let ((head (first form)))
       (cond
         ((%name= head 'setq)
          (%assign (second form) (eval-expr (third form) env) env))
         ((or (%name= head 'for-grid-stride) (%name= head 'for-range))
          (%exec-loop (second form) (cddr form) env))
         ((%name= head 'progn) (exec-body (rest form) env))
         ((%name= head 'let)
          (%exec-let (second form) (cddr form) env))
         ((%name= head 'if)
          (if (eval-expr (second form) env)
              (exec-stmt (third form) env)
              (when (= 4 (length form)) (exec-stmt (fourth form) env))))
         ((%name= head 'while)
          (loop while (eval-expr (second form) env)
                do (exec-body (cddr form) env)))
         ((%name= head 'cond)
          (dolist (clause (rest form))
            (when (or (eq (first clause) t) (eval-expr (first clause) env))
              (return (exec-body (rest clause) env)))))
         ((%name= head 'case)
          (let ((key (eval-expr (second form) env)))
            (dolist (clause (cddr form))
              (when (or (member (first clause) '(otherwise t) :test #'%name=)
                        (eql (first clause) key))
                (return (exec-body (rest clause) env))))))
         ((%name= head 'atomic-add)
          (%assign (second form)
                   (+ (eval-expr (second form) env) (eval-expr (third form) env))
                   env))
         ((%name= head 'atomic-cas)
          (when (= (eval-expr (second form) env) (eval-expr (third form) env))
            (%assign (second form) (eval-expr (fourth form) env) env)))
         ;; (vload4 (R0 R1 R2 R3) BASE IDX body) -- scalar model of the 128-bit
         ;; load: bind R0..R3 = BASE[IDX..IDX+3], run body.  Bit-identical to the
         ;; device vector load (same 4 contiguous floats).
         ((%name= head 'vload4)
          (let ((vec (%env-get env (%cname (third form))))
                (idx (eval-expr (fourth form) env)))
            (loop for r in (second form) for k from 0
                  do (%env-set env (%cname r) (aref vec (+ idx k))))
            (exec-body (nthcdr 4 form) env)))
         ;; (vstore4 BASE IDX E0 E1 E2 E3)
         ((%name= head 'vstore4)
          (let ((vec (%env-get env (%cname (second form))))
                (idx (eval-expr (third form) env))
                (vals (mapcar (lambda (e) (eval-expr e env)) (cdddr form))))
            (loop for v in vals for k from 0 do (setf (aref vec (+ idx k)) v))))
         (t (eval-expr form env)))))))

(defun %exec-let (bindings body env)
  (let ((keys '()))
    (unwind-protect
         (progn
           (dolist (b bindings)
             (let* ((typed (= 3 (length b)))
                    (var (%cname (first b)))
                    (expr (if typed (third b) (second b))))
               (%env-set env var (eval-expr expr env))
               (push var keys)))
           (exec-body body env))
      (dolist (k keys) (remhash k env)))))

;;; --- entry points ---------------------------------------------------------

(defun interpret-kernel-spec (spec args)
  "Execute KERNEL-SPEC against ARGS (aligned with the param list).  Pointer
params are Lisp vectors mutated in place; scalar params are numbers.  Returns
ARGS so callers can read back the (now-updated) output vectors."
  (unless (kernel-spec-p spec)
    (error 'dsl-syntax-error :form spec :reason "interpret-kernel expects a KERNEL-SPEC"))
  (let ((params (kernel-spec-params spec))
        (env (make-hash-table :test #'equal)))
    (unless (= (length params) (length args))
      (error 'dsl-syntax-error :form args
             :reason (format nil "kernel ~S expects ~D args, got ~D"
                             (kernel-spec-name spec) (length params) (length args))))
    (loop for (name . nil) in params
          for a in args
          do (%env-set env (%cname name) a))
    (exec-stmt (kernel-spec-body spec) env)
    args))

(defun interpret-kernel (spec &rest args)
  "Variadic wrapper around INTERPRET-KERNEL-SPEC."
  (interpret-kernel-spec spec args))
