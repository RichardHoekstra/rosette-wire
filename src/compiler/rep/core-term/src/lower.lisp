;;;; lower.lisp --- coincidence 3 of 3: COMPILE (bridge to the silicon IR).
;;;;
;;;; The SAME core term is lowered to a rosette-gpu-kernel-dsl kernel-spec and run
;;;; BIT-EXACT against the evaluator.  We do NOT reinvent the lowering: the
;;;; core term's function/if/arithmetic fragment is exactly rosette-lisp-codegen's
;;;; integer Lisp, whose COMPILE-PROGRAM assembles bytecode that the proven
;;;; rosette-lisp-codegen /native path already lowers through the kernel-IR VM.
;;;; We reuse that VM AS A KERNEL-SPEC (the *CORE-VM-SPEC* below is the data of
;;;; rosette-lisp-codegen/native's *VM-SPEC*, kept verbatim so we take on no
;;;; LLVM/CUDA dependency) and execute it on the CPU ORACLE
;;;; (interpret-kernel-spec) -- the very oracle every silicon backend is gated
;;;; against.  BRIDGED, not re-derived.
;;;;
;;;; The one genuine step here is CORE -> integer-Lisp: LET is not in the
;;;; kernel IR, so we substitute it away.  The fragment is pure, so
;;;; (let x e b) == b[x := e] semantically; substitution is exact.

(in-package #:rosette-core-term)

;;; --- LET elimination by substitution (pure fragment => exact) ---------------

(defun %free-variable-names (term &optional bound)
  "Return the variable names occurring free in TERM.

Operator/function-name positions and lambda type annotations are not variable
occurrences.  Names are compared exactly as the core language compares them:
by SYMBOL-NAME with STRING=."
  (labels ((walk (node environment)
             (cond
               ((integerp node) nil)
               ((symbolp node)
                (let ((name (symbol-name node)))
                  (unless (member name environment :test #'string=)
                    (list name))))
               ((consp node)
                (let ((head (first node)))
                  (cond
                    ((sym= head "IF")
                     (mapcan (lambda (child) (walk child environment))
                             (rest node)))
                    ((sym= head "LET")
                     (append (walk (third node) environment)
                             (walk (fourth node)
                                   (cons (symbol-name (second node))
                                         environment))))
                    ((sym= head "LAM")
                     (walk (fourth node)
                           (cons (symbol-name (second node)) environment)))
                    ((sym= head "APP")
                     (append (walk (second node) environment)
                             (walk (third node) environment)))
                    ((binop-name head)
                     (append (walk (second node) environment)
                             (walk (third node) environment)))
                    ;; A first-order call's head is a function name, not a
                    ;; value occurrence.
                    (t (mapcan (lambda (child) (walk child environment))
                               (rest node))))))
               (t nil))))
    (remove-duplicates (walk term bound) :test #'string=)))

(defun %all-symbol-names (term)
  "Return every symbol name in TERM, for deterministic fresh-name selection."
  (remove-duplicates
   (labels ((walk (node)
              (cond ((symbolp node) (list (symbol-name node)))
                    ((consp node) (mapcan #'walk node))
                    (t nil))))
     (walk term))
   :test #'string=))

(defun %fresh-variable (term value &rest forbidden-names)
  "Return a deterministic uninterned variable absent from all inputs.

FORBIDDEN-NAMES includes substitution targets that need not occur in TERM or
VALUE but must not become the alpha-renamed binder."
  (let ((used (append (%all-symbol-names term)
                      (%all-symbol-names value)
                      forbidden-names)))
    (loop for index from 0
          for name = (format nil "ROSETTE$ALPHA$~D" index)
          unless (member name used :test #'string=)
            return (make-symbol name))))

(defun %subst (term var val)
  "Capture-avoiding substitution of VAL for free occurrences of VAR in TERM.

VAR is a symbol-name string.  Operator and function-name positions (heads) are
not substituted -- only value positions.  A LET/LAM binder that occurs free in
VAL is alpha-renamed before descent."
  (cond
    ((integerp term) term)
    ((symbolp term) (if (string= (symbol-name term) var) val term))
    ((consp term)
     (let ((head (first term)))
       (cond
         ((sym= head "IF")
          (list head (%subst (second term) var val)
                (%subst (third term) var val)
                (%subst (fourth term) var val)))
         ((sym= head "LET")
          (let* ((binder (second term))
                 (y (symbol-name binder))
                 (value (%subst (third term) var val)))
            (cond
              ;; The inner binding shadows VAR in the body, but not in its
              ;; initializer.
              ((string= y var)
               (list head binder value (fourth term)))
              ;; Substituting a free Y below this binder would capture it.
              ;; Rename the binder first, then perform the requested
              ;; substitution on the alpha-equivalent body.
              ((member y (%free-variable-names val) :test #'string=)
               (let* ((fresh (%fresh-variable term val var))
                      (renamed-body (%subst (fourth term) y fresh)))
                 (list head fresh value (%subst renamed-body var val))))
              (t
               (list head binder value (%subst (fourth term) var val))))))
         ((sym= head "LAM")
          (let* ((binder (second term))
                 (y (symbol-name binder)))
            (cond
              ((string= y var) (list head binder (third term) (fourth term)))
              ((member y (%free-variable-names val) :test #'string=)
               (let* ((fresh (%fresh-variable term val var))
                      (renamed-body (%subst (fourth term) y fresh)))
                 (list head fresh (third term)
                       (%subst renamed-body var val))))
              (t
               (list head binder (third term)
                     (%subst (fourth term) var val))))))
         ((sym= head "APP")
          (list head (%subst (second term) var val) (%subst (third term) var val)))
         ((binop-name head)
          (list head (%subst (second term) var val)
                (%subst (third term) var val)))
         (t                                    ; function call: keep head
          (cons head (mapcar (lambda (a) (%subst a var val)) (rest term)))))))
    (t term)))

(defun strip-lets (term)
  "Return TERM with every LET eliminated by substitution -- a let-free term in
the exact integer-Lisp fragment rosette-lisp-codegen compiles."
  (cond
    ((integerp term) term)
    ((symbolp term) term)
    ((consp term)
     (let ((head (first term)))
       (cond
         ((sym= head "IF")
          (list head (strip-lets (second term))
                (strip-lets (third term)) (strip-lets (fourth term))))
         ((sym= head "LET")
          (strip-lets (%subst (fourth term)
                              (symbol-name (second term))
                              (strip-lets (third term)))))
         ;; APP of a LAM is a beta redex: eliminate it by substitution so the
         ;; lambda layer lowers to first-order integer Lisp when it is
         ;; beta-normalizable.  (A non-redex/residual lambda cannot be lowered.)
         ((sym= head "APP")
          (let ((f (second term)) (a (strip-lets (third term))))
            (unless (and (consp f) (sym= (first f) "LAM"))
              (%type-error term "cannot lower a non-redex application to first-order Lisp"))
            (strip-lets (%subst (fourth f) (symbol-name (second f)) a))))
         ((sym= head "LAM")
          (%type-error term "cannot lower a residual lambda to first-order Lisp"))
         ((binop-name head)
          (list head (strip-lets (second term)) (strip-lets (third term))))
         (t (cons head (mapcar #'strip-lets (rest term)))))))
    (t term)))

(defun lower-to-integer-lisp (program)
  "Lower PROGRAM to rosette-lisp-codegen integer Lisp.  Returns (values DEFS MAIN),
DEFS = list of (name (params) let-free-body), MAIN = let-free main term."
  (term-check program)
  (values
   (loop for (name params body) in (core-program-funs program)
         collect (list name params (strip-lets body)))
   (strip-lets (core-program-main program))))

(defun lower-to-program (program)
  "Lower PROGRAM all the way to a rosette-lisp-codegen PROGRAM (bytecode)."
  (multiple-value-bind (defs main) (lower-to-integer-lisp program)
    (compile-program defs main)))

;;; --- the kernel-IR VM as a KERNEL-SPEC (the silicon floor) ------------------
;;; Verbatim data of rosette-lisp-codegen/native's *VM-SPEC*: the stack machine as
;;; ONE rosette-gpu-kernel-dsl WHILE-loop kernel (fetch-decode-execute; operand +
;;; call stacks in int arrays).  Kept here as data so this lib depends only on
;;; the kernel-IR + its CPU interpreter, taking on no LLVM/CUDA toolchain.

(defparameter *core-vm-spec*
  (make-kernel-spec :name 'rosette-core-term-vm
    :params '(code :const-int* fe :const-int* fa :const-int*
              ops :int* cs :int* out :int* entry :int)
    :body
    '(let ((pc :int entry) (sp :int 0) (fp :int 0) (csp :int 0) (running :int 1) (z :int 0))
       (while (= running 1)
         (let ((pc1 :int (+ pc 1)))
           (let ((op :int code[pc]) (arg :int code[pc1]))
             (if (= op 0)
                 (let ((shalt :int (- sp 1)))
                   (setq out[z] ops[shalt]) (setq running 0))
                 (if (= op 1)
                     (progn (setq ops[sp] arg) (setq sp (+ sp 1)) (setq pc (+ pc 2)))
                     (if (= op 2)
                         (let ((li :int (+ fp arg)))
                           (setq ops[sp] ops[li]) (setq sp (+ sp 1)) (setq pc (+ pc 2)))
                         (if (< op 11)
                             (let ((sa :int (- sp 2)))
                               (let ((sb :int (- sp 1)))
                                 (let ((va :int (/ ops[sa] 8)))
                                   (let ((vb :int (/ ops[sb] 8)))
                                     (let ((res :int 0))
                                       (if (= op 3) (setq res (+ va vb))
                                           (if (= op 4) (setq res (- va vb))
                                               (if (= op 5) (setq res (* va vb))
                                                   (if (= op 6) (setq res (/ va vb))
                                                       (if (= op 7) (setq res (% va vb))
                                                           (if (= op 8) (if (< va vb) (setq res 1) (setq res 0))
                                                               (if (= op 9) (if (> va vb) (setq res 1) (setq res 0))
                                                                   (if (= va vb) (setq res 1) (setq res 0)))))))))
                                       (setq ops[sa] (* res 8))
                                       (setq sp (- sp 1))
                                       (setq pc (+ pc 2)))))))
                             (if (= op 11)
                                 (setq pc arg)
                                 (if (= op 12)
                                     (let ((sj :int (- sp 1)))
                                       (setq sp sj)
                                       (if (= ops[sj] 0) (setq pc arg) (setq pc (+ pc 2))))
                                     (if (= op 13)
                                         (let ((k :int fa[arg]))
                                           (let ((en :int fe[arg]))
                                             (setq cs[csp] (+ pc 2))
                                             (let ((csp1 :int (+ csp 1)))
                                               (setq cs[csp1] fp))
                                             (setq csp (+ csp 2))
                                             (setq fp (- sp k))
                                             (setq pc en)))
                                         (let ((sr :int (- sp 1)))
                                           (let ((rv :int ops[sr]))
                                             (setq csp (- csp 2))
                                             (let ((rpc :int cs[csp]))
                                               (let ((csp1b :int (+ csp 1)))
                                                 (let ((ofp :int cs[csp1b]))
                                                   (setq sp fp)
                                                   (setq ops[sp] rv)
                                                   (setq sp (+ sp 1))
                                                   (setq fp ofp)
                                                   (setq pc rpc))))))))))))))))))
  "The rosette-lisp-codegen stack VM as ONE rosette-gpu-kernel-dsl kernel-spec.")

;;; --- run a lowered program on the CPU oracle -------------------------------

;;; --- i32-fixnum regime: the tagged-fixnum VM word is 3 bits of tag + value,
;;; laid in a 32-bit machine word, so the FAITHFUL range is [-2^28, 2^28)
;;; (value*8 must fit a signed-byte-32).  Outside it the CPU oracle is out of
;;; scope: it does NOT wrap like real i32 silicon (the wrap semantics live in the
;;; shared interpret-kernel-spec, which core-term does not own).  Left unguarded,
;;; an out-of-range program would either SILENTLY corrupt (a literal whose tagged
;;; word overflows gets masked by %TO-I32) or CRASH with a raw TYPE-ERROR (an
;;; intermediate result too big to store into the (signed-byte 32) VM arrays).
;;; We DETECT both and refuse cleanly via CORE-VM-I32-RANGE-ERROR -- no silent
;;; corruption, no crash advertised as wraparound.

(defconstant +core-vm-value-limit+ (ash 1 28)
  "Faithful value bound for the tagged-fixnum CPU oracle: a value V is
representable iff (- +CORE-VM-VALUE-LIMIT+) <= V < +CORE-VM-VALUE-LIMIT+,
i.e. V*8 fits a signed 32-bit machine word (3 tag bits + value in one i32).")

(define-condition core-vm-i32-range-error (error)
  ((detail :initarg :detail :reader core-vm-i32-range-error-detail :initform nil))
  (:report (lambda (c s)
             (format s "rosette-core-term: value outside the i32 tagged-fixnum ~
oracle range (must be in [-2^28, 2^28))~@[: ~A~]"
                     (core-vm-i32-range-error-detail c))))
  (:documentation "Signalled when a lowered program would push, or compute, a
value the tagged-fixnum i32 oracle cannot represent faithfully.  Refusing is
honest: the oracle is bit-identical to silicon only on [-2^28, 2^28)."))

(defun %i32-word-faithful-p (w)
  "T iff the u64 machine word W already IS a faithful signed-32-bit two's-
complement value -- its bits 32..63 are a pure sign-extension of bit 31, so
masking to i32 (via %TO-I32) loses nothing.  A tagged fixnum of an in-range
value (small positive, or a small negative laid near 2^64) passes; an out-of-
range constant lands in the forbidden middle band and fails."
  (or (< w #x80000000)               ; small non-negative i32
      (>= w #xffffffff80000000)))    ; small negative i32 (sign-extended u64)

(defun %to-i32 (w)
  "Reinterpret machine word W (an unsigned tagged-fixnum word from mk-fixnum,
where a negative encodes as a large u64) as a signed 32-bit value.  W is
required to already be a faithful i32 word (see %I32-WORD-FAITHFUL-P); the caller
guards the range so this never silently masks away information."
  (let ((m (logand w #xffffffff)))
    (if (>= m #x80000000) (- m #x100000000) m)))

(defun %i32 (seq)
  ;; Refuse (loudly, cleanly) any word that is NOT already a faithful i32 -- e.g.
  ;; a PUSHC constant whose tagged value overflows i32 -- instead of silently
  ;; masking it to a corrupt value.
  (let ((bad (find-if-not #'%i32-word-faithful-p seq)))
    (when bad
      (error 'core-vm-i32-range-error
             :detail (format nil "constant word ~A exceeds the i32 tagged-fixnum range" bad))))
  (make-array (length seq) :element-type '(signed-byte 32)
              :initial-contents (map 'list #'%to-i32 seq)))
(defun %zeros (n)
  (make-array n :element-type '(signed-byte 32) :initial-element 0))

(defun make-core-vm-invocation
    (rosette-program &key (ops-size 200000) (call-size 100000) corrupt)
  "Return (values KERNEL-SPEC ARGS) for a lowered core-term VM PROGRAM.

ARGS is the public rosette-gpu-kernel-dsl calling convention aligned with
*CORE-VM-SPEC*: `(code fe fa ops cs out entry)`.  Tagged constants are range
checked and converted to faithful signed i32 words here, once, so downstream
backend agreement gates never need to duplicate the VM packing logic.  CORRUPT
is the existing negative-gate injection and damages a private code copy."
  (unless (and (integerp ops-size) (plusp ops-size))
    (error "OPS-SIZE must be a positive integer, got ~S." ops-size))
  (unless (and (integerp call-size) (plusp call-size))
    (error "CALL-SIZE must be a positive integer, got ~S." call-size))
  (let* ((raw (copy-seq (program-code rosette-program)))
         (code (progn (when corrupt (%corrupt-code! raw)) (%i32 raw)))
         (fe (%i32 (program-fn-entry rosette-program)))
         (fa (%i32 (program-fn-arity rosette-program)))
         (ops (%zeros ops-size))
         (cs (%zeros call-size))
         (out (%zeros 1)))
    (values *core-vm-spec*
            (list code fe fa ops cs out (program-entry rosette-program)))))

(defun core-vm-invocation-result (args)
  "Decode the integer result from ARGS after a core VM backend has run."
  (unless (and (listp args) (= (length args) 7)
               (vectorp (sixth args)) (plusp (length (sixth args))))
    (error "Malformed core VM invocation arguments: ~S" args))
  (truncate (aref (sixth args) 0) 8))

(defun oracle-run (rosette-program &key (ops-size 200000) (call-size 100000) corrupt)
  "Run a lowered rosette-lisp-codegen PROGRAM on the kernel-IR VM through the CPU
oracle (interpret-kernel-spec) and return the integer result.  When CORRUPT is
non-nil the code stream is deliberately damaged (first arithmetic opcode ADD->
SUB, else first PUSHC constant bumped) so the bit-check can be shown to have
teeth."
  (multiple-value-bind (spec args)
      (make-core-vm-invocation rosette-program
                               :ops-size ops-size
                               :call-size call-size
                               :corrupt corrupt)
    ;; An INTERMEDIATE result too big for a (signed-byte 32) VM array store
    ;; raises a raw TYPE-ERROR from interpret-kernel-spec; convert it into the
    ;; clean range condition -- the oracle is out of scope beyond
    ;; [-2^28, 2^28),
    ;; refused rather than crashed or silently wrapped.
    (handler-case
        (interpret-kernel-spec spec args)
      (type-error (e)
        (error 'core-vm-i32-range-error
               :detail (format nil "intermediate result overflowed the i32 VM (~A)"
                               (type-error-datum e)))))
    (core-vm-invocation-result args)))

(defun %corrupt-code! (code)
  "Damage one instruction of CODE in place (opcode at even indices)."
  (loop for i from 0 below (1- (length code)) by 2
        for op = (aref code i)
        when (= op 3)                        ; +op-add+ -> +op-sub+
          do (setf (aref code i) 4) (return-from %corrupt-code! code))
  (loop for i from 0 below (1- (length code)) by 2
        when (= (aref code i) 1)             ; +op-pushc+ : bump the constant
          do (incf (aref code (1+ i)) 8) (return-from %corrupt-code! code))
  code)

;;; --- the acceptance instrument: eval == oracle across an input battery -----

(defun battery-check (funs entry-name inputs &key corrupt)
  "For each integer x in INPUTS, build MAIN = (ENTRY-NAME x), then compare the
DIRECT EVALUATOR against the CPU-oracle-of-the-lowered-kernel.  Returns a list
of plists (:input :eval :oracle :match).  With CORRUPT non-nil the lowering is
damaged, so a green run here means the bit-check FAILED to catch it (used by
the negative gate)."
  (let ((entry-sym (or (first (find (string entry-name) funs
                                    :key (lambda (f) (symbol-name (first f)))
                                    :test #'string=))
                       (error "battery-check: no function ~A" entry-name))))
   (loop for x in inputs
        for prog = (make-core-program
                    :funs funs
                    :main (list entry-sym x))
        for ev = (program-eval prog)
        for or* = (oracle-run (lower-to-program prog) :corrupt corrupt)
        collect (list :input x :eval ev :oracle or* :match (eql ev or*)))))
