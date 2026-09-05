;;;; backend-types.lisp --- shared int/float type discipline for the portable
;;;; lowering backends (LLVM / WASM / WGSL).
;;;;
;;;; The typed-expression lowering -- infer :i32 vs :float, map DSL param and
;;;; let-binding types, and register local-variable types by a scoped walk --
;;;; recurred verbatim across three backends.  It is target-INDEPENDENT
;;;; analysis (string emission stays per-backend), so it lives here once and
;;;; the backends call it through thin adapters.  See the per-backend
;;;; `*...-tenv*` specials and `*...-float-calls*` tables for the parts that
;;;; genuinely differ (WASM has only native-f32 libm ops; WGSL adds
;;;; transcendentals; LLVM keeps its own inline (code,type) lowering and only
;;;; shares PARAM-INTERNAL-TYPE).

(in-package #:rosette-gpu-kernel-dsl)

(defparameter *dsl-float-call-names*
  '("sqrtf" "sqrt" "expf" "exp" "logf" "log" "sinf" "sin"
    "cosf" "cos" "tanhf" "tanh" "fabsf" "fabs" "fmaxf" "fmax"
    "fminf" "fmin" "floorf" "floor" "ceilf" "ceil" "rintf" "rint"
    "roundevenf" "roundeven" "powf" "pow" "max" "min" "abs")
  "Common float-returning CALL spellings used by shared type validation.

Backends may pass a narrower predicate when their executable call catalog is a
proper subset.  Overloaded min/max/abs remain integer when no operand is f32;
INFER-DSL-TYPE applies that operand check independently.")

(defun dsl-float-call-p (cname)
  (and (member cname *dsl-float-call-names* :test #'string=) t))

(defun dsl-overloaded-numeric-call-p (cname)
  "True for CALL spellings whose result follows their operand domain.

The portable integer backends intentionally accept the C-flavoured aliases as
integer min/max/abs, plus non-negative constant-exponent pow.  Their float
overload is selected only by an f32 operand; other libm spellings are always
float-returning."
  (and (member cname
               '("fmax" "fmaxf" "max" "fmin" "fminf" "min"
                 "fabs" "fabsf" "abs" "pow" "powf")
               :test #'string=)
       t))

(defun param-internal-type (dsl-type)
  "Internal VALUE type (:i32 / :i64 / :float) of a DSL param; pointers map to
the value their indexed load yields.  :uint8* yields i32 (a zero-extended
byte)."
  (ecase dsl-type
    ((:int) :i32) ((:int64) :i64) ((:float) :float)
    ((:int* :const-int*) :i32) ((:float* :const-float*) :float)
    ((:int64* :const-int64*) :i64)
    ((:uint8* :const-uint8*) :i32)))

(defun dsl-builtin-form-p (head)
  (member head *dsl-cuda-builtins* :test #'%sym-name=))

(declaim (ftype function infer-dsl-type))

(defun dsl-let-internal-type (binding tenv float-call-p)
  "Internal type of a LET binding: its typed tag, else inferred from the init.
:bool is treated as :i32 (the WASM/WGSL convention; LLVM types bools itself)."
  (if (= 3 (length binding))
      (ecase (second binding)
        ((:int :uint32 :bool) :i32) ((:int64) :i64) ((:float) :float))
      (infer-dsl-type (second binding) tenv :float-call-p float-call-p)))

(defun infer-dsl-type (form tenv &key float-call-p)
  "Infer the internal type (:i32 / :i64 / :float) of a DSL expression. TENV maps
c-name strings -> internal type (params; the grid/loop index, registered as
:i32 or :i64; and locals).  FLOAT-CALL-P, if supplied, is a predicate on a call's
c-name string deciding whether that call returns float.  Comparisons report
:i32 (the WASM/WGSL stack-bool convention)."
  (cond
    ((integerp form) (if (typep form '(signed-byte 32)) :i32 :i64))
    ((floatp form) :float)
    ((and (symbolp form) (%indexed-symbol-p form))
     (multiple-value-bind (base idx) (%decode-indexed-symbol form)
       (declare (ignore idx))
       (or (gethash base tenv) :i32)))
    ((symbolp form) (or (gethash (%symbol-c-name form) tenv) :i32))
    ((not (consp form)) :i32)
    (t
     (let ((head (first form)) (args (rest form)))
       (cond
         ((dsl-builtin-form-p head) :i32)
         ((member head '(+ - * /) :test #'%sym-name=)
          (let ((types (mapcar (lambda (a)
                                 (infer-dsl-type a tenv :float-call-p float-call-p))
                               args)))
            (cond ((member :float types) :float)
                  ((member :i64 types) :i64)
                  (t :i32))))
         ((%sym-name= head '%)
          (if (some (lambda (a)
                      (eq (infer-dsl-type a tenv :float-call-p float-call-p) :i64))
                    args)
              :i64 :i32))
         ((member head '(band bor bxor) :test #'%sym-name=)
          (if (some (lambda (a)
                      (eq (infer-dsl-type a tenv :float-call-p float-call-p) :i64))
                    args)
              :i64 :i32))
         ((member head '(shl shr) :test #'%sym-name=)
          ;; The shift count does not widen the shifted value.
          (if (eq (infer-dsl-type (first args) tenv
                                  :float-call-p float-call-p)
                  :i64)
              :i64 :i32))
         ;; PTX POPC always produces a 32-bit count, even for a b64 source.
         ((%sym-name= head 'popcount) :i32)
         ;; MULHI is the high half of a promoted unsigned 64x64 product.
         ((%sym-name= head 'mulhi) :i64)
         ;; MULMOD's public contract is deliberately the existing u32 path.
         ((%sym-name= head 'mulmod) :i32)
         ((%sym-name= head 'half-to-float) :float)
         ((%sym-name= head 'float-to-half) :i32)
         ;; CUDA/NVPTX shuffle transports one register word without changing
         ;; its interpretation.  Keep the value type; the selector is always
         ;; i32.  The currently supported payloads are f32 and i32/b32 (wide
         ;; values must be split explicitly so register pressure stays visible).
         ((member head '(shfl-down shfl-index) :test #'%sym-name=)
          (infer-dsl-type (first args) tenv :float-call-p float-call-p))
         ((member head '(< > <= >= = /=) :test #'%sym-name=) :i32)
         ((%sym-name= head 'call)
          (let* ((cname (%symbol-c-name (first args)))
                 (arg-types
                  (mapcar (lambda (a)
                            (infer-dsl-type a tenv :float-call-p float-call-p))
                          (rest args))))
            (cond
              ;; Explicit float calls return f32 independently of the source
              ;; operand spelling.  Only the unqualified numeric overloads
              ;; MAX/MIN/ABS select their result domain from their operands.
              ((and float-call-p
                    (funcall float-call-p cname)
                    (or (not (dsl-overloaded-numeric-call-p cname))
                        (member :float arg-types)))
               :float)
              ((member :i64 arg-types) :i64)
              (t :i32))))
         (t :i32))))))

(defun register-dsl-local-types (stmt tenv &key float-call-p)
  "Populate TENV with each local's internal type (LET / for-range / scalar
setq), in source order so an init expression sees the outer locals."
  (labels ((reg (sym ty) (setf (gethash (%symbol-c-name sym) tenv) ty))
           (walk (form)
             (when (consp form)
               (let ((head (first form)))
                 (cond
                   ((and (%sym-name= head 'setq) (symbolp (second form))
                         (not (%indexed-symbol-p (second form))))
                    (unless (gethash (%symbol-c-name (second form)) tenv)
                      (reg (second form)
                           (infer-dsl-type (third form) tenv :float-call-p float-call-p))))
                   ((%sym-name= head 'let)
                    (dolist (b (second form))
                      (reg (first b) (dsl-let-internal-type b tenv float-call-p)))
                    (dolist (sub (cddr form)) (walk sub)))
                   ((or (%sym-name= head 'for-range)
                        (%sym-name= head 'for-grid-stride))
                    (let* ((header (second form))
                           (limit-type
                             (infer-dsl-type (second header) tenv
                                             :float-call-p float-call-p)))
                      (reg (first header) (if (eq limit-type :i64) :i64 :i32)))
                    (dolist (sub (cddr form)) (walk sub)))
                   ((%sym-name= head 'while)
                    (dolist (sub (cddr form)) (walk sub)))
                   ((%sym-name= head 'cond)
                    (dolist (cl (rest form)) (dolist (sub (rest cl)) (walk sub))))
                   ((%sym-name= head 'case)
                    (dolist (cl (cddr form)) (dolist (sub (rest cl)) (walk sub))))
                   ((%sym-name= head 'progn) (dolist (sub (rest form)) (walk sub)))
                   ((%sym-name= head 'if)
                    (walk (third form))
                    (when (= 4 (length form)) (walk (fourth form)))))))))
    (walk stmt)))

(defun kernel-spec-type-environment
    (spec &key (float-call-p #'dsl-float-call-p))
  "Return the shared internal type environment for kernel SPEC."
  (let ((tenv (make-hash-table :test #'equal)))
    (dolist (param (kernel-spec-params spec))
      (setf (gethash (%symbol-c-name (first param)) tenv)
            (param-internal-type (getf (rest param) :type))))
    (register-dsl-local-types
     (kernel-spec-body spec) tenv :float-call-p float-call-p)
    tenv))

(defun ensure-dsl-shuffle-types
    (form tenv &key (float-call-p #'dsl-float-call-p))
  "Validate one SHFL-DOWN/SHFL-INDEX FORM and return its payload type.

The portable register-word contract admits exactly f32 and i32/b32 payloads;
the offset/source-lane selector is i32.  Wide values must be split explicitly.
This is the one type-policy function used by every shuffle-capable backend."
  (unless (and (consp form)
               (member (first form) '(shfl-down shfl-index)
                       :test #'%sym-name=)
               (= 3 (length form)))
    (error 'dsl-syntax-error :form form
           :reason "warp shuffle expects VALUE and one i32 selector"))
  (let ((payload-type
          (infer-dsl-type (second form) tenv :float-call-p float-call-p))
        (selector-type
          (infer-dsl-type (third form) tenv :float-call-p float-call-p)))
    (unless (member payload-type '(:float :i32))
      (error 'dsl-syntax-error :form form
             :reason "warp shuffle payload must be f32 or i32/b32; split wide values explicitly"))
    (unless (eq selector-type :i32)
      (error 'dsl-syntax-error :form form
             :reason "warp shuffle offset/source-lane selector must be i32"))
    payload-type))

(defstruct (kernel-shuffle-analysis
             (:constructor %make-kernel-shuffle-analysis))
  "Lexical type receipt for the warp collectives in one KERNEL-SPEC.

LEGACY-TYPE-ENVIRONMENT preserves the existing whole-kernel map for consumers
that still use it for non-collective details such as grid-index width.
EXPRESSION-TYPES and SHUFFLE-TYPES are keyed by the identity of the source cons,
so two lexically distinct occurrences with the same printed variable name can
never overwrite one another."
  legacy-type-environment
  (expression-types (make-hash-table :test #'eq))
  (shuffle-types (make-hash-table :test #'eq))
  (shuffle-count 0 :type fixnum)
  (shuffle-bearing-p nil))

(defun kernel-analyzed-expression-type (analysis form &optional errorp)
  "Return FORM's semantic type from lexical shuffle ANALYSIS.

When ERRORP is true, signal DSL-SYNTAX-ERROR instead of returning NIL for an
expression outside the analyzed shuffle-bearing tree."
  (multiple-value-bind (type present)
      (gethash form (kernel-shuffle-analysis-expression-types analysis))
    (cond (present type)
          (errorp
           (error 'dsl-syntax-error :form form
                  :reason "expression has no lexical type-analysis receipt"))
          (t nil))))

(defun kernel-shuffle-type (analysis form)
  "Return the exact lexical f32/i32 carrier recorded for shuffle FORM."
  (multiple-value-bind (type present)
      (gethash form (kernel-shuffle-analysis-shuffle-types analysis))
    (unless present
      (error 'dsl-syntax-error :form form
             :reason "warp shuffle has no lexical type-analysis receipt"))
    type))

(defun %copy-dsl-type-environment (environment)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (name type) (setf (gethash name copy) type)) environment)
    copy))

(defun %default-dsl-clause-p (value)
  (or (eq value t)
      (and (symbolp value)
           (or (%sym-name= value 't) (%sym-name= value 'otherwise)))))

(defun %shuffle-in-expression-p (form)
  "Syntax-directed shuffle search in expression position."
  (cond
    ((and (symbolp form) (%indexed-symbol-p form))
     (multiple-value-bind (base index) (%decode-indexed-symbol form)
       (declare (ignore base))
       (%shuffle-in-expression-p index)))
    ((not (consp form)) nil)
    ((member (first form) '(shfl-down shfl-index) :test #'%sym-name=) t)
    ((%sym-name= (first form) 'call)
     (some #'%shuffle-in-expression-p (cddr form)))
    (t (some #'%shuffle-in-expression-p (rest form)))))

(defun %shuffle-in-statement-p (form)
  "Syntax-directed shuffle search in statement position.

Unlike a generic tree walk, this visits the first LET initializer and COND test
while never interpreting binding records or CASE keys as executable forms."
  (cond
    ((not (consp form)) (%shuffle-in-expression-p form))
    (t
     (let ((head (first form)))
       (cond
         ((%sym-name= head 'setq)
          (or (and (symbolp (second form)) (%indexed-symbol-p (second form))
                   (multiple-value-bind (base index)
                       (%decode-indexed-symbol (second form))
                     (declare (ignore base))
                     (%shuffle-in-expression-p index)))
              (%shuffle-in-expression-p (third form))))
         ((or (%sym-name= head 'for-grid-stride)
              (%sym-name= head 'for-range))
          (or (%shuffle-in-expression-p (second (second form)))
              (some #'%shuffle-in-statement-p (cddr form))))
         ((%sym-name= head 'while)
          (or (%shuffle-in-expression-p (second form))
              (some #'%shuffle-in-statement-p (cddr form))))
         ((%sym-name= head 'progn)
          (some #'%shuffle-in-statement-p (rest form)))
         ((%sym-name= head 'let)
          (or (some (lambda (binding)
                      (%shuffle-in-expression-p
                       (if (= 3 (length binding))
                           (third binding) (second binding))))
                    (second form))
              (some #'%shuffle-in-statement-p (cddr form))))
         ((%sym-name= head 'if)
          (or (%shuffle-in-expression-p (second form))
              (some #'%shuffle-in-statement-p (cddr form))))
         ((%sym-name= head 'cond)
          (some (lambda (clause)
                  (or (and (not (%default-dsl-clause-p (first clause)))
                           (%shuffle-in-expression-p (first clause)))
                      (some #'%shuffle-in-statement-p (rest clause))))
                (rest form)))
         ((%sym-name= head 'case)
          (or (%shuffle-in-expression-p (second form))
              (some (lambda (clause)
                      (some #'%shuffle-in-statement-p (rest clause)))
                    (cddr form))))
         ((%sym-name= head 'atomic-add)
          (or (%shuffle-in-expression-p (second form))
              (%shuffle-in-expression-p (third form))))
         ((%sym-name= head 'atomic-cas)
          (some #'%shuffle-in-expression-p (rest form)))
         ((%sym-name= head 'vload4)
          (or (%shuffle-in-expression-p (fourth form))
              (some #'%shuffle-in-statement-p (nthcdr 4 form))))
         ((%sym-name= head 'vstore4)
          (or (%shuffle-in-expression-p (third form))
              (some #'%shuffle-in-expression-p (cdddr form))))
         ((or (%sym-name= head 'shared-array) (%sym-name= head 'sync)) nil)
         (t (%shuffle-in-expression-p form)))))))

(defun %expression-has-overloaded-call-p (form)
  (cond
    ((and (symbolp form) (%indexed-symbol-p form))
     (multiple-value-bind (base index) (%decode-indexed-symbol form)
       (declare (ignore base))
       (%expression-has-overloaded-call-p index)))
    ((not (consp form)) nil)
    ((%sym-name= (first form) 'call)
     (or (and (symbolp (second form))
              (dsl-overloaded-numeric-call-p
               (%symbol-c-name (second form))))
         (some #'%expression-has-overloaded-call-p (cddr form))))
    (t (some #'%expression-has-overloaded-call-p (rest form)))))

(defun %semantic-let-type (binding)
  "Canonical semantic type of a DSL LET binder.

The documented untyped form defaults to f32.  This deliberately does not reuse
the legacy portable-backend inference, whose historical integer auto-typing is
kept outside the shuffle-bearing tranche until those backends migrate."
  (if (= 3 (length binding))
      (ecase (second binding)
        ((:int :uint32) :i32)
        (:int64 :i64)
        (:float :float)
        (:bool :bool))
      :float))

(defun %semantic-shared-type (dsl-type)
  (ecase dsl-type
    (:float :float)
    ((:int :uint32) :i32)
    (:double :f64)))

(defun %semantic-numeric-type (types form)
  (when (member :bool types)
    (error 'dsl-syntax-error :form form
           :reason "boolean value must be materialized explicitly before numeric use"))
  (cond ((member :float types) :float)
        ((member :f64 types) :f64)
        ((member :i64 types) :i64)
        (t :i32)))

(defun %analyze-kernel-shuffles (spec legacy-environment)
  "Build the strict lexical type receipt for a shuffle-bearing SPEC."
  (let* ((analysis
           (%make-kernel-shuffle-analysis
            :legacy-type-environment legacy-environment
            :shuffle-bearing-p t))
         (expression-types (kernel-shuffle-analysis-expression-types analysis))
         (shuffle-types (kernel-shuffle-analysis-shuffle-types analysis)))
    (labels
        ((lookup (environment name form)
           (multiple-value-bind (type present) (gethash name environment)
             (unless present
               (error 'dsl-syntax-error :form form
                      :reason (format nil "unbound identifier ~A in shuffle-bearing kernel"
                                      name)))
             type))
         (record (form type)
           (when (consp form) (setf (gethash form expression-types) type))
           type)
         (bind (environment symbol type form)
           (let ((name (%symbol-c-name symbol)))
             ;; Printed names remain the allocation identity in CUDA/direct
             ;; PTX, so simultaneous lexical shadowing is not representable
             ;; yet.  A copied child environment makes this an active-scope
             ;; check: disjoint generated stages may safely reuse a loop name.
             (when (nth-value 1 (gethash name environment))
               (error 'dsl-syntax-error :form form
                      :reason (format nil
                                      "binder ~A shadows an active binding in a shuffle-bearing kernel; alpha-renaming is required"
                                      name)))
             (setf (gethash name environment) type)
             environment))
         (indexed-type (symbol environment)
           (multiple-value-bind (base index) (%decode-indexed-symbol symbol)
             (expr index environment)
             (lookup environment base symbol)))
         (call-type (form environment)
           (let* ((name (%symbol-c-name (second form)))
                  (types (mapcar (lambda (arg) (expr arg environment))
                                 (cddr form))))
             (cond
               ((and (dsl-float-call-p name)
                     (or (not (dsl-overloaded-numeric-call-p name))
                         (member :float types)))
                :float)
               ((member :i64 types) :i64)
               ((member :bool types)
                (error 'dsl-syntax-error :form form
                       :reason "boolean CALL operands need an explicit materialization"))
               (t :i32))))
         (expr (form environment)
           (cond
             ((integerp form) (if (typep form '(signed-byte 32)) :i32 :i64))
             ((floatp form) :float)
             ((and (symbolp form) (%indexed-symbol-p form))
              (indexed-type form environment))
             ((symbolp form)
              (lookup environment (%symbol-c-name form) form))
             ((not (consp form))
              (error 'dsl-syntax-error :form form
                     :reason "unrecognized expression in shuffle-bearing kernel"))
             (t
              (let ((head (first form)) (args (rest form)))
                (record
                 form
                 (cond
                   ((dsl-builtin-form-p head) :i32)
                   ((member head '(+ - * /) :test #'%sym-name=)
                    (%semantic-numeric-type
                     (mapcar (lambda (arg) (expr arg environment)) args) form))
                   ((%sym-name= head '%)
                    (%semantic-numeric-type
                     (mapcar (lambda (arg) (expr arg environment)) args) form))
                   ((member head '(band bor bxor) :test #'%sym-name=)
                    (let ((types (mapcar (lambda (arg) (expr arg environment)) args)))
                      (when (or (member :float types) (member :f64 types)
                                (member :bool types))
                        (error 'dsl-syntax-error :form form
                               :reason "bit operation requires an i32/i64 value"))
                      (if (member :i64 types) :i64 :i32)))
                   ((member head '(shl shr) :test #'%sym-name=)
                    (let ((value-type (expr (first args) environment))
                          (count-type (expr (second args) environment)))
                      (unless (and (member value-type '(:i32 :i64))
                                   (member count-type '(:i32 :i64)))
                        (error 'dsl-syntax-error :form form
                               :reason "shift requires integer value and count"))
                      value-type))
                   ((%sym-name= head 'popcount)
                    (expr (first args) environment)
                    :i32)
                   ((%sym-name= head 'mulhi)
                    (mapc (lambda (arg) (expr arg environment)) args)
                    :i64)
                   ((%sym-name= head 'mulmod)
                    (mapc (lambda (arg) (expr arg environment)) args)
                    :i32)
                   ((%sym-name= head 'half-to-float)
                    (expr (first args) environment)
                    :float)
                   ((%sym-name= head 'float-to-half)
                    (expr (first args) environment)
                    :i32)
                   ((member head '(shfl-down shfl-index) :test #'%sym-name=)
                    (unless (= 2 (length args))
                      (error 'dsl-syntax-error :form form
                             :reason "warp shuffle expects VALUE and one i32 selector"))
                    (when (or (%expression-has-overloaded-call-p (first args))
                              (%expression-has-overloaded-call-p (second args)))
                      (error 'dsl-syntax-error :form form
                             :reason "overloaded CALL is not yet an unambiguous warp-shuffle carrier"))
                    (let ((payload-type (expr (first args) environment))
                          (selector-type (expr (second args) environment)))
                      (unless (member payload-type '(:float :i32))
                        (error 'dsl-syntax-error :form form
                               :reason "warp shuffle payload must be f32 or a true i32/b32 word"))
                      (unless (eq selector-type :i32)
                        (error 'dsl-syntax-error :form form
                               :reason "warp shuffle offset/source-lane selector must be a true i32 value"))
                      (setf (gethash form shuffle-types) payload-type)
                      (incf (kernel-shuffle-analysis-shuffle-count analysis))
                      payload-type))
                   ((member head '(< > <= >= = /=) :test #'%sym-name=)
                    (mapc (lambda (arg) (expr arg environment)) args)
                    :bool)
                   ((%sym-name= head 'call) (call-type form environment))
                   (t
                    (error 'dsl-syntax-error :form form
                           :reason "expression head has no lexical type rule"))))))))
         (lvalue (form environment)
           (cond
             ((and (symbolp form) (%indexed-symbol-p form))
              (indexed-type form environment))
             ((symbolp form)
              (lookup environment (%symbol-c-name form) form))
             (t (error 'dsl-syntax-error :form form :reason "invalid DSL l-value"))))
         (body (forms environment)
           (dolist (form forms environment)
             (setf environment (stmt form environment))))
         (stmt (form environment)
           (cond
             ((not (consp form)) (expr form environment) environment)
             (t
              (let ((head (first form)))
                (cond
                  ((%sym-name= head 'setq)
                   (lvalue (second form) environment)
                   (expr (third form) environment)
                   environment)
                  ((or (%sym-name= head 'for-grid-stride)
                       (%sym-name= head 'for-range))
                   (let* ((header (second form))
                          (limit-type (expr (second header) environment))
                          (inner (%copy-dsl-type-environment environment)))
                     (bind inner (first header)
                           (if (eq limit-type :i64) :i64 :i32) header)
                     (body (cddr form) inner)
                     environment))
                  ((%sym-name= head 'while)
                   (expr (second form) environment)
                   (body (cddr form) (%copy-dsl-type-environment environment))
                   environment)
                  ((%sym-name= head 'progn)
                   (body (rest form) environment))
                  ((%sym-name= head 'let)
                   (let ((inner (%copy-dsl-type-environment environment)))
                     (dolist (binding (second form))
                       ;; LET is sequential in the executable DSL: evaluate the
                       ;; initializer before publishing this binder, while prior
                       ;; bindings in the same list remain visible.
                       (expr (if (= 3 (length binding))
                                 (third binding) (second binding))
                             inner)
                       (bind inner (first binding)
                             (%semantic-let-type binding) binding))
                     (body (cddr form) inner)
                     environment))
                  ((%sym-name= head 'if)
                   (expr (second form) environment)
                   (stmt (third form) (%copy-dsl-type-environment environment))
                   (when (= 4 (length form))
                     (stmt (fourth form) (%copy-dsl-type-environment environment)))
                   environment)
                  ((%sym-name= head 'cond)
                   (dolist (clause (rest form))
                     (unless (%default-dsl-clause-p (first clause))
                       (expr (first clause) environment))
                     (body (rest clause) (%copy-dsl-type-environment environment)))
                   environment)
                  ((%sym-name= head 'case)
                   (expr (second form) environment)
                   (dolist (clause (cddr form))
                     (body (rest clause) (%copy-dsl-type-environment environment)))
                   environment)
                  ((or (%sym-name= head 'atomic-add)
                       (%sym-name= head 'atomic-cas))
                   (lvalue (second form) environment)
                   (mapc (lambda (value) (expr value environment)) (cddr form))
                   environment)
                  ((%sym-name= head 'shared-array)
                   (bind environment (second form)
                         (%semantic-shared-type (third form)) form))
                  ((%sym-name= head 'sync) environment)
                  ((%sym-name= head 'vload4)
                   (lookup environment (%symbol-c-name (third form)) (third form))
                   (expr (fourth form) environment)
                   (let ((inner (%copy-dsl-type-environment environment)))
                     (dolist (register (second form))
                       (bind inner register :float form))
                     (body (nthcdr 4 form) inner))
                   environment)
                  ((%sym-name= head 'vstore4)
                   (lookup environment (%symbol-c-name (second form)) (second form))
                   (expr (third form) environment)
                   (mapc (lambda (value) (expr value environment)) (cdddr form))
                   environment)
                  (t (expr form environment) environment)))))))
      (let ((environment (make-hash-table :test #'equal)))
        (dolist (param (kernel-spec-params spec))
          (bind environment (first param)
                (param-internal-type (getf (rest param) :type)) param))
        (stmt (kernel-spec-body spec) environment))
      analysis)))

(defun validate-kernel-shuffle-types
    (spec &key (float-call-p #'dsl-float-call-p))
  "Validate every warp collective in SPEC and return a lexical analysis receipt.

Shuffle-free kernels retain the legacy permissive type walk and are not
reinterpreted by this tranche.  FLOAT-CALL-P remains the legacy-environment
policy hook; shuffle semantics themselves use the one DSL call-domain table."
  (let ((legacy
          (kernel-spec-type-environment spec :float-call-p float-call-p)))
    (if (%shuffle-in-statement-p (kernel-spec-body spec))
        (%analyze-kernel-shuffles spec legacy)
        (%make-kernel-shuffle-analysis
         :legacy-type-environment legacy :shuffle-bearing-p nil))))
