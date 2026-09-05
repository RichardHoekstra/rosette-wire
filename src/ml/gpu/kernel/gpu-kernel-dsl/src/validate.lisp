;;;; validate.lisp --- DSL grammar validation
;;;;
;;;; The DSL is a typed subset of CUDA-C.  v0.1.0 grammar:
;;;;
;;;;   STMT ::= (setq INDEXED-LVAL EXPR)
;;;;          | (setq SCALAR-VAR EXPR)
;;;;          | (for-grid-stride (VAR LIMIT-EXPR) STMT ...)
;;;;          | (for-range (VAR LIMIT-EXPR) STMT ...)
;;;;          | (progn STMT ...)
;;;;          | (let ((VAR EXPR) ...) STMT ...)
;;;;          | (if EXPR STMT [STMT])
;;;;          | (cond (EXPR STMT...) ... [(t STMT...)])
;;;;          | (case EXPR (KEY STMT...) ... [(otherwise STMT...)])
;;;;          | (atomic-add INDEXED-LVAL EXPR)
;;;;          | (atomic-cas INDEXED-LVAL OLD NEW)
;;;;
;;;;   EXPR ::= INTEGER | FLOAT
;;;;          | SCALAR-VAR
;;;;          | INDEXED-EXPR                       ; a[i], y[idx], ...
;;;;          | (+ EXPR EXPR ...)
;;;;          | (- EXPR EXPR ...)
;;;;          | (* EXPR EXPR ...)
;;;;          | (/ EXPR EXPR)
;;;;          | (< EXPR EXPR) | (<= ...) | (= ...)  ; comparison
;;;;          | (THREAD-IDX-X) | (BLOCK-IDX-X)      ; CUDA built-ins
;;;;          | (BLOCK-DIM-X)  | (GRID-DIM-X)
;;;;          | (CALL FN-NAME EXPR ...)             ; named C function
;;;;
;;;;   INDEXED-EXPR / INDEXED-LVAL :
;;;;     A symbol whose printed name contains "[" and ends in "]" —
;;;;     e.g. A[I], C[IDX].  The validator splits on "[" to recover
;;;;     (BASE INDEX-EXPR) where INDEX-EXPR is parsed as a separate DSL
;;;;     expression (recursively).  This lets the caller write
;;;;     (setq c[i] (+ a[i] b[i])) which reads naturally.
;;;;
;;;; The validator returns T or NIL — an asserter ASSERT-VALID-DSL-FORM
;;;; signals DSL-SYNTAX-ERROR with the offending sub-form.

(in-package #:rosette-gpu-kernel-dsl)

;;; --- Operator catalog ------------------------------------------------

(defparameter *dsl-arithmetic-operators*
  '(+ - * / % mulmod mulhi band bor bxor shl shr popcount)
  "DSL arithmetic operators.  Compared by symbol-name (case-insensitive).
MULMOD is the ternary (mulmod a b p) = (a*b) mod p with an EXACT 64-bit
intermediate product -- the NTT-butterfly / Miller-Rabin core op.  Operands
and result are 32-bit non-negative; only the a*b product is widened.
MULHI is the binary (mulhi a b) = high 64 bits of the unsigned 64-bit product
a*b (PTX mul.hi.u64) -- the 64-bit-mode primitive that lets Montgomery modular
multiply form the 128-bit product without a 128-bit type.")

(defparameter *dsl-comparison-operators*
  '(< <= > >= = /=)
  "DSL comparison operators.")

(defparameter *dsl-warp-operators*
  '(shfl-down shfl-index)
  "Full-warp f32/b32 register-exchange operators.")

(defparameter *dsl-statement-heads*
  '(setq for-grid-stride for-range while progn let if cond case atomic-add atomic-cas
    shared-array sync vload4 vstore4)
  "Head symbols accepted in statement position.")

(defparameter *dsl-cuda-builtins*
  '(thread-idx-x thread-idx-y thread-idx-z
    block-idx-x  block-idx-y  block-idx-z
    block-dim-x  block-dim-y  block-dim-z
    grid-dim-x   grid-dim-y   grid-dim-z)
  "Zero-arity CUDA-C identifiers exposed to the DSL.")

(defun dsl-operator-p (sym)
  "T if SYM is a recognised DSL operator (arithmetic, comparison, or
   statement head)."
  (and (symbolp sym)
       (or (member sym *dsl-arithmetic-operators* :test #'%sym-name=)
           (member sym *dsl-comparison-operators* :test #'%sym-name=)
           (member sym *dsl-warp-operators* :test #'%sym-name=)
           (member sym *dsl-statement-heads* :test #'%sym-name=)
           (member sym *dsl-cuda-builtins* :test #'%sym-name=)
           (%sym-name= sym 'call))))

(defun %sym-name= (a b)
  "Compare two symbols by their names, case-insensitively, ignoring
   package.  Used so callers can write the DSL in any package without
   importing rosette-gpu-kernel-dsl symbols."
  (and (symbolp a) (symbolp b)
       (string-equal (symbol-name a) (symbol-name b))))

(defun %name= (sym str)
  "T if SYM's symbol-name string-equals STR."
  (and (symbolp sym) (string-equal (symbol-name sym) str)))

;;; --- Indexed-symbol decoding ----------------------------------------

(defun %indexed-symbol-p (sym)
  "T if SYM's name has the shape BASE[INDEX] — i.e. contains a left
   bracket and ends with a right bracket.  v0.1.0 limit: exactly one
   bracket pair."
  (and (symbolp sym)
       (not (keywordp sym))
       (let* ((name (symbol-name sym))
              (lbr (position #\[ name))
              (last-pos (1- (length name))))
         (and lbr
              (> lbr 0)
              (char= #\] (char name last-pos))
              (= 1 (count #\[ name))
              (= 1 (count #\] name))))))

(defun %decode-indexed-symbol (sym)
  "Return (values BASE INDEX-EXPR) for an indexed symbol like A[I].

   BASE is the lower-case base-name string (suitable for direct
   emission as a C identifier).  INDEX-EXPR is a Lisp object obtained
   by READ-FROM-STRING on the bracketed substring; it is then re-
   validated as a DSL expression by the caller.

   Signals DSL-SYNTAX-ERROR if SYM does not satisfy %INDEXED-SYMBOL-P."
  (unless (%indexed-symbol-p sym)
    (error 'dsl-syntax-error
           :form sym :reason "expected indexed symbol BASE[INDEX]"))
  (let* ((name (symbol-name sym))
         (lbr (position #\[ name))
         (rbr (1- (length name)))
         ;; Apply the same C-identifier mangling as %sym-c does for
         ;; non-indexed symbols: lowercase + hyphen → underscore.
         ;; Without the substitute step, an indexed symbol like
         ;; `cell-idx[a]` emits as `cell-idx[a]` which is invalid C.
         (base (substitute #\_ #\-
                           (string-downcase (subseq name 0 lbr))))
         (idx-string (subseq name (1+ lbr) rbr))
         ;; Read the index in a fresh package so foreign symbols
         ;; like I and IDX become uninterned-looking but still
         ;; comparable by name.  We use the current package for
         ;; symmetry with the surrounding DSL form.
         (idx-expr (let ((*read-eval* nil))
                     (with-input-from-string (s idx-string)
                       (read s)))))
    (values base idx-expr)))

;;; --- Predicates ------------------------------------------------------

(defun %valid-dsl-exprs-p (forms)
  "T iff every element of FORMS is a valid DSL expression."
  (every #'valid-dsl-expr-p forms))

(defun %valid-dsl-body-p (forms)
  "T iff every element of FORMS is a valid DSL statement."
  (every #'valid-dsl-stmt-p forms))

(defun %valid-loop-header-p (header)
  "T iff HEADER has the DSL loop shape (VAR LIMIT-EXPR)."
  (and (consp header)
       (= 2 (length header))
       (symbolp (first header))
       (not (keywordp (first header)))
       (valid-dsl-expr-p (second header))))

(defun valid-dsl-expr-p (form)
  "Recursive predicate: is FORM a valid DSL expression?

   Atoms:
     - integer / float                                       -> T
     - symbol that is an indexed-symbol (A[I])               -> T iff index is a valid expr
     - symbol naming a CUDA built-in (THREAD-IDX-X, ...)     -> N/A as atom
     - other non-keyword symbol                              -> T  (variable reference)
     - keyword                                                -> NIL
     - T / NIL                                                -> NIL  (no booleans as values)

     Compound forms:
     (OP EXPR ...)        for OP in *dsl-arithmetic-operators*  -> arity rule varies:
                          +,* : arity ≥ 2; - : arity ≥ 1; / and % : arity = 2
                          for OP in *dsl-comparison-operators*  -> arity = 2
     (BUILTIN)            for BUILTIN in *dsl-cuda-builtins*    -> arity = 0
     (CALL fn-name EXPR ...)                                    -> fn-name a symbol
   "
  (cond
    ((or (integerp form) (floatp form)) t)
    ((eql form t) nil)
    ((null form) nil)
    ((keywordp form) nil)
    ((symbolp form)
     (cond
       ((%indexed-symbol-p form)
        (multiple-value-bind (base idx) (%decode-indexed-symbol form)
          (declare (ignore base))
          (valid-dsl-expr-p idx)))
       (t t)))
    ((not (consp form)) nil)
    (t
     (let ((head (first form)))
       (cond
         ((not (symbolp head)) nil)
         ;; Zero-arity CUDA built-ins
         ((member head *dsl-cuda-builtins* :test #'%sym-name=)
          (null (rest form)))
         ;; Arithmetic (+,-,*,/,%)
         ((member head *dsl-arithmetic-operators* :test #'%sym-name=)
          (and (cond ((%sym-name= head '-) (>= (length (rest form)) 1))
                     ((%sym-name= head 'popcount) (= 1 (length (rest form))))
                     ((member head '(% / shl shr) :test #'%sym-name=) (= 2 (length (rest form))))
                     (t (>= (length (rest form)) 2)))
               (%valid-dsl-exprs-p (rest form))))
         ;; Comparison (<,<=,>,>=,=,/=)
         ((member head *dsl-comparison-operators* :test #'%sym-name=)
          (and (= 2 (length (rest form)))
               (%valid-dsl-exprs-p (rest form))))
         ;; (half-to-float EXPR) -- decode an fp16 bit-pattern (low 16 bits of an
         ;; i32) to f32.  The quantized-block scale primitive.
         ((%sym-name= head 'half-to-float)
          (and (= 2 (length form)) (valid-dsl-expr-p (second form))))
         ;; (float-to-half EXPR) -- round f32 to IEEE binary16 bits in the low
         ;; 16 bits of the i32 result.  The inverse carrier operation.
         ((%sym-name= head 'float-to-half)
          (and (= 2 (length form)) (valid-dsl-expr-p (second form))))
         ;; Full-mask, width-32 f32/b32 warp register exchange.  SHFL-DOWN
         ;; selects lane+OFFSET (or self past lane 31); SHFL-INDEX selects
         ;; SOURCE-LANE.  Payload type is checked by backend type inference.
         ((member head *dsl-warp-operators* :test #'%sym-name=)
          (and (= 3 (length form)) (%valid-dsl-exprs-p (rest form))))
         ;; (call fn-name args...)
         ((%sym-name= head 'call)
          (and (>= (length form) 2)
               (symbolp (second form))
               (%valid-dsl-exprs-p (cddr form))))
         ;; (atomic-add LVAL EXPR) in EXPRESSION position -> FETCH-AND-ADD: the
         ;; atomic returns the OLD value (unique-slot allocation, histograms).
         ((%sym-name= head 'atomic-add)
          (and (= 3 (length form))
               (or (symbolp (second form)) (%indexed-symbol-p (second form)))
               (valid-dsl-expr-p (third form))))
         (t nil))))))

(defun valid-dsl-stmt-p (form)
  "Recursive predicate: is FORM a valid DSL statement?

   Statement heads:
     setq             - (setq LVAL EXPR)
     for-grid-stride  - (for-grid-stride (VAR LIMIT-EXPR) STMT ...)
     for-range        - (for-range (VAR LIMIT-EXPR) STMT ...)
     progn            - (progn STMT ...)
     let              - (let ((VAR EXPR) ...) STMT ...)
     if               - (if EXPR STMT [STMT])
     cond             - (cond (EXPR STMT...) ... [(t STMT...)])
     case             - (case EXPR (KEY STMT...) ... [(otherwise STMT...)])
     atomic-add       - (atomic-add LVAL EXPR)
     atomic-cas       - (atomic-cas LVAL OLD NEW)

   Expression-position forms count as statements (expression-statement
   semantics, like C's `expr;`)."
  (cond
    ((not (consp form)) (valid-dsl-expr-p form))
    (t
     (let ((head (first form)))
       (cond
         ;; (setq LVAL EXPR)
         ((%sym-name= head 'setq)
          (and (= 3 (length form))
               (or (and (symbolp (second form))
                        (not (keywordp (second form))))
                   (%indexed-symbol-p (second form)))
               (or (not (%indexed-symbol-p (second form)))
                   (multiple-value-bind (base idx)
                       (%decode-indexed-symbol (second form))
                     (declare (ignore base))
                     (valid-dsl-expr-p idx)))
               (valid-dsl-expr-p (third form))))
         ;; (for-grid-stride (VAR LIMIT-EXPR) STMT ...)
         ((%sym-name= head 'for-grid-stride)
          (and (>= (length form) 3)
               (%valid-loop-header-p (second form))
               (%valid-dsl-body-p (cddr form))))
         ;; (for-range (VAR LIMIT-EXPR) STMT ...)
         ((%sym-name= head 'for-range)
          (and (>= (length form) 3)
               (%valid-loop-header-p (second form))
               (%valid-dsl-body-p (cddr form))))
         ;; (progn STMT ...)
         ((%sym-name= head 'progn)
          (%valid-dsl-body-p (rest form)))
         ;; (let ((VAR EXPR) ...) STMT ...)
         ;; (let ((VAR :TYPE EXPR) ...) STMT ...)
         ;; The 2-element binding form is untyped — emitter defaults to float
         ;; (backward compatible). The 3-element form takes an explicit type
         ;; keyword between the var and the init expression; allowed types
         ;; are :float, :int, :uint32, :bool. Integer types are essential
         ;; for array indexing and multi-dimensional grid traversal where
         ;; the auto-float default would corrupt indices.
         ((%sym-name= head 'let)
          (and (>= (length form) 3)
               (listp (second form))
               (every (lambda (b)
                        (and (consp b)
                             (or
                              ;; (var expr)
                              (and (= 2 (length b))
                                   (symbolp (first b))
                                   (not (keywordp (first b)))
                                   (valid-dsl-expr-p (second b)))
                              ;; (var :type expr)
                              (and (= 3 (length b))
                                   (symbolp (first b))
                                   (not (keywordp (first b)))
                                   (member (second b)
                                           '(:float :int :uint32 :bool :int64)
                                           :test #'eql)
                                   (valid-dsl-expr-p (third b))))))
                      (second form))
               (%valid-dsl-body-p (cddr form))))
         ;; (if EXPR STMT [STMT])
         ((%sym-name= head 'if)
          (and (or (= 3 (length form)) (= 4 (length form)))
               (valid-dsl-expr-p (second form))
               (%valid-dsl-body-p (cddr form))))
         ;; (while EXPR STMT ...) — the unbounded loop (condition + body)
         ((%sym-name= head 'while)
          (and (>= (length form) 2)
               (valid-dsl-expr-p (second form))
               (%valid-dsl-body-p (cddr form))))
         ;; (cond (TEST STMT...) ...)
         ((%sym-name= head 'cond)
          (and (rest form)
               (every (lambda (clause)
                        (and (consp clause)
                             (or (eql (first clause) t)
                                 (valid-dsl-expr-p (first clause)))
                             (%valid-dsl-body-p (rest clause))))
                      (rest form))))
         ;; (case EXPR (KEY STMT...) ...)
         ((%sym-name= head 'case)
          (and (>= (length form) 3)
               (valid-dsl-expr-p (second form))
               (every (lambda (clause)
                        (and (consp clause)
                             (or (member (first clause) '(otherwise t) :test #'%sym-name=)
                                 (integerp (first clause))
                                 (symbolp (first clause)))
                             (%valid-dsl-body-p (rest clause))))
                      (cddr form))))
         ;; (atomic-add LVAL EXPR)
         ((%sym-name= head 'atomic-add)
          (and (= 3 (length form))
               (or (symbolp (second form)) (%indexed-symbol-p (second form)))
               (valid-dsl-expr-p (third form))))
         ;; (atomic-cas LVAL OLD NEW)
         ((%sym-name= head 'atomic-cas)
          (and (= 4 (length form))
               (or (symbolp (second form)) (%indexed-symbol-p (second form)))
               (valid-dsl-expr-p (third form))
               (valid-dsl-expr-p (fourth form))))
         ;; (shared-array NAME :TYPE SIZE) — block-shared static array.
         ((%sym-name= head 'shared-array)
          (and (= 4 (length form))
               (symbolp (second form)) (not (keywordp (second form)))
               (member (third form) '(:float :int :uint32 :double))
               (integerp (fourth form)) (plusp (fourth form))))
         ;; (sync) — block barrier, no args.
         ((%sym-name= head 'sync)
          (= 1 (length form)))
         ;; (vload4 (R0 R1 R2 R3) BASE IDX STMT...) — 128-bit vector load:
         ;; binds R0..R3 (float) = BASE[IDX..IDX+3] over the body.  IDX must be
         ;; 4-element-aligned (caller contract) for the aligned device op.
         ((%sym-name= head 'vload4)
          (and (>= (length form) 4)
               (listp (second form)) (= 4 (length (second form)))
               (every (lambda (v) (and (symbolp v) (not (keywordp v)))) (second form))
               (symbolp (third form)) (not (keywordp (third form)))
               (valid-dsl-expr-p (fourth form))
               (every #'valid-dsl-stmt-p (nthcdr 4 form))))
         ;; (vstore4 BASE IDX E0 E1 E2 E3) — 128-bit vector store.
         ((%sym-name= head 'vstore4)
          (and (= 7 (length form))
               (symbolp (second form)) (not (keywordp (second form)))
               (valid-dsl-expr-p (third form))
               (every #'valid-dsl-expr-p (nthcdr 3 form))))
         ;; Otherwise: must parse as an expression-statement.
         (t (valid-dsl-expr-p form)))))))

(defun valid-dsl-form-p (form)
  "Top-level predicate.  T iff FORM is a valid DSL statement (or
   degenerate single expression, treated as expression-statement).
   This is the predicate the test suite exercises; callers who want a
   strict expression check should use VALID-DSL-EXPR-P directly."
  (valid-dsl-stmt-p form))

;;; --- IR symbol canonicalisation -------------------------------------
;;;
;;; The public DSL surface lets callers write a kernel body with symbols
;;; from *their own* package — (setq out[i] (+ a[i] b[i])) reads naturally
;;; whether the caller is in CL-USER, a backend package, or a fresh test
;;; package.  But the stored IR must be package-INDEPENDENT: a backend that
;;; keys on symbol identity (EQ / CASE / ECASE against the operator symbols)
;;; must see the SAME symbol no matter which package the caller's symbols
;;; came from.  Without this, an operator read in a foreign package falls
;;; through an ECASE (NIL-not-VECTOR / fall-through errors) downstream.
;;;
;;; The fix: at the MAKE-KERNEL-SPEC boundary, re-intern every symbol in the
;;; body into the rosette-gpu-kernel-dsl package, BY NAME.  Operators, statement
;;; heads, CUDA built-ins, loop variables, scalar identifiers, and indexed
;;; symbols (a[i] — name preserved verbatim including brackets) all become
;;; canonical ROSETTE-GPU-KERNEL-DSL symbols.  Keywords (type tags like :float
;;; in a typed LET binding) are already package-canonical and are left as-is.
;;; Numbers pass through untouched.  Printed names are preserved exactly, so
;;; codegen, the interpreter, %INDEXED-SYMBOL-P, and %DECODE-INDEXED-SYMBOL
;;; all behave identically — only the package identity is normalised.

(defun %canonical-symbol (sym)
  "Re-intern SYM into the ROSETTE-GPU-KERNEL-DSL package by name.  Keywords are
   returned unchanged (already canonical and package-stable).  T and NIL are
   left as themselves (CL package).  Idempotent: a symbol already in this
   package re-interns to itself."
  (cond
    ((keywordp sym) sym)
    ((null sym) sym)
    ((eq sym t) sym)
    (t (intern (symbol-name sym) '#:rosette-gpu-kernel-dsl))))

(defun %canonicalize-dsl-form (form)
  "Recursively re-intern every symbol in the DSL body FORM into the
   ROSETTE-GPU-KERNEL-DSL package, so the stored IR is package-independent.
   Structure, numbers, and keywords are preserved verbatim; only the
   identity of operator / identifier symbols changes."
  (cond
    ((symbolp form) (%canonical-symbol form))
    ((consp form)
     (cons (%canonicalize-dsl-form (car form))
           (%canonicalize-dsl-form (cdr form))))
    (t form)))
