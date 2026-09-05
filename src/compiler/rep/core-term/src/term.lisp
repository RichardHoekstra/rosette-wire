;;;; term.lisp --- the ONE core term: syntax, type-check, and the direct
;;;; evaluator (coincidence 1 of 3).
;;;;
;;;; A CORE TERM is a typed first-order s-expression over the single value
;;;; type :int (booleans are 0/1 ints, as in the whole substrate silicon stack):
;;;;
;;;;   <int>                 integer literal
;;;;   <sym>                 variable (a bound parameter or let name)
;;;;   (if c t e)            conditional (c=0 is false)
;;;;   (let x e b)           bind x = e in b   (pure, so substitution-safe)
;;;;   (OP a b)              OP in + - * / % < > =   (binary)
;;;;   (f a ...)             call a defined first-order function (recursion ok)
;;;;
;;;; A CORE-PROGRAM is a set of named first-order functions plus a MAIN term.
;;;; The functions are the "bounded recursion / primitive-rec" fragment: each
;;;; call is a delta-unfold and, for the ground inputs used, the recursion
;;;; terminates -- so normalization (proof) is total and lowering (compile)
;;;; is exact.  This is precisely the fragment the kernel IR can express, via
;;;; rosette-lisp-codegen's recursive-call calling convention.
;;;;
;;;; Operator/function symbols are compared BY NAME (string=) so a term
;;;; written in any package (`+`, `<`, `%` are not CL symbols) resolves
;;;; identically -- the same discipline rosette-lisp-codegen uses.

(in-package #:rosette-core-term)

;;; --- the ONE object ---------------------------------------------------------

(defstruct (core-program
            (:constructor make-core-program (&key funs main)))
  "A first-order program: FUNS = list of (name (param ...) body-term),
MAIN = a closed term that may call the FUNS."
  (funs nil :type list)
  (main nil :type t))

(defun sym= (s name) (and (symbolp s) (string= (symbol-name s) name)))

;;; --- types (the "typed" earns its keep once lambdas arrive) ----------------
;;; A type is either the base :INT or a function type (-> DOM COD).  The first-
;;; order fragment is entirely :INT; the lambda layer introduces (-> a b).  The
;;; top-level MAIN is required to be :INT so every coincidence still bottoms out
;;; in an integer normal form / silicon result.

(defun arrow-type-p (ty) (and (consp ty) (sym= (first ty) "->") (= (length ty) 3)))

(defun valid-type-p (ty)
  (or (eq ty :int)
      (and (arrow-type-p ty)
           (valid-type-p (second ty)) (valid-type-p (third ty)))))

(defun type= (a b)
  (cond ((and (eq a :int) (eq b :int)) t)
        ((and (arrow-type-p a) (arrow-type-p b))
         (and (type= (second a) (second b)) (type= (third a) (third b))))
        (t nil)))

;;; --- closures: the meta-level value of a lambda ----------------------------
;;; Evaluating (lam x ty body) yields a CLOSURE capturing the lexical env; a
;;; well-typed :INT MAIN never lets a closure escape to the normal form.

(defstruct (core-closure (:constructor make-closure (param ty body env)))
  (param nil) (ty nil) (body nil) (env nil :type list))

(defparameter *binops*
  '(("+" . +) ("-" . -) ("*" . *) ("/" . trunc/) ("%" . rem%)
    ("<" . lt?) (">" . gt?) ("=" . eq?))
  "Core binary operators, keyed by NAME.  Division truncates and modulus is
REM -- bit-identical to rosette-lisp-codegen's reference VM and the kernel-IR VM
(untag / *8), which is what makes the compile coincidence bit-exact.")

(defun binop-name (head)
  (and (symbolp head)
       (car (assoc (symbol-name head) *binops* :test #'string=))))

(defun trunc/ (a b) (values (truncate a b)))
(defun rem%   (a b) (rem a b))
(defun lt?    (a b) (if (< a b) 1 0))
(defun gt?    (a b) (if (> a b) 1 0))
(defun eq?    (a b) (if (= a b) 1 0))

(defun apply-binop (name a b)
  (funcall (cdr (assoc name *binops* :test #'string=)) a b))

;;; --- signature (name -> arity), by name ------------------------------------

(defun program-signature (program)
  "Return an alist FNAME-STRING -> (params . body) for PROGRAM's functions."
  (loop for (name params body) in (core-program-funs program)
        collect (cons (symbol-name name) (cons params body))))

(defun sig-lookup (sig head)
  (and (symbolp head) (cdr (assoc (symbol-name head) sig :test #'string=))))

;;; --- type-check (the "typed" in typed first-order term) --------------------

(define-condition core-type-error (error)
  ((term :initarg :term :reader core-type-error-term)
   (reason :initarg :reason :reader core-type-error-reason))
  (:report (lambda (c s)
             (format s "rosette-core-term: ~A in ~S"
                     (core-type-error-reason c) (core-type-error-term c)))))

(defun %type-error (term reason)
  (error 'core-type-error :term term :reason reason))

(defun check-term (term env sig)
  "Check TERM is well-formed under ENV (an alist SYMBOL-NAME -> TYPE) and SIG
(from PROGRAM-SIGNATURE), returning its TYPE.  The first-order fragment is all
:INT; the lambda layer adds (-> a b).  Signals CORE-TYPE-ERROR on an unbound
variable, a bad arity, a non-:int operator/argument, a lambda/application type
mismatch, or a call to an undefined function -- the negative gates of the
type discipline."
  (cond
    ((integerp term) :int)
    ((symbolp term)
     (let ((cell (assoc (symbol-name term) env :test #'string=)))
       (unless cell (%type-error term "unbound variable"))
       (cdr cell)))
    ((consp term)
     (let ((head (first term)))
       (cond
         ((sym= head "IF")
          (unless (= (length term) 4) (%type-error term "IF wants 3 args"))
          (unless (eq (check-term (second term) env sig) :int)
            (%type-error term "IF condition must be :int"))
          (let ((tt (check-term (third term) env sig))
                (te (check-term (fourth term) env sig)))
            (unless (type= tt te)
              (%type-error term "IF branches must have the same type"))
            tt))
         ((sym= head "LET")
          (unless (= (length term) 4) (%type-error term "LET wants (let x e b)"))
          (let ((x (second term)))
            (unless (and (symbolp x) (not (binop-name x)))
              (%type-error term "LET variable must be a plain symbol"))
            (let ((te (check-term (third term) env sig)))
              (check-term (fourth term)
                          (cons (cons (symbol-name x) te) env) sig))))
         ((sym= head "LAM")                     ; (lam x TY body)
          (unless (= (length term) 4) (%type-error term "LAM wants (lam x ty body)"))
          (let ((x (second term)) (ty (third term)))
            (unless (and (symbolp x) (not (binop-name x)))
              (%type-error term "LAM variable must be a plain symbol"))
            (unless (valid-type-p ty) (%type-error term "LAM has an invalid domain type"))
            (list '-> ty (check-term (fourth term)
                                     (cons (cons (symbol-name x) ty) env) sig))))
         ((sym= head "APP")                     ; (app f a)
          (unless (= (length term) 3) (%type-error term "APP wants (app f a)"))
          (let ((tf (check-term (second term) env sig))
                (ta (check-term (third term) env sig)))
            (unless (arrow-type-p tf) (%type-error term "APP applies a non-function"))
            (unless (type= (second tf) ta)
              (%type-error term "APP argument type mismatch"))
            (third tf)))
         ((binop-name head)
          (unless (= (length term) 3) (%type-error term "binary operator wants 2 args"))
          (unless (and (eq (check-term (second term) env sig) :int)
                       (eq (check-term (third term) env sig) :int))
            (%type-error term "operator operands must be :int"))
          :int)
         (t                                     ; first-order function call
          (let ((entry (sig-lookup sig head)))
            (unless entry (%type-error term "call to undefined function"))
            (unless (= (length (car entry)) (length (rest term)))
              (%type-error term "call arity mismatch"))
            (dolist (a (rest term))
              (unless (eq (check-term a env sig) :int)
                (%type-error term "function argument must be :int")))
            :int)))))
    (t (%type-error term "not a core term"))))

(defun term-check (program)
  "Type-check every function body and MAIN of PROGRAM.  Returns T or signals
CORE-TYPE-ERROR.  MAIN must be closed (no free variables)."
  (let ((sig (program-signature program)))
    (loop for (nil params body) in (core-program-funs program)
          for body-type =
            (check-term body
                        (mapcar (lambda (p) (cons (symbol-name p) :int)) params)
                        sig)
          unless (eq body-type :int)
            do (%type-error body "function body must have type :int"))
    (unless (eq (check-term (core-program-main program) '() sig) :int)
      (%type-error (core-program-main program) "MAIN must have type :int"))
    t))

;;; --- coincidence 1: EVAL (direct interpreter) ------------------------------

(defun term-eval (term env sig)
  "Evaluate TERM to an integer under ENV (alist symbol-name -> int) and SIG."
  (cond
    ((integerp term) term)
    ((symbolp term) (cdr (assoc (symbol-name term) env :test #'string=)))
    ((consp term)
     (let ((head (first term)))
       (cond
         ((sym= head "IF")
          (if (zerop (term-eval (second term) env sig))
              (term-eval (fourth term) env sig)
              (term-eval (third term) env sig)))
         ((sym= head "LET")
          (let ((v (term-eval (third term) env sig)))
            (term-eval (fourth term)
                       (cons (cons (symbol-name (second term)) v) env) sig)))
         ((sym= head "LAM")                     ; capture the lexical env
          (make-closure (second term) (third term) (fourth term) env))
         ((sym= head "APP")
          (let ((cl (term-eval (second term) env sig))
                (av (term-eval (third term) env sig)))
            (unless (core-closure-p cl)
              (error "rosette-core-term: applied a non-closure ~S" cl))
            (term-eval (core-closure-body cl)
                       (cons (cons (symbol-name (core-closure-param cl)) av)
                             (core-closure-env cl))
                       sig)))
         ((binop-name head)
          (apply-binop (binop-name head)
                       (term-eval (second term) env sig)
                       (term-eval (third term) env sig)))
         (t
          (let* ((entry (sig-lookup sig head))
                 (params (car entry)) (body (cdr entry))
                 (args (mapcar (lambda (a) (term-eval a env sig)) (rest term))))
            (term-eval body
                       (mapcar (lambda (p a) (cons (symbol-name p) a)) params args)
                       sig))))))
    (t (error "rosette-core-term: bad term ~S" term))))

(defun program-eval (program)
  "Evaluate PROGRAM's MAIN.  Type-checks first."
  (term-check program)
  (term-eval (core-program-main program) '() (program-signature program)))
