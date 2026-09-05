;;;; core.lisp --- first-order unification and SLD resolution.
;;;;
;;;; Terms are Lisp data:
;;;;   - an LVAR struct      = a logic variable (identity by EQ)
;;;;   - a CONS (f . args)   = a compound, car the functor symbol, cdr the args
;;;;   - any other atom      = a constant (symbol, number, ...)
;;;; A substitution is an alist mapping LVAR -> term; UNIFY returns an extended
;;;; substitution on success (NIL = the empty success) or the keyword :FAIL.

(in-package #:rosette-unification)

;;; ----------------------------------------------------------------------
;;; Terms
;;; ----------------------------------------------------------------------

(defstruct (lvar (:constructor make-var (&optional name))
                 (:print-object %print-lvar))
  (name nil))

(defun %print-lvar (v stream) (format stream "?~A" (or (lvar-name v) "_")))

(defun var-p (x) (lvar-p x))
(defun compound-p (x) (and (consp x) (not (lvar-p x))))      ; a struct is never a cons
(defun constant-p (x) (and (not (lvar-p x)) (not (consp x))))
(defun functor (term) (car term))
(defun term-args (term) (cdr term))

(defparameter *var-marker* #\?
  "A symbol whose name starts with this character parses to a logic variable.")

(defun %var-symbol-p (x)
  (and (symbolp x) x
       (let ((n (symbol-name x)))
         (and (plusp (length n)) (char= (char n 0) *var-marker*)))))

(defun parse-term (sexp &optional (vmap (make-hash-table :test 'equal)))
  "Convert SEXP into a term. Symbols whose name starts with ? become logic
variables; the same name maps to the same variable within one VMAP (so pass a
shared VMAP to keep variables linked across several terms of one clause/query)."
  (cond ((%var-symbol-p sexp)
         (let ((key (symbol-name sexp)))
           (or (gethash key vmap)
               (setf (gethash key vmap) (make-var (subseq key 1))))))
        ((consp sexp) (mapcar (lambda (s) (parse-term s vmap)) sexp))
        (t sexp)))

(defun term->sexp (term)
  "Render TERM back to an s-expr (variables as ?name symbols)."
  (cond ((var-p term) (intern (format nil "?~A" (or (lvar-name term) "_"))))
        ((compound-p term) (cons (functor term) (mapcar #'term->sexp (term-args term))))
        (t term)))

;;; ----------------------------------------------------------------------
;;; Substitution + Robinson unification
;;; ----------------------------------------------------------------------

(defparameter *occurs-check* t
  "When true (the default, sound), UNIFY refuses to bind X to a term containing X.")

(defun walk (term subst)
  "Dereference TERM through SUBST to its current binding (shallow: a variable
bound to a variable is followed; compound structure is not descended)."
  (if (var-p term)
      (let ((pair (assoc term subst :test #'eq)))
        (if pair (walk (cdr pair) subst) term))
      term))

(defun occurs-p (var term subst)
  "Does VAR occur anywhere in TERM (under SUBST)?"
  (let ((term (walk term subst)))
    (cond ((eq var term) t)
          ((compound-p term) (some (lambda (a) (occurs-p var a subst)) (term-args term)))
          (t nil))))

(defun unify-failure-p (x) (eq x :fail))

(defun %extend (var term subst)
  (if (and *occurs-check* (occurs-p var term subst))
      :fail
      (acons var term subst)))

(defun unify (a b &optional subst)
  "Robinson unification of terms A and B under SUBST. Returns the extended
substitution (NIL is the empty success substitution) or :FAIL on failure."
  (if (unify-failure-p subst)
      :fail
      (let ((a (walk a subst)) (b (walk b subst)))
        (cond
          ((and (var-p a) (var-p b) (eq a b)) subst)
          ((var-p a) (%extend a b subst))
          ((var-p b) (%extend b a subst))
          ((and (compound-p a) (compound-p b))
           (if (and (eql (functor a) (functor b))
                    (= (length (term-args a)) (length (term-args b))))
               (loop with s = subst
                     for x in (term-args a) for y in (term-args b)
                     do (setf s (unify x y s))
                        (when (unify-failure-p s) (return :fail))
                     finally (return s))
               :fail))
          ((eql a b) subst)                 ; constants
          (t :fail)))))

(defun mgu (a b)
  "Most general unifier of A and B. Returns two values: the substitution alist
(possibly NIL) and a boolean that is true iff A and B unify."
  (let ((s (unify a b nil)))
    (if (unify-failure-p s) (values nil nil) (values s t))))

(defun resolve (term subst)
  "Apply SUBST to TERM recursively, returning the fully substituted term."
  (let ((term (walk term subst)))
    (if (compound-p term)
        (cons (functor term) (mapcar (lambda (a) (resolve a subst)) (term-args term)))
        term)))

;;; ----------------------------------------------------------------------
;;; Clauses + SLD resolution
;;; ----------------------------------------------------------------------

(defstruct (rule (:constructor %make-rule (head body)))
  "A Horn clause HEAD :- BODY (BODY a list of goal terms; NIL for a fact)."
  head body)

(defun make-rule (head-sexp &rest body-sexps)
  "Build a rule HEAD :- BODY1, BODY2, ... from s-exprs sharing one variable
scope (so a ?X in the head is the same variable as ?X in the body)."
  (let ((vmap (make-hash-table :test 'equal)))
    (%make-rule (parse-term head-sexp vmap)
                (mapcar (lambda (g) (parse-term g vmap)) body-sexps))))

(defun make-fact (head-sexp) "A rule with an empty body." (make-rule head-sexp))

(defun %vars-in (term &optional acc)
  (cond ((var-p term) (if (member term acc :test #'eq) acc (cons term acc)))
        ((compound-p term) (dolist (a (term-args term) acc) (setf acc (%vars-in a acc))))
        (t acc)))

(defun %subst-vars (term map)
  (cond ((var-p term) (let ((p (assoc term map :test #'eq))) (if p (cdr p) term)))
        ((compound-p term)
         (cons (functor term) (mapcar (lambda (a) (%subst-vars a map)) (term-args term))))
        (t term)))

(defun rename-rule (rule)
  "Return a copy of RULE with every variable replaced by a fresh one -- required
before each use of a clause so distinct uses do not share bindings."
  (let* ((vars (let ((acc (%vars-in (rule-head rule))))
                 (dolist (g (rule-body rule) acc) (setf acc (%vars-in g acc)))))
         (map (mapcar (lambda (v) (cons v (make-var (lvar-name v)))) vars)))
    (%make-rule (%subst-vars (rule-head rule) map)
                (mapcar (lambda (g) (%subst-vars g map)) (rule-body rule)))))

(defparameter *default-depth* 200
  "Resolution depth bound -- keeps SLD terminating on left-recursive programs.")

(defun sld-resolve (goals kb subst depth)
  "Prove the conjunction GOALS (a list of terms) against the rule list KB under
SUBST, returning the list of all solution substitutions (depth-first, leftmost
goal first). DEPTH bounds the search."
  (cond ((unify-failure-p subst) '())
        ((null goals) (list subst))
        ((<= depth 0) '())
        (t (let ((goal (first goals)) (rest (rest goals)) (sols '()))
             (dolist (r kb (nreverse sols))
               (let* ((fresh (rename-rule r))
                      (s2 (unify goal (rule-head fresh) subst)))
                 (unless (unify-failure-p s2)
                   (dolist (sol (sld-resolve (append (rule-body fresh) rest) kb s2 (1- depth)))
                     (push sol sols)))))))))

(defun prove (goal kb &key (depth *default-depth*))
  "Prove GOAL (a term; parse an s-expr with PARSE-TERM first) against KB,
returning the list of solution substitutions."
  (sld-resolve (list goal) kb nil depth))

(defun query (goal-sexp kb &key (depth *default-depth*))
  "Parse GOAL-SEXP (an s-expr with ?variables), prove it against KB, and return
one answer per solution: an alist of (variable-name-string . resolved-value-sexp)
for the goal's variables. A ground provable goal yields (NIL); an unprovable
goal yields NIL."
  (let* ((vmap (make-hash-table :test 'equal))
         (goal (parse-term goal-sexp vmap))
         (qvars '()))
    (maphash (lambda (k v) (push (cons k v) qvars)) vmap)
    (setf qvars (nreverse qvars))
    (mapcar (lambda (subst)
              (mapcar (lambda (nv)
                        (cons (subseq (car nv) 1)        ; strip the leading ?
                              (term->sexp (resolve (cdr nv) subst))))
                      qvars))
            (sld-resolve (list goal) kb nil depth))))

(defun provable-p (goal-sexp kb &key (depth *default-depth*))
  "True iff GOAL-SEXP has at least one solution against KB."
  (and (sld-resolve (list (parse-term goal-sexp (make-hash-table :test 'equal)))
                    kb nil depth)
       t))
