;;;; analysis.lisp --- Structural analysis and manipulation of expression AST
;;;;
;;;; Substitution, composition, node count, depth, free-variable set,
;;;; closedness, special-function detection, and CSE-aware sub-expression
;;;; tabulation.

(in-package #:rosette-expression-core)

(defun cf-substitution-value (name substitutions)
  (let ((cell (assoc name substitutions :test #'equal)))
    (and cell (cdr cell))))

(defun cf-map-children (function args)
  (mapcar function args))

(defun %cf-substitute-children (op args substitutions)
  (make-cf-node op (cf-map-children
                    (lambda (arg) (cf-substitute arg substitutions))
                    args)))

(defun cf-substitute (expr substitutions)
  "Substitute variables in EXPR according to an alist NAME . CF-NODE.
Unmentioned variables are preserved.  This is the executable form of
finite closed-form composition (Definition 8 condition 2)."
  (let ((op (cf-node-op expr))
        (args (cf-node-args expr)))
    (ecase op
      (:const expr)
      (:var (or (cf-substitution-value (first args) substitutions) expr))
      ((:add :sub :mul :div)
       (%cf-substitute-children op args substitutions))
      ((:exp :log :sqrt :normal-cdf :inverse-normal-cdf)
       (%cf-substitute-children op args substitutions))
      (:inverse-gaussian-quantile
       (%cf-substitute-children op args substitutions))
      (:piecewise
       (%cf-substitute-children op args substitutions)))))

(defun cf-compose (outer variable inner)
  "Return OUTER with VARIABLE replaced by INNER."
  (cf-substitute outer (list (cons variable inner))))

(defun %cf-fold-children (expr leaf-value combine)
  (let ((args (cf-node-args expr)))
    (case (cf-node-op expr)
      ((:const :var) leaf-value)
      (otherwise
       (funcall combine (mapcar (lambda (arg)
                                  (%cf-fold-children arg leaf-value combine))
                                args))))))

(defun cf-node-count (expr)
  "Count of AST nodes in EXPR (constants and variables count as 1)."
  (%cf-fold-children expr
                     1
                     (lambda (child-counts)
                       (1+ (reduce #'+ child-counts)))))

(defun cf-depth (expr)
  "Depth of the deepest path in EXPR."
  (%cf-fold-children expr
                     1
                     (lambda (child-depths)
                       (1+ (reduce #'max child-depths)))))

(defun %cf-variable-union (args)
  (remove-duplicates
   (mapcan (lambda (arg) (copy-list (cf-variables arg))) args)
   :test #'equal))

(defun cf-variables (expr)
  "Sorted-by-first-occurrence list of free variable names in EXPR."
  (let ((op (cf-node-op expr))
        (args (cf-node-args expr)))
    (ecase op
      (:const nil)
      (:var (list (first args)))
      ((:add :sub :mul :div)
       (%cf-variable-union args))
      ((:exp :log :sqrt :normal-cdf :inverse-normal-cdf)
       (cf-variables (first args)))
      (:inverse-gaussian-quantile
       (%cf-variable-union args))
      (:piecewise
       (%cf-variable-union args)))))

(defun cf-closed-p (expr)
  "T iff EXPR has no free variables (purely constant after substitution)."
  (null (cf-variables expr)))

(defun cf-uses-special-p (expr)
  "T iff EXPR contains any of normal-cdf, inverse-normal-cdf, or
inverse-gaussian-quantile in its AST."
  (let ((op (cf-node-op expr))
        (args (cf-node-args expr)))
    (case op
      ((:const :var) nil)
      ((:normal-cdf :inverse-normal-cdf :inverse-gaussian-quantile) t)
      (otherwise
       (some #'cf-uses-special-p args)))))

(defun cf-cse-keys (expr)
  "Return a hash-table mapping each distinct sub-expression of EXPR to
a unique integer id.  Keys are (op . child-keys) where child-keys are
the ids of children for compound nodes, or the literal args for :const
and :var leaves."
  (let ((table (make-hash-table :test #'equal))
        (id-counter 0))
    (labels ((cse (node)
               (let* ((op (cf-node-op node))
                      (args (cf-node-args node))
                      (child-keys
                       (case op
                         ((:const :var) args)
                         (otherwise
                          (mapcar #'cse args))))
                      (key (cons op child-keys)))
                 (or (gethash key table)
                     (let ((id (incf id-counter)))
                       (setf (gethash key table) id)
                       id)))))
      (cse expr))
    table))
