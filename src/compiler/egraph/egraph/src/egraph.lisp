;;;; egraph.lisp --- equality graph kernel.

(in-package #:rosette-egraph)

(defstruct (egraph
            (:constructor %make-egraph)
            (:copier nil))
  (parent (make-hash-table :test #'eq))
  (rank (make-hash-table :test #'eq))
  (members (make-hash-table :test #'eq)))

(defun make-egraph ()
  "Create an empty e-graph."
  (%make-egraph))

(defun %as-form (form-or-value)
  (if (form-p form-or-value)
      form-or-value
      (intern-form form-or-value)))

(defun egraph-add (graph form-or-value)
  "Add FORM-OR-VALUE to GRAPH and return its canonical Form."
  (let ((form (%as-form form-or-value)))
    (unless (gethash form (egraph-parent graph))
      (setf (gethash form (egraph-parent graph)) form
            (gethash form (egraph-rank graph)) 0
            (gethash form (egraph-members graph)) (list form)))
    form))

(defun egraph-find (graph form-or-value)
  "Return the canonical representative for FORM-OR-VALUE in GRAPH."
  (let* ((form (egraph-add graph form-or-value))
         (parent (gethash form (egraph-parent graph))))
    (if (eq parent form)
        form
        (setf (gethash form (egraph-parent graph))
              (egraph-find graph parent)))))

(defun egraph-merge (graph left right)
  "Merge LEFT and RIGHT classes in GRAPH and return the new representative."
  (let* ((left-root (egraph-find graph left))
         (right-root (egraph-find graph right)))
    (cond
      ((eq left-root right-root) left-root)
      (t
       (let ((left-rank (gethash left-root (egraph-rank graph)))
             (right-rank (gethash right-root (egraph-rank graph))))
         (when (< left-rank right-rank)
           (rotatef left-root right-root)
           (rotatef left-rank right-rank))
         (setf (gethash right-root (egraph-parent graph)) left-root
               (gethash left-root (egraph-members graph))
               (nconc (gethash left-root (egraph-members graph))
                      (gethash right-root (egraph-members graph))))
         (remhash right-root (egraph-members graph))
         (when (= left-rank right-rank)
           (incf (gethash left-root (egraph-rank graph))))
         left-root)))))

(defun egraph-equivalent-p (graph left right)
  "Return true when LEFT and RIGHT are in the same e-class."
  (eq (egraph-find graph left) (egraph-find graph right)))

(defun egraph-class-members (graph form-or-value)
  "Return the Forms in FORM-OR-VALUE's e-class."
  (copy-list (gethash (egraph-find graph form-or-value)
                      (egraph-members graph))))

(defun unify-forms (graph left right)
  "Unify structurally equal Forms, otherwise record them as equivalent."
  (let ((left-form (%as-form left))
        (right-form (%as-form right)))
    (if (equal (form-value left-form) (form-value right-form))
        (egraph-add graph left-form)
        (egraph-merge graph left-form right-form))))
