;;;; analysis-isomorphism.lisp --- structural isomorphism and concept lattice reports.

(in-package #:rosette-sexpr-dag)

(defun normalize-sexpr-shape (sexp)
  "Normalize symbols and scalar literals while preserving list structure.

This is deliberately coarser than hash-consing: two expressions with the
same control/data shape but different local names land in the same bucket."
  (let ((symbol-map (make-hash-table :test #'eq))
        (next-symbol 0))
    (labels ((norm-symbol (sym)
               (cond ((keywordp sym) sym)
                     ((member sym '(quote function lambda let let* flet labels
                                    loop dotimes dolist setf incf decf if when
                                    unless cond progn defun defmacro defmethod
                                    declare declaim the type)
                              :test #'eq)
                      sym)
                     (t
                      (or (gethash sym symbol-map)
                          (setf (gethash sym symbol-map)
                                (intern (format nil "?S~D" (incf next-symbol))
                                        :keyword))))))
             (walk (x)
               (cond ((consp x) (cons (walk (car x)) (walk (cdr x))))
                     ((symbolp x) (norm-symbol x))
                     ((numberp x) :?number)
                     ((stringp x) :?string)
                     ((characterp x) :?character)
                     (t :?literal))))
      (walk sexp))))

(defun structural-isomorphism-groups (index &key (limit 12)
                                                (min-size 8)
                                                (min-occurrences 2)
                                                actionable-only
                                                scope)
  "Return groups whose normalized sexpr shape appears more than once.

Unlike LARGEST-SHARED-SUBDAGS, this scans all DAG nodes.  That catches
same-shaped code where local variable names or scalar literals differ, which
is the useful case for finding latent abstractions across libraries."
  (let ((table (%make-equal-table)))
    (loop for node across (dag-index-nodes index)
          when (>= (dag-node-size node) min-size)
            do (let* ((sexp (dag->sexp index node))
                      (key (normalize-sexpr-shape sexp))
                      (row (make-shared-subdag
                            node
                            (dag-node-size node)
                            (dag-node-occurrences node)
                            (* (dag-node-size node)
                               (max 0 (1- (dag-node-occurrences node))))
                            (reverse (dag-node-provenance node)))))
                 (unless (and actionable-only
                              (not (%actionable-shared-subdag-p row sexp)))
                   (setf (gethash key table)
                         (cons row (gethash key table))))))
    (let ((groups nil))
      (maphash (lambda (key rows)
                 (when (>= (length rows) min-occurrences)
                   (let* ((sorted-rows (sort rows #'>
                                             :key #'shared-subdag-size))
                          (libraries (%isomorphism-group-libraries sorted-rows))
                          (group-scope (%isomorphism-group-scope libraries)))
                     (when (or (null scope) (eq scope group-scope))
                       (push (list :shape key
                                   :count (length sorted-rows)
                                   :beta1 (loop for row in sorted-rows
                                                 sum (shared-subdag-size row))
                                   :scope group-scope
                                   :libraries libraries
                                   :rows sorted-rows)
                             groups)))))
               table)
      (subseq (sort groups #'> :key (lambda (g) (getf g :beta1)))
              0
              (min limit (length groups))))))

(defun %isomorphism-group-libraries (rows)
  (sort (remove-duplicates
         (loop for row in rows append (shared-subdag-libraries row))
         :test #'string=)
        #'string<))

(defun %isomorphism-group-scope (libraries)
  (cond ((null libraries) :unknown)
        ((null (rest libraries)) :intra-library)
        (t :cross-library)))

(defun %library-summary-string (libraries &optional (limit 5))
  (cond
    ((null libraries) "unknown")
    ((<= (length libraries) limit)
     (format nil "~{~A~^,~}" libraries))
    (t
     (format nil "~D: ~{~A~^,~},..."
             (length libraries)
             (subseq libraries 0 limit)))))

(defun print-structural-isomorphism-groups
    (index &key (stream *standard-output*) (limit 12) (min-size 8)
                (min-occurrences 2) actionable-only scope (sexp-limit 80))
  "Print normalized sexpr-shape groups."
  (format stream "~&Structural isomorphism groups~%")
  (when actionable-only
    (format stream "  filter:     actionable only~%"))
  (when scope
    (format stream "  scope:      ~S~%" scope))
  (let ((groups (structural-isomorphism-groups
                 index
                 :limit limit
                 :min-size min-size
                 :min-occurrences min-occurrences
                 :actionable-only actionable-only
                 :scope scope)))
    (if groups
        (loop for group in groups
              for rank from 1
              do (format stream "~&~D. count=~D beta1=~D scope=~S libs=~A~%"
                         rank
                         (getf group :count)
                         (getf group :beta1)
                         (getf group :scope)
                         (%library-summary-string (getf group :libraries)))
                 (loop for row in (subseq (getf group :rows)
                                          0 (min 3 (length (getf group :rows))))
                       for sexp = (shared-subdag-sexp index row)
                       do (format stream "   size=~D occurrences=~D motif=~S role=~S ~A~%"
                                  (shared-subdag-size row)
                                  (shared-subdag-occurrences row)
                                  (shared-subdag-motif sexp)
                                  (shared-subdag-role sexp)
                                  (%short-sexp-string sexp sexp-limit))))
        (format stream "  none at current thresholds~%"))))
