;;;; analysis.lisp --- Shared-subgraph analysis.

(in-package #:rosette-sexpr-dag)

(defstruct (shared-subdag
            (:constructor make-shared-subdag
                (node size occurrences beta1 provenance)))
  node
  size
  occurrences
  beta1
  provenance)

(defun %shared-node-p (node min-size min-occurrences)
  (and (>= (dag-node-size node) min-size)
       (>= (dag-node-occurrences node) min-occurrences)))

(defun largest-shared-subdags (index &key (limit 20) (min-size 4) (min-occurrences 2))
  "Return the largest repeated sub-DAGs in INDEX.

The per-node beta1 estimate is SIZE * (OCCURRENCES - 1), which is the
amount of tree mass saved by sharing that sub-DAG occurrence in isolation."
  (let ((rows nil))
    (loop for node across (dag-index-nodes index)
          when (%shared-node-p node min-size min-occurrences)
            do (push (make-shared-subdag
                      node
                      (dag-node-size node)
                      (dag-node-occurrences node)
                      (* (dag-node-size node)
                         (1- (dag-node-occurrences node)))
                      (reverse (dag-node-provenance node)))
                     rows))
    (subseq (sort rows #'>
                  :key (lambda (row)
                         (+ (* 1000000 (shared-subdag-beta1 row))
                            (shared-subdag-size row))))
            0 (min limit (length rows)))))

(defun %short-sexp-string (sexp &optional (limit 100))
  (let ((*print-length* 8)
        (*print-level* 5)
        (*print-circle* nil))
    (let ((s (prin1-to-string sexp)))
      (if (> (length s) limit)
	          (concatenate 'string (subseq s 0 limit) "...")
	          s))))

(defun %proper-list-p (value)
  (loop for tail = value then (cdr tail)
        do (cond ((null tail) (return t))
                 ((not (consp tail)) (return nil)))))

(defun %every-proper-list-p (predicate value)
  (and (%proper-list-p value)
       (ignore-errors (every predicate value))))

(defun %path-kind (path)
  (cond ((search "/src/" path :test #'char-equal) :source)
        ((search "/tests/" path :test #'char-equal) :test)
        ((search "/examples/" path :test #'char-equal) :example)
        ((search "/bench/" path :test #'char-equal) :bench)
        ((search "/scripts/" path :test #'char-equal) :script)
        ((search "/tools/" path :test #'char-equal) :tool)
        (t :other)))

(defun shared-subdag-kinds (row)
  "Return source-location kinds present in ROW provenance."
  (sort (remove-duplicates
         (mapcar (lambda (prov)
                   (%path-kind (getf prov :file "")))
                 (shared-subdag-provenance row))
         :test #'eq)
        #'string<
        :key #'symbol-name))

(defun %library-name (path)
  (let* ((marker "/rosette-")
         (hit (search marker path :from-end t :test #'char-equal)))
    (when hit
      (let* ((start (1+ hit))
             (slash (position #\/ path :start start)))
        (and slash (subseq path start slash))))))

(defun shared-subdag-libraries (row)
  "Return sorted library names present in ROW provenance."
  (sort (remove nil
                (remove-duplicates
                 (mapcar (lambda (prov)
                           (%library-name (getf prov :file "")))
                         (shared-subdag-provenance row))
                 :test #'string=))
        #'string<))

(defun shared-subdag-scope (row)
  "Return :INTRA-LIBRARY, :CROSS-LIBRARY, or :UNKNOWN for ROW provenance."
  (let ((libs (shared-subdag-libraries row)))
    (cond ((null libs) :unknown)
          ((null (rest libs)) :intra-library)
          (t :cross-library))))

(defun shared-subdag-sexp (index row)
  "Return ROW's materialized s-expression from INDEX."
  (dag->sexp index (shared-subdag-node row)))

