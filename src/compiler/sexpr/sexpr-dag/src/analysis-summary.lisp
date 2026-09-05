;;;; analysis-summary.lisp --- shared-structure summaries and printers.

(in-package #:rosette-sexpr-dag)

(defun %summary-bucket-key (index row)
  (let ((sexp (shared-subdag-sexp index row)))
    (list :scope (shared-subdag-scope row)
          :shape (shared-subdag-shape sexp)
          :motif (shared-subdag-motif sexp)
          :role (shared-subdag-role sexp)
          :gravity (shared-subdag-gravity row sexp)
          :kinds (shared-subdag-kinds row)
          :hint (shared-subdag-extraction-hint row sexp))))

(defun %summary-bucket-key-for-sexp (row sexp)
  (list :scope (shared-subdag-scope row)
        :shape (shared-subdag-shape sexp)
        :motif (shared-subdag-motif sexp)
        :role (shared-subdag-role sexp)
        :gravity (shared-subdag-gravity row sexp)
        :kinds (shared-subdag-kinds row)
        :hint (shared-subdag-extraction-hint row sexp)))

(defun %maybe-update-bucket-sample (bucket row key-fn)
  (let ((sample (getf bucket :sample)))
    (when (or (null sample)
              (> (funcall key-fn row)
                 (funcall key-fn sample)))
      (setf (getf bucket :sample) row)))
  bucket)

(defun %shared-subdag-tree-mass (row)
  (* (shared-subdag-size row)
     (shared-subdag-occurrences row)))

(defun %make-aggregate-bucket (&rest initargs)
  (append initargs
          (list :rows 0
                :beta1 0
                :tree-mass 0
                :roles (make-hash-table :test #'eq)
                :gravities (%make-equal-table)
                :sample nil)))

(defun %update-aggregate-bucket (bucket row sexp)
  (incf (getf bucket :rows))
  (incf (getf bucket :beta1) (shared-subdag-beta1 row))
  (incf (getf bucket :tree-mass) (%shared-subdag-tree-mass row))
  (%incf-hash (getf bucket :roles) (shared-subdag-role sexp))
  (%incf-hash (getf bucket :gravities) (shared-subdag-gravity row sexp))
  (%maybe-update-bucket-sample bucket row #'shared-subdag-beta1))

(defun %largest-rows-for-pass (index limit min-size min-occurrences multiplier)
  (largest-shared-subdags index
                          :limit (* limit multiplier)
                          :min-size min-size
                          :min-occurrences min-occurrences))

(defun %update-summary-bucket (bucket row)
  (incf (getf bucket :count 0))
  (incf (getf bucket :beta1 0) (shared-subdag-beta1 row))
  (incf (getf bucket :tree-mass 0) (%shared-subdag-tree-mass row))
  (%maybe-update-bucket-sample bucket row #'shared-subdag-beta1))

(defun %make-summary-bucket (key)
  (append key
          (list :count 0
                :beta1 0
                :tree-mass 0
                :sample nil)))

(defun shared-structure-summary (index &key (limit 20)
                                         (min-size 4)
                                         (min-occurrences 2))
  "Return aggregate buckets for repeated sub-DAGs in INDEX.

Each bucket groups repeated structure by scope, syntactic shape, source kind,
and extraction hint.  This makes the report useful for architecture work: a
  large cross-library :LOCAL-COMPUTATION bucket points at missing primitives,
while a large :LITERAL-DATA bucket points at shared constants or fixtures."
  (let ((table (%make-equal-table)))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 16))
      (let* ((sexp (shared-subdag-sexp index row))
             (key (%summary-bucket-key-for-sexp row sexp))
             (bucket (or (gethash key table)
                         (setf (gethash key table)
                               (%make-summary-bucket key)))))
        (%update-summary-bucket bucket row)))
    (let ((rows nil))
      (maphash (lambda (_ bucket)
                 (declare (ignore _))
                 (push bucket rows))
               table)
      (subseq (sort rows #'>
                    :key (lambda (bucket)
                           (+ (* 1000000 (getf bucket :beta1 0))
                              (getf bucket :tree-mass 0))))
              0
              (min limit (length rows))))))

(defun %print-shared-subdag-provenance-lines
    (stream sample &key (limit 2) library)
  (when (and sample (plusp limit))
    (let ((provenance
            (if library
                (remove-if-not
                 (lambda (prov)
                   (string= library (%library-name (getf prov :file ""))))
                 (shared-subdag-provenance sample))
                (shared-subdag-provenance sample))))
      (dolist (prov (subseq provenance 0 (min limit (length provenance))))
      (format stream "   at ~A form ~A~%"
              (getf prov :file)
              (getf prov :form-index))))))

(defun %print-shared-subdag-sample-line
    (stream index sample sexp-limit &key motif gravity (provenance-limit 0)
                                      provenance-library)
  (when sample
    (let ((sexp (shared-subdag-sexp index sample)))
      (format stream
              (if (or motif gravity)
                  "   sample size=~D occurrences=~D motif=~S gravity=~A ~A~%"
                  "   sample size=~D occurrences=~D ~*~*~A~%")
              (shared-subdag-size sample)
              (shared-subdag-occurrences sample)
              (and motif (shared-subdag-motif sexp))
              (and gravity (shared-subdag-gravity sample sexp))
              (%short-sexp-string sexp sexp-limit))
      (%print-shared-subdag-provenance-lines
       stream sample
       :limit provenance-limit
       :library provenance-library))))

(defun print-shared-structure-summary (index &key (stream *standard-output*)
                                                (limit 12)
                                                (min-size 4)
                                                (min-occurrences 2)
                                                (sexp-limit 80))
  "Print aggregate repeated-structure buckets for INDEX."
  (format stream "~&Shared structure summary~%")
  (format stream "  tree nodes: ~D~%" (dag-index-tree-node-count index))
  (format stream "  dag nodes:  ~D~%" (dag-index-node-count index))
  (format stream "  beta1:      ~D~%" (beta1 index))
  (format stream "  ratio:      ~,4F~2%" (shared-ratio index))
  (loop for bucket in (shared-structure-summary index
                                                :limit limit
                                                :min-size min-size
                                                :min-occurrences min-occurrences)
        for rank from 1
        for sample = (getf bucket :sample)
        do (format stream "~&~D. scope=~S shape=~S motif=~S role=~S kinds=~S~%"
                   rank
                   (getf bucket :scope)
                   (getf bucket :shape)
                   (getf bucket :motif)
                   (getf bucket :role)
                   (getf bucket :kinds))
           (format stream "   buckets=~D beta1=~D tree-mass=~D gravity=~A hint=~A~%"
                   (getf bucket :count)
                   (getf bucket :beta1)
                   (getf bucket :tree-mass)
                   (getf bucket :gravity)
                   (getf bucket :hint))
           (%print-shared-subdag-sample-line
            stream index sample sexp-limit)))

(defun print-shared-subdags (index &key (stream *standard-output*)
                                      (limit 20)
                                      (min-size 4)
                                      (min-occurrences 2)
                                      kind
                                      scope
                                      shape
                                      (sexp-limit 100))
  "Print a compact repeated-sub-DAG report."
  (format stream "~&Shared s-expression DAG report~%")
  (format stream "  tree nodes: ~D~%" (dag-index-tree-node-count index))
  (format stream "  dag nodes:  ~D~%" (dag-index-node-count index))
  (format stream "  beta1:      ~D~%" (beta1 index))
  (format stream "  ratio:      ~,4F~2%" (shared-ratio index))
  (loop for row in (remove-if-not
                    (lambda (row)
	                      (and (or (null kind)
	                               (member kind (shared-subdag-kinds row) :test #'eq))
	                           (or (null scope)
	                               (eq scope (shared-subdag-scope row)))
	                           (or (null shape)
	                               (eq shape (shared-subdag-shape
	                                          (shared-subdag-sexp index row))))))
                    (largest-shared-subdags index
                                            :limit (* limit 8)
                                            :min-size min-size
                                            :min-occurrences min-occurrences))
	        for rank from 1
	        for sexp = (shared-subdag-sexp index row)
	        while (<= rank limit)
	        do (format stream "~&~D. size=~D occurrences=~D beta1=~D~%"
                   rank
	                   (shared-subdag-size row)
	                   (shared-subdag-occurrences row)
	                   (shared-subdag-beta1 row))
	           (format stream "   kinds=~S scope=~S shape=~S motif=~S role=~S gravity=~A hint=~A~%"
	                   (shared-subdag-kinds row)
	                   (shared-subdag-scope row)
	                   (shared-subdag-shape sexp)
                       (shared-subdag-motif sexp)
                       (shared-subdag-role sexp)
                       (shared-subdag-gravity row sexp)
	                   (shared-subdag-extraction-hint row sexp))
	           (format stream "   ~A~%" (%short-sexp-string
                                     sexp sexp-limit))
           (dolist (prov (subseq (shared-subdag-provenance row)
                                 0
                                 (min 3 (length (shared-subdag-provenance row)))))
             (format stream "   at ~A form ~A~%"
                     (getf prov :file)
                     (getf prov :form-index))))
  (values))
