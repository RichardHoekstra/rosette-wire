;;;; analysis-hotspots.lisp --- library hotspot, dependency cone, and role reports.

(in-package #:rosette-sexpr-dag)

(defun %incf-hash (table key &optional (delta 1))
  (incf (gethash key table 0) delta))

(defun %hotspot-top-key (table)
  (let ((pairs nil))
    (maphash (lambda (key value)
               (push (cons key value) pairs))
             table)
    (caar (sort pairs #'> :key #'cdr))))

(defun %transparent-role-p (role)
  (member role '(:build-metadata :compiler-policy :test-harness
                 :timing-scaffold :analysis-scaffold :config-scaffold
                 :reporting-scaffold :equality-policy :metadata)
          :test #'eq))

(defun %actionable-shared-subdag-p (row sexp)
  (let ((role (shared-subdag-role sexp))
        (shape (shared-subdag-shape sexp))
        (kinds (shared-subdag-kinds row)))
    (and (member :source kinds :test #'eq)
         (not (%transparent-role-p role))
         (not (eq role :byte-buffer))
         (not (and (eq role :allocator)
                   (%sexp-contains-symbol-p
                    sexp '("zeros" "zeros3" "make-double-float-array"))))
         (not (and (eq role :state-transition)
                   (%sexp-contains-symbol-p
                    sexp '("finite-state-merge-plist-for-states"
                           "finite-state-advance-plist-for-state"))))
         (not (%sexp-contains-symbol-p
               sexp '("limit" "min-size" "min-occurrences")))
         (not (%paired-vector-access-p sexp))
         (not (%coerced-vector-access-p sexp))
         (not (%float-accumulator-finalization-p sexp))
         (not (%vector-element-difference-p sexp))
         (not (member shape '(:declaration :lambda-list
                              :lambda-list-fragment :build-metadata
                              :equality-policy
                              :test-harness :array-dimensions :type-spec
                              :domain-parameter-list
                              :macro-binding-spec
                              :literal-data)
                      :test #'eq)))))

(defun %hierarchy-evidence-subdag-p (row sexp)
  (let ((role (shared-subdag-role sexp))
        (shape (shared-subdag-shape sexp))
        (kinds (shared-subdag-kinds row)))
    (and (member :source kinds :test #'eq)
         (not (%transparent-role-p role))
         (not (and (eq role :allocator)
                   (%sexp-contains-symbol-p
                    sexp '("zeros" "zeros3" "make-double-float-array"))))
         (not (and (eq role :state-transition)
                   (%sexp-contains-symbol-p
                    sexp '("finite-state-merge-plist-for-states"
                           "finite-state-advance-plist-for-state"))))
         (not (%sexp-contains-symbol-p
               sexp '("limit" "min-size" "min-occurrences")))
         (not (%paired-vector-access-p sexp))
         (not (%coerced-vector-access-p sexp))
         (not (%float-accumulator-finalization-p sexp))
         (not (%vector-element-difference-p sexp))
         (not (member shape '(:declaration :lambda-list
                              :lambda-list-fragment :build-metadata
                              :equality-policy
                              :test-harness :array-dimensions :type-spec
                              :domain-parameter-list
                              :macro-binding-spec
                              :literal-data)
                      :test #'eq)))))

(defun %intentional-library-shared-subdag-p (library sexp)
  (and (string= library "rosette-foundation-categorical")
       (eq (shared-subdag-role sexp) :normalizer)
       (%sexp-contains-symbol-p sexp '("coerce" "double-float"))))

(defun %hotspot-bucket-profile (bucket)
  (list :rows (getf bucket :rows)
        :beta1 (getf bucket :beta1)
        :tree-mass (getf bucket :tree-mass)
        :top-role (%hotspot-top-key
                   (getf bucket :roles))
        :top-gravity (%hotspot-top-key
                      (getf bucket :gravities))
        :sample (getf bucket :sample)))

(defun library-hotspots (index &key (limit 20)
                                   (min-size 8)
                                   (min-occurrences 2)
                                   actionable-only)
  "Return per-library repeated sexpr hotspots.

Each shared sub-DAG contributes its beta1/tree mass once to every library in
its provenance. The result orders libraries by how much reusable structure they
participate in, with top semantic role and extraction gravity attached."
  (let ((buckets (%make-equal-table)))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 12))
      (let* ((sexp (shared-subdag-sexp index row))
             (provenance (if actionable-only
                             (remove-if-not
                              (lambda (prov)
                                (eq (%path-kind (getf prov :file ""))
                                    :source))
                              (shared-subdag-provenance row))
                             (shared-subdag-provenance row)))
             (libs (remove-duplicates
                    (remove nil
                            (mapcar (lambda (prov)
                                      (%library-name (getf prov :file "")))
                                    provenance))
                    :test #'string=)))
        (unless (and actionable-only
                     (not (%actionable-shared-subdag-p row sexp)))
          (dolist (lib libs)
            (unless (and actionable-only
                         (%intentional-library-shared-subdag-p lib sexp))
              (let ((bucket (or (gethash lib buckets)
                                (setf (gethash lib buckets)
                                      (%make-aggregate-bucket :library lib)))))
                (%update-aggregate-bucket bucket row sexp)))))))
    (let ((rows nil))
      (maphash (lambda (_ bucket)
                 (declare (ignore _))
                 (push (append (list :library (getf bucket :library))
                               (%hotspot-bucket-profile bucket))
                       rows))
               buckets)
      (subseq (sort rows #'>
                    :key (lambda (row)
                           (+ (* 1000000 (getf row :beta1 0))
                              (getf row :tree-mass 0))))
              0
              (min limit (length rows))))))

(defun print-library-hotspots
    (index &key (stream *standard-output*) (limit 20) (min-size 8)
                (min-occurrences 2) actionable-only (sexp-limit 80))
  "Print per-library repeated sexpr hotspot ranking."
  (format stream "~&Library shared-structure hotspots~%")
  (format stream "  tree nodes: ~D~%" (dag-index-tree-node-count index))
  (format stream "  dag nodes:  ~D~%" (dag-index-node-count index))
  (format stream "  beta1:      ~D~%" (beta1 index))
  (when actionable-only
    (format stream "  filter:     actionable only~%"))
  (format stream "  ratio:      ~,4F~2%" (shared-ratio index))
  (let ((rows (library-hotspots index
                                :limit limit
                                :min-size min-size
                                :min-occurrences min-occurrences
                                :actionable-only actionable-only)))
    (if rows
        (loop for row in rows
              for rank from 1
              for sample = (getf row :sample)
              for sexp = (and sample (shared-subdag-sexp index sample))
              for displayed-role = (or (and sexp (shared-subdag-role sexp))
                                       (getf row :top-role))
              for displayed-gravity = (or (and sexp
                                               (shared-subdag-gravity sample sexp))
                                          (getf row :top-gravity))
              do (format stream "~&~D. ~A rows=~D beta1=~D tree-mass=~D role=~S gravity=~A~%"
                         rank
                         (getf row :library)
                         (getf row :rows)
                         (getf row :beta1)
                         (getf row :tree-mass)
                         displayed-role
                         displayed-gravity)
                 (%print-shared-subdag-sample-line
                  stream index sample sexp-limit
                  :provenance-limit 2
                  :provenance-library (getf row :library)))
        (format stream "  none at current thresholds~%"))))

(defun dependency-cones (index &key (limit 20)
                                   (min-size 8)
                                   (min-occurrences 2)
                                   actionable-only)
  "Return repeated-structure buckets keyed by participating libraries.

A one-library cone is isolated work: it can usually be extracted or lowered
without coordinating other libraries.  A wider cone marks cross-library
coupling and should be treated like a causal cone for API design."
  (let ((buckets (%make-equal-table)))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 12))
      (let* ((sexp (shared-subdag-sexp index row))
             (libs (shared-subdag-libraries row)))
        (unless (and actionable-only
                     (not (%actionable-shared-subdag-p row sexp)))
          (when libs
            (let* ((key libs)
                   (bucket (or (gethash key buckets)
                               (setf (gethash key buckets)
                                     (%make-aggregate-bucket
                                      :libraries libs
                                      :width (length libs)
                                      :scope (if (rest libs)
                                                 :cross-library
                                                 :intra-library))))))
              (%update-aggregate-bucket bucket row sexp))))))
    (let ((rows nil))
      (maphash (lambda (_ bucket)
                 (declare (ignore _))
                 (push (append (list :libraries (getf bucket :libraries)
                                     :width (getf bucket :width)
                                     :scope (getf bucket :scope))
                               (%hotspot-bucket-profile bucket))
                       rows))
               buckets)
      (subseq (sort rows #'>
                    :key (lambda (row)
                           (+ (* 1000000000 (getf row :width 0))
                              (* 1000000 (getf row :beta1 0))
                              (getf row :tree-mass 0))))
              0
              (min limit (length rows))))))

(defun print-dependency-cones
    (index &key (stream *standard-output*) (limit 20) (min-size 8)
                (min-occurrences 2) actionable-only (sexp-limit 80))
  "Print repeated-structure dependency cones grouped by library set."
  (format stream "~&Dependency cones~%")
  (when actionable-only
    (format stream "  filter:     actionable only~%"))
  (let ((rows (dependency-cones index
                                :limit limit
                                :min-size min-size
                                :min-occurrences min-occurrences
                                :actionable-only actionable-only)))
    (if rows
        (loop for row in rows
              for rank from 1
              for sample = (getf row :sample)
              do (format stream "~&~D. width=~D scope=~S libs=~{~A~^,~} rows=~D beta1=~D tree-mass=~D role=~S gravity=~A~%"
                         rank
                         (getf row :width)
                         (getf row :scope)
                         (getf row :libraries)
                         (getf row :rows)
                         (getf row :beta1)
                         (getf row :tree-mass)
                         (getf row :top-role)
                         (getf row :top-gravity))
                 (%print-shared-subdag-sample-line
                  stream index sample sexp-limit))
        (format stream "  none at current thresholds~%"))))

(defun role-summary (index &key (limit 20)
                               (min-size 4)
                               (min-occurrences 2)
                               actionable-only)
  "Return aggregate buckets for repeated sub-DAG semantic roles."
  (remove-if-not
   (lambda (node)
     (and (eq (getf node :level) :role)
          (or (not actionable-only)
              (let* ((sample (getf node :sample))
                     (sexp (and sample
                                (shared-subdag-sexp index sample))))
                (and sexp
                     (%actionable-shared-subdag-p sample sexp))))))
   (concept-lattice index
                    :limit (* limit 8)
                    :min-size min-size
                    :min-occurrences min-occurrences)))

(defun print-role-summary
    (index &key (stream *standard-output*) (limit 20) (min-size 4)
                (min-occurrences 2) actionable-only (sexp-limit 80))
  "Print repeated sub-DAG semantic-role buckets."
  (format stream "~&Semantic role summary~%")
  (format stream "  tree nodes: ~D~%" (dag-index-tree-node-count index))
  (format stream "  dag nodes:  ~D~%" (dag-index-node-count index))
  (format stream "  beta1:      ~D~%" (beta1 index))
  (when actionable-only
    (format stream "  filter:     actionable only~%"))
  (format stream "  ratio:      ~,4F~2%" (shared-ratio index))
  (let ((rows (role-summary index
                            :limit limit
                            :min-size min-size
                            :min-occurrences min-occurrences
                            :actionable-only actionable-only)))
    (if rows
        (loop for node in (subseq rows 0 (min limit (length rows)))
              for rank from 1
              for sample = (getf node :sample)
              do (format stream "~&~D. role=~S count=~D beta1=~D tree-mass=~D~%"
                         rank
                         (getf node :key)
                         (getf node :count)
                         (getf node :beta1)
                         (getf node :tree-mass))
                 (%print-shared-subdag-sample-line
                  stream index sample sexp-limit :motif t :gravity t))
        (format stream "  none at current thresholds~%"))))
