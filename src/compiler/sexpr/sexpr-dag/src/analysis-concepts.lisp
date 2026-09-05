;;;; analysis-concepts.lisp --- concept lattice and implication reports.

(in-package #:rosette-sexpr-dag)

(defun %row-concept-chain (index row)
  (let* ((sexp (shared-subdag-sexp index row))
         (shape (shared-subdag-shape sexp))
         (motif (shared-subdag-motif sexp))
         (role (shared-subdag-role sexp))
         (scope (shared-subdag-scope row))
         (kinds (shared-subdag-kinds row)))
    (list (list :level :exact
                :key sexp
                :label "exact repeated sexpr")
          (list :level :normalized
                :key (normalize-sexpr-shape sexp)
                :label "same normalized sexpr shape")
          (list :level :motif
                :key motif
                :label "same reusable motif")
          (list :level :role
                :key role
                :label "same semantic role")
          (list :level :shape
                :key shape
                :label "same syntactic shape")
          (list :level :scope
                :key scope
                :label "same sharing scope")
          (list :level :kind
                :key kinds
                :label "same source kind set"))))

(defun %update-concept-bucket (bucket row)
  (incf (getf bucket :count 0))
  (incf (getf bucket :beta1 0) (shared-subdag-beta1 row))
  (incf (getf bucket :tree-mass 0)
        (* (shared-subdag-size row)
           (max 1 (shared-subdag-occurrences row))))
  (%maybe-update-bucket-sample bucket row #'shared-subdag-size))

(defun concept-lattice (index &key (limit 30)
                                  (min-size 8)
                                  (min-occurrences 2))
  "Return aggregate concept-lattice nodes for repeated sexpr structure.

Each repeated sub-DAG contributes a chain from exact sexpr identity through
normalized shape, motif, syntactic shape, sharing scope, and source-kind set.
The result is not a complete formal concept lattice; it is the useful
  architecture lattice for navigating from concrete code to higher layers."
  (let ((table (%make-equal-table)))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 12))
      (dolist (concept (%row-concept-chain index row))
        (let* ((table-key (list (getf concept :level)
                                (getf concept :key)))
               (bucket (copy-list
                        (or (gethash table-key table)
                            (list :level (getf concept :level)
                                  :key (getf concept :key)
                                  :label (getf concept :label))))))
          (setf (gethash table-key table)
                (%update-concept-bucket bucket row)))))
    (let ((nodes nil))
      (maphash (lambda (_ bucket)
                 (declare (ignore _))
                 (push bucket nodes))
               table)
      (subseq (sort nodes #'>
                    :key (lambda (bucket)
                           (+ (* 1000000 (getf bucket :tree-mass 0))
                              (getf bucket :beta1 0))))
              0
              (min limit (length nodes))))))

(defun print-concept-lattice
    (index &key (stream *standard-output*) (limit 30) (min-size 8)
                (min-occurrences 2) (sexp-limit 80))
  "Print aggregate concept-lattice nodes for repeated sexpr structure."
  (format stream "~&S-expression concept lattice~%")
  (format stream "  tree nodes: ~D~%" (dag-index-tree-node-count index))
  (format stream "  dag nodes:  ~D~%" (dag-index-node-count index))
  (format stream "  beta1:      ~D~%" (beta1 index))
  (format stream "  ratio:      ~,4F~2%" (shared-ratio index))
  (loop for node in (concept-lattice index
                                     :limit limit
                                     :min-size min-size
                                     :min-occurrences min-occurrences)
        for rank from 1
        for sample = (getf node :sample)
        for sexp = (and sample (shared-subdag-sexp index sample))
        do (format stream "~&~D. level=~S key=~A~%"
                   rank
                   (getf node :level)
                   (%short-sexp-string (getf node :key) sexp-limit))
           (format stream "   count=~D beta1=~D tree-mass=~D label=~A~%"
                   (getf node :count)
                   (getf node :beta1)
                   (getf node :tree-mass)
                   (getf node :label))
           (when sample
             (format stream "   sample size=~D occurrences=~D motif=~S role=~S gravity=~A ~A~%"
                     (shared-subdag-size sample)
                     (shared-subdag-occurrences sample)
                     (and sexp (shared-subdag-motif sexp))
                     (and sexp (shared-subdag-role sexp))
                     (and sexp (shared-subdag-gravity sample sexp))
                     (%short-sexp-string sexp sexp-limit)))))

(defun print-concept-lattice-dot
    (index &key (stream *standard-output*) (limit 30) (min-size 8)
                (min-occurrences 2) (sexp-limit 40))
  "Print the repeated sexpr concept lattice as Graphviz DOT."
  (let* ((nodes (concept-lattice index
                                 :limit limit
                                 :min-size min-size
                                 :min-occurrences min-occurrences))
         (node-ids (%make-equal-table))
         (edge-counts (%make-equal-table)))
    (loop for node in nodes
          for id from 0
          do (setf (gethash (list (getf node :level) (getf node :key))
                            node-ids)
                   id))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 12))
      (let ((chain (%row-concept-chain index row)))
        (loop for (left right) on chain
              while right
              for left-id = (gethash (list (getf left :level)
                                           (getf left :key))
                                     node-ids)
              for right-id = (gethash (list (getf right :level)
                                            (getf right :key))
                                      node-ids)
              when (and left-id right-id)
                do (incf (gethash (list left-id right-id) edge-counts 0)))))
    (format stream "digraph rosette_sexpr_concept_lattice {~%")
    (format stream "  rankdir=LR;~%")
    (format stream "  node [shape=box style=rounded];~%")
    (loop for node in nodes
          for id from 0
          do (format stream
                     "  c~D [label=\"~A\\n~A\\ncount=~D beta1=~D\"];~%"
                     id
                     (dot-escape (getf node :level) :readably t)
                     (dot-escape (%short-sexp-string (getf node :key)
                                                      sexp-limit)
                                 :readably t)
                     (getf node :count 0)
                     (getf node :beta1 0)))
    (maphash (lambda (edge count)
               (destructuring-bind (left-id right-id) edge
                 (format stream "  c~D -> c~D [label=\"~D\"];~%"
                         left-id right-id count)))
             edge-counts)
    (format stream "}~%")))

(defun %row-concept-attributes (row sexp)
  (remove-duplicates
   (list (list :scope (shared-subdag-scope row))
         (list :shape (shared-subdag-shape row))
         (list :motif (shared-subdag-motif sexp))
         (list :role (shared-subdag-role sexp))
         (list :gravity (shared-subdag-gravity row sexp)))
   :test #'equal))

(defun %format-concept-attribute (attribute)
  (format nil "~(~A=~A~)" (first attribute) (second attribute)))

(defun concept-implications (index &key (limit 20)
                                       (min-size 8)
                                       (min-occurrences 2)
                                       (min-support 2)
                                       (min-confidence 1.0)
                                       actionable-only)
  "Return high-confidence implications between repeated sexpr attributes.

This mines the lightweight formal-concept layer: if an attribute such as
ROLE=ALLOCATOR consistently implies GRAVITY=ARRAY-CORE across repeated
sub-DAGs, the implication becomes a candidate hierarchy rule."
  (let ((antecedent-counts (%make-equal-table))
        (pair-counts (%make-equal-table))
        (examples (%make-equal-table)))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 12))
      (let ((sexp (shared-subdag-sexp index row)))
        (unless (and actionable-only
                     (not (%actionable-shared-subdag-p row sexp)))
          (let ((attributes (%row-concept-attributes row sexp)))
            (dolist (left attributes)
              (incf (gethash left antecedent-counts 0))
              (dolist (right attributes)
                (unless (equal left right)
                  (let ((key (list left right)))
                    (incf (gethash key pair-counts 0))
                    (unless (gethash key examples)
                      (setf (gethash key examples) row))))))))))
    (let ((rules nil))
      (maphash
       (lambda (key support)
         (destructuring-bind (left right) key
           (let* ((antecedent-count (gethash left antecedent-counts 0))
                  (confidence (if (zerop antecedent-count)
                                  0.0
                                  (/ support antecedent-count))))
             (when (and (>= support min-support)
                        (>= confidence min-confidence))
               (push (list :antecedent left
                           :consequent right
                           :support support
                           :antecedent-count antecedent-count
                           :confidence confidence
                           :sample (gethash key examples))
                     rules)))))
       pair-counts)
      (subseq (sort rules #'>
                    :key (lambda (rule)
                           (+ (* 1000000 (getf rule :support 0))
                              (round (* 1000 (getf rule :confidence 0.0))))))
              0
              (min limit (length rules))))))

(defun print-concept-implications
    (index &key (stream *standard-output*) (limit 20) (min-size 8)
                (min-occurrences 2) (min-support 2)
                (min-confidence 1.0) actionable-only (sexp-limit 80))
  "Print high-confidence implications between repeated sexpr attributes."
  (format stream "~&S-expression concept implications~%")
  (format stream "  tree nodes: ~D~%" (dag-index-tree-node-count index))
  (format stream "  dag nodes:  ~D~%" (dag-index-node-count index))
  (format stream "  beta1:      ~D~%" (beta1 index))
  (when actionable-only
    (format stream "  filter:     actionable only~%"))
  (format stream "  ratio:      ~,4F~2%" (shared-ratio index))
  (let ((rules (concept-implications index
                                     :limit limit
                                     :min-size min-size
                                     :min-occurrences min-occurrences
                                     :min-support min-support
                                     :min-confidence min-confidence
                                     :actionable-only actionable-only)))
    (if rules
        (loop for rule in rules
              for rank from 1
              for sample = (getf rule :sample)
              for sexp = (and sample (shared-subdag-sexp index sample))
              do (format stream "~&~D. ~A -> ~A~%"
                         rank
                         (%format-concept-attribute (getf rule :antecedent))
                         (%format-concept-attribute (getf rule :consequent)))
                 (format stream "   support=~D antecedent-count=~D confidence=~,3F~%"
                         (getf rule :support)
                         (getf rule :antecedent-count)
                         (float (getf rule :confidence)))
                 (when sample
                   (format stream "   sample size=~D occurrences=~D ~A~%"
                           (shared-subdag-size sample)
                           (shared-subdag-occurrences sample)
                           (%short-sexp-string sexp sexp-limit))))
        (format stream "  none at current thresholds~%"))))
