;;;; analysis-profiles.lisp --- project, concept, knot, and collapse profiles.

(in-package #:rosette-sexpr-dag)

(defun project-summary (index)
  "Return a plist summary for INDEX."
  (list :tree-nodes (dag-index-tree-node-count index)
        :dag-nodes (dag-index-node-count index)
        :beta1 (beta1 index)
        :shared-ratio (shared-ratio index)
        :root-count (length (dag-index-root-ids index))))

(defun %profile-sample-summary (index row)
  (when row
    (let ((sexp (shared-subdag-sexp index row)))
      (list :size (shared-subdag-size row)
            :occurrences (shared-subdag-occurrences row)
            :beta1 (shared-subdag-beta1 row)
            :shape (shared-subdag-shape sexp)
            :motif (shared-subdag-motif sexp)
            :role (shared-subdag-role sexp)
            :gravity (shared-subdag-gravity row sexp)
            :sexp sexp))))

(defun %profile-concept-node (index node)
  (list :level (getf node :level)
        :key (getf node :key)
        :label (getf node :label)
        :count (getf node :count)
        :beta1 (getf node :beta1)
        :tree-mass (getf node :tree-mass)
        :sample (%profile-sample-summary index (getf node :sample))))

(defun %profile-implication (index rule)
  (list :antecedent (getf rule :antecedent)
        :consequent (getf rule :consequent)
        :support (getf rule :support)
        :antecedent-count (getf rule :antecedent-count)
        :confidence (getf rule :confidence)
        :sample (%profile-sample-summary index (getf rule :sample))))

(defun %profile-hotspot (index row)
  (list :library (getf row :library)
        :rows (getf row :rows)
        :beta1 (getf row :beta1)
        :tree-mass (getf row :tree-mass)
        :top-role (getf row :top-role)
        :top-gravity (getf row :top-gravity)
        :sample (%profile-sample-summary index (getf row :sample))))

(defun sexpr-type-profile (sexp)
  "Return a data-only typed-IR profile for one raw S-expression."
  (list :shape (shared-subdag-shape sexp)
        :role (shared-subdag-role sexp)
        :motif (shared-subdag-motif sexp)
        :gravity "manual review"
        :normalized-shape (normalize-sexpr-shape sexp)
        :literal-p (atom sexp)
        :operator (and (consp sexp) (first sexp))))

(defun %analysis-pass-args (limit min-size min-occurrences)
  (list :limit limit
        :min-size min-size
        :min-occurrences min-occurrences))

(defun %pressure-row (role gravity)
  (list :role role
        :gravity gravity
        :count 0
        :beta1 0
        :tree-mass 0
        :sample nil))

(defun %update-pressure-row (bucket row sexp)
  (declare (ignore sexp))
  (incf (getf bucket :count))
  (incf (getf bucket :beta1) (shared-subdag-beta1 row))
  (incf (getf bucket :tree-mass) (%shared-subdag-tree-mass row))
  (%maybe-update-bucket-sample bucket row #'shared-subdag-beta1))

(defun %dag-knot-score (row library-width kind-width runtime-weight)
  (* runtime-weight
     (+ (* 1000000 (shared-subdag-beta1 row))
        (* 10000 library-width)
        (* 1000 kind-width)
        (%shared-subdag-tree-mass row))))

(defun %dag-knot-row (index row runtime-costs)
  (let* ((sexp (shared-subdag-sexp index row))
         (libraries (shared-subdag-libraries row))
         (kinds (shared-subdag-kinds row))
         (library-width (length libraries))
         (kind-width (length kinds)))
    (multiple-value-bind (runtime-weight runtime-label)
        (%row-runtime-cost row sexp runtime-costs)
      (list :score (%dag-knot-score row library-width kind-width runtime-weight)
          :scope (shared-subdag-scope row)
          :libraries libraries
          :library-width library-width
          :kinds kinds
          :kind-width kind-width
          :runtime-weight runtime-weight
          :runtime-label runtime-label
          :role (shared-subdag-role sexp)
          :gravity (shared-subdag-gravity row sexp)
          :size (shared-subdag-size row)
          :occurrences (shared-subdag-occurrences row)
          :beta1 (shared-subdag-beta1 row)
          :tree-mass (%shared-subdag-tree-mass row)
          :sample (%profile-sample-summary index row)))))

(defun dag-knots (index &key (limit 20)
                            (min-size 8)
                            (min-occurrences 2)
                            actionable-only
                            runtime-costs)
  "Return high-pressure shared sub-DAGs that behave like refactoring knots.

A knot is still acyclic: the score highlights repeated structure whose reuse
has high beta1 mass and spans libraries or source kinds.  These rows are good
places to distinguish a small helper extraction from a larger domain object or
DSL boundary.  RUNTIME-COSTS accepts the same records as EXTRACTION-CANDIDATES
and raises knots that match traced source files or symbols."
  (let ((rows nil))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 16))
      (let ((sexp (shared-subdag-sexp index row)))
        (when (and (or (not actionable-only)
                       (%actionable-shared-subdag-p row sexp))
                   (not (eq (shared-subdag-role sexp) :metadata)))
          (push (%dag-knot-row index row runtime-costs) rows))))
    (subseq (sort rows #'> :key (lambda (row) (getf row :score)))
            0
            (min limit (length rows)))))

(defun layer-target-pressure (index &key (limit 20)
                                        (min-size 8)
                                        (min-occurrences 2))
  "Return actionable pressure buckets keyed by semantic ROLE and target GRAVITY.

This is the hierarchy-oriented complement to per-library hotspots: it answers
which lower layer, primitive family, or local helper class is being requested
by repeated sexpr structure."
  (let ((buckets (%make-equal-table)))
    (dolist (row (%largest-rows-for-pass index limit min-size
                                         min-occurrences 12))
      (let ((sexp (shared-subdag-sexp index row)))
        (when (%hierarchy-evidence-subdag-p row sexp)
          (let* ((role (shared-subdag-role sexp))
                 (gravity (shared-subdag-gravity row sexp))
                 (key (list role gravity))
                 (bucket (or (gethash key buckets)
                             (setf (gethash key buckets)
                                   (%pressure-row role gravity)))))
            (%update-pressure-row bucket row sexp)))))
    (let ((rows nil))
      (maphash (lambda (_ bucket)
                 (declare (ignore _))
                 (push bucket rows))
               buckets)
      (subseq (sort rows #'>
                    :key (lambda (row)
                           (+ (* 1000000 (getf row :beta1 0))
                              (getf row :tree-mass 0))))
              0
              (min limit (length rows))))))

(defun concept-profile (index &key (limit 20)
                                  (min-size 8)
                                  (min-occurrences 2)
                                  actionable-only)
  "Return a machine-readable concept profile for repeated sexpr structure.

The profile is intended as a bridge from DAG CSE reports into hierarchy
tooling: it packages the project summary, top concept-lattice nodes,
high-confidence implications, and per-library hotspots in one plist."
  (let ((pass-args (%analysis-pass-args limit min-size min-occurrences)))
    (list :summary (project-summary index)
          :concepts (mapcar (lambda (node)
                              (%profile-concept-node index node))
                            (apply #'concept-lattice index pass-args))
          :implications (mapcar (lambda (rule)
                                  (%profile-implication index rule))
                                (apply #'concept-implications
                                       index
                                       :actionable-only actionable-only
                                       pass-args))
          :hotspots (mapcar (lambda (row)
                              (%profile-hotspot index row))
                            (apply #'library-hotspots
                                   index
                                   :actionable-only actionable-only
                                   pass-args)))))

(defun layer-collapse-certificate (index &key (limit 20)
                                             (min-size 8)
                                             (min-occurrences 2))
  "Return a data-only certificate for reducing code to lower-layer concepts.

This does not prove semantic equivalence.  It records the graph evidence a
lowering pass needs next: current DAG compression, repeated concept nodes,
actionable hotspots, role buckets, and implications that suggest primitive
ownership."
  (let ((profile (concept-profile index
                                  :limit limit
                                  :min-size min-size
                                  :min-occurrences min-occurrences
                                  :actionable-only t))
        (pass-args (%analysis-pass-args limit min-size min-occurrences)))
    (list :kind :layer-collapse-certificate
          :summary (getf profile :summary)
          :primitive-roles (mapcar (lambda (node)
                                     (list :role (getf node :key)
                                           :count (getf node :count)
                                           :beta1 (getf node :beta1)
                                           :tree-mass (getf node :tree-mass)
                                           :sample (%profile-sample-summary
                                                    index
                                                    (getf node :sample))))
                                   (apply #'role-summary
                                          index
                                          :actionable-only t
                                          pass-args))
          :layer-targets (mapcar (lambda (row)
                                   (%profile-layer-target-row index row))
                                 (apply #'layer-target-pressure
                                        index
                                        pass-args))
          :concepts (getf profile :concepts)
          :implications (getf profile :implications)
          :hotspots (getf profile :hotspots))))

(defun %profile-extraction-candidate (index candidate)
  (let ((row (extraction-candidate-row candidate)))
    (list :name (extraction-candidate-name candidate)
          :parameters (extraction-candidate-parameters candidate)
          :template (extraction-candidate-template candidate)
          :call-template (extraction-candidate-call-template candidate)
          :score (extraction-candidate-score candidate)
          :runtime-weight (extraction-candidate-runtime-weight candidate)
          :runtime-label (extraction-candidate-runtime-label candidate)
          :sample (%profile-sample-summary index row))))

(defun %profile-isomorphism-group (index group)
  (list :shape (getf group :shape)
        :count (getf group :count)
        :beta1 (getf group :beta1)
        :scope (getf group :scope)
        :libraries (getf group :libraries)
        :sample (%profile-sample-summary
                 index
                 (first (getf group :rows)))))

(defun %profile-dependency-cone (index row)
  (list :libraries (getf row :libraries)
        :width (getf row :width)
        :scope (getf row :scope)
        :rows (getf row :rows)
        :beta1 (getf row :beta1)
        :tree-mass (getf row :tree-mass)
        :top-role (getf row :top-role)
        :top-gravity (getf row :top-gravity)
        :sample (%profile-sample-summary index (getf row :sample))))

(defun %profile-layer-target-row (index row)
  (list :role (getf row :role)
        :gravity (getf row :gravity)
        :count (getf row :count)
        :beta1 (getf row :beta1)
        :tree-mass (getf row :tree-mass)
        :sample (%profile-sample-summary index (getf row :sample))))

(defun %profile-layer-target (index row)
  (%profile-layer-target-row index row))

(defun simplification-leaps (index &key (limit 5)
                                       (min-size 8)
                                       (min-occurrences 2))
  "Return the five highest-leverage sexpr simplification reports.

The result is data-only and deliberately operational: extraction drafts name
concrete helpers, isomorphism groups find same-shaped forms beyond exact
duplication, dependency cones expose library coupling, layer targets say where
lower abstractions are wanted, and implications turn the concept lattice into
hierarchy rules."
  (let ((pass-args (%analysis-pass-args limit min-size min-occurrences)))
    (list :kind :simplification-leaps
          :summary (project-summary index)
          :extractions
          (mapcar (lambda (candidate)
                    (%profile-extraction-candidate index candidate))
                  (apply #'extraction-candidates index pass-args))
          :isomorphisms
          (mapcar (lambda (group)
                    (%profile-isomorphism-group index group))
                  (apply #'structural-isomorphism-groups
                         index
                         :actionable-only t
                         pass-args))
          :dependency-cones
          (mapcar (lambda (row)
                    (%profile-dependency-cone index row))
                  (apply #'dependency-cones
                         index
                         :actionable-only t
                         pass-args))
          :layer-targets
          (mapcar (lambda (row)
                    (%profile-layer-target index row))
                  (apply #'layer-target-pressure index pass-args))
          :implications
          (mapcar (lambda (rule)
                    (%profile-implication index rule))
                  (apply #'concept-implications
                         index
                         :actionable-only t
                         pass-args)))))

