;;;; package.lisp --- Public API.

(in-package #:cl-user)

(defpackage #:rosette-sexpr-dag
  (:use #:cl)
  (:import-from #:rosette-scalar-core
                #:as-f64)
  (:import-from #:rosette-string-escape
                #:dot-escape)
  (:export
   #:dag-node
   #:dag-node-id
   #:dag-node-kind
   #:dag-node-value
   #:dag-node-children
   #:dag-node-size
   #:dag-node-occurrences
   #:dag-node-provenance

   #:dag-index
   #:make-dag-index
   #:dag-index-nodes
   #:dag-index-root-ids
   #:dag-index-node-count
   #:dag-index-tree-node-count

   #:sexp->dag
   #:dag->sexp
   #:tree-node-count
   #:dag-node-count
   #:beta1
   #:shared-ratio
   #:dot-escape
   #:print-dag-dot

   #:read-lisp-forms
   #:index-lisp-file
   #:index-lisp-files
   #:index-directory

   #:shared-subdag
   #:shared-subdag-node
   #:shared-subdag-size
   #:shared-subdag-occurrences
   #:shared-subdag-beta1
   #:shared-subdag-provenance
   #:shared-subdag-kinds
   #:shared-subdag-libraries
   #:shared-subdag-scope
   #:shared-subdag-shape
   #:shared-subdag-sexp
   #:shared-subdag-motif
   #:shared-subdag-role
   #:shared-subdag-gravity
   #:shared-subdag-extraction-hint
   #:largest-shared-subdags
   #:sexpr-type-profile
   #:shared-structure-summary
   #:print-shared-structure-summary
   #:normalize-sexpr-shape
   #:structural-isomorphism-groups
   #:print-structural-isomorphism-groups
   #:concept-lattice
   #:concept-profile
   #:dag-knots
   #:print-dag-knots
   #:layer-target-pressure
   #:print-layer-target-pressure
   #:layer-collapse-certificate
   #:print-layer-collapse-certificate
   #:simplification-leaps
   #:print-simplification-leaps
   #:print-concept-lattice
   #:print-concept-lattice-dot
   #:concept-implications
   #:print-concept-implications
   #:library-hotspots
   #:print-library-hotspots
   #:dependency-cones
   #:print-dependency-cones
   #:role-summary
   #:print-role-summary
   #:extraction-candidate
   #:extraction-candidate-name
   #:extraction-candidate-row
   #:extraction-candidate-parameters
   #:extraction-candidate-template
   #:extraction-candidate-call-template
   #:extraction-candidate-score
   #:extraction-candidate-runtime-weight
   #:extraction-candidate-runtime-label
   #:extraction-candidates
   #:print-extraction-candidates
   #:print-extraction-patches
   #:read-runtime-costs
   #:print-shared-subdags
   #:project-summary

   ;; rosette-graph-core interop
   #:dag-index->csr
   #:dag-index-bfs-order
   #:dag-index-postorder))
