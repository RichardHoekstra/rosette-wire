(in-package #:rosette-sexpr-dag)

(defun shared-subdag-role (sexp)
  "Return the semantic role that SEXP appears to play."
  (cond
    ((%compiler-policy-p sexp) :compiler-policy)
    ((%test-harness-p sexp) :test-harness)
    ((%build-metadata-p sexp) :build-metadata)
    ((%equality-policy-fragment-p sexp) :equality-policy)
    ((%literal-data-loader-p sexp) :literal-data)
    ((or (%timing-scaffold-p sexp)
         (%bench-scaffold-p sexp))
     :timing-scaffold)
    ((%byte-buffer-allocator-p sexp) :byte-buffer)
    ((or (%sexp-contains-symbol-p sexp '("make-array"
                                         "make-double-float-array"
                                         "zeros"
                                         "zeros3"
                                         "make-instance"))
         (and (consp sexp)
              (%proper-list-p sexp)
              (member :element-type sexp :test #'eq)))
     :allocator)
    ((or (%sexp-contains-symbol-p sexp '("verify" "valid" "check"
                                         "near=" "approx="))
         (and (symbolp sexp)
              (let ((name (string-downcase (symbol-name sexp))))
                (or (%string-suffix-p "-p" name)
                    (%string-prefix-p "valid-" name)))))
     :predicate)
    ((%sexp-contains-symbol-p sexp '("certificate" "certify" "witness"
                                     "verify-certificate" "kkt"))
     :certificate-check)
    ((%sexp-contains-symbol-p sexp '("spectrum" "eigenvalue"
                                     "multiplicity" "zeta" "units"))
     :spectrum-table)
    ((%sexp-contains-symbol-p sexp '("advance-state" "merge-states"
                                     "transition" "step-state"
                                     "next-state"
                                     "finite-state-merge-plist-for-states"
                                     "finite-state-advance-plist-for-state"))
     :state-transition)
    ((or (%vector-reduction-p sexp)
         (%paired-vector-access-p sexp)
         (%coerced-vector-access-p sexp)
         (%float-accumulator-finalization-p sexp)
         (%vector-element-difference-p sexp))
     :vector-reduction)
    ((%sexp-contains-symbol-p sexp '("periodic-index" "aref"
                                     "row-major-aref" "do-grid" "do-grid3"))
     :indexing)
    ((%serializer-scaffold-p sexp) :serializer)
    ((%config-scaffold-p sexp) :config-scaffold)
    ((%gpu-pipeline-p sexp) :gpu-pipeline)
    ((%analysis-scaffold-p sexp) :analysis-scaffold)
    ((%reporting-scaffold-p sexp) :reporting-scaffold)
    ((%work-scheduler-p sexp) :work-scheduler)
    ((eq (shared-subdag-shape sexp) :literal-data) :literal-data)
    ((%bit-packing-p sexp) :bit-packing)
    ((%representation-table-p sexp) :representation-table)
    ((%geometry-metric-p sexp) :geometry-metric)
    ((%symbolic-expression-p sexp) :symbolic-expression)
    ((%finite-difference-p sexp) :finite-difference)
    ((or (member (shared-subdag-shape sexp) '(:iteration :mutation)
                 :test #'eq)
         (%sexp-contains-symbol-p sexp '("kernel" "stencil")))
     :kernel)
    ((%sexp-contains-symbol-p sexp '("normalize" "canonical" "clamp"
                                     "coerce" "sort" "remove-duplicates"))
     :normalizer)
    ((member (shared-subdag-shape sexp) '(:declaration :lambda-list
                                          :lambda-list-fragment
                                          :array-dimensions
                                          :type-spec
                                          :domain-parameter-list
                                          :macro-binding-spec)
             :test #'eq)
     :metadata)
    (t :computation)))

(defun shared-subdag-gravity (row &optional sexp)
  "Infer the lowest likely target layer for extracting ROW.

This is a conservative routing hint, not an edit decision.  It combines
semantic role, motif, and sharing scope to point repeated structure at the
primitive/core layer that should probably own it."
  (let* ((form (or sexp nil))
         (role (and form (shared-subdag-role form)))
         (motif (and form (shared-subdag-motif form)))
         (scope (shared-subdag-scope row)))
    (cond
      ((eq role :compiler-policy) "keep as local compiler policy")
      ((eq role :test-harness) "test harness / rosette-assert-core")
      ((eq role :build-metadata) "keep as ASDF/build metadata")
      ((eq role :timing-scaffold) "benchmark/probe timing scaffold")
      ((eq role :vector-reduction) "rosette-linear-algebra")
      ((eq role :serializer) "local I/O/parser helper")
      ((eq role :config-scaffold) "local config/env helper")
      ((eq role :gpu-pipeline) "rosette-gpu-bridge / GGUF GPU helper")
      ((eq role :analysis-scaffold) "sexpr DAG analyzer scaffold")
      ((eq role :reporting-scaffold) "local reporting scaffold")
      ((eq role :work-scheduler) "rosette-work-scheduler")
      ((eq role :literal-data) "keep as literal data / named constant")
      ((eq role :byte-buffer) "rosette-byte-core")
      ((eq role :bit-packing) "rosette-byte-core")
      ((eq role :representation-table) "rosette-representation-core")
      ((eq role :geometry-metric) "rosette-vec3-core")
      ((eq role :symbolic-expression) "rosette-expression-core")
      ((eq role :finite-difference) "rosette-diff-ops")
      ((and (eq role :allocator)
            (%sexp-contains-symbol-p form '("zeros" "zeros3")))
       "rosette-discrete-grid")
      ((member role '(:allocator) :test #'eq) "rosette-array-core")
      ((member role '(:indexing) :test #'eq) "rosette-index-core")
      ((member role '(:predicate :normalizer) :test #'eq) "rosette-scalar-core")
      ((eq role :state-transition) "rosette-finite-data-core")
      ((eq role :certificate-check) "rosette-proof-witness")
      ((eq role :spectrum-table) "rosette-multiplicity-spectral")
      ((eq motif :grid-stencil) "rosette-discrete-grid")
      ((eq motif :finite-state-carrier) "rosette-finite-data-core")
      ((and (eq scope :cross-library)
            (member role '(:kernel :computation) :test #'eq))
       "new primitive/core helper")
      ((eq scope :intra-library) "local helper")
      (t "manual review"))))

(defun %shape-extraction-hint (shape)
  (case shape
    (:lambda-list "shared public/API lambda list; usually leave or document")
    (:lambda-list-fragment "shared lambda-list fragment; likely API symmetry")
    (:binding-list "shared binding list; inspect caller before extracting")
    (:definition "whole definition duplicate")
    (:declaration "shared declaration metadata; usually ignore")
    (:local-computation "shared local computation")
    (:iteration "shared loop/iteration")
    (:mutation "shared mutation/update")
    (:test-harness "shared test harness scaffolding")
    (:build-metadata "shared build metadata; usually ignore")
    (:array-dimensions "shared array dimension metadata; usually ignore")
    (:type-spec "shared Common Lisp type metadata; usually ignore")
    (:domain-parameter-list "shared domain parameter order; usually document")
    (:macro-binding-spec "shared macro binding metadata; usually ignore")
    (:literal-data "shared literal data; consider a named constant")
    (t nil)))

(defun shared-subdag-extraction-hint (row &optional sexp)
  "Return a coarse recommendation for what kind of extraction ROW suggests."
  (let* ((kinds (shared-subdag-kinds row))
         (scope (shared-subdag-scope row))
         (shape-hint (and sexp (%shape-extraction-hint
                                (shared-subdag-shape sexp)))))
    (or shape-hint
        (cond
          ((and (equal kinds '(:source)) (eq scope :cross-library))
           "candidate core/library primitive")
          ((and (equal kinds '(:source)) (eq scope :intra-library))
           "candidate local helper")
          ((member :source kinds)
           "mixed runtime + support; consider exporting a helper")
          ((subsetp kinds '(:test) :test #'eq)
           "test support/helper")
          ((subsetp kinds '(:example :bench :test) :test #'eq)
           "fixture/example support")
          ((member :script kinds)
           "script/tool utility")
          (t "inspect manually")))))
