;;;; distribution-composition.lisp --- distribution-composition implementation.

(in-package #:distribution-composition)

(defun %plan-text (plan)
  (with-output-to-string (stream)
    (let ((*print-case* :downcase)
          (*print-pretty* nil)
          (*print-readably* t))
      (write (distribution-plan->form plan) :stream stream))))

(defun %compile-plan (definition deps-fn resolvable-fn)
  (compile-distribution-plan definition
                             :deps-fn deps-fn
                             :resolvable-fn resolvable-fn))

(defun %derived-id (label plan-id)
  (canonical-id `(("kind" . ,label) ("planId" . ,plan-id))))

(defun %operation-descriptor (name operation inputs verifiers)
  (let ((string (scalar-type :string)))
    (make-component-descriptor
     :name name :version "0.1.0" :imports nil
     :exports (list (make-port
                     "distribution"
                     (list (make-component-operation operation inputs string))))
     :effects '(:pure) :capabilities nil
     :adapter '(("kind" . "rosette-distribution-compiler"))
     :verifiers verifiers)))

(defun make-distribution-compiler-composition
    (definition &key
                  (deps-fn #'rosette-ship:asdf-system-deps)
                  (resolvable-fn
                    (lambda (name) (asdf:find-system name nil))))
  "Compile DEFINITION once to bind its exact plan into a Rosette Composition.

Executing the returned Composition invokes DISTRIBUTION-COMPILER afresh at
each of three pure stages: manifest compilation, identity checking, and replay
checking.  The Composition is therefore a bounded self-hosting witness for the
distribution pipeline, not a claim that arbitrary Rosette programs self-host."
  (check-type definition distribution-definition)
  (let* ((plan (%compile-plan definition deps-fn resolvable-fn))
         (plan-id (distribution-plan-id plan))
         (string (scalar-type :string))
         (manifest-descriptor
           (%operation-descriptor "org.rosette/distribution-compile"
                                  "compile" nil nil))
         (identity-descriptor
           (%operation-descriptor
            "org.rosette/distribution-identify" "identify"
            (list (make-component-field "manifest" string)) nil))
         (replay-descriptor
           (%operation-descriptor
            "org.rosette/distribution-replay" "replay"
            (list (make-component-field "planId" string))
            '("distribution-plan-recompiled")))
         (compile-node
           (make-component-node
            "compile" manifest-descriptor
            (%derived-id "compile-implementation" plan-id)
            :dependency-set-id (%derived-id "compile-dependencies" plan-id)))
         (identify-node
           (make-component-node
            "identify" identity-descriptor
            (%derived-id "identify-implementation" plan-id)
            :dependency-set-id (%derived-id "identify-dependencies" plan-id)))
         (replay-node
           (make-component-node
            "replay" replay-descriptor
            (%derived-id "replay-implementation" plan-id)
            :dependency-set-id (%derived-id "replay-dependencies" plan-id))))
    (make-composition
     :name "org.rosette/distribution-compiler"
     :nodes (list compile-node identify-node replay-node)
     :services nil
     :steps
     (list
      (make-wire-step :id "01-compile" :node-id "compile"
                      :port "distribution" :operation "compile" :bindings nil)
      (make-wire-step
       :id "02-identify" :node-id "identify"
       :port "distribution" :operation "identify"
       :bindings (list (make-data-binding "manifest"
                                         (step-source "01-compile"))))
      (make-wire-step
       :id "03-replay" :node-id "replay"
       :port "distribution" :operation "replay"
       :bindings (list (make-data-binding "planId"
                                         (step-source "02-identify")))))
     :inputs nil
     :outputs (list (make-wire-output "manifest" "01-compile")
                    (make-wire-output "planId" "03-replay"))
     :capability-grants nil
     :required-evidence '("distribution-plan-recompiled")
     :limits '(("maxSteps" . 3) ("maxOutputBytes" . 1048576)))))

(defun make-distribution-composition-runner
    (composition definition &key
                              (deps-fn #'rosette-ship:asdf-system-deps)
                              (resolvable-fn
                                (lambda (name) (asdf:find-system name nil))))
  "Return the explicit authority that executes COMPOSITION for DEFINITION.

The runner refuses a definition/composition graft before registering any
handler.  Its verifier recompiles independently and never trusts a stored
receipt verdict."
  (check-type definition distribution-definition)
  (let* ((expected (make-distribution-compiler-composition
                    definition :deps-fn deps-fn :resolvable-fn resolvable-fn))
         (expected-id (composition-id expected)))
    (unless (string= expected-id (composition-id composition))
      (error "Definition/composition identity mismatch: expected ~A, got ~A"
             expected-id (composition-id composition)))
    (let ((runner (make-composition-runner)))
      (labels ((fresh-plan () (%compile-plan definition deps-fn resolvable-fn))
               (argument (name arguments)
                 (or (cdr (assoc name arguments :test #'string=))
                     (error "Missing Composition argument ~S" name))))
        (register-component-handler
         runner "compile" "distribution" "compile"
         (lambda (arguments services context)
           (declare (ignore arguments services context))
           (%plan-text (fresh-plan))))
        (register-component-handler
         runner "identify" "distribution" "identify"
         (lambda (arguments services context)
           (declare (ignore services context))
           (let* ((plan (fresh-plan))
                  (manifest (argument "manifest" arguments)))
             (unless (string= manifest (%plan-text plan))
               (error "Manifest differs from a fresh distribution plan"))
             (distribution-plan-id plan))))
        (register-component-handler
         runner "replay" "distribution" "replay"
         (lambda (arguments services context)
           (declare (ignore services context))
           (let* ((plan (fresh-plan))
                  (claimed-id (argument "planId" arguments)))
             (unless (string= claimed-id (distribution-plan-id plan))
               (error "Plan identity differs on fresh replay"))
             (distribution-plan-id plan))))
        (register-component-verifier
         runner "distribution-plan-recompiled"
         (lambda (graph receipt)
           (let* ((plan (fresh-plan))
                  (outputs (wire-receipt-outputs receipt))
                  (manifest (cdr (assoc "manifest" outputs :test #'string=)))
                  (plan-id (cdr (assoc "planId" outputs :test #'string=)))
                  (pass-p (and (string= (composition-id graph) expected-id)
                               (stringp manifest)
                               (string= manifest (%plan-text plan))
                               (stringp plan-id)
                               (string= plan-id (distribution-plan-id plan)))))
             (values pass-p
                     `(("compositionId" . ,(composition-id graph))
                       ("planId" . ,(distribution-plan-id plan)))))))
        runner))))

(defun run-distribution-compiler-composition (composition runner)
  "Execute and independently certify the fixed distribution Composition."
  (certify-composition composition runner nil))

(defun verify-distribution-compiler-receipt (composition receipt runner)
  "Re-run the distribution verifier against RECEIPT and current dependencies."
  (verify-composition-receipt composition receipt runner))

(defun replay-distribution-compiler-composition (composition runner receipt)
  "Freshly rerun COMPOSITION and require the same content-addressed receipt."
  (let ((replayed (run-distribution-compiler-composition composition runner)))
    (unless (and (eq :pass (wire-receipt-verdict replayed))
                 (string= (wire-receipt-id receipt)
                          (wire-receipt-id replayed)))
      (error "Distribution Composition replay diverged from receipt ~A"
             (wire-receipt-id receipt)))
    replayed))
