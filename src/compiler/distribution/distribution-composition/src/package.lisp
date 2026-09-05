;;;; distribution-composition/src/package.lisp --- Public API.
;;;;
;;;; Execute and independently verify the policy-defined distribution compiler as a Rosette Composition.

(defpackage #:distribution-composition
  (:use #:cl)
  (:import-from #:rosette-distribution-compiler
                #:distribution-definition
                #:distribution-plan
                #:compile-distribution-plan
                #:distribution-plan->form
                #:distribution-plan-id)
  (:import-from #:rosette-wire
                #:scalar-type #:make-component-field
                #:make-component-operation #:make-port
                #:make-component-descriptor #:make-component-node
                #:make-data-binding #:step-source #:make-wire-step
                #:make-wire-output #:make-composition #:composition-id
                #:make-composition-runner #:register-component-handler
                #:register-component-verifier #:certify-composition
                #:verify-composition-receipt #:wire-receipt-verdict
                #:wire-receipt-outputs #:wire-receipt-id #:canonical-id)
  (:export
   #:make-distribution-compiler-composition
   #:make-distribution-composition-runner
   #:run-distribution-compiler-composition
   #:verify-distribution-compiler-receipt
   #:replay-distribution-compiler-composition))
