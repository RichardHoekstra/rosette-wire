;;;; chemistry-composition/src/package.lisp --- Public API.
;;;;
;;;; Flagship Rosette chemistry Composition spanning molecular analysis, exact reaction balancing, network and thermochemical evidence, and proof-producing expert conclusions.

(defpackage #:chemistry-composition
  (:use #:cl)
  (:import-from #:rosette-wire
                #:canonical-id #:scalar-type #:list-type #:record-type
                #:make-component-field #:make-component-operation #:make-port
                #:make-component-descriptor #:make-component-node
                #:make-data-binding #:input-source #:step-source
                #:make-wire-step #:make-wire-output #:make-composition
                #:make-composition-runner #:register-component-handler
                #:register-component-verifier #:run-composition
                #:verify-composition-receipt #:wire-receipt-verdict
                #:wire-receipt-p #:wire-receipt-outputs)
  (:import-from #:expert-components
                #:make-expert-fact #:expert-fact->value
                #:expert-fact-from-value #:expert-fact-id
                #:make-expert-rule #:make-expert-system
                #:make-expert-component #:make-expert-handler
                #:expert-fact-wire-type #:expert-result-wire-type
                #:run-expert-system #:expert-run->value)
  (:import-from #:rosette-chemical-formula
                #:parse-formula #:formula-molecular-weight)
  (:import-from #:rosette-reaction-balancer
                #:balance-reaction #:reaction-balance-coefficients
                #:reaction-balance-reaction #:reaction-balance-balanced-p)
  (:import-from #:rosette-reaction-network
                #:make-reaction-step #:make-reaction-network
                #:reaction-network-balanced-p #:run-reaction-network-report
                #:reaction-network-report-mass-balance
                #:reaction-network-report-element-balance
                #:element-balance-closed-p)
  (:import-from #:rosette-chemical-thermo
                #:reaction-standard-enthalpy #:reaction-standard-gibbs)
  (:import-from #:rosette-process-engineering
                #:*default-component-catalog* #:catalog-molecular-weight
                #:make-stream #:mass-balance-closed-p)
  (:export
   #:chemistry-composition-refusal #:chemistry-composition-refusal-code
   #:chemistry-composition-refusal-detail
   #:chemistry-expert-system
   #:molecular-evidence-wire-type #:reaction-evidence-wire-type
   #:make-chemistry-composition #:make-chemistry-runner
   #:run-chemistry-flagship #:verify-chemistry-receipt))
