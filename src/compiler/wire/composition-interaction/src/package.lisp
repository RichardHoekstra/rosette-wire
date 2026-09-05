;;;; composition-interaction/src/package.lisp --- Public API.
;;;;
;;;; Lower pure Rosette Compositions to interaction nets and issue replayable schedule-gauge execution certificates.

(defpackage #:composition-interaction
  (:use #:cl)
  (:import-from #:rosette-wire
                #:composition #:composition-id #:composition-nodes
                #:composition->value #:validate-composition
                #:component-node-id #:component-node-descriptor
                #:component-descriptor-adapter
                #:component-descriptor-effects
                #:component-descriptor-capabilities
                #:component-descriptor-imports
                #:component-descriptor-exports
                #:port-name #:port-operations
                #:component-operation-name #:component-operation-inputs
                #:component-operation-output
                #:component-field-name #:component-field-type
                #:wire-type->value #:wire-receipt-verdict
                #:wire-receipt-step-order #:canonical-id)
  (:import-from #:rosette-foundation-rewrite
                #:tlc-term #:tlc-term-kind
                #:tlc-var #:tlc-lam #:tlc-app
                #:tlc-var-name #:tlc-lam-param #:tlc-lam-body
                #:tlc-app-fn #:tlc-app-arg)
  (:import-from #:rosette-sharing-reduction
                #:church-numeral #:church-count
                #:church-add #:church-mul #:church-exp
                #:lam->net #:reduce-all #:net->term #:net-interactions)
  (:export
   #:interaction-refusal #:interaction-refusal-code
   #:interaction-refusal-detail
   #:interaction-var #:interaction-lam #:interaction-app
   #:interaction-term->value #:interaction-term-from-value
   #:natural-add-term #:natural-mul-term #:natural-exp-term
   #:make-interaction-operation-adapter #:make-interaction-adapter
   #:lower-composition-to-interaction-term
   #:interaction-receipt #:interaction-receipt-p
   #:interaction-receipt-verdict #:interaction-receipt-composition-id
   #:interaction-receipt-inputs-id #:interaction-receipt-source-term
   #:interaction-receipt-output
   #:interaction-receipt-sequential-normal-form
   #:interaction-receipt-parallel-normal-form
   #:interaction-receipt-sequential-rounds #:interaction-receipt-parallel-rounds
   #:interaction-receipt-sequential-interactions
   #:interaction-receipt-parallel-interactions
   #:interaction-receipt-max-rounds #:interaction-receipt-max-natural
   #:interaction-receipt-id #:interaction-receipt->value
   #:run-interaction-composition #:verify-interaction-receipt))
