;;;; expert-components/src/package.lisp --- Public API.
;;;;
;;;; Bounded deterministic proof-producing expert rules exposed as Rosette Components with replayable content-addressed derivations.

(defpackage #:expert-components
  (:use #:cl)
  (:import-from #:rosette-wire
                #:canonical-id #:scalar-type #:list-type #:record-type
                #:make-component-field #:make-component-operation #:make-port
                #:make-component-descriptor)
  (:import-from #:rosette-proof-witness
                #:make-lean-witness #:make-certificate)
  (:export
   #:expert-refusal #:expert-refusal-code #:expert-refusal-detail
   #:expert-fact #:expert-fact-p #:make-expert-fact
   #:expert-fact-predicate #:expert-fact-arguments
   #:expert-fact-id #:expert-fact->value #:expert-fact-from-value
   #:expert-rule #:expert-rule-p #:make-expert-rule
   #:expert-rule-name #:expert-rule-premises #:expert-rule-conclusion
   #:expert-rule-id #:expert-rule->value
   #:expert-system #:expert-system-p #:make-expert-system
   #:expert-system-name #:expert-system-rules #:expert-system-id
   #:expert-system->value
   #:expert-derivation #:expert-derivation-p
   #:expert-derivation-kind #:expert-derivation-fact-id
   #:expert-derivation-rule-id #:expert-derivation-premise-ids
   #:expert-derivation-id #:expert-derivation->value
   #:expert-run #:expert-run-p #:expert-run-verdict #:expert-run-system-id
   #:expert-run-inputs-id #:expert-run-facts #:expert-run-derivations
   #:expert-run-rounds #:expert-run-max-rounds #:expert-run-max-facts
   #:expert-run-id #:expert-run->value
   #:run-expert-system #:verify-expert-run #:expert-run-certificate
   #:expert-fact-wire-type #:expert-result-wire-type
   #:make-expert-component #:make-expert-handler))
