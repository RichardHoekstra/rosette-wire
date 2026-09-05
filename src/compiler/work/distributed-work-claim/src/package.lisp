;;;; distributed-work-claim/src/package.lisp --- Public API.
;;;;
;;;; Issue and verify content-addressed capability-bounded distributed work claims over Rosette Compositions.

(defpackage #:distributed-work-claim
  (:use #:cl)
  (:import-from #:rosette-wire
                #:composition #:composition-p #:composition-id
                #:composition-nodes #:composition-steps #:composition->value
                #:component-node-descriptor #:component-descriptor-capabilities
                #:validate-composition #:certify-composition
                #:wire-receipt #:wire-receipt-p #:wire-receipt-verdict
                #:wire-receipt-step-order #:wire-receipt-outputs
                #:wire-receipt-id #:canonical-json #:canonical-id)
  (:import-from #:rosette-symmetric-crypto #:string->bytes)
  (:import-from #:rosette-work-scheduler
                #:make-work-item #:plan-work-schedule #:schedule-certificate
                #:verify-parallel-plan #:verify-schedule-certificate)
  (:export
   #:work-claim-error #:work-claim-error-code #:work-claim-error-detail
   #:work-worker #:work-worker-p #:make-work-worker #:work-worker-id
   #:work-worker-capabilities #:work-worker-max-steps
   #:work-worker-max-output-bytes
   #:distributed-work-claim #:distributed-work-claim-p
   #:make-distributed-work-claim #:distributed-work-claim-id
   #:distributed-work-claim-composition-id
   #:distributed-work-claim-worker-id #:distributed-work-claim-input-id
   #:distributed-work-claim-inputs
   #:distributed-work-claim-required-capabilities
   #:distributed-work-claim-max-steps
   #:distributed-work-claim-max-output-bytes
   #:distributed-work-claim-depends-on
   #:distributed-work-claim-financial-authority
   #:distributed-work-claim-publication-authority
   #:work-claim-receipt #:work-claim-receipt-p
   #:work-claim-receipt-id #:work-claim-receipt-verdict
   #:work-claim-receipt-claim-id #:work-claim-receipt-worker-id
   #:work-claim-receipt-output-id #:work-claim-receipt-step-count
   #:work-claim-receipt-output-bytes
   #:work-claim-receipt-composition-receipt
   #:work-claim-receipt-refusal
   #:execute-distributed-work-claim #:verify-distributed-work-receipt
   #:plan-distributed-work-claims #:verify-distributed-work-schedule))
