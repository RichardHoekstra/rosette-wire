;;;; port-equivalence/src/package.lisp --- Public API.
;;;;
;;;; Certify executable HoTT/cubical equivalences between Rosette Ports and transport implementations and proof obligations across them.

(defpackage #:port-equivalence
  (:use #:cl)
  (:import-from #:rosette-wire
                #:wire-type-p #:wire-type->value #:canonical-id
                #:port #:port-name #:port-operations
                #:component-operation-name #:component-operation-inputs
                #:component-operation-output
                #:component-field-name #:component-field-type)
  (:import-from #:rosette-hott-core
                #:make-hott-type #:make-hott-path #:make-hott-equivalence
                #:inverse-equivalence #:hott-equivalence)
  (:import-from #:rosette-cubical-core #:ua-beta)
  (:export
   #:port-equivalence-refusal #:port-equivalence-refusal-code
   #:port-equivalence-refusal-detail
   #:type-equivalence-certificate #:type-equivalence-certificate-p
   #:type-equivalence-certificate-name
   #:type-equivalence-certificate-source-wire-type
   #:type-equivalence-certificate-target-wire-type
   #:type-equivalence-certificate-source-samples
   #:type-equivalence-certificate-target-samples
   #:type-equivalence-certificate-id
   #:certify-type-equivalence #:verify-type-equivalence-certificate
   #:type-equivalence-certificate->value
   #:transport-value-forward #:transport-value-backward
   #:input-equivalence #:input-equivalence-p #:make-input-equivalence
   #:input-equivalence-source-field #:input-equivalence-target-field
   #:input-equivalence-certificate
   #:operation-equivalence #:operation-equivalence-p
   #:make-operation-equivalence
   #:operation-equivalence-source-operation
   #:operation-equivalence-target-operation
   #:operation-equivalence-inputs #:operation-equivalence-output
   #:port-equivalence-certificate #:port-equivalence-certificate-p
   #:port-equivalence-certificate-source-port
   #:port-equivalence-certificate-target-port
   #:port-equivalence-certificate-operations
   #:port-equivalence-certificate-id
   #:certify-port-equivalence #:verify-port-equivalence-certificate
   #:port-equivalence-certificate->value
   #:transport-operation-handler
   #:transport-correctness-receipt #:transport-correctness-receipt-p
   #:transport-correctness-receipt-id
   #:transport-correctness-receipt-port-equivalence-id
   #:transport-correctness-receipt-operation
   #:transport-correctness-receipt-implementation-id
   #:transport-correctness-receipt-specification-id
   #:transport-correctness-receipt-cases-id
   #:transport-correctness-receipt->value
   #:certify-transported-correctness
   #:verify-transported-correctness))
