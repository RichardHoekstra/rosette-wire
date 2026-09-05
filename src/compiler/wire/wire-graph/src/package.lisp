;;;; wire-graph/src/package.lisp --- Public API.
;;;;
;;;; Immutable typed component descriptors, canonical graph identities, deterministic validation, and bounded Wire execution.

(defpackage #:rosette-wire
  (:use #:cl)
  (:import-from #:rosette-json
                #:json-string #:json-true #:json-false #:json-null
                #:json-parse #:json-parse-error)
  (:import-from #:rosette-symmetric-crypto #:sha256-hex #:string->bytes)
  (:import-from #:rosette-graph-core #:csr-from-edges)
  (:import-from #:rosette-graph-algorithms #:topological-sort)
  (:import-from #:rosette-wire-protocol
                #:+cell-magic+ #:+wire-magic+ #:+wire-version+
                #:+cell-header-length+ #:+frame-header-length+
                #:wire-protocol-error #:wire-protocol-error-reason
                #:wire-protocol-error-offset #:wire-protocol-error-detail
                #:cell #:cell-p #:make-cell #:cell-tag #:cell-media-type
                #:cell-payload #:cell-id #:encode-cell #:decode-cell
                #:frame #:frame-p #:make-frame #:make-cell-frame
                #:frame-kind #:frame-flags #:frame-stream-id #:frame-sequence
                #:frame-payload #:frame-payload-id #:encode-frame #:decode-frame
                #:wire-decoder #:wire-decoder-p #:make-wire-decoder
                #:wire-decoder-fault #:wire-decoder-buffered-bytes
                #:feed-wire-decoder)
  (:export
   ;; Byte transport: Cell -> Frame -> Wire.
   #:+cell-magic+ #:+wire-magic+ #:+wire-version+
   #:+cell-header-length+ #:+frame-header-length+
   #:wire-protocol-error #:wire-protocol-error-reason
   #:wire-protocol-error-offset #:wire-protocol-error-detail
   #:cell #:cell-p #:make-cell #:cell-tag #:cell-media-type
   #:cell-payload #:cell-id #:encode-cell #:decode-cell
   #:frame #:frame-p #:make-frame #:make-cell-frame
   #:frame-kind #:frame-flags #:frame-stream-id #:frame-sequence
   #:frame-payload #:frame-payload-id #:encode-frame #:decode-frame
   #:wire-decoder #:wire-decoder-p #:make-wire-decoder
   #:wire-decoder-fault #:wire-decoder-buffered-bytes
   #:feed-wire-decoder
   ;; Canonical semantic grammar. The wire-* graph names below are v0 aliases.
   #:+component-schema+ #:+composition-schema+
   #:component-field #:component-field-p #:component-field-name
   #:component-field-type #:make-component-field
   #:component-operation #:component-operation-p #:component-operation-name
   #:component-operation-inputs #:component-operation-output
   #:make-component-operation
   #:port #:port-p #:port-name #:port-operations #:make-port
   #:component-descriptor #:component-descriptor-p
   #:component-descriptor-name #:component-descriptor-version
   #:component-descriptor-imports #:component-descriptor-exports
   #:component-descriptor-effects #:component-descriptor-capabilities
   #:component-descriptor-adapter #:component-descriptor-verifiers
   #:make-component-descriptor #:component-descriptor->value
   #:component-contract-id
   #:component-node #:component-node-p #:component-node-id
   #:component-node-descriptor #:make-component-node
   #:composition #:composition-p #:composition-name #:composition-nodes
   #:composition-steps #:make-composition #:composition->value #:composition-id
   #:validate-composition #:plan-composition
   #:composition-runner #:composition-runner-p #:make-composition-runner
   #:register-component-handler #:register-component-verifier
   #:run-composition #:verify-composition-receipt #:certify-composition
   ;; Protocol constants and errors.
   #:+wire-schema+ #:+graph-schema+ #:+receipt-schema+
   #:wire-error #:wire-error-code #:wire-error-path #:wire-error-detail
   ;; Immutable interface types.
   #:wire-type-p #:wire-type-kind #:wire-type->value
   #:scalar-type #:list-type #:option-type #:result-type #:tuple-type
   #:record-type #:variant-type #:tensor-type #:resource-type
   #:wire-field-p #:wire-field-name #:wire-field-type #:make-wire-field
   ;; Immutable operations, ports, and descriptors.
   #:wire-operation-p #:wire-operation-name #:wire-operation-inputs
   #:wire-operation-output #:make-wire-operation
   #:wire-port-p #:wire-port-name #:wire-port-operations #:make-wire-port
   #:wire-descriptor-p #:wire-descriptor-name #:wire-descriptor-version
   #:wire-descriptor-imports #:wire-descriptor-exports
   #:wire-descriptor-effects #:wire-descriptor-capabilities
   #:wire-descriptor-adapter #:wire-descriptor-verifiers
   #:make-wire-descriptor #:wire-descriptor->value #:wire-contract-id
   ;; Canonical protocol values.
   #:canonical-json #:canonical-id
   ;; Immutable graph model.
   #:wire-node-p #:wire-node-id #:wire-node-descriptor #:make-wire-node
   #:service-binding-p #:make-service-binding
   #:data-source-p #:input-source #:literal-source #:step-source
   #:data-binding-p #:make-data-binding
   #:wire-step-p #:wire-step-id #:make-wire-step
   #:wire-output-p #:make-wire-output
   #:wire-graph-p #:wire-graph-name #:wire-graph-nodes #:wire-graph-steps
   #:make-wire-graph #:wire-graph->value #:wire-graph-id
   ;; Validation, planning, receipts.
   #:wire-violation-p #:wire-violation-stage #:wire-violation-path
   #:wire-violation-code #:wire-violation-detail
   #:wire-receipt-p #:wire-receipt-kind #:wire-receipt-verdict
   #:wire-receipt-graph-id #:wire-receipt-violations
   #:wire-receipt-step-order #:wire-receipt-outputs
   #:wire-receipt->value #:wire-receipt-id
   #:validate-wire-graph #:plan-wire-graph
   ;; Strict JSON protocol decoding.
   #:wire-descriptor-from-value #:wire-descriptor-from-json
   #:wire-graph-from-value #:wire-graph-from-json
   #:wire-receipt-from-value #:wire-receipt-from-json
   ;; Deterministic bounded execution.
   #:wire-runner-p #:make-wire-runner #:register-wire-handler
   #:register-wire-verifier
   #:run-wire-graph #:verify-wire-receipt #:certify-wire-graph
   ;; Six-command machine interface. ROSETTE-COMMAND is embeddable; MAIN writes.
   #:rosette-command #:rosette-main))
