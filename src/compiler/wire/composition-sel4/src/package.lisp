;;;; composition-sel4/src/package.lisp --- Public API.
;;;;
;;;; Lower Rosette Component capabilities and explicit service/data-flow edges into a content-addressed seL4/Microkit authority description with deadlock and information-flow verification.

(defpackage #:composition-sel4
  (:use #:cl)
  (:local-nicknames (#:cap #:rosette-sel4-cap)
                    (#:cid #:rosette-content-identity)
                    (#:rw #:rosette-wire)
                    (#:sv #:rosette-sel4-verify))
  (:export
   #:composition-sel4-error #:composition-sel4-error-code
   #:composition-sel4-error-detail
   #:sel4-lowering-policy #:sel4-lowering-policy-p
   #:make-sel4-lowering-policy #:sel4-lowering-policy-base-endpoint-cap
   #:sel4-lowering-policy-node-levels
   #:sel4-lowering-policy-orchestrator-level
   #:sel4-lowering-policy-service-modes
   #:protection-domain #:protection-domain-p #:protection-domain-name
   #:protection-domain-wire-node #:protection-domain-priority
   #:protection-domain-level
   #:authority-grant #:authority-grant-p #:authority-grant-holder
   #:authority-grant-capability #:authority-grant-channel
   #:authority-grant-endpoint-slot
   #:microkit-channel #:microkit-channel-p #:microkit-channel-id
   #:microkit-channel-source #:microkit-channel-target
   #:microkit-channel-mode #:microkit-channel-kind
   #:sel4-authority-description #:sel4-authority-description-p
   #:copy-sel4-authority-description
   #:sel4-authority-description-id
   #:sel4-authority-description-composition-id
   #:sel4-authority-description-status
   #:sel4-authority-description-protection-domains
   #:sel4-authority-description-authority-grants
   #:sel4-authority-description-channels
   #:sel4-authority-description-deadlock-verdict
   #:sel4-authority-description-information-flow-verdict
   #:sel4-authority-description-violations
   #:sel4-authority-description-system-xml
   #:sel4-authority-description-safe-p
   #:sel4-authority-description->form
   #:lower-composition-to-sel4
   #:verify-sel4-authority-description))
