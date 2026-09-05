;;;; rosette-sel4-cap/src/package.lisp --- Public API.
;;;;
;;;; Typed raw seL4 endpoint-capability call contract and seed conformance gate.

(defpackage #:rosette-sel4-cap
  (:use #:cl)
  (:export
   #:invalid-capability-call
   #:capability-call-reason
   #:cap-call
   #:cap-call-p
   #:cap-call-endpoint
   #:cap-call-message-registers
   #:cap-call-message-info
   #:endpoint-capability
   #:make-cap-call1
   #:header-contract-errors))
