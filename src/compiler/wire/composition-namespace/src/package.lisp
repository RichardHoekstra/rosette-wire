;;;; composition-namespace/src/package.lisp --- Public API.
;;;;
;;;; Project Rosette Compositions into a deterministic Plan 9-style namespace with Wire-backed operation endpoints and a bounded semantic 9P2000 adapter.

(defpackage #:composition-namespace
  (:use #:cl)
  (:local-nicknames (#:cid #:rosette-content-identity)
                    (#:crypto #:rosette-symmetric-crypto)
                    (#:json #:rosette-json)
                    (#:rw #:rosette-wire))
  (:export
   #:namespace-error #:namespace-error-code #:namespace-error-detail
   #:namespace-entry #:namespace-entry-p #:namespace-entry-path
   #:namespace-entry-kind #:namespace-entry-content
   #:namespace-entry-endpoint
   #:operation-endpoint #:operation-endpoint-p #:operation-endpoint-node
   #:operation-endpoint-port #:operation-endpoint-operation
   #:operation-endpoint-id
   #:composition-namespace #:composition-namespace-p
   #:composition-namespace-id #:composition-namespace-composition-id
   #:composition-namespace-entries
   #:project-composition-namespace #:verify-composition-namespace
   #:namespace-lookup #:namespace-list
   #:make-rosette-composition-invoker
   ;; Bounded semantic 9P2000 request/reply surface.
   #:ninep-request #:ninep-request-p #:make-ninep-request
   #:ninep-request-type #:ninep-request-tag
   #:ninep-reply #:ninep-reply-p #:ninep-reply-type #:ninep-reply-tag
   #:ninep-reply-data #:ninep-reply-count #:ninep-reply-qids
   #:ninep-reply-msize #:ninep-reply-version #:ninep-reply-error
   #:ninep-session #:ninep-session-p #:make-ninep-session
   #:ninep-session-request-count #:handle-ninep-request))
