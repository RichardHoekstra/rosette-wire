(defpackage #:rosette-isolated-worker
  (:use #:cl)
  (:local-nicknames (#:envelope #:rosette-tool-envelope)
                    (#:cid #:rosette-content-identity))
  (:export
   #:isolation-policy #:isolation-policy-p #:make-isolation-policy
   #:isolation-policy->form
   ;; Persistent, line-framed worker sessions.
   #:isolated-session #:isolated-session-p
   #:isolated-session-status #:isolated-session-running-p
   #:start-isolated-session #:isolated-session-send-line
   #:isolated-session-read-line #:cancel-isolated-session
   #:stop-isolated-session #:isolated-session-receipt
   ;; Finite command boundary.
   #:run-isolated-command))
(in-package #:rosette-isolated-worker)
