;;;; package.lisp --- package definition for rosette-tool-envelope.

(defpackage #:rosette-tool-envelope
  (:use #:cl)
  (:export
   #:tool-envelope
   #:tool-envelope-p
   #:make-tool-envelope
   #:tool-envelope-ok
   #:tool-envelope-status
   #:tool-envelope-data
   #:tool-envelope-metadata
   #:tool-envelope-warnings
   #:tool-envelope->plist
   #:ok-envelope
   #:error-envelope
   #:merge-envelope-warning))

(in-package #:rosette-tool-envelope)
