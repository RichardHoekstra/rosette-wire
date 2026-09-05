;;;; package.lisp --- package definition for rosette-content-identity.

(defpackage #:rosette-content-identity
  (:use #:cl)
  (:import-from #:rosette-form-core
                #:form-address-of
                #:form-addresses-of-domain-separated)
  (:export
   #:canonical-content-form
   #:content-id
   #:content-id-long
   #:content-id-p
   #:content-id-long-p
   #:content-id-string
   #:content-id-string-long
   #:content-id-short
   #:content-id-handle
   #:content-id-handle-p
   #:durable-content-id-p
   #:legacy-content-id-only-p
   #:assert-durable-content-id
   #:content-equal-p
   #:content-equal-long-p))

(in-package #:rosette-content-identity)
