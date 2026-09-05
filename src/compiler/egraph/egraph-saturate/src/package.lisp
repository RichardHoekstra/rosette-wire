;;;; package.lisp --- package definition for rosette-egraph-saturate.

(defpackage #:rosette-egraph-saturate
  (:use #:cl)
  (:import-from #:rosette-form-core
                #:intern-form #:form-value #:form-address #:form-p)
  (:import-from #:rosette-egraph
                #:make-egraph #:egraph-add #:egraph-merge
                #:egraph-equivalent-p #:egraph-class-members)
  (:import-from #:rosette-proof-witness
                #:make-certificate #:certificate-passed #:make-lean-witness)
  (:export
   ;; expression language / evaluation
   #:expr-eval
   #:*default-battery*
   #:make-battery
   #:forms-equal-on-battery-p
   ;; certificate-gated equality
   #:certify-merge
   #:symbolically-equal-p
   #:certificate-passed
   #:runtime-verify
   ;; equality saturation
   #:*default-rules*
   #:rewrites-at-subterms
   #:saturate
   #:saturation-certificates
   #:equivalent-p
   #:class-spellings
   ;; sharing meter
   #:op-count
   #:sharing-ratio))

(in-package #:rosette-egraph-saturate)
