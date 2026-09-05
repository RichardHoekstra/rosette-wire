;;;; package.lisp --- public API for rosette-foundation-rewrite.

(in-package #:cl-user)

(defpackage #:rosette-foundation-rewrite
  (:use #:cl)
  (:export
   #:normal-form-decompose
   #:normal-form-invariant
   #:normal-form-equivalent-p
   #:tlc-term
   #:tlc-term-kind
   #:tlc-term-parts
   #:tlc-var-name
   #:tlc-lam-param
   #:tlc-lam-body
   #:tlc-app-fn
   #:tlc-app-arg
   #:make-tlc-term
   #:tlc-var
   #:tlc-lam
   #:tlc-app
   #:tlc-equal-p
   #:beta-step
   #:normalize
   #:confluent-p
   #:*normalize-max-iter*
   #:diverging-omega
   #:diverging-omega-term
   #:dag-node-count
   #:shared-dag-node-count
   #:axiom-rank-entry
   #:axiom-rank-entry-name
   #:axiom-rank-entry-k-axiom
   #:axiom-rank-entry-k-rest
   #:axiom-rank-entry-k-combined
   #:optimal-axiom-rank))
