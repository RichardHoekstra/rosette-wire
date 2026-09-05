;;;; package.lisp --- expression AST primitives.

(in-package #:cl-user)

(defpackage #:rosette-expression-core
  (:nicknames #:expression-core #:expr-core)
  (:use #:cl)
  (:export
   ;; Struct
   #:cf-node
   #:cf-node-op
   #:cf-node-args

   ;; AST builders
   #:cf-const
   #:cf-var
   #:cf-add
   #:cf-sub
   #:cf-mul
   #:cf-div
   #:cf-exp
   #:cf-log
   #:cf-sqrt
   #:cf-power
   #:cf-normal-cdf
   #:cf-inverse-normal-cdf
   #:cf-inverse-gaussian-quantile
   #:cf-piecewise

   ;; Manipulation
   #:cf-substitute
   #:cf-compose

   ;; Inspection / analysis
   #:cf-node-count
   #:cf-depth
   #:cf-variables
   #:cf-closed-p
   #:cf-uses-special-p
   #:cf-cse-keys))
