;;;; rosette-blc-reduce/src/package.lisp --- Public API.

(defpackage #:rosette-blc-reduce
  (:use #:cl #:rosette-blc)
  (:export
   ;; bracket abstraction: rosette-blc lambda term -> combinator term
   #:lambda->combinators
   ;; readback: combinator graph (heap root) -> combinator s-expr
   #:combinators->term
   ;; reduction
   #:reduce-whnf
   #:reduce-to-normal-form
   #:reduce-church
   #:reduce-bool
   ;; the step counter (reductions performed by the last entry point)
   #:last-step-count))
