;;;; package.lisp --- public surface for exact rational subquotients.

(defpackage #:rosette-exact-linear-subquotient
  (:use #:cl)
  (:export
   ;; H = ker(A) / im(B).
   #:make-linear-subquotient
   #:linear-subquotient-p
   #:linear-subquotient-ambient-dimension
   #:linear-subquotient-dimension
   #:linear-subquotient-representatives
   #:linear-subquotient-class-coordinates
   #:linear-subquotient-receipt
   #:linear-subquotient-valid-p

   ;; Exact descent F : H_source -> H_target.
   #:descend-linear-map
   #:linear-map-descent-p
   #:linear-map-descent-matrix
   #:linear-map-descent-receipt
   #:linear-map-descent-valid-p

   ;; A refused descent retains the first nonzero, located residual.
   #:linear-descent-error
   #:linear-descent-error-kind
   #:linear-descent-error-residual))

(in-package #:rosette-exact-linear-subquotient)
