;;;; package.lisp --- public API for rosette-ccc.

(defpackage #:rosette-ccc
  (:use #:cl)
  (:export
   ;; morphisms of FinSet (objects are plain lists of elements)
   #:mor
   #:mor-p
   #:mor-dom
   #:mor-cod
   #:mapply
   #:mid
   #:mcompose
   #:mor-equal
   #:set-equal
   ;; terminal object (the unit type)
   #:terminal
   #:terminal-arrow
   ;; binary products
   #:prod
   #:proj1
   #:proj2
   #:pairing
   #:prod-mor
   ;; exponentials  B^A  (the function space) + the adjunction
   #:all-functions
   #:exponential
   #:ev
   #:curry
   #:uncurry
   #:exp-mor
   ;; the simply-typed lambda-calculus (de Bruijn) and its denotation
   #:denote-type
   #:ctx-obj
   #:term-type
   #:denote
   ;; beta/eta normalisation of STLC terms
   #:tshift
   #:tsubst
   #:subst-top
   #:nf))
