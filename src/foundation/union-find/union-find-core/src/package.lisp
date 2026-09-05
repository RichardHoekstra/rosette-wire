;;;; package.lisp --- package definition for rosette-union-find-core.

(defpackage #:rosette-union-find-core
  (:use #:cl)
  (:export
   ;; type + constructor
   #:union-find
   #:union-find-p
   #:make-union-find
   ;; core operations
   #:uf-find
   #:uf-union
   #:uf-connected-p
   ;; growth
   #:uf-add-element
   ;; queries
   #:uf-size
   #:uf-component-count
   #:uf-component-sizes
   #:uf-components))
(in-package #:rosette-union-find-core)
