;;;; package.lisp --- package definition for rosette-array-core.

(defpackage #:rosette-array-core
  (:use #:cl)
  (:import-from #:rosette-scalar-core
   #:float-kind-type
   #:float-kind-bytes
   #:as-float-kind)
  (:export
   #:float-kind-type
   #:f-scalar
   #:f-array
   #:f-vector
   #:f64-array
   #:f32-array
   #:shape3
   #:f-array-storage-bytes
   #:make-f-array
   #:make-double-float-array
   #:make-single-float-array
   #:copy-array
   #:array-same-dimensions-p))

(in-package #:rosette-array-core)
