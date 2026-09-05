;;;; package.lisp --- package definition for rosette-scalar-core.

(defpackage #:rosette-scalar-core
  (:use #:cl)
  (:export
   #:+default-tolerance+
   #:float-kind-p
   #:float-kind-type
   #:float-kind-zero
   #:float-kind-one
   #:float-kind-pi
   #:float-kind-bits
   #:float-kind-bytes
   #:float-kind-tolerance
   #:float-kind-descriptor
   #:scalar-kind-of
   #:as-float-kind
   #:with-float-kind
   #:as-f64
   #:as-f32
   #:f64-add
   #:f64-sub
   #:f64-mul
   #:f64-ratio
   #:f64-distance<
   #:as-f64-pi
   #:approx=
   #:near=))

(in-package #:rosette-scalar-core)
