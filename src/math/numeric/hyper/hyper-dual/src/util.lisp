;;;; util.lisp --- probability-core compatibility layer.
;;;;
;;;; Scalar probability primitives moved to rosette-probability-core. This file
;;;; remains in the serial load order so existing ASDF component ordering is
;;;; stable while rosette-hyper-dual imports and re-exports those primitives.

(in-package #:rosette-hyper-dual)

(defun lift-real-binary-args (x y lift-real)
  "Lift numeric X and Y with LIFT-REAL, returning two values."
  (values (if (numberp x) (funcall lift-real x) x)
          (if (numberp y) (funcall lift-real y) y)))
