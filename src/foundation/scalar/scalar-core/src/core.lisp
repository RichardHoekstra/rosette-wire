;;;; core.lisp --- Scalar tolerance primitives.

(in-package #:rosette-scalar-core)

(defconstant +default-tolerance+ 1d-8)

(defun float-kind-p (kind)
  "Return true when KIND names a supported transparent float precision."
  (member kind '(:f64 :f32) :test #'eq))

(defun float-kind-type (kind)
  "Return the Common Lisp scalar type for float KIND."
  (ecase kind
    (:f64 'double-float)
    (:f32 'single-float)))

(defun float-kind-zero (kind)
  "Return the additive identity specialized for float KIND."
  (ecase kind
    (:f64 0d0)
    (:f32 0f0)))

(defun float-kind-one (kind)
  "Return the multiplicative identity specialized for float KIND."
  (ecase kind
    (:f64 1d0)
    (:f32 1f0)))

(defun float-kind-pi (kind)
  "Return PI specialized for float KIND."
  (coerce pi (float-kind-type kind)))

(defun float-kind-bits (kind)
  "Return the storage bit width for float KIND."
  (ecase kind
    (:f64 64)
    (:f32 32)))

(defun float-kind-bytes (kind)
  "Return the storage byte width for float KIND."
  (/ (float-kind-bits kind) 8))

(defun float-kind-tolerance (kind)
  "Return the default comparison tolerance for float KIND."
  (ecase kind
    (:f64 +default-tolerance+)
    (:f32 1f-5)))

(defun float-kind-descriptor (kind)
  "Return a plist describing transparent scalar precision KIND.

The descriptor is deliberately data-only so graph and hierarchy tools can
compare scalar policies without depending on implementation classes."
  (list :kind kind
        :type (float-kind-type kind)
        :bits (float-kind-bits kind)
        :bytes (float-kind-bytes kind)
        :zero (float-kind-zero kind)
        :one (float-kind-one kind)
        :tolerance (float-kind-tolerance kind)
        :exact nil
        :storage (ecase kind
                   (:f64 :ieee-binary64)
                   (:f32 :ieee-binary32))))

(defun scalar-kind-of (value)
  "Return the transparent scalar kind that best describes VALUE."
  (etypecase value
    (double-float :f64)
    (single-float :f32)
    (rational :exact-rational)))

(declaim (inline as-float-kind))
(defun as-float-kind (kind value)
  "Coerce VALUE to the scalar type selected by float KIND."
  (ecase kind
    (:f64 (coerce value 'double-float))
    (:f32 (coerce value 'single-float))))

(defun %find-call-symbol (name forms)
  (labels ((walk (form)
             (cond
               ((and (consp form)
                     (symbolp (first form))
                     (string= (symbol-name (first form)) name))
                (first form))
               ((consp form)
                (or (walk (first form))
                    (walk (rest form)))))))
    (or (walk forms)
        (intern name *package*))))

(defmacro with-float-kind ((kind) &body body)
  "Evaluate BODY with local F, F-ZERO, and F-ONE helpers for KIND."
  (let ((kind-var (gensym "KIND"))
        (f-sym (%find-call-symbol "F" body))
        (zero-sym (%find-call-symbol "F-ZERO" body))
        (one-sym (%find-call-symbol "F-ONE" body)))
    `(let ((,kind-var ,kind))
       (flet ((,f-sym (value)
                (as-float-kind ,kind-var value))
              (,zero-sym ()
                (float-kind-zero ,kind-var))
              (,one-sym ()
                (float-kind-one ,kind-var)))
         ,@body))))

(declaim (inline as-f64 as-f32 f64-add f64-sub f64-mul f64-ratio f64-distance< as-f64-pi)
         (ftype (function () double-float) as-f64-pi))

(defun as-f64 (value)
  "Coerce VALUE to DOUBLE-FLOAT."
  (as-float-kind :f64 value))

(defun as-f32 (value)
  "Coerce VALUE to SINGLE-FLOAT."
  (as-float-kind :f32 value))

(declaim (inline %as-f64-pair))
(defun %as-f64-pair (a b)
  (values (as-f64 a) (as-f64 b)))

(defun f64-add (a b)
  "Return A + B after transparent DOUBLE-FLOAT coercion."
  (multiple-value-bind (aa bb) (%as-f64-pair a b)
    (declare (double-float aa bb))
    (+ aa bb)))

(defun f64-sub (a b)
  "Return A - B after transparent DOUBLE-FLOAT coercion."
  (multiple-value-bind (aa bb) (%as-f64-pair a b)
    (declare (double-float aa bb))
    (- aa bb)))

(defun f64-mul (a b)
  "Return A * B after transparent DOUBLE-FLOAT coercion."
  (multiple-value-bind (aa bb) (%as-f64-pair a b)
    (declare (double-float aa bb))
    (* aa bb)))

(defun f64-ratio (numerator denominator)
  "Return NUMERATOR / DENOMINATOR after transparent DOUBLE-FLOAT coercion."
  (multiple-value-bind (n d) (%as-f64-pair numerator denominator)
    (declare (double-float n d))
    (/ n d)))

(declaim (inline %f64-distance))
(defun %f64-distance (a b)
  (multiple-value-bind (aa bb) (%as-f64-pair a b)
    (declare (double-float aa bb))
    (abs (- aa bb))))

(defun f64-distance< (a b tolerance)
  "Return true when the DOUBLE-FLOAT distance between A and B is below TOLERANCE."
  (< (the double-float (%f64-distance a b))
     (the double-float (as-f64 tolerance))))

(defun as-f64-pi ()
  "Return PI as a DOUBLE-FLOAT scalar."
  (float-kind-pi :f64))

(defun approx= (a b &optional (tolerance +default-tolerance+))
  "Return T iff |A - B| <= TOLERANCE after coercing A and B to double-float."
  (<= (the double-float (%f64-distance a b))
      (the double-float (as-f64 tolerance))))

(defun near= (a b &optional (tolerance +default-tolerance+))
  "Alias for APPROX= using the short name common in tests and examples."
  (approx= a b tolerance))
