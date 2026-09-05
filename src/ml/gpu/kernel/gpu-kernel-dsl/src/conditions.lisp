;;;; conditions.lisp --- Typed conditions for rosette-gpu-kernel-dsl
;;;;
;;;; Main condition classes:
;;;;
;;;;   DSL-SYNTAX-ERROR        — raised by the validator and the
;;;;                             emitters when a form does not match
;;;;                             the DSL grammar.  Slots FORM and
;;;;                             REASON let callers surface a precise
;;;;                             diagnostic.
;;;;   GPU-KERNEL-BRIDGE-UNAVAILABLE
;;;;                           — raised by the base launch layer when
;;;;                             callers request launch without loading
;;;;                             the optional NVRTC bridge system.

(in-package #:rosette-gpu-kernel-dsl)

(define-condition dsl-syntax-error (error)
  ((form   :initarg :form
           :reader dsl-syntax-error-form
           :initform nil
           :documentation "The offending DSL form.")
   (reason :initarg :reason
           :reader dsl-syntax-error-reason
           :initform "<unspecified>"
           :documentation "Human-readable reason."))
  (:report
   (lambda (c stream)
     (format stream
             "rosette-gpu-kernel-dsl: invalid DSL form ~S — ~A"
             (dsl-syntax-error-form c)
             (dsl-syntax-error-reason c))))
  (:documentation
   "Raised by VALID-DSL-FORM-P (when called as an asserter) and by the
    emitters when a form fails the DSL grammar."))

(define-condition gpu-kernel-bridge-unavailable (error)
  ((spec :initarg :spec
         :reader gpu-kernel-bridge-unavailable-spec
         :initform nil)
   (remedy :initarg :remedy
           :reader gpu-kernel-bridge-unavailable-remedy
           :initform "Load ASDF system :ROSETTE-GPU-KERNEL-DSL/BRIDGE on a CUDA host."))
  (:report
   (lambda (c stream)
     (format stream
             "rosette-gpu-kernel-dsl: launch bridge unavailable for ~S. ~A"
             (gpu-kernel-bridge-unavailable-spec c)
             (gpu-kernel-bridge-unavailable-remedy c))))
  (:documentation
   "Raised by the base launch layer after validating the launch request
    when the optional NVRTC bridge system has not replaced LAUNCH-KERNEL."))
