;;;; conditions.lisp --- error hierarchy for rosette-raster-core
;;;;
;;;; RASTER-ERROR is the top of the library's condition tree; everything
;;;; signalled by user-facing API calls inherits from it so client code
;;;; can install a single handler.  RASTER-NOT-IMPLEMENTED is reserved
;;;; for downstream extensions that want to share the same condition
;;;; root while developing future raster operations.

(in-package #:rosette-raster-core)

(define-condition raster-error (simple-error) ()
  (:documentation "Top-level error condition for raster-core users."))

(define-condition raster-not-implemented (raster-error)
  ((feature :initarg :feature :reader raster-not-implemented-feature
            :type symbol
            :documentation "Symbol naming the unimplemented feature."))
  (:report (lambda (c s)
             (format s "Raster feature ~A is not implemented."
                     (raster-not-implemented-feature c))))
  (:documentation
   "Shared condition for explicitly skeletoned raster operations."))
