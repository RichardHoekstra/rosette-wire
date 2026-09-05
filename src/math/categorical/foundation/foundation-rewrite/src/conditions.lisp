;;;; conditions.lisp --- error and warning conditions.

(in-package #:rosette-foundation-rewrite)

(define-condition invalid-five-object (error)
  ((reason
    :initarg :reason
    :reader invalid-five-object-reason
    :documentation "Keyword tag for the structural failure."))
  (:report (lambda (c stream)
             (format stream "Invalid Five-Object stack: ~A"
                     (invalid-five-object-reason c))))
  (:documentation
   "Signalled when a Five-Object stack fails an internal consistency
check, e.g. a thickening whose square-zero validator rejects D⊗D, or
a transport whose N is not nilpotent."))

(define-condition validate-failed (error)
  ((reason
    :initarg :reason
    :reader validate-failed-reason)
   (datum
    :initarg :datum
    :initform nil
    :reader validate-failed-datum))
  (:report (lambda (c stream)
             (format stream "Validation failed: ~A" (validate-failed-reason c))))
  (:documentation
   "Generic validation failure for stack-checks, normal-form
decompositions, and domain colimit consistency."))

(define-condition diverging-omega (error)
  ((term
    :initarg :term
    :reader diverging-omega-term)
   (max-iter
    :initarg :max-iter
    :initform nil
    :reader diverging-omega-max-iter))
  (:report (lambda (c stream)
             (format stream
                     "Beta-reduction did not converge within ~A iterations~@[ on term ~S~]."
                     (diverging-omega-max-iter c)
                     (diverging-omega-term c))))
  (:documentation
   "Signalled by NORMALIZE when the iteration cap is exceeded.  The
canonical example is the Omega combinator (lambda x. x x)(lambda x. x x),
which never reaches a beta-normal form."))
