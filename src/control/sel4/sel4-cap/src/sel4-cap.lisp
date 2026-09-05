;;;; sel4-cap.lisp --- rosette-sel4-cap implementation.

(in-package #:rosette-sel4-cap)

(define-condition invalid-capability-call (error)
  ((reason :initarg :reason :reader capability-call-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid seL4 capability call: ~A"
                     (capability-call-reason condition)))))

(defun %refuse (control &rest arguments)
  (error 'invalid-capability-call
         :reason (apply #'format nil control arguments)))

(defun %word-p (value)
  (and (integerp value) (<= 0 value #xffffffffffffffff)))

(defstruct (cap-call (:constructor %make-cap-call))
  endpoint
  message-registers
  message-info)

(defun endpoint-capability (base-endpoint-cap channel)
  "Return the endpoint capability slot BASE-ENDPOINT-CAP + CHANNEL.

Both operands and the result are checked as unsigned 64-bit model words."
  (unless (%word-p base-endpoint-cap)
    (%refuse "base endpoint capability must be an unsigned 64-bit word"))
  (unless (%word-p channel)
    (%refuse "channel must be an unsigned 64-bit word"))
  (let ((slot (+ base-endpoint-cap channel)))
    (unless (%word-p slot)
      (%refuse "base endpoint capability plus channel overflows a word"))
    slot))

(defun make-cap-call1 (base-endpoint-cap channel argument)
  "Model one synchronous endpoint call carrying one message-register word.

The message-info plist mirrors `seL4_MessageInfo_new(0, 0, 0, 1)`: no label,
unwrapped capabilities, or extra capabilities, and exactly one message word."
  (unless (%word-p argument)
    (%refuse "argument must be an unsigned 64-bit word"))
  (%make-cap-call
   :endpoint (endpoint-capability base-endpoint-cap channel)
   :message-registers (vector argument)
   :message-info '(:label 0 :caps-unwrapped 0 :extra-caps 0 :length 1)))

(defparameter *header-contract-fragments*
  '("seL4_SetMR(0, arg);"
    "seL4_Call(BASE_ENDPOINT_CAP + ch, seL4_MessageInfo_new(0, 0, 0, 1));"
    "return seL4_GetMR(0);"))

(defun header-contract-errors (header-text)
  "Return raw one-word endpoint-call fragments absent from HEADER-TEXT."
  (unless (stringp header-text)
    (%refuse "header text must be a string"))
  (remove-if (lambda (fragment) (search fragment header-text :test #'char=))
             *header-contract-fragments*))
