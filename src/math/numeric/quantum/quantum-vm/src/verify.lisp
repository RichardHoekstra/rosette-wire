;;;; verify.lisp --- replayable semantic receipts and tamper detection.

(in-package #:rosette-quantum-vm)

(defun verify-execution-result (result &key receipt fingerprint)
  "Replay RESULT and verify its semantic receipt.

RECEIPT and FINGERPRINT default to immutable copies stored by RESULT.  Passing
an altered public copy is the supported tamper test; verification returns NIL."
  (and (execution-result-p result)
       (handler-case
           (let* ((candidate-receipt (or receipt (%execution-receipt result)))
                  (candidate-fingerprint
                    (or fingerprint (%execution-fingerprint result)))
                  (fresh (%execute-quantum-tape (%execution-tape result)
                                                (%execution-bindings result))))
             (and (equal candidate-receipt (%execution-receipt fresh))
                  (string= candidate-fingerprint
                           (%stable-fingerprint candidate-receipt))
                  (string= candidate-fingerprint (%execution-fingerprint fresh))
                  (%state-close-p (%execution-state result)
                                  (%execution-state fresh))))
         (error () nil))))

(defun verify-gradient-result (result &key receipt fingerprint)
  "Replay RESULT's differentiation method and reject altered content."
  (and (gradient-result-p result)
       (handler-case
           (let* ((candidate-receipt (or receipt (%gradient-receipt result)))
                  (candidate-fingerprint
                    (or fingerprint (%gradient-fingerprint result)))
                  (fresh
                    (if (%gradient-hessian result)
                        (hyperdual-gradient-hessian
                         (%gradient-tape result) (%gradient-observable result)
                         (%gradient-bindings result))
                        (value-and-gradient
                         (%gradient-tape result) (%gradient-observable result)
                         (%gradient-bindings result)
                         :method (%gradient-method result)))))
             (and (equal candidate-receipt (%gradient-receipt fresh))
                  (string= candidate-fingerprint
                           (%stable-fingerprint candidate-receipt))
                  (string= candidate-fingerprint (%gradient-fingerprint fresh))))
         (error () nil))))
