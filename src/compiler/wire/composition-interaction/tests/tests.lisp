;;;; composition-interaction/tests/tests.lisp --- Test suite.

(defpackage #:composition-interaction/tests
  (:use #:cl #:composition-interaction)
  (:import-from #:rosette-assert-core #:assert-true)
  (:import-from #:rosette-wire
                #:scalar-type #:make-component-field
                #:make-component-operation #:make-port
                #:make-component-descriptor #:make-component-node
                #:make-data-binding #:input-source #:step-source
                #:make-wire-step #:make-wire-output #:make-composition)
  (:export #:run-all-tests))

(in-package #:composition-interaction/tests)

(defvar *assertions* 0)

(defun check (condition control &rest arguments)
  (incf *assertions*)
  (assert-true condition (apply #'format nil control arguments)))

(defun digest (digit)
  (format nil "sha256:~64,'0X" digit))

(defun arithmetic-composition (&key (effects '(:pure)) capabilities adapter)
  (let* ((u64 (scalar-type :u64))
         (a (make-component-field "a" u64))
         (b (make-component-field "b" u64))
         (add (make-component-operation "add" (list a b) u64))
         (mul (make-component-operation "mul" (list a b) u64))
         (math (make-port "math" (list add mul)))
         (adapter
           (or adapter
               (make-interaction-adapter
                (list (make-interaction-operation-adapter
                       "math" "add" (natural-add-term))
                      (make-interaction-operation-adapter
                       "math" "mul" (natural-mul-term))))))
         (descriptor
           (make-component-descriptor
            :name "org.rosette/interaction-arithmetic" :version "1.0.0"
            :imports nil :exports (list math) :effects effects
            :capabilities capabilities :adapter adapter :verifiers nil))
         (node (make-component-node "arithmetic" descriptor (digest 1)))
         (sum
           (make-wire-step
            :id "sum" :node-id "arithmetic" :port "math" :operation "add"
            :bindings (list (make-data-binding "a" (input-source "x"))
                            (make-data-binding "b" (input-source "y")))))
         ;; The same SUM is consumed twice. The nested-let lowering must
         ;; produce a DUP fan rather than copy the SUM implementation.
         (square
           (make-wire-step
            :id "square" :node-id "arithmetic" :port "math" :operation "mul"
            :bindings (list (make-data-binding "a" (step-source "sum"))
                            (make-data-binding "b" (step-source "sum"))))))
    (make-composition
     :name "org.rosette/interaction-square-of-sum"
     :nodes (list node) :services nil :steps (list sum square)
     :inputs (list (make-component-field "x" u64)
                   (make-component-field "y" u64))
     :outputs (list (make-wire-output "result" "square"))
     :capability-grants capabilities :required-evidence nil
     :limits '(("maxSteps" . 8)))))

(defun refusal-code (thunk)
  (handler-case (progn (funcall thunk) nil)
    (interaction-refusal (condition) (interaction-refusal-code condition))))

(defun test-portable-term ()
  (let* ((value (natural-add-term))
         (decoded (interaction-term-from-value value)))
    (check (equal value (interaction-term->value decoded))
           "portable term did not round-trip")
    (check (eq :free-variable
               (refusal-code
                (lambda ()
                  (interaction-term-from-value (interaction-var "free")))))
           "free portable term was accepted")
    (check (eq :invalid-term
               (refusal-code
                (lambda () (interaction-term-from-value '("wat")))))
           "unknown portable term form was accepted")))

(defun test-reduction-and-replay ()
  (let* ((composition (arithmetic-composition))
         (inputs '(("x" . 2) ("y" . 3)))
         (receipt (run-interaction-composition composition inputs)))
    (check (= 25 (interaction-receipt-output receipt))
           "interaction runtime returned ~S, expected 25"
           (interaction-receipt-output receipt))
    (check (eq :pass (interaction-receipt-verdict receipt))
           "interaction receipt did not pass")
    (check (equal (interaction-receipt-sequential-normal-form receipt)
                  (interaction-receipt-parallel-normal-form receipt))
           "schedule normal forms differ")
    (check (= (interaction-receipt-sequential-interactions receipt)
              (interaction-receipt-parallel-interactions receipt))
           "schedule interaction counts differ")
    (check (plusp (interaction-receipt-parallel-rounds receipt))
           "parallel reduction performed no rounds")
    (check (verify-interaction-receipt composition inputs receipt)
           "fresh replay rejected an unchanged receipt")
    (check (not (verify-interaction-receipt
                 composition '(("x" . 2) ("y" . 4)) receipt))
           "receipt grafted onto changed inputs")
    (let ((again (run-interaction-composition composition inputs)))
      (check (string= (interaction-receipt-id receipt)
                      (interaction-receipt-id again))
             "identical execution changed receipt identity"))))

(defun test-refusals ()
  (check (eq :effectful-component
             (refusal-code
              (lambda ()
                (run-interaction-composition
                 (arithmetic-composition :effects '(:clock))
                 '(("x" . 2) ("y" . 3))))))
         "effectful Component crossed the interaction floor")
  (check (eq :capability-component
             (refusal-code
              (lambda ()
                (run-interaction-composition
                 (arithmetic-composition
                  :capabilities '("clock/monotonic@1"))
                 '(("x" . 2) ("y" . 3))))))
         "capability-bearing Component crossed the interaction floor")
  (check (eq :adapter-unavailable
             (refusal-code
              (lambda ()
                (run-interaction-composition
                 (arithmetic-composition
                  :adapter '(("kind" . "local-function")))
                 '(("x" . 2) ("y" . 3))))))
         "missing interaction adapter was silently routed")
  (check (eq :input-set
             (refusal-code
              (lambda ()
                (run-interaction-composition
                 (arithmetic-composition) '(("x" . 2))))))
         "missing graph input was accepted")
  (check (eq :natural-bound
             (refusal-code
              (lambda ()
                (run-interaction-composition
                 (arithmetic-composition) '(("x" . 2) ("y" . -1))))))
         "negative natural input was accepted")
  (check (eq :reduction-fault
             (refusal-code
              (lambda ()
                (run-interaction-composition
                 (arithmetic-composition) '(("x" . 2) ("y" . 3))
                 :max-rounds 1))))
         "exhausted reduction budget did not fail closed"))

(defun run-all-tests ()
  (setf *assertions* 0)
  (test-portable-term)
  (test-reduction-and-replay)
  (test-refusals)
  (format t "composition-interaction: ~D assertions passed~%" *assertions*)
  t)
