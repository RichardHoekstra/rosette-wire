;;;; distributed-work-claim/tests/tests.lisp --- Test suite.

(defpackage #:distributed-work-claim/tests
  (:use #:cl #:distributed-work-claim)
  (:import-from #:rosette-assert-core #:assert-true)
  (:import-from #:rosette-wire
                #:scalar-type #:make-component-field
                #:make-component-operation #:make-port
                #:make-component-descriptor #:make-component-node
                #:make-data-binding #:input-source #:make-wire-step
                #:make-wire-output #:make-composition
                #:make-composition-runner #:register-component-handler
                #:register-component-verifier #:wire-receipt-outputs
                #:canonical-id)
  (:export #:run-all-tests))

(in-package #:distributed-work-claim/tests)

(defparameter *assertion-count* 0)

(defun expect (condition control &rest arguments)
  (incf *assertion-count*)
  (assert-true condition (apply #'format nil control arguments)))

(defun digest (integer)
  (format nil "sha256:~64,'0x" integer))

(defun make-fixture (&key extra-grant)
  (let* ((s64 (scalar-type :s64))
         (operation
           (make-component-operation
            "double" (list (make-component-field "x" s64)) s64))
         (descriptor
           (make-component-descriptor
            :name "org.rosette/work-double" :version "0.1.0"
            :imports nil :exports (list (make-port "math" (list operation)))
            :effects '(:accelerator) :capabilities '("compute/cpu@1")
            :adapter '(("kind" . "test")) :verifiers '("answer-is-even")))
         (node (make-component-node "double" descriptor (digest 1)
                                    :dependency-set-id (digest 2)))
         (step (make-wire-step
                :id "01-double" :node-id "double" :port "math"
                :operation "double"
                :bindings (list (make-data-binding "x" (input-source "x"))))))
    (make-composition
     :name "org.rosette/work-fixture" :nodes (list node) :services nil
     :steps (list step) :inputs (list (make-component-field "x" s64))
     :outputs (list (make-wire-output "answer" "01-double"))
     :capability-grants (append '("compute/cpu@1")
                                (when extra-grant '("network/raw@1")))
     :required-evidence '("answer-is-even")
     :limits '(("maxSteps" . 1) ("maxOutputBytes" . 1024)))))

(defun make-runner (&key fail)
  (let ((runner (make-composition-runner)))
    (register-component-handler
     runner "double" "math" "double"
     (lambda (arguments services context)
       (declare (ignore services context))
       (when fail (error "deliberate worker refusal"))
       (* 2 (cdr (assoc "x" arguments :test #'string=)))))
    (register-component-verifier
     runner "answer-is-even"
     (lambda (graph receipt)
       (declare (ignore graph))
       (let ((answer (cdr (assoc "answer" (wire-receipt-outputs receipt)
                                 :test #'string=))))
         (values (and (integerp answer) (evenp answer))
                 `(("answer" . ,answer))))))
    runner))

(defun make-worker (&key (id "worker-a") (capabilities '("compute/cpu@1"))
                          (max-steps 4) (max-output-bytes 4096))
  (make-work-worker :id id :capabilities capabilities :max-steps max-steps
                    :max-output-bytes max-output-bytes))

(defun test-completed-and-replay ()
  (let* ((composition (make-fixture))
         (worker (make-worker))
         (claim (make-distributed-work-claim
                 worker composition '(("x" . 21))
                 :max-steps 1 :max-output-bytes 1024))
         (same (make-distributed-work-claim
                worker composition '(("x" . 21))
                :max-steps 1 :max-output-bytes 1024))
         (runner (make-runner))
         (receipt (execute-distributed-work-claim
                   claim worker composition runner)))
    (expect (string= (distributed-work-claim-id claim)
                     (distributed-work-claim-id same))
            "claim identity is not deterministic")
    (expect (eq :completed (work-claim-receipt-verdict receipt))
            "certified work did not complete: ~A"
            (work-claim-receipt-refusal receipt))
    (expect (= 1 (work-claim-receipt-step-count receipt))
            "receipt did not bind observed step count")
    (expect (string= (work-claim-receipt-output-id receipt)
                     (canonical-id '(("answer" . 42))))
            "receipt output identity differs from Rosette output")
    (expect (plusp (work-claim-receipt-output-bytes receipt))
            "receipt did not measure canonical output bytes")
    (expect (eq :absent (distributed-work-claim-financial-authority claim))
            "claim unexpectedly has financial authority")
    (expect (eq :absent (distributed-work-claim-publication-authority claim))
            "claim unexpectedly has publication authority")
    (multiple-value-bind (valid fresh)
        (verify-distributed-work-receipt
         claim worker composition runner receipt)
      (expect valid "fresh receipt replay failed")
      (expect (string= (work-claim-receipt-id receipt)
                       (work-claim-receipt-id fresh))
              "fresh receipt identity diverged"))))

(defun test-refusals ()
  (let ((composition (make-fixture)))
    (expect
     (handler-case
         (progn
           (make-distributed-work-claim
            (make-worker :capabilities nil) composition '(("x" . 1)))
           nil)
       (work-claim-error (condition)
         (eq :capability-unavailable (work-claim-error-code condition))))
     "missing worker capability was accepted")
    (expect
     (handler-case
         (progn
           (make-distributed-work-claim
            (make-worker :max-steps 1) composition '(("x" . 1))
            :max-steps 2)
           nil)
       (work-claim-error (condition)
         (eq :step-budget (work-claim-error-code condition))))
     "claim above worker step ceiling was accepted")
    (expect
     (handler-case
         (progn
           (make-distributed-work-claim
            (make-worker :capabilities '("compute/cpu@1" "network/raw@1"))
            (make-fixture :extra-grant t) '(("x" . 1)))
           nil)
       (work-claim-error (condition)
         (eq :surplus-authority (work-claim-error-code condition))))
     "surplus Composition authority was accepted")
    (let* ((worker (make-worker))
           (claim (make-distributed-work-claim
                   worker composition '(("x" . 21))
                   :max-output-bytes 1024))
           (refused (execute-distributed-work-claim
                     claim worker composition (make-runner :fail t))))
      (expect (eq :refused (work-claim-receipt-verdict refused))
              "Rosette execution failure was not preserved as refusal")
      (expect (string= "rosette-certification-refused"
                       (work-claim-receipt-refusal refused))
              "refusal did not identify Rosette authority")
      (multiple-value-bind (valid fresh)
          (verify-distributed-work-receipt
           claim worker composition (make-runner :fail t) refused)
        (declare (ignore fresh))
        (expect valid "refused receipt was not replayable"))
      (let* ((other-claim
               (make-distributed-work-claim
                worker composition '(("x" . 22)) :max-output-bytes 1024)))
        (multiple-value-bind (valid fresh)
            (verify-distributed-work-receipt
             other-claim worker composition (make-runner) refused)
          (declare (ignore fresh))
          (expect (not valid) "receipt graft across inputs was accepted"))))))

(defun test-distributed-schedule ()
  (let* ((composition (make-fixture))
         (worker-a (make-worker :id "worker-a"))
         (worker-b (make-worker :id "worker-b"))
         (claim-a (make-distributed-work-claim
                   worker-a composition '(("x" . 20))))
         (claim-b (make-distributed-work-claim
                   worker-b composition '(("x" . 21))))
         (claims (list claim-a claim-b)))
    (multiple-value-bind (plan certificate)
        (plan-distributed-work-claims claims :max-workers 2)
      (multiple-value-bind (valid reasons)
          (verify-distributed-work-schedule claims plan certificate)
        (expect valid "distributed schedule failed verification: ~S" reasons)))))

(defun run-all-tests ()
  (setf *assertion-count* 0)
  (test-completed-and-replay)
  (test-refusals)
  (test-distributed-schedule)
  (format t "distributed-work-claim: ~D assertions passed.~%"
          *assertion-count*)
  t)
