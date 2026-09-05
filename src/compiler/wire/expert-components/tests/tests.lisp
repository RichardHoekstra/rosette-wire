;;;; expert-components/tests/tests.lisp --- Test suite.

(defpackage #:expert-components/tests
  (:use #:cl #:expert-components)
  (:import-from #:rosette-assert-core #:assert-true)
  (:import-from #:rosette-proof-witness #:runtime-verify)
  (:import-from #:rosette-wire
                #:make-component-node #:make-wire-step #:make-data-binding
                #:input-source #:make-wire-output #:make-composition
                #:make-component-field #:list-type #:make-composition-runner
                #:register-component-handler #:register-component-verifier
                #:run-composition
                #:wire-receipt-verdict #:wire-receipt-outputs
                #:verify-composition-receipt)
  (:export #:run-all-tests))

(in-package #:expert-components/tests)

(defvar *assertions* 0)

(defun check (condition control &rest arguments)
  (incf *assertions*)
  (assert-true condition (apply #'format nil control arguments)))

(defun refusal-code (thunk)
  (handler-case (progn (funcall thunk) nil)
    (expert-refusal (condition) (expert-refusal-code condition))))

(defun chemistry-system ()
  (make-expert-system
   "org.rosette/chemistry-expert"
   (list
    (make-expert-rule
     "combustion-candidate"
     (list (make-expert-fact "fuel" '("?species") :pattern t)
           (make-expert-fact "oxidizer-present" '("oxygen") :pattern t))
     (make-expert-fact "reaction-candidate"
                       '("?species" "oxygen") :pattern t))
    (make-expert-rule
     "admit-balanced-candidate"
     (list (make-expert-fact "reaction-candidate"
                             '("?fuel" "?oxidizer") :pattern t)
           (make-expert-fact "element-balanced"
                             '("?fuel" "?oxidizer") :pattern t))
     (make-expert-fact "admissible-reaction"
                       '("?fuel" "?oxidizer") :pattern t)))))

(defun initial-facts ()
  (list (make-expert-fact "fuel" '("hydrogen"))
        (make-expert-fact "oxidizer-present" '("oxygen"))
        (make-expert-fact "element-balanced" '("hydrogen" "oxygen"))))

(defun fact-present-p (run predicate arguments)
  (find-if (lambda (fact)
             (and (string= predicate (expert-fact-predicate fact))
                  (equal arguments (expert-fact-arguments fact))))
           (expert-run-facts run)))

(defun test-inference-and-proof ()
  (let* ((system (chemistry-system))
         (inputs (initial-facts))
         (run (run-expert-system system inputs)))
    (check (fact-present-p run "admissible-reaction" '("hydrogen" "oxygen"))
           "two-round chemistry conclusion was not derived")
    (check (= 2 (expert-run-rounds run))
           "expected two inference rounds, got ~D" (expert-run-rounds run))
    (check (verify-expert-run system inputs run)
           "expert proof ledger did not replay")
    (check (runtime-verify (expert-run-certificate system inputs run))
           "proof-witness wrapper did not rederive the expert receipt")
    (check (not (verify-expert-run
                 system (list (first inputs) (second inputs)) run))
           "expert receipt grafted onto missing evidence")
    (check (eq :round-budget
               (refusal-code
                (lambda ()
                  (run-expert-system system inputs :max-rounds 1))))
           "round bound did not fail closed")
    (check (not (fact-present-p
                 (run-expert-system system (list (first inputs) (second inputs)))
                 "admissible-reaction" '("hydrogen" "oxygen")))
           "expert admitted a reaction without balance evidence")))

(defun test-rosette-component ()
  (let* ((system (chemistry-system))
         (descriptor (make-expert-component system))
         (node (make-component-node
                "expert" descriptor
                "sha256:0000000000000000000000000000000000000000000000000000000000000001"))
         (composition
           (make-composition
            :name "org.rosette/chemistry-expert-witness"
            :nodes (list node) :services nil
            :steps
            (list
             (make-wire-step
              :id "infer" :node-id "expert" :port "expert" :operation "infer"
              :bindings
              (list (make-data-binding "facts" (input-source "facts")))))
            :inputs
            (list (make-component-field
                   "facts" (list-type (expert-fact-wire-type))))
            :outputs (list (make-wire-output "result" "infer"))
            :capability-grants nil
            :required-evidence '("expert-derivation-replay/v1")
            :limits '(("maxSteps" . 2) ("maxOutputBytes" . 65536))))
         (runner (make-composition-runner)))
    (register-component-handler runner "expert" "expert" "infer"
                                (make-expert-handler system))
    (let* ((input-values (mapcar #'expert-fact->value (initial-facts)))
           (expected
             (expert-run->value
              (run-expert-system system (initial-facts))))
           (receipt (run-composition composition runner
                                     (list (cons "facts" input-values)))))
      (check (eq :pass (wire-receipt-verdict receipt))
             "Rosette refused the expert Component execution")
      (check (wire-receipt-outputs receipt)
             "expert Component produced no output")
      (register-component-verifier
       runner "expert-derivation-replay/v1"
       (lambda (graph candidate)
         (declare (ignore graph))
         (let ((actual (cdr (assoc "result"
                                   (wire-receipt-outputs candidate)
                                   :test #'string=))))
           (values (equal actual expected)
                   `(("expectedReceiptId" .
                      ,(cdr (assoc "receiptId" expected :test #'string=))))))))
      (let ((verification
              (verify-composition-receipt composition receipt runner)))
        (check (eq :pass (wire-receipt-verdict verification))
               "Rosette expert Component receipt did not replay")))))

(defun test-rule-refusals ()
  (check (eq :unbound-conclusion
             (refusal-code
              (lambda ()
                (make-expert-rule
                 "bad" (list (make-expert-fact "a" '("?x") :pattern t))
                 (make-expert-fact "b" '("?y") :pattern t)))))
         "rule with an unbound conclusion variable was accepted"))

(defun test-constant-only-rule ()
  (let* ((system
           (make-expert-system
            "org.rosette/constant-rule"
            (list
             (make-expert-rule
              "close" (list (make-expert-fact "ready" '("yes") :pattern t))
              (make-expert-fact "closed" '("yes") :pattern t)))))
         (run (run-expert-system
               system (list (make-expert-fact "ready" '("yes"))))))
    (check (fact-present-p run "closed" '("yes"))
           "constant-only rule did not fire")))

(defun run-all-tests ()
  (setf *assertions* 0)
  (test-inference-and-proof)
  (test-rosette-component)
  (test-rule-refusals)
  (test-constant-only-rule)
  (format t "expert-components: ~D assertions passed~%" *assertions*)
  t)
