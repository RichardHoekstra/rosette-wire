;;;; distribution-composition/tests/tests.lisp --- Test suite.

(defpackage #:distribution-composition/tests
  (:use #:cl #:distribution-composition)
  (:import-from #:rosette-assert-core #:assert-true)
  (:import-from #:rosette-distribution-compiler
                #:make-distribution-definition #:distribution-error
                #:compile-distribution-plan #:distribution-plan-id)
  (:import-from #:rosette-wire
                #:composition-id #:wire-receipt-verdict #:wire-receipt-outputs
                #:wire-receipt-id #:make-composition-runner
                #:register-component-handler #:register-component-verifier
                #:run-composition)
  (:export #:run-all-tests))

(in-package #:distribution-composition/tests)

(defparameter *assertion-count* 0)

(defun expect (condition control &rest arguments)
  (incf *assertion-count*)
  (assert-true condition (apply #'format nil control arguments)))

(defun fixture-definition (&key (expected '("cell" "composition" "rosette")))
  (make-distribution-definition
   :name "rosette-fixture" :version "0.1.0" :roots '("rosette")
   :expected-systems expected :forbidden-systems '("private-garden")
   :forbidden-prefixes '("internal-") :max-systems 4
   :public-license "MIT" :candidate-floors '("eshkol")
   :cut-policy-id "sha256:cut" :export-policy-id "sha256:export"
   :license-grants-id "sha256:grants"))

(defun fixture-deps (name)
  (cond ((string= name "rosette") '("composition" "cell"))
        ((string= name "composition") '("cell"))
        (t nil)))

(defun fixture-resolvable (name)
  (member name '("rosette" "composition" "cell") :test #'string=))

(defun output (name receipt)
  (cdr (assoc name (wire-receipt-outputs receipt) :test #'string=)))

(defun test-certified-self-hosting ()
  (let* ((definition (fixture-definition))
         (plan (compile-distribution-plan
                definition :deps-fn #'fixture-deps
                :resolvable-fn #'fixture-resolvable))
         (composition
           (make-distribution-compiler-composition
            definition :deps-fn #'fixture-deps
            :resolvable-fn #'fixture-resolvable))
         (runner (make-distribution-composition-runner
                  composition definition :deps-fn #'fixture-deps
                  :resolvable-fn #'fixture-resolvable))
         (receipt (run-distribution-compiler-composition composition runner))
         (verification
           (verify-distribution-compiler-receipt composition receipt runner))
         (replayed
           (replay-distribution-compiler-composition composition runner receipt)))
    (expect (eq :pass (wire-receipt-verdict receipt))
            "self-hosted distribution Composition refused")
    (expect (eq :pass (wire-receipt-verdict verification))
            "fresh independent plan verification failed")
    (expect (search ":distribution" (output "manifest" receipt))
            "compiled manifest was not exposed")
    (expect (string= (distribution-plan-id plan) (output "planId" receipt))
            "Composition output differs from compiler closed form")
    (expect (string= (wire-receipt-id receipt) (wire-receipt-id replayed))
            "fresh replay did not reproduce receipt identity")
    (expect (string= (composition-id composition)
                     (composition-id
                      (make-distribution-compiler-composition
                       definition :deps-fn #'fixture-deps
                       :resolvable-fn #'fixture-resolvable)))
            "Composition identity is not deterministic")))

(defun test-refusals ()
  (expect
   (handler-case
       (progn
         (make-distribution-compiler-composition
          (fixture-definition :expected '("cell" "rosette"))
          :deps-fn #'fixture-deps :resolvable-fn #'fixture-resolvable)
         nil)
     (distribution-error () t))
   "closure drift did not refuse Composition construction")
  (let* ((left-definition (fixture-definition))
         (right-definition
           (make-distribution-definition
            :name "other-fixture" :version "0.1.0" :roots '("rosette")
            :expected-systems '("cell" "composition" "rosette")
            :forbidden-systems nil :forbidden-prefixes nil :max-systems 4
            :public-license "MIT" :candidate-floors nil
            :cut-policy-id "sha256:other" :export-policy-id "sha256:export"
            :license-grants-id "sha256:grants"))
         (left (make-distribution-compiler-composition
                left-definition :deps-fn #'fixture-deps
                :resolvable-fn #'fixture-resolvable))
         (right (make-distribution-compiler-composition
                 right-definition :deps-fn #'fixture-deps
                 :resolvable-fn #'fixture-resolvable)))
    (expect
     (handler-case
         (progn
           (make-distribution-composition-runner
            left right-definition :deps-fn #'fixture-deps
            :resolvable-fn #'fixture-resolvable)
           nil)
       (error () t))
     "definition/composition graft was accepted")
    (let* ((runner (make-distribution-composition-runner
                    left left-definition :deps-fn #'fixture-deps
                    :resolvable-fn #'fixture-resolvable))
           (receipt (run-composition left runner nil))
           (wrong-runner
             (make-distribution-composition-runner
              right right-definition :deps-fn #'fixture-deps
              :resolvable-fn #'fixture-resolvable))
           (verification
             (verify-distribution-compiler-receipt right receipt wrong-runner)))
      (expect (eq :fail (wire-receipt-verdict verification))
              "receipt graft across distribution identities was accepted"))))

(defun run-all-tests ()
  (setf *assertion-count* 0)
  (test-certified-self-hosting)
  (test-refusals)
  (format t "distribution-composition: ~D assertions passed.~%"
          *assertion-count*)
  t)
