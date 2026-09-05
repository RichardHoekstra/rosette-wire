;;;; chemistry-composition/tests/tests.lisp --- Test suite.

(defpackage #:chemistry-composition/tests
  (:use #:cl #:chemistry-composition)
  (:import-from #:rosette-assert-core #:assert-true)
  (:import-from #:rosette-wire
                #:wire-receipt-verdict #:wire-receipt-outputs
                #:make-composition-runner #:run-composition)
  (:export #:run-all-tests))

(in-package #:chemistry-composition/tests)

(defvar *assertions* 0)

(defun check (condition control &rest arguments)
  (incf *assertions*)
  (assert-true condition (apply #'format nil control arguments)))

(defun output (receipt name)
  (cdr (assoc name (wire-receipt-outputs receipt) :test #'string=)))

(defun object-ref (object key &optional default)
  (let ((entry (and (listp object) (assoc key object :test #'string=))))
    (if entry (cdr entry) default)))

(defun fact-present-p (facts predicate &optional arguments)
  (find-if
   (lambda (fact)
     (and (string= predicate (object-ref fact "predicate" ""))
          (or (null arguments)
              (equal arguments (object-ref fact "arguments" nil)))))
   facts))

(defun test-flagship ()
  (let ((receipt (run-chemistry-flagship)))
    (check (eq :pass (wire-receipt-verdict receipt))
           "flagship chemistry Composition did not execute")
    (let ((molecule (output receipt "molecule"))
          (reaction (output receipt "reactionEvidence"))
          (expert (output receipt "expertProof")))
      (check (= 32042 (object-ref molecule "molecularWeightMilli"))
             "methanol molecular mass did not round to 32.042 g/mol")
      (check (fact-present-p
              reaction "reaction-stoichiometry"
              '("methanol-synthesis" "1" "2" "1"))
             "exact 1:2:1 methanol stoichiometry is absent")
      (check (fact-present-p reaction "mass-closed"
                            '("methanol-synthesis"))
             "network mass evidence is absent")
      (check (fact-present-p reaction "elements-closed"
                            '("methanol-synthesis"))
             "network element evidence is absent")
      (check (fact-present-p reaction "exothermic"
                            '("methanol-synthesis"))
             "standard reaction enthalpy sign is absent")
      (check (fact-present-p
              (object-ref expert "facts" nil)
              "chemistry-vertical-complete" '("methanol-synthesis"))
             "expert proof did not close the chemistry vertical"))
    (let ((verification (verify-chemistry-receipt receipt)))
      (check (eq :pass (wire-receipt-verdict verification))
             "flagship chemistry evidence did not independently replay"))))

(defun test-negative-controls ()
  (let ((wrong (run-chemistry-flagship :formula "H2O")))
    (check (eq :fail (wire-receipt-verdict wrong))
           "non-methanol formula crossed the fixed flagship boundary"))
  (let* ((composition (make-chemistry-composition))
         (unregistered (make-composition-runner))
         (receipt (run-chemistry-flagship))
         (verification
           (rosette-wire:verify-composition-receipt
            composition receipt unregistered)))
    (check (eq :fail (wire-receipt-verdict verification))
           "missing evidence verifiers were silently trusted")))

(defun run-all-tests ()
  (setf *assertions* 0)
  (test-flagship)
  (test-negative-controls)
  (format t "chemistry-composition: ~D assertions passed~%" *assertions*)
  t)
