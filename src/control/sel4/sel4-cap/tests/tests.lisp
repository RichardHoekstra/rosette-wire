;;;; rosette-sel4-cap/tests/tests.lisp --- Test suite.

(defpackage #:rosette-sel4-cap/tests
  (:use #:cl #:rosette-sel4-cap)
  (:import-from #:rosette-assert-core #:assert-true)
  (:export #:run-all-tests))

(in-package #:rosette-sel4-cap/tests)

(defun rejects-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (invalid-capability-call () t)))

(defun seed-path (name)
  (merge-pathnames
   (concatenate 'string "sel4/" name)
   (asdf:system-source-directory :lisp-codegen)))

(defun slurp (pathname)
  (uiop:read-file-string pathname))

(defun run-all-tests ()
  ;; Address law: Microkit channel n selects BASE_ENDPOINT_CAP+n.
  (assert-true (= #x1003 (endpoint-capability #x1000 3))
               "endpoint capability is the base slot plus channel")
  ;; Marshalling law: one argument occupies MR0 and message length is exactly 1.
  (let ((call (make-cap-call1 #x1000 3 #xdeadbeef)))
    (assert-true (= #x1003 (cap-call-endpoint call))
                 "call targets the derived endpoint slot")
    (assert-true (equalp #(#xdeadbeef) (cap-call-message-registers call))
                 "call carries exactly the argument in MR0")
    (assert-true
     (equal '(:label 0 :caps-unwrapped 0 :extra-caps 0 :length 1)
            (cap-call-message-info call))
     "message-info carries no caps and exactly one word"))
  ;; Negative controls: signed values and capability arithmetic overflow refuse.
  (assert-true (rejects-p (lambda () (endpoint-capability 0 -1)))
               "negative channels are rejected")
  (assert-true
   (rejects-p (lambda () (endpoint-capability #xffffffffffffffff 1)))
   "endpoint slot overflow is rejected")
  (assert-true (rejects-p (lambda () (make-cap-call1 0 0 -1)))
               "negative message words are rejected")
  ;; Integration gate: the freestanding header implements the modeled sequence.
  (let ((header (slurp (seed-path "rosette_sel4.h"))))
    (assert-true (null (header-contract-errors header))
                 "seed header conforms to the raw call model")
    (let* ((needle "seL4_GetMR(0)")
           (position (search needle header))
           (damaged (copy-seq header)))
      (setf (char damaged position) #\X)
      (assert-true (not (null (header-contract-errors damaged)))
                   "mutated MR0 receive is rejected")))
  ;; Composition gate: reuse the existing static referee for the two-PD witness.
  (let* ((system
           (rosette-sel4-verify:parse-system-file (seed-path "rawcap.system")))
         (verdict (rosette-sel4-verify:referee system)))
    (assert-true (= 2 (length (rosette-sel4-verify:system-pds system)))
                 "raw-cap witness has two protection domains")
    (assert-true
     (and (rosette-sel4-verify:verdict-deadlock-free verdict)
          (rosette-sel4-verify:priority-monotone-p system))
     "raw-cap protected call has a rising-priority progress potential"))
  ;; Witness gate: the client uses the raw wrapper and its expected reply is 81.
  (let ((client (slurp (seed-path "rawcap_client.c"))))
    (assert-true (and (search "rosette_cap_call1(CH, 9)" client)
                      (search "expect 81" client))
                 "client binds the raw call to its observable reply"))
  t)
