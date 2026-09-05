;;;; tests.lisp --- tests for rosette-metadata-core.

(defpackage #:rosette-metadata-core/tests
  (:use #:cl #:rosette-metadata-core #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-metadata-core/tests)

(defun signals-error-p (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (error () t)))

(defun run-all-tests ()
  "Run rosette-metadata-core tests."
  (with-test-run (run "rosette-metadata-core")
    (check run (plist-p nil)
           "NIL is a valid empty plist")
    (check run (plist-p '(:source :test :stage 1))
           "symbol-key plist is accepted")
    (check run (not (plist-p '(:source)))
           "odd-length list is rejected")
    (check run (not (plist-p '(1 :bad)))
           "non-symbol keys are rejected")
    (check run (eq (require-plist '(:source :test)) '(:source :test))
           "require-plist returns valid plist")
    (check run (signals-error-p
                (lambda () (require-plist '(:dangling))))
           "require-plist rejects malformed plist")
    (let* ((source (list :source :caller))
           (copy (copy-plist source)))
      (setf (getf source :source) :mutated)
      (check run (equal copy '(:source :caller))
             "copy-plist isolates caller-owned conses"))
    (let ((merged (merge-plist-right '(:a 1 :b 2)
                                     '(:b 3 :c 4))))
      (check run (and (= (getf merged :a) 1)
                      (= (getf merged :b) 3)
                      (= (getf merged :c) 4))
             "merge-plist-right keeps later values"))
    t))
