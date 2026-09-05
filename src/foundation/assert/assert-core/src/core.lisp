;;;; core.lisp --- assertion primitives.

(in-package #:rosette-assert-core)

(defun assert-true (condition message)
  "Signal an error unless CONDITION is true."
  (unless condition
    (error "Assertion failed: ~A" message)))

(defun %expected-actual-message (message expected actual)
  (format nil "~A expected ~A got ~A" message expected actual))

(defun assert= (expected actual message)
  "Signal unless EXPECTED and ACTUAL are numerically =."
  (assert-true (= expected actual)
               (%expected-actual-message message expected actual)))

(defun assert-close (expected actual message &optional (tolerance 1d-12))
  "Signal unless EXPECTED and ACTUAL are within TOLERANCE."
  (assert-true (approx= expected actual tolerance)
               (%expected-actual-message message expected actual)))

(defun assert-array-close (expected actual message &optional (tolerance 1d-12))
  "Signal unless EXPECTED and ACTUAL arrays have equal size and close elements."
  (assert-true (= (array-total-size expected) (array-total-size actual))
               (format nil "~A size mismatch" message))
  (loop for idx below (array-total-size expected) do
    (assert-close (row-major-aref expected idx)
                  (row-major-aref actual idx)
                  message
                  tolerance)))

(defstruct (test-run
            (:constructor make-test-run (name)))
  "Minimal counted test run state for dependency-light library tests."
  (name nil :type t)
  (assertions 0 :type fixnum)
  (failures 0 :type fixnum))

(defun record-test (run condition message &optional form)
  "Record one assertion result in RUN and print a compact failure line."
  (incf (test-run-assertions run))
  (unless condition
    (incf (test-run-failures run))
    (format t "  FAIL: ~A~%" message)
    (when form
      (format t "       form: ~S~%" form)))
  condition)

(defmacro check (run form &optional (message "assertion failed"))
  "Evaluate FORM and record it as one assertion in RUN."
  `(record-test ,run ,form ,message ',form))

(defun finish-test-run (run)
  "Print RUN summary, signal on failure, and return the assertion count."
  (format t "~&~A: ~D assertions, ~D failures~%"
          (test-run-name run)
          (test-run-assertions run)
          (test-run-failures run))
  (when (plusp (test-run-failures run))
    (error "~A test failures: ~D"
           (test-run-name run)
           (test-run-failures run)))
  (test-run-assertions run))

(defmacro with-test-run ((run name) &body body)
  "Bind RUN to a counted test run, execute BODY, then finish it."
  `(let ((,run (make-test-run ,name)))
     ,@body
     (finish-test-run ,run)))
