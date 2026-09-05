#.(progn (require :asdf) nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((self-dir (make-pathname :defaults *load-pathname* :name nil :type nil))
         (root (loop for dir = self-dir
                     then (make-pathname :directory (butlast (pathname-directory dir)) :defaults dir)
                     while (cdr (pathname-directory dir))
                     when (probe-file (merge-pathnames ".rosette-wire-root" dir)) return dir)))
    (if root
        (asdf:initialize-source-registry `(:source-registry (:tree ,root) :ignore-inherited-configuration))
        (pushnew self-dir asdf:*central-registry* :test #'equal))))

(in-package :asdf-user)


(defsystem #:claim-referee
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Generic falsifiable-claim referee: model an extraordinary claim as
data (name, claimed value or inequality, tolerance, measurement thunk), run the
measurement, and emit a structured verdict -- :confirmed / :refuted / :partial /
:untestable -- carrying the claimed value, the measured value, the ratio/margin,
and a human-readable reason.  Composite claims decompose into sub-claims so the
verdict separates which parts hold (the fusion 'ignition != Q>1 != plant-positive'
pattern).  Skeptical by default: a measurement that errors or is missing is
:untestable, never :confirmed.  The honest-accounting moat as a reusable kernel."
  :version
  "0.1.0"
  :depends-on
  (#:scalar-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:claim-referee/tests))))


(defsystem #:claim-referee/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law-verified tests for claim-referee."
  :depends-on
  (#:claim-referee)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-claim-referee/tests :run-all-tests)))
