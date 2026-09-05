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


(defsystem #:process-engineering
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Small law-tested process-engineering kernel for material streams, native stoichiometric reactions, unit-operation carriers, heat ledgers, and mass-balance residuals."
  :version
  "0.1.0"
  :depends-on
  (#:chemical-formula #:scalar-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:process-engineering/tests))))


(defsystem #:process-engineering/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law tests for process-engineering."
  :depends-on
  (#:process-engineering)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-process-engineering/tests :run-all-tests)))
