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


(defsystem #:chemical-thermo
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Chemical thermochemistry evidence over process-engineering streams and reaction networks."
  :version
  "0.1.0"
  :depends-on
  (#:process-engineering #:reaction-network)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:chemical-thermo/tests))))


(defsystem #:chemical-thermo/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law tests for chemical-thermo."
  :depends-on
  (#:chemical-thermo)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-chemical-thermo/tests :run-all-tests)))
