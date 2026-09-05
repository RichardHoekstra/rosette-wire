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


(defsystem #:proof-carrier-core
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Explicit proof-certificate AST and stable plist serialization."
  :version
  "0.1.0"
  :depends-on
  (#:metadata-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "carrier"))
  :in-order-to
  ((test-op (test-op #:proof-carrier-core/tests))))


(defsystem #:proof-carrier-core/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for proof-carrier-core."
  :depends-on
  (#:proof-carrier-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-proof-carrier-core/tests :run-all-tests)))
