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


(defsystem #:egraph
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Small equality graph kernel over content-addressed Forms."
  :version
  "0.1.0"
  :depends-on
  (#:form-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "egraph"))
  :in-order-to
  ((test-op (test-op #:egraph/tests))))


(defsystem #:egraph/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for egraph."
  :depends-on
  (#:egraph #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-egraph/tests :run-all-tests)))
