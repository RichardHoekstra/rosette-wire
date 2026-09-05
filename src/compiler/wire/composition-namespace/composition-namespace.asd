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


(defsystem #:composition-namespace
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Project Rosette Compositions into a deterministic Plan 9-style namespace with Wire-backed operation endpoints and a bounded semantic 9P2000 adapter."
  :version
  "0.1.0"
  :depends-on
  (#:wire-graph #:content-identity #:json #:symmetric-crypto)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "composition-namespace"))
  :in-order-to
  ((test-op (test-op #:composition-namespace/tests))))


(defsystem #:composition-namespace/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for composition-namespace."
  :depends-on
  (#:composition-namespace #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :composition-namespace/tests :run-all-tests)))
