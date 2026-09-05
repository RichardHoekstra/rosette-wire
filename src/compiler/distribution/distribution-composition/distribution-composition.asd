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


(defsystem #:distribution-composition
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Execute and independently verify the policy-defined distribution compiler as a Rosette Composition."
  :version
  "0.1.0"
  :depends-on
  (#:distribution-compiler #:wire-graph)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "distribution-composition"))
  :in-order-to
  ((test-op (test-op #:distribution-composition/tests))))


(defsystem #:distribution-composition/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for distribution-composition."
  :depends-on
  (#:distribution-composition #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :distribution-composition/tests :run-all-tests)))
