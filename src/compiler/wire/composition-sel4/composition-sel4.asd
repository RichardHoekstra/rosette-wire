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


(defsystem #:composition-sel4
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Lower Rosette Component capabilities and explicit service/data-flow edges into a content-addressed seL4/Microkit authority description with deadlock and information-flow verification."
  :version
  "0.1.0"
  :depends-on
  (#:wire-graph #:sel4-verify #:sel4-cap #:content-identity)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "composition-sel4"))
  :in-order-to
  ((test-op (test-op #:composition-sel4/tests))))


(defsystem #:composition-sel4/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for composition-sel4."
  :depends-on
  (#:composition-sel4 #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :composition-sel4/tests :run-all-tests)))
