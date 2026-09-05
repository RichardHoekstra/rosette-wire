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


(defsystem #:sel4-cap
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Typed raw seL4 endpoint-capability call contract and seed conformance gate."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "sel4-cap"))
  :in-order-to
  ((test-op (test-op #:sel4-cap/tests))))


(defsystem #:sel4-cap/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for sel4-cap."
  :depends-on
  (#:sel4-cap #:assert-core #:sel4-verify)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-sel4-cap/tests :run-all-tests)))
