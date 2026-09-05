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


(defsystem #:proof-witness
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Proof witness handles, registries, and runtime verification hooks."
  :version
  "0.1.0"
  :depends-on
  (#:metadata-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "lean-witness") (:file "certificate")
   (:file "delta") (:file "publish") (:file "runtime-verify"))
  :in-order-to
  ((test-op (test-op #:proof-witness/tests))))


(defsystem #:proof-witness/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for proof-witness."
  :depends-on
  (#:proof-witness #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-proof-witness/tests :run-all-tests)))
