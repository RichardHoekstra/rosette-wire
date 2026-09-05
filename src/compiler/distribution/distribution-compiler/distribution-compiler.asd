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


(defsystem #:distribution-compiler
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Compile policy-defined Rosette system closures into standalone source repositories with checked namespaces, licenses, provenance, and reproducible identities."
  :version
  "0.1.0"
  :depends-on
  (#:ship #:toml #:symmetric-crypto)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "distribution-compiler"))
  :in-order-to
  ((test-op (test-op #:distribution-compiler/tests))))


(defsystem #:distribution-compiler/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for distribution-compiler."
  :depends-on
  (#:distribution-compiler #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :distribution-compiler/tests :run-all-tests)))
