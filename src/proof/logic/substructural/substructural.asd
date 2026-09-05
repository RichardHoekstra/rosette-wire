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


(defsystem #:substructural
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "A propositional sequent calculus parameterized by first-class structural usage profiles: linear, affine, relevant, unrestricted, and ordered arise from which of exchange, weakening, and contraction a derivation may spend."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:substructural/tests))))


(defsystem #:substructural/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for substructural."
  :depends-on
  (#:substructural #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-substructural/tests :run-all-tests)))
