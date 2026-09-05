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


(defsystem #:foundation-rewrite
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Rewrite kernels for foundation categorical: normal forms, TLC beta-reduction, confluence, and DAG-CSE axiom ranking."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "conditions") (:file "util") (:file "normal-form")
   (:file "church-rosser") (:file "optimal-axiom"))
  :in-order-to
  ((test-op (test-op #:foundation-rewrite/tests))))


(defsystem #:foundation-rewrite/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for foundation-rewrite."
  :depends-on
  (#:foundation-rewrite)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-foundation-rewrite/tests :run-all-tests)))
