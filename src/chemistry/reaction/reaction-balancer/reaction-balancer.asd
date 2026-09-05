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


(defsystem #:reaction-balancer
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Balance chemical reactions from component formulas into native process-engineering stoichiometric reactions."
  :version
  "0.1.0"
  :depends-on
  (#:process-engineering)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:reaction-balancer/tests))))


(defsystem #:reaction-balancer/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for reaction-balancer."
  :depends-on
  (#:reaction-balancer)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-reaction-balancer/tests :run-all-tests)))
