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


(defsystem #:program-ir
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Minimal homoiconic program AST, conversion, walking, and canonical equality primitives."
  :version
  "0.1.0"
  :depends-on
  (#:metadata-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "ast") (:file "convert") (:file "walk")
   (:file "equality"))
  :in-order-to
  ((test-op (test-op #:program-ir/tests))))


(defsystem #:program-ir/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for program-ir."
  :depends-on
  (#:program-ir)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-program-ir/tests :run-all-tests)))
