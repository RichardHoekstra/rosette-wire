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


(defsystem #:assert-core
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Small assertion primitives over scalar tolerance checks."
  :version
  "0.1.0"
  :depends-on
  (#:scalar-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:assert-core/tests))))


(defsystem #:assert-core/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for assert-core."
  :depends-on
  (#:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-assert-core/tests :run-all-tests)))
