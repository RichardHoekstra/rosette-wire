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


(defsystem #:toml
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Dependency-free strict TOML subset for scientific case configuration: tables, dotted keys, strings, booleans, finite numbers, and homogeneous arrays, with located parse errors and deterministic writing."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "toml"))
  :in-order-to
  ((test-op (test-op #:toml/tests))))


(defsystem #:toml/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Round-trip, table, array, and rejection laws for toml."
  :depends-on
  (#:toml #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-toml/tests :run-all-tests)))
