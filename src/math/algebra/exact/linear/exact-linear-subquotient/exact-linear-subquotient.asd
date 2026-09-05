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


(defsystem #:exact-linear-subquotient
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Zero-dependency exact rational linear algebra for H=ker(A)/im(B): immutable carriers, quotient representatives and class coordinates, executable projection/lift/retraction receipts, and map descent with separately located cycle- and relation-preservation residuals. Row-list matrices act on columns; all arithmetic is exact Common Lisp rational arithmetic."
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
  ((test-op (test-op #:exact-linear-subquotient/tests))))


(defsystem #:exact-linear-subquotient/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Exact quotient, mutation-isolation, shape-wall, and map-descent tests."
  :depends-on
  (#:exact-linear-subquotient)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-exact-linear-subquotient/tests :run-all-tests)))
