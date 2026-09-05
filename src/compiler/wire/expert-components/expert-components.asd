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


(defsystem #:expert-components
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Bounded deterministic proof-producing expert rules exposed as Rosette Components with replayable content-addressed derivations."
  :version
  "0.1.0"
  :depends-on
  (#:wire-graph #:proof-witness)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "expert-components"))
  :in-order-to
  ((test-op (test-op #:expert-components/tests))))


(defsystem #:expert-components/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for expert-components."
  :depends-on
  (#:expert-components #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :expert-components/tests :run-all-tests)))
