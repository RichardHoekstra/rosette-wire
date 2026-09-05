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


(defsystem #:chemistry-composition
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Flagship Rosette chemistry Composition spanning molecular analysis, exact reaction balancing, network and thermochemical evidence, and proof-producing expert conclusions."
  :version
  "0.1.0"
  :depends-on
  (#:wire-graph #:expert-components #:chemical-formula #:reaction-balancer
   #:reaction-network #:chemical-thermo #:process-engineering)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "chemistry-composition"))
  :in-order-to
  ((test-op (test-op #:chemistry-composition/tests))))


(defsystem #:chemistry-composition/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for chemistry-composition."
  :depends-on
  (#:chemistry-composition #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :chemistry-composition/tests :run-all-tests)))
