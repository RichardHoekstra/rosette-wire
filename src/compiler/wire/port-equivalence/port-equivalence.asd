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


(defsystem #:port-equivalence
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Certify executable HoTT/cubical equivalences between Rosette Ports and transport implementations and proof obligations across them."
  :version
  "0.1.0"
  :depends-on
  (#:wire-graph #:hott-core #:cubical-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "port-equivalence"))
  :in-order-to
  ((test-op (test-op #:port-equivalence/tests))))


(defsystem #:port-equivalence/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for port-equivalence."
  :depends-on
  (#:port-equivalence #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :port-equivalence/tests :run-all-tests)))
