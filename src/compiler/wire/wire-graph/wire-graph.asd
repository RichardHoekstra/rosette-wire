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


(defsystem #:wire-graph
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Immutable typed component descriptors, canonical graph identities, deterministic validation, and bounded Wire execution."
  :version
  "0.1.0"
  :depends-on
  (#:graph-algorithms #:graph-core #:json #:symmetric-crypto #:wire-protocol)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "wire-graph") (:file "canonical-json")
   (:file "graph") (:file "runtime") (:file "decode") (:file "cli")
   (:file "semantic-aliases"))
  :in-order-to
  ((test-op (test-op #:wire-graph/tests))))


(defsystem #:wire-graph/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for wire-graph."
  :depends-on
  (#:wire-graph #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :wire-graph/tests :run-all-tests)))
