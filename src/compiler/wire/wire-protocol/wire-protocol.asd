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


(defsystem #:wire-protocol
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Bounded byte-stream transport for immutable content-addressed Cells using fixed binary Frames and an incremental multiplexed Wire decoder."
  :version
  "0.1.0"
  :depends-on
  (#:symmetric-crypto)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "wire-protocol"))
  :in-order-to
  ((test-op (test-op #:wire-protocol/tests))))


(defsystem #:wire-protocol/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for wire-protocol."
  :depends-on
  (#:wire-protocol #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :wire-protocol/tests :run-all-tests)))
