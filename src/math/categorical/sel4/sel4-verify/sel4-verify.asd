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


(defsystem #:sel4-verify
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "An seL4 / Microkit system-description referee: OS correctness as a computable obstruction. A synchronous protected-call graph deadlocks iff it has a directed cycle; it is deadlock-free iff a global progress potential exists. Information-flow security has the same monotone-potential shape: security labels must not drop along flows. Parses system descriptions, derives protected-call edges, certifies deadlock-freedom and information-flow security, and reports the undirected b1 from rosette-chain-complex as a homological shadow only."
  :version
  "0.1.0"
  :depends-on
  (#:chain-complex)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "verify"))
  :in-order-to
  ((test-op (test-op #:sel4-verify/tests))))


(defsystem #:sel4-verify/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for sel4-verify: deadlock-freedom, info-flow, and parsing on a corpus of Microkit .system descriptions."
  :depends-on
  (#:sel4-verify)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-sel4-verify/tests :run-all-tests)))
