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


(defsystem #:ship
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Ship any rosette system: reachable closure -> reduced self-contained file -> delivery floor, parity-certified."
  :version
  "0.1.0"
  :depends-on
  (#:browser-artifact #:proof-witness #:content-identity #:sound-self-rewrite
   #:egraph)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "mcp-runtime") (:file "closure") (:file "emit")
   (:file "reduce") (:file "ddmin") (:file "shrink") (:file "rewrite")
   (:file "rewrite-general") (:file "floors") (:file "parity")
   (:file "crossfloor") (:file "jit-runtime") (:file "identity")
   (:file "toolchain") (:file "cache") (:file "ship") (:file "main")
   (:file "parity-suite") (:file "deliver"))
  :in-order-to
  ((test-op (test-op #:ship/tests))))


(defsystem #:ship/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for ship."
  :depends-on
  (#:ship)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-ship/tests :run-all-tests)))
