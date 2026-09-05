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


(defsystem #:reaction-network
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Chemical process reaction-network IR over native stoichiometric reactions and material streams, with element ledgers and conservation diagnostics."
  :version
  "0.1.0"
  :depends-on
  (#:process-engineering)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:reaction-network/tests))))


(defsystem #:reaction-network/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law tests for reaction-network."
  :depends-on
  (#:reaction-network)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-reaction-network/tests :run-all-tests)))
