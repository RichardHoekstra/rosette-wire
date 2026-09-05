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


(defsystem #:distributed-work-claim
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Issue and verify content-addressed capability-bounded distributed work claims over Rosette Compositions."
  :version
  "0.1.0"
  :depends-on
  (#:wire-graph #:work-scheduler #:proof-witness #:symmetric-crypto)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "distributed-work-claim"))
  :in-order-to
  ((test-op (test-op #:distributed-work-claim/tests))))


(defsystem #:distributed-work-claim/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for distributed-work-claim."
  :depends-on
  (#:distributed-work-claim #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :distributed-work-claim/tests :run-all-tests)))
