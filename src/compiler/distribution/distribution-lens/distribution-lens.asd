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


(defsystem #:distribution-lens
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Checked bidirectional distribution views with provenance-bearing edit lift, typed obstructions, and round-trip laws."
  :version
  "0.1.0"
  :depends-on
  (#:content-identity #:symmetric-crypto)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "distribution-lens"))
  :in-order-to
  ((test-op (test-op #:distribution-lens/tests))))


(defsystem #:distribution-lens/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for distribution-lens."
  :depends-on
  (#:distribution-lens #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :distribution-lens/tests :run-all-tests)))
