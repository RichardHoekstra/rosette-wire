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


(defsystem #:browser-artifact
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Deterministic, policy-audited assembly of self-contained browser artifacts."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "browser-artifact"))
  :in-order-to
  ((test-op (test-op #:browser-artifact/tests))))


(defsystem #:browser-artifact/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for browser-artifact."
  :depends-on
  (#:browser-artifact #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-browser-artifact/tests :run-all-tests)))
