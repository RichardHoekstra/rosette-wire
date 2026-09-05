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


(defsystem #:sharing-reduction
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Interaction-net optimal reduction with replayable per-term schedule-gauge certificates: maximal sharing tames naive duplication, while local strongly-confluent rewrites certify sequential and parallel normal-form agreement."
  :version
  "0.1.0"
  :depends-on
  (#:blc #:form-core #:sexpr-dag #:foundation-rewrite)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "net") (:file "rules") (:file "reduce")
   (:file "lambda") (:file "readback") (:file "church") (:file "verdict"))
  :in-order-to
  ((test-op (test-op #:sharing-reduction/tests))))


(defsystem #:sharing-reduction/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for sharing-reduction."
  :depends-on
  (#:sharing-reduction)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-sharing-reduction/tests :run-all-tests)))
