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


(defsystem #:composition-interaction
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Lower pure Rosette Compositions to interaction nets and issue replayable schedule-gauge execution certificates."
  :version
  "0.1.0"
  :depends-on
  (#:wire-graph #:sharing-reduction #:foundation-rewrite)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "composition-interaction"))
  :in-order-to
  ((test-op (test-op #:composition-interaction/tests))))


(defsystem #:composition-interaction/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for composition-interaction."
  :depends-on
  (#:composition-interaction #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :composition-interaction/tests :run-all-tests)))
