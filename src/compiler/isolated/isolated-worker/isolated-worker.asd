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


(defsystem #:isolated-worker
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Bounded subprocess supervisor with argv-only launch, resource ceilings, compact receipt parsing, and privacy-safe output identities."
  :version
  "0.1.0"
  :depends-on
  (#:tool-envelope #:content-identity)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "worker"))
  :in-order-to
  ((test-op (test-op #:isolated-worker/tests))))


(defsystem #:isolated-worker/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :depends-on
  (#:isolated-worker)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-isolated-worker/tests :run-all-tests)))
