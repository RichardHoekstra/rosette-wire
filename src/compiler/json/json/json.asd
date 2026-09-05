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


(defsystem #:json
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Dependency-free JSON codec: parse a JSON string into Lisp data (alists/lists/strings/numbers/booleans/null) and encode Lisp data back to JSON, with round-trip and clear errors on malformed input."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "encode") (:file "parse"))
  :in-order-to
  ((test-op (test-op #:json/tests))))


(defsystem #:json/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for json."
  :depends-on
  (#:json #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "package") (:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-json/tests :run-all-tests)))
