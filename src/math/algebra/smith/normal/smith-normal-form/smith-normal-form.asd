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


(defsystem #:smith-normal-form
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Exact integer Smith normal form (D = U A V, U/V unimodular, invariant factors d_1|d_2|...) and integer homology WITH torsion -- the torsion the rational/Betti matrix-rank homology misses (Klein bottle H_1 = Z (+) Z/2)."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "smith") (:file "homology"))
  :in-order-to
  ((test-op (test-op #:smith-normal-form/tests))))


(defsystem #:smith-normal-form/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for smith-normal-form."
  :depends-on
  (#:smith-normal-form #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-smith-normal-form/tests :run-all-tests)))
