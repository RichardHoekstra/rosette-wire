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


(defsystem #:hott-core
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Homotopy type theory vocabulary: types, paths, equivalences, dependent products/sums, quotients, and truncation levels."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core") (:file "paths") (:file "families")
   (:file "equivalences") (:file "equivalence-fibers")
   (:file "pointed-equivalences") (:file "equivalences-pullbacks")
   (:file "dependent-types") (:file "quotients-truncation")
   (:file "hit-pushouts") (:file "pointed-constructions")
   (:file "pointed-products") (:file "circle-interval")
   (:file "suspension-spheres"))
  :in-order-to
  ((test-op (test-op #:hott-core/tests))))


(defsystem #:hott-core/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for hott-core."
  :depends-on
  (#:hott-core #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-hott-core/tests :run-all-tests)))
