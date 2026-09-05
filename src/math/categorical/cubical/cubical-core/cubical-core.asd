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


(defsystem #:cubical-core
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "A minimal computational cubical kernel over rosette-hott-core: a primitive interval I (De Morgan algebra), paths as functions I->A (endpoints reduce definitionally), Kan operations transp/hcomp, and COMPUTATIONAL univalence -- transp (ua e) x reduces to (e x) by structural recursion on the Glue type-line, the beta-rule that is STUCK in book-HoTT."
  :version
  "0.1.0"
  :depends-on
  (#:hott-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "interval") (:file "paths") (:file "kan")
   (:file "examples") (:file "circle") (:file "squares") (:file "pathp")
   (:file "dependent") (:file "homotopy-groups") (:file "face-lattice")
   (:file "connections") (:file "type-formers") (:file "glue")
   (:file "sphere2") (:file "suspension") (:file "hopf") (:file "flattening")
   (:file "circle-hspace") (:file "hspace-coherence") (:file "whitehead")
   (:file "pi4-s3") (:file "join") (:file "hopf-construction")
   (:file "moore-space") (:file "truncation") (:file "cyclic-homotopy")
   (:file "pi4-encode") (:file "pi4-equiv"))
  :in-order-to
  ((test-op (test-op #:cubical-core/tests))))


(defsystem #:cubical-core/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for cubical-core."
  :depends-on
  (#:cubical-core #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-cubical-core/tests :run-all-tests)))
