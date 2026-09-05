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


(defsystem #:chain-complex
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "The abstract (co)chain complex: exact d^2=0, field Betti numbers, integer torsion, relative coordinate quotients, and Euler-Poincare. Exact rational H^n is promoted from a dimension to ker(d_(n+1)^T)/im(d_n^T) with cocycle representatives and class coordinates. Exact per-degree chain maps carry replayable d f = f d receipts and induce certified contravariant cohomology maps by subquotient descent; identity and composition are verifier-backed. Simplicial/graph/Cech builders make this the common topological carrier."
  :version
  "0.2.0"
  :depends-on
  (#:smith-normal-form #:exact-linear-subquotient)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "rank") (:file "complex") (:file "relative")
   (:file "homology") (:file "rational-cohomology") (:file "chain-map")
   (:file "builders") (:file "cup"))
  :in-order-to
  ((test-op (test-op #:chain-complex/tests))))


(defsystem #:chain-complex/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for chain-complex."
  :depends-on
  (#:chain-complex #:assert-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-chain-complex/tests :run-all-tests)))
