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


(defsystem #:ccc
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "A cartesian closed category (FinSet -- finite sets and functions) and the Lambek correspondence to the simply-typed lambda-calculus: the categorical THIRD leg of Curry-Howard-Lambek. Objects/morphisms with identity + composition (associativity + unit laws), a terminal object (unit type), binary products (projections + pairing with the mediating-morphism universal property), and exponentials B^A with eval + currying. The headline law is the exponential ADJUNCTION Hom(A x B, C) ~ Hom(A, C^B): curry/uncurry are a NATURAL bijection -- this IS currying, and a CCC is the simply-typed lambda-calculus (Lambek 1970 / Lawvere). beta and eta become categorical equations (eval . (curry f x id) = f ; curry eval = id). A de-Bruijn STLC denotes compositionally into FinSet and a small term round-trips through its morphism (term -> morphism -> value -> normal form), with denotation invariant under beta/eta reduction."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core"))
  :in-order-to
  ((test-op (test-op #:ccc/tests))))


(defsystem #:ccc/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Law-verifying tests for ccc."
  :depends-on
  (#:ccc)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-ccc/tests :run-all-tests)))
