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


(defsystem #:blc
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "John Tromp's Binary Lambda Calculus: the encoding that makes Minimum-Description-Length literal -- a de-Bruijn lambda term gets an EXACT bit-length. Terms are (:var n)/(:lam body)/(:app f a). blc-encode lowers a term to a bitstring (variable index n -> 1^(n+1) 0; abstraction -> 00 enc(body); application -> 01 enc(f) enc(g)); blc-length gives the bit count without building the string; blc-decode parses bits back to a term (round-trip). Ships the S/K/I combinators and Church numerals as terms so substrate laws can be MEASURED in bits."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "core") (:file "sharing") (:file "blc2"))
  :in-order-to
  ((test-op (test-op #:blc/tests))))


(defsystem #:blc/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for blc."
  :depends-on
  (#:blc #:assert-core)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-blc/tests :run-all-tests)))
