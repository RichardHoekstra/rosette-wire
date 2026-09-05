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


(defsystem #:lisp-objects
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "The value representation for a from-scratch Lisp runtime: low-3-bit lowtags on a flat machine-word heap (fixnum/cons/char/singleton/closure), a precise self-describing tracer, a copying (compacting) collector, and flat-closure objects. Every word says pointer-or-not, so the heap is exactly traceable -- no conservative scanning -- and cycles are handled by marking. The substrate a tagged-value JIT backend (rosette-gpu-kernel-dsl native path) must emit and a GC must trace, zero deps."
  :version
  "0.1.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "objects"))
  :in-order-to
  ((test-op (test-op #:lisp-objects/tests))))


(defsystem #:lisp-objects/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for lisp-objects: lowtag round-trips, precise/exact tracing, copying GC, cyclic structure, and flat closures."
  :depends-on
  (#:lisp-objects)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-lisp-objects/tests :run-all-tests)))
