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


(defsystem #:lisp-codegen
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "A compiler from a small integer Lisp (literals, arithmetic, comparison, IF, and recursive function calls) to stack-machine bytecode, plus a reference virtual machine that runs it.  VM values are rosette-lisp-objects tagged fixnums, under which ADD/SUB/MOD are raw word ops, MUL=(a*b)/8, DIV=(a/b)*8, and signed compare is monotonic -- so the SAME bytecode runs unchanged on the native VM (see /native, lowered through rosette-gpu-kernel-dsl's WHILE loop).  The calling convention uses a separate operand stack and call stack: CALL saves (ret-pc, old-fp), RET collapses the frame.  Recursion (factorial, fib, gcd, ackermann, mutual even/odd) compiles and runs."
  :version
  "0.1.0"
  :depends-on
  (#:lisp-objects)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "codegen"))
  :in-order-to
  ((test-op (test-op #:lisp-codegen/tests))))


(defsystem #:lisp-codegen/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for lisp-codegen: compile + run recursive integer Lisp against a CL reference oracle."
  :depends-on
  (#:lisp-codegen)
  :pathname
  "tests/"
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-lisp-codegen/tests :run-all-tests)))
