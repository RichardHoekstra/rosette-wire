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


(defsystem #:gpu-kernel-dsl
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "s-expr CUDA-C kernel DSL: defkernel and pure source codegen."
  :version
  "0.3.0"
  :depends-on
  nil
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "conditions") (:file "warp-reduction")
   (:file "register-fragment") (:file "online-softmax-moment")
   (:file "memory-schedule") (:file "spec") (:file "validate")
   (:file "codegen-expr") (:file "backend-types") (:file "codegen")
   (:file "launch-base") (:file "bundled-kernels")))


(defsystem #:gpu-kernel-dsl/cpu
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Pure-Lisp CPU interpreter for the gpu-kernel-dsl DSL: execute a KERNEL-SPEC with no CUDA."
  :version
  "0.2.0"
  :depends-on
  (#:gpu-kernel-dsl)
  :pathname
  "src/"
  :components
  ((:file "interpret"))
  :in-order-to
  ((test-op (test-op #:gpu-kernel-dsl/cpu/tests))))


(defsystem #:gpu-kernel-dsl/cpu/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for the CPU interpreter."
  :depends-on
  (#:gpu-kernel-dsl/cpu)
  :pathname
  "tests/"
  :components
  ((:file "cpu-tests"))
  :perform
  (test-op (op c) (symbol-call :rosette-gpu-kernel-dsl/cpu/tests :run-all-tests)))
