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


(defsystem #:quantum-vm
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Coefficient-generic quantum tape VM with state-free qIR specialization, hyper-dual/adjoint/occurrence-shift AD, matrix-free QGT callbacks, and a bounded data-only circuit value/gradient filter."
  :version
  "0.3.0"
  :depends-on
  (#:quantum-ir #:hyper-dual)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "scalars") (:file "state") (:file "interpreter")
   (:file "specialize") (:file "observable") (:file "adjoint")
   (:file "gradients") (:file "verify") (:file "circuit-grad")))
