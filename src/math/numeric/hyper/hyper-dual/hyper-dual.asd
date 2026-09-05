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


(defsystem #:hyper-dual
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Hyper-dual numbers plus order-3/4 and arbitrary-order univariate jets for exact forward-mode autodiff, with executable scalar-tolerance certificates that transport one absolute/relative budget across every derivative order using explicit input-scale units."
  :version
  "0.1.0"
  :depends-on
  (#:scalar-core #:probability-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "util") (:file "hyper-dual")
   (:file "black-scholes") (:file "jet3") (:file "jet4") (:file "jet-n")
   (:file "tolerance") (:file "exact-jet") (:file "grad-hessian")
   (:file "mvjet")))
