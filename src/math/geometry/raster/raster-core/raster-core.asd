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


(defsystem #:raster-core
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Primitive RGB framebuffer, pixel blending, line rasterization, and PPM I/O."
  :version
  "0.1.0"
  :depends-on
  (#:byte-core)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "conditions") (:file "types") (:file "frame")
   (:file "lines") (:file "ppm")))
