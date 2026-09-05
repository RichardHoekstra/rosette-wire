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


(defsystem #:media-profile
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Rosette's typed temporal media profile, renderer-independent semantic scene contract, deterministic SVG/CPU projections, and first public-data quantum microscope vertical."
  :version
  "0.1.0"
  :depends-on
  (#:wire-graph #:scene-ir #:json #:raster-core #:compression-codec #:net-frame)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "media-profile") (:file "debug-scenes")
   (:file "dendritic-garden") (:file "audio-adapter")))
