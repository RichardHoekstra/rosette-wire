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


(defsystem #:scene-ir
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "One homoiconic property-tree s-expr that is simultaneously the fb_scene display list, the compositor layer list, and the layout target: display-list IR + executor over rosette-raster-core, a z-ordered SRC_OVER layer compositor (the promoted seL4 fb_blit), a single-pass block/flow layout lowering, and an fb_scene.txt emitter that feeds the existing freestanding seL4 renderer byte-exact."
  :version
  "0.1.0"
  :depends-on
  (#:raster-core #:vector-font)
  :pathname
  "src/"
  :serial
  t
  :components
  ((:file "package") (:file "xform") (:file "ir") (:file "ops")
   (:file "render") (:file "layout") (:file "fbscene"))
  :in-order-to
  ((test-op (test-op #:scene-ir/tests))))


(defsystem #:scene-ir/tests
  :author
  "Rosette contributors"
  :license
  "Apache-2.0"
  :description
  "Tests for scene-ir (laws: round-trip, executor==oracle, direct==layered, flatten-parity, op-closure, xform/clip algebra, opacity, text-lowering, layout size-preserving, seL4 emission fidelity)."
  :depends-on
  (#:scene-ir #:raster-core)
  :pathname
  "tests/"
  :serial
  t
  :components
  ((:file "tests"))
  :perform
  (test-op (op c) (declare (ignore op c))
   (symbol-call :rosette-scene-ir/tests :run-all-tests)))
