;;;; package.lisp --- rosette-scene-ir package.
;;;;
;;;; ONE s-expr, THREE faces.  Node/op heads are KEYWORDS (a package-safe
;;;; canonical reader across the substrate); coords are integer device px;
;;;; colour channels 0..255; ALPHA is A1000 in 0..1000 (== fb_scene.txt).

(in-package #:cl-user)

(defpackage #:rosette-scene-ir
  (:nicknames #:scene-ir)
  (:use #:cl)
  (:import-from #:rosette-raster-core
                #:make-frame #:frame-clear #:frame-width #:frame-height
                #:frame-pixels #:set-pixel #:set-pixel-blend #:draw-line-thick)
  (:export
   ;; IR: parse / unparse / accessors
   #:parse-scene #:unparse-scene #:scene-size #:scene-root #:scene-p
   #:node-head #:*node-heads* #:*ops-vocabulary*
   ;; affine algebra (2x3)
   #:xform-identity #:xform-translate #:xform-scale #:xform-rotate
   #:xform-compose #:xform-apply #:clip-intersect
   ;; ops as closures + flat executors
   #:op->closure #:render-display-list #:flatten-scene
   ;; retained executor + compositor
   #:render-node #:render-scene #:compose-layers #:flatten-to-layers #:fb-blit
   ;; layout (Face C -> Face B) + convenience
   #:layout #:render-ui #:ui-size
   ;; text lowering
   #:text->lines
   ;; seL4 fb_scene.txt bridge + fingerprint
   #:scene->fb-scene-text #:fb-scene-text->scene #:scene-fingerprint
   #:frame-fingerprint))
