;;;; ops.lisp --- Face A: paint ops executed on rosette-raster-core.
;;;;
;;;; Every op is a PURE closure (frame)->frame.  clear/line are byte-for-byte
;;;; fb_scene.txt; fill-rect/text/blit LOWER into the same raster-core calls.
;;;; A1000 alpha (0..1000) -> single-float alpha == the C brick's a*1000
;;;; convention.  rosette-raster-core:set-pixel-blend/draw-line-thick are the SOLE
;;;; rasteriser -- no private blend is ever re-derived.

(in-package #:rosette-scene-ir)

(declaim (inline a1000->float))
(defun a1000->float (a1000)
  "Convert an A1000 integer alpha (0..1000) to single-float in [0,1].
Rational division then single-round == the substrate's (float a 1f0)
semantics (byte-exact against the seL4 oracles)."
  (float (/ a1000 1000) 1f0))

;;; ---- frame utilities ----

(defun copy-frame (frame)
  "Deep copy of an rosette-frame (fresh pixel buffer)."
  (let* ((w (frame-width frame)) (h (frame-height frame))
         (dst (make-frame w h))
         (sp (frame-pixels frame)) (dp (frame-pixels dst)))
    (replace dp sp)
    dst))

(defun fb-blit (dst src ox oy a1000)
  "SRC_OVER the SRC frame onto DST at (OX OY) with A1000 opacity, clipped to
DST bounds.  Byte-exact Lisp mirror of rosette_fb.c fb_blit: repeated
rosette-raster-core:set-pixel-blend (never a private blend)."
  (let ((sw (frame-width src)) (sh (frame-height src))
        (spx (frame-pixels src)) (a (a1000->float a1000)))
    (dotimes (y sh dst)
      (dotimes (x sw)
        (let ((i (* 3 (+ x (* y sw)))))
          (set-pixel-blend dst (+ ox x) (+ oy y)
                           (aref spx i) (aref spx (+ i 1)) (aref spx (+ i 2))
                           a))))))

;;; ---- text lowering (Face A superset -> line ops) ----

(defun text->lines (op)
  "Lower a (:text X Y SCALE1000 R G B THICK STR) op to a deterministic list
of (:line ...) ops via rosette-vector-font strokes, integer endpoints, screen
Y-down.  No floats survive onto the emit path (vertices rounded here)."
  (destructuring-bind (x y scale1000 r g b thick str) (cdr op)
    (let* ((size (/ (float scale1000 1d0) 1000d0))
           (polys (rosette-vector-font:string-polylines
                   str :size size :x (float x 1d0) :y (float y 1d0)
                       :y-up nil))
           (out '()))
      (dolist (poly polys)
        (loop for (p q) on poly while q
              do (let ((x0 (round (car p))) (y0 (round (cdr p)))
                       (x1 (round (car q))) (y1 (round (cdr q))))
                   (push (list :line x0 y0 x1 y1 r g b thick 1000) out))))
      (nreverse out))))

;;; ---- the raw op executor (absolute coords, no clip stack) ----

(defun %draw-op-raw (frame op surfaces)
  "Execute one absolute-coord op onto FRAME (no clip).  SURFACES is an alist
(NAME . frame) for (:blit ...)."
  (ecase (car op)
    (:clear
     (destructuring-bind (r g b) (cdr op) (frame-clear frame :r r :g g :b b)))
    (:line
     (destructuring-bind (x0 y0 x1 y1 r g b thick a1000) (cdr op)
       (draw-line-thick frame x0 y0 x1 y1 :r r :g g :b b
                        :alpha (a1000->float a1000) :thickness thick)))
    (:fill-rect
     (destructuring-bind (x y w h r g b a1000) (cdr op)
       (let ((a (a1000->float a1000)))
         (dotimes (row h)
           ;; one opaque/blended scanline per row, thickness 1
           (draw-line-thick frame x (+ y row) (+ x w -1) (+ y row)
                            :r r :g g :b b :alpha a :thickness 1)))))
    (:text
     (dolist (ln (text->lines op)) (%draw-op-raw frame ln surfaces)))
    (:blit
     (destructuring-bind (name ox oy a1000) (cdr op)
       (let ((src (cdr (assoc name surfaces))))
         (unless src (error "rosette-scene-ir: (:blit ~S ...) no such surface" name))
         (fb-blit frame src ox oy a1000)))))
  frame)

(defun %draw-op (frame op &key clip surfaces)
  "Execute OP onto FRAME, honouring an optional CLIP rect (X Y W H).
Clipping is byte-exact: draw onto a copy then copy back only the clip-rect
pixels, so blended ops still see the correct destination and a draw fully
outside the clip writes zero pixels."
  (if (null clip)
      (%draw-op-raw frame op surfaces)
      (destructuring-bind (cx cy cw ch) clip
        (if (or (<= cw 0) (<= ch 0))
            frame                       ; empty clip: no-op
            (let* ((scratch (copy-frame frame))
                   (w (frame-width frame)) (h (frame-height frame))
                   (fp (frame-pixels frame)) (spx (frame-pixels scratch)))
              (%draw-op-raw scratch op surfaces)
              (loop for y from (max 0 cy) below (min h (+ cy ch)) do
                (loop for x from (max 0 cx) below (min w (+ cx cw)) do
                  (let ((idx (* 3 (+ x (* y w)))))
                    (setf (aref fp idx) (aref spx idx)
                          (aref fp (+ idx 1)) (aref spx (+ idx 1))
                          (aref fp (+ idx 2)) (aref spx (+ idx 2))))))
              frame)))))

(defun op->closure (op &key surfaces)
  "Reify a paint OP as a pure closure (frame)->frame: a scene is a fold of
these.  OP-CLOSURE-FAITHFUL: (funcall (op->closure op) f) draws exactly what
render-display-list of the singleton (op) draws."
  (lambda (frame) (%draw-op-raw frame op surfaces)))
