;;;; lines.lisp --- thick line rasterizer + multi-pass glow.
;;;;
;;;; The line is rasterized via Bresenham-style incremental DDA along
;;;; the longer screen axis (Bresenham 1965, "Algorithm for computer
;;;; control of a digital plotter", IBM Systems Journal, 4(1)).  We do
;;;; not use the integer-only DDA: incremental float updates with one
;;;; ROUND per step are entirely sufficient at the resolutions the
;;;; library targets (<= 4096 px per axis) and let us emit lines of
;;;; arbitrary thickness with a single brush loop.
;;;;
;;;; "Glow" is a multi-pass approximation of a soft round line:
;;;;   - outer pass:  thickness = thick + 4, alpha = 0.10 * alpha_base
;;;;   - middle pass: thickness = thick + 2, alpha = 0.40 * alpha_base
;;;;   - core pass:   thickness = thick    , alpha = 1.00 * alpha_base
;;;; SRC_OVER blending makes the cumulative color taper away from the
;;;; centre line.  This is intentionally cheap; for true gaussian glow
;;;; use a separate convolution pass after rasterizing.
;;;;
;;;; ANTIALIASED-LINE uses the same alpha blend primitive with a
;;;; distance-to-segment coverage estimate.

(in-package #:rosette-raster-core)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defun draw-line-thick (frame x0 y0 x1 y1
                        &key (r 255) (g 255) (b 255)
                             (alpha 1.0) (thickness 1))
  "Draw an alpha-blended line of integer THICKNESS from (X0, Y0) to
(X1, Y1) into FRAME with color (R G B) and opacity ALPHA in [0, 1].

Implementation: incremental DDA along the longer screen axis; at each
step a square brush of side THICKNESS pixels is alpha-blended.  No
sub-pixel anti-aliasing — for that, use multi-pass DRAW-LINE-GLOW or
wait for v0.2's ANTIALIASED-LINE."
  (declare (type rosette-frame frame)
           (type fixnum x0 y0 x1 y1)
           (type rgb-channel r g b)
           (type real alpha)
           (type (integer 1 #.most-positive-fixnum) thickness))
  (let* ((dx     (- x1 x0))
         (dy     (- y1 y0))
         (steps  (max (abs dx) (abs dy)))
         (a      (float alpha 1f0))
         (sx     (if (zerop steps) 0f0 (/ (float dx 1f0) (float steps 1f0))))
         (sy     (if (zerop steps) 0f0 (/ (float dy 1f0) (float steps 1f0))))
         (half   (truncate thickness 2))
         (xx     (float x0 1f0))
         (yy     (float y0 1f0)))
    (declare (type single-float sx sy xx yy a)
             (type fixnum dx dy steps half))
    (loop for i of-type fixnum from 0 to steps do
      (let ((cx (round xx))
            (cy (round yy)))
        (declare (type fixnum cx cy))
        (loop for dyb of-type fixnum from (- half) to (- thickness half 1) do
          (loop for dxb of-type fixnum from (- half) to (- thickness half 1) do
            (set-pixel-blend frame (+ cx dxb) (+ cy dyb) r g b a))))
      (incf xx sx)
      (incf yy sy)))
  (values))

(defun draw-line-glow (frame x0 y0 x1 y1
                       &key (r 255) (g 255) (b 255)
                            (alpha 1.0) (thickness 1)
                            (passes 3))
  "Draw a soft glowing line.  Performs PASSES Bresenham passes from the
outside in, each pass narrower and more opaque than the last:

  pass k (k=0 outermost ... k=PASSES-1 core):
    thickness_k = thickness + 2 * (PASSES - 1 - k)
    alpha_k     = alpha * ((k + 1) / PASSES)^2

The geometric falloff makes a 3-pass glow look essentially gaussian on
8-bit displays without the cost of an FFT-blurred separate buffer."
  (declare (type rosette-frame frame)
           (type fixnum x0 y0 x1 y1)
           (type rgb-channel r g b)
           (type real alpha)
           (type (integer 1 #.most-positive-fixnum) thickness passes))
  (let ((a-base (float alpha 1f0)))
    (loop for k of-type fixnum from 0 below passes do
      (let* ((tk (+ thickness (* 2 (- passes 1 k))))
             (frac (/ (float (1+ k) 1f0) (float passes 1f0)))
             (ak (* a-base frac frac)))
        (declare (type single-float frac ak)
                 (type fixnum tk))
        (draw-line-thick frame x0 y0 x1 y1
                         :r r :g g :b b
                         :alpha ak :thickness tk))))
  (values))

(defun antialiased-line (frame x0 y0 x1 y1
                         &key (r 255) (g 255) (b 255) (alpha 1.0))
  "Wu's anti-aliased line algorithm (Wu 1991, ACM TOG 10(2)).

This implementation computes pixel coverage from distance to the line
segment centerline.  It handles all slopes and clips through
SET-PIXEL-BLEND."
  (declare (type rosette-frame frame)
           (type fixnum x0 y0 x1 y1)
           (type rgb-channel r g b)
           (type real alpha))
  (let* ((dx (- x1 x0))
         (dy (- y1 y0))
         (len2 (+ (* dx dx) (* dy dy)))
         (xmin (- (min x0 x1) 1))
         (xmax (+ (max x0 x1) 1))
         (ymin (- (min y0 y1) 1))
         (ymax (+ (max y0 y1) 1))
         (a (float alpha 1f0)))
    (if (zerop len2)
        (set-pixel-blend frame x0 y0 r g b a)
        (loop for y from ymin to ymax do
          (loop for x from xmin to xmax do
            (let* ((tau (/ (+ (* (- x x0) dx) (* (- y y0) dy))
                           (float len2 1f0)))
                   (tc (max 0f0 (min 1f0 tau)))
                   (px (+ x0 (* tc dx)))
                   (py (+ y0 (* tc dy)))
                   (dist (sqrt (+ (expt (- x px) 2) (expt (- y py) 2))))
                   (coverage (max 0f0 (min 1f0 (- 1.5f0 (float dist 1f0))))))
              (when (plusp coverage)
                (set-pixel-blend frame x y r g b (* a coverage))))))))
  (values))
