;;;; frame.lisp --- RGB framebuffer primitive + pixel/blend operations.
;;;;
;;;; A frame is a flat RGB-BUFFER of length
;;;; 3*W*H storing row-major RGB pixels.  No padding, no scanline
;;;; alignment, no separate alpha plane: alpha enters only via the
;;;; blending operations at draw-time, then collapses into the 8-bit
;;;; RGB destination using SRC_OVER (Porter-Duff 1984):
;;;;
;;;;   dst' = alpha * src + (1 - alpha) * dst
;;;;
;;;; with alpha in [0, 1] and src/dst in [0, 255].

(in-package #:rosette-raster-core)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defstruct (rosette-frame (:constructor %make-frame))
  "RGB framebuffer.  WIDTH, HEIGHT in pixels; PIXELS is a flat
RGB-BUFFER of length 3*WIDTH*HEIGHT laid
out row-major as (R G B  R G B  ...)."
  (width  0 :type fixnum)
  (height 0 :type fixnum)
  (pixels (the rgb-buffer (make-byte-vector 0))
          :type rgb-buffer))

(declaim (inline %fill-rgb-vector))
(defun %fill-rgb-vector (px r g b)
  (declare (type rgb-buffer px)
           (type rgb-channel r g b))
  (loop for i of-type fixnum below (length px) by 3
        do (setf (aref px i)        r
                 (aref px (+ i 1))  g
                 (aref px (+ i 2))  b))
  px)

(defun make-frame (width height &key (r 0) (g 0) (b 0))
  "Allocate an ROSETTE-FRAME of WIDTH x HEIGHT pixels, initialised to (R G B)."
  (declare (type (integer 1 #.most-positive-fixnum) width height)
           (type rgb-channel r g b))
  (let* ((w (the fixnum width))
         (h (the fixnum height))
         (n (the fixnum (* 3 w h)))
         (px (the rgb-buffer (make-byte-vector n))))
    (when (or (/= r 0) (/= g 0) (/= b 0))
      (%fill-rgb-vector px r g b))
    (%make-frame :width w :height h :pixels px)))

(declaim (inline frame-clear))
(defun frame-clear (frame &key (r 0) (g 0) (b 0))
  "Overwrite every pixel of FRAME with (R G B)."
  (declare (type rosette-frame frame)
           (type rgb-channel r g b))
  (let ((px (rosette-frame-pixels frame)))
    (declare (type rgb-buffer px))
    (%fill-rgb-vector px r g b)
    frame))

;;; The defstruct slot accessor for FRAME-WIDTH / -HEIGHT / -PIXELS is
;;; named ROSETTE-FRAME-WIDTH etc.  Re-export under the shorter names
;;; declared in package.lisp:

(declaim (inline frame-width frame-height frame-pixels))
(defun frame-width  (frame) (rosette-frame-width  frame))
(defun frame-height (frame) (rosette-frame-height frame))
(defun frame-pixels (frame) (rosette-frame-pixels frame))

(declaim (inline frame-coordinate-in-bounds-p))
(defun frame-coordinate-in-bounds-p (frame x y)
  "Return true when integer coordinate (X, Y) lies inside FRAME."
  (declare (type rosette-frame frame)
           (type fixnum x y))
  (and (<= 0 x) (< x (rosette-frame-width frame))
       (<= 0 y) (< y (rosette-frame-height frame))))

(declaim (inline %rgb-pixel-index %rgb-green-index %rgb-blue-index))

(defun %rgb-pixel-index (w x y)
  (declare (type fixnum w x y))
  (the fixnum (* 3 (+ x (the fixnum (* y w))))))

(defun %rgb-green-index (idx)
  (declare (type fixnum idx))
  (the fixnum (+ idx 1)))

(defun %rgb-blue-index (idx)
  (declare (type fixnum idx))
  (the fixnum (+ idx 2)))

(declaim (inline %blend-rgb-channel))
(defun %blend-rgb-channel (src dst alpha inv-alpha)
  (declare (type (unsigned-byte 8) src dst)
           (type single-float alpha inv-alpha))
  (the rgb-channel
       (round (+ (* alpha (float src 1f0))
                 (* inv-alpha (float dst 1f0))))))

(declaim (inline set-pixel))
(defun set-pixel (frame x y r g b)
  "Overwrite pixel (X, Y) of FRAME with (R G B).  Out-of-bounds is a
silent no-op so callers can rasterise without bounds-check noise."
  (declare (type rosette-frame frame)
           (type fixnum x y)
           (type rgb-channel r g b))
  (let ((w (rosette-frame-width frame)))
    (when (frame-coordinate-in-bounds-p frame x y)
      (let* ((px (rosette-frame-pixels frame))
             (idx (%rgb-pixel-index w x y)))
        (declare (type rgb-buffer px))
        (setf (aref px idx)        r
              (aref px (%rgb-green-index idx))  g
              (aref px (%rgb-blue-index idx))  b))))
  (values))

(declaim (inline set-pixel-blend))
(defun set-pixel-blend (frame x y r g b alpha)
  "Alpha-blend (R G B, ALPHA in [0, 1]) into pixel (X, Y) of FRAME using
SRC_OVER (Porter-Duff 1984):

   dst' = alpha * src + (1 - alpha) * dst.

Out-of-bounds is a silent no-op."
  (declare (type rosette-frame frame)
           (type fixnum x y)
           (type rgb-channel r g b)
           (type single-float alpha))
  (let ((w (rosette-frame-width frame)))
    (when (frame-coordinate-in-bounds-p frame x y)
      (let* ((px  (rosette-frame-pixels frame))
             (idx (%rgb-pixel-index w x y))
             (a   (max 0f0 (min 1f0 alpha)))
             (inv (- 1f0 a))
             (dr  (aref px idx))
             (dg  (aref px (%rgb-green-index idx)))
             (db  (aref px (%rgb-blue-index idx))))
        (declare (type rgb-buffer px)
                 (type single-float a inv))
        (setf (aref px idx)
              (%blend-rgb-channel r dr a inv))
        (setf (aref px (%rgb-green-index idx))
              (%blend-rgb-channel g dg a inv))
        (setf (aref px (%rgb-blue-index idx))
              (%blend-rgb-channel b db a inv)))))
  (values))
