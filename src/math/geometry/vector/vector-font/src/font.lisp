;;;; font.lisp --- vector-font geometry: char/string -> polylines.
;;;;
;;;; ZERO dependencies.  This atom turns text into pure 2-D polyline
;;;; geometry in a normalized coordinate frame; it does NOT rasterize.
;;;; Consumers (rosette-figures' AA stroker, an SVG <path> emitter, a GPU
;;;; line-list, a CNC/pen-plotter G-code backend) all read the same
;;;; strokes and render in their own gauge.  Backend = gauge; the
;;;; geometry here is the gauge-invariant.

(in-package #:rosette-vector-font)

(declaim (optimize (speed 2) (safety 1) (debug 1)))

;;; ---- grid metrics (Hershey units; Y increases DOWNWARD from midline) ----

(defconstant +baseline-y+ 9
  "Hershey grid Y of the cap baseline.  Glyph vertices with Y below this
sit below the baseline (descenders); above it rise toward the cap line.")

(defparameter *missing-advance* 16
  "Advance width (grid units) used for a code with no glyph in the table.")

(defun glyph-present-p (char)
  "True if CHAR has an explicit glyph (printable ASCII 32..126)."
  (and (gethash (char-code char) *glyph-table*) t))

(defun glyph-advance (char)
  "Advance width of CHAR in Hershey grid units."
  (let ((g (gethash (char-code char) *glyph-table*)))
    (if g (first g) *missing-advance*)))

(defun glyph-strokes (char)
  "List of pen-down strokes for CHAR; each stroke a list of (X . Y) integer
vertices (X from 0 at the left bearing, Y in Hershey down-positive units).
NIL for a blank glyph (e.g. space) or a missing code."
  (rest (gethash (char-code char) *glyph-table*)))

(defparameter +cap-height+
  ;; measured from the capital 'H' rather than guessed: baseline minus the
  ;; topmost vertex Y.  rowmans 'H' is a clean two-post + crossbar cap.
  (let ((top (loop for s in (glyph-strokes #\H)
                   minimize (loop for (x . y) in s minimize y))))
    (- +baseline-y+ top))
  "Cap height in grid units (baseline to top of a capital).")

(defun scale-for-cap-height (pixels)
  "Scale factor (px per grid unit) that renders capitals PIXELS tall."
  (/ (float pixels 1d0) +cap-height+))

;;; ---- string-level geometry ----

(defun string-advance (string &key (tracking 1d0))
  "Total pen advance of STRING in grid units, with TRACKING multiplying
each glyph's advance (1.0 = natural spacing)."
  (let ((w 0d0))
    (loop for ch across string do (incf w (* tracking (glyph-advance ch))))
    w))

(defun string-width (string &key (size 1d0) (tracking 1d0))
  "Rendered pen-advance width of STRING in the same units as SIZE
(SIZE = px per grid unit)."
  (* size (string-advance string :tracking tracking)))

(defun string-polylines (string
                         &key (size 1d0) (x 0d0) (y 0d0)
                              (tracking 1d0) (y-up t) (rotate 0d0))
  "Lay STRING out as a list of polylines (each a list of (X . Y) double
conses) ready to stroke.

  SIZE     px per grid unit (see SCALE-FOR-CAP-HEIGHT).
  X, Y     pen origin = left end of the baseline, in output pixels.
  TRACKING multiplies inter-glyph advance.
  Y-UP     T  -> output Y increases upward (math/Cartesian);
           NIL-> output Y increases downward (screen rasters).
  ROTATE   radians CCW about (X, Y).

Pure geometry: no rasterization, no clipping, no frame."
  (let* ((s (float size 1d0))
         (ox (float x 1d0)) (oy (float y 1d0))
         (cs (cos (float rotate 1d0))) (sn (sin (float rotate 1d0)))
         (pen 0d0)
         (out '()))
    (flet ((place (vx vy)
             ;; local frame: baseline at 0, x rightward, y per Y-UP
             (let* ((lx (* s (+ pen vx)))
                    (ly (* s (if y-up (- +baseline-y+ vy) (- vy +baseline-y+)))))
               (cons (+ ox (- (* lx cs) (* ly sn)))
                     (+ oy (+ (* lx sn) (* ly cs)))))))
      (loop for ch across string
            do (dolist (stroke (glyph-strokes ch))
                 (push (mapcar (lambda (pt) (place (car pt) (cdr pt))) stroke) out))
               (incf pen (* tracking (glyph-advance ch)))))
    (nreverse out)))

(defun string-bounds (string &key (size 1d0) (tracking 1d0) (y-up t))
  "Axis-aligned bounds of STRING laid out at origin (no rotation).
Returns (values xmin ymin xmax ymax) in output units."
  (let ((xmin most-positive-double-float) (ymin most-positive-double-float)
        (xmax most-negative-double-float) (ymax most-negative-double-float)
        (any nil))
    (dolist (stroke (string-polylines string :size size :tracking tracking
                                             :y-up y-up))
      (dolist (pt stroke)
        (setf any t)
        (setf xmin (min xmin (car pt)) xmax (max xmax (car pt))
              ymin (min ymin (cdr pt)) ymax (max ymax (cdr pt)))))
    (if any
        (values xmin ymin xmax ymax)
        (values 0d0 0d0 (string-width string :size size :tracking tracking) 0d0))))
