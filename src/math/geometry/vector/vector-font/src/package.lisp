;;;; package.lisp --- rosette-vector-font package definition.

(in-package #:cl-user)

(defpackage #:rosette-vector-font
  (:nicknames #:vfont)
  (:use #:cl)
  (:export
   ;; metrics (Hershey grid units)
   #:+baseline-y+
   #:+cap-height+
   #:*missing-advance*
   #:*glyph-table*
   ;; per-glyph access
   #:glyph-present-p
   #:glyph-advance
   #:glyph-strokes
   ;; string-level geometry
   #:string-advance
   #:string-width
   #:string-polylines
   #:string-bounds
   ;; convenience
   #:scale-for-cap-height))
