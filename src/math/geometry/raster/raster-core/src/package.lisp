;;;; package.lisp --- primitive RGB raster substrate.

(in-package #:cl-user)

(defpackage #:rosette-raster-core
  (:nicknames #:raster-core)
  (:use #:cl)
  (:import-from #:rosette-byte-core
                #:make-byte-vector)
  (:export
   ;; Conditions
   #:raster-error
   #:raster-not-implemented
   #:raster-not-implemented-feature

   ;; Byte / RGB storage types
   #:u8
   #:u8-vector
   #:rgb-channel
   #:rgb-buffer

   ;; Frame structure + accessors
   #:rosette-frame
   #:make-frame
   #:frame-width
   #:frame-height
   #:frame-pixels
   #:frame-clear
   #:frame-coordinate-in-bounds-p

   ;; Pixel + line primitives
   #:set-pixel
   #:set-pixel-blend
   #:draw-line-thick
   #:draw-line-glow
   #:antialiased-line

   ;; PPM I/O
   #:write-ppm-header
   #:write-ppm-pixels
   #:write-ppm-file
   #:read-ppm-file))
