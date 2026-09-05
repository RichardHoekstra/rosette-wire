;;;; types.lisp --- named byte and RGB framebuffer type vocabulary.

(in-package #:rosette-raster-core)

(deftype u8 ()
  "An unsigned 8-bit byte."
  '(unsigned-byte 8))

(deftype u8-vector (&optional (length '*))
  "A simple vector specialized to unsigned 8-bit bytes."
  `(simple-array u8 (,length)))

(deftype rgb-channel ()
  "A single 8-bit RGB channel value."
  'u8)

(deftype rgb-buffer (&optional (length '*))
  "A flat RGB byte buffer laid out as R G B triples."
  `(u8-vector ,length))
