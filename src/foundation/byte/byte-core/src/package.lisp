;;;; package.lisp --- package definition for rosette-byte-core.

(in-package #:cl-user)

(defpackage #:rosette-byte-core
  (:use #:cl)
  (:export
   #:u8
   #:byte-vector
   #:make-byte-vector
   #:coerce-byte-vector
   #:base64-encode-bytes
   #:base64-decode-bytes
   #:byte-windows
   #:bits-to-bytes
   #:bytes-to-bits
   #:low-nibble
   #:high-nibble
   #:pack-nibbles))
