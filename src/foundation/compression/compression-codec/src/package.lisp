;;;; rosette-compression-codec/src/package.lisp --- Public API.

(defpackage #:rosette-compression-codec
  (:use #:cl)
  (:export
   ;; bitstream (LSB-first, matching rosette-inflate's bit reader convention)
   #:make-bit-writer
   #:bw-put-bits
   #:bw-put-huffman-code
   #:bw-finish
   #:make-bit-reader
   #:br-get-bits
   #:br-get-huffman-code

   ;; canonical Huffman
   #:huffman-code-lengths
   #:huffman-canonical-codes
   #:huffman-kraft-sum
   #:huffman-prefix-free-p
   #:huffman-compress
   #:huffman-decompress

   ;; LZ77
   #:lz77-token-literal
   #:lz77-token-match
   #:lz77-token-length
   #:lz77-token-distance
   #:lz77-compress
   #:lz77-decompress

   ;; DEFLATE (RFC 1951) encoder, decodable by rosette-inflate:inflate
   #:deflate-compress-stored
   #:deflate-compress-fixed
   #:zlib-compress
   #:adler32

   ;; range / arithmetic coder
   #:byte-frequency-table
   #:range-encode-bytes
   #:range-decode-bytes))
