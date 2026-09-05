;;;; rosette-compression-codec/src/deflate.lisp --- DEFLATE (RFC 1951) encoder.
;;;;
;;;; Produces raw DEFLATE bytes decodable by rosette-inflate:inflate.  Two
;;;; block strategies:
;;;;
;;;;  - DEFLATE-COMPRESS-STORED: BTYPE=00 stored block (no compression,
;;;;    always valid; the fallback/baseline).
;;;;  - DEFLATE-COMPRESS-FIXED: LZ77-tokenize then BTYPE=01 fixed-Huffman
;;;;    block (RFC 1951 sec 3.2.6's predefined literal/length and distance
;;;;    code tables -- no header to transmit, but the LZ77 back-references
;;;;    still buy real compression on repetitive input).
;;;;
;;;; Both emit a single final block (BFINAL=1).

(in-package #:rosette-compression-codec)

(defparameter *len-base*
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter *len-extra*
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter *dist-base*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter *dist-extra*
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(defun %base-index (base-table value)
  "Highest index I such that BASE-TABLE[I] <= VALUE."
  (loop for i from (1- (length base-table)) downto 0
        when (<= (aref base-table i) value) return i
        finally (error "value ~D below smallest base ~D" value (aref base-table 0))))

(defun %fixed-litlen-code (sym)
  "RFC 1951 3.2.6 fixed literal/length Huffman table.  Returns (values code len)."
  (cond
    ((<= 0 sym 143) (values (+ #x30 sym) 8))
    ((<= 144 sym 255) (values (+ #x190 (- sym 144)) 9))
    ((<= 256 sym 279) (values (- sym 256) 7))
    ((<= 280 sym 287) (values (+ #xc0 (- sym 280)) 8))
    (t (error "bad literal/length symbol ~D" sym))))

(defun %fixed-dist-code (sym)
  "RFC 1951 3.2.6 fixed distance table: all 30 codes are 5 bits, code = symbol."
  (values sym 5))

(defun %emit-length-symbol (w len)
  (let* ((li (%base-index *len-base* len))
         (sym (+ 257 li))
         (extra (- len (aref *len-base* li))))
    (multiple-value-bind (code clen) (%fixed-litlen-code sym)
      (bw-put-huffman-code w code clen))
    (when (plusp (aref *len-extra* li)) (bw-put-bits w extra (aref *len-extra* li)))))

(defun %emit-distance-symbol (w dist)
  (let* ((di (%base-index *dist-base* dist))
         (extra (- dist (aref *dist-base* di))))
    (multiple-value-bind (code dlen) (%fixed-dist-code di)
      (bw-put-huffman-code w code dlen))
    (when (plusp (aref *dist-extra* di)) (bw-put-bits w extra (aref *dist-extra* di)))))

(defun deflate-compress-stored (bytes)
  "A single BTYPE=00 stored final block.  Always valid; zero compression."
  (let ((w (make-bit-writer)) (n (length bytes)))
    (bw-put-bits w 1 1)                ; BFINAL=1
    (bw-put-bits w 0 2)                ; BTYPE=00
    ;; align to byte boundary, then LEN/NLEN (16-bit LE) + raw bytes
    (let ((header (bw-finish w)))
      (let ((out (make-array (+ (length header) 4 n) :element-type '(unsigned-byte 8))))
        (replace out header)
        (let ((p (length header)))
          (setf (aref out p) (ldb (byte 8 0) n) (aref out (+ p 1)) (ldb (byte 8 8) n))
          (setf (aref out (+ p 2)) (logxor (aref out p) #xff)
                (aref out (+ p 3)) (logxor (aref out (+ p 1)) #xff))
          (replace out bytes :start1 (+ p 4)))
        out))))

(defun deflate-compress-fixed (bytes)
  "LZ77-tokenize BYTES and emit a single BTYPE=01 fixed-Huffman final block."
  (let ((w (make-bit-writer)) (tokens (lz77-compress bytes)))
    (bw-put-bits w 1 1)                ; BFINAL=1
    (bw-put-bits w 1 2)                ; BTYPE=01 (fixed Huffman)
    (dolist (tk tokens)
      (if (eq (lz-token-kind tk) :literal)
          (multiple-value-bind (code len) (%fixed-litlen-code (lz-token-byte tk))
            (bw-put-huffman-code w code len))
          (progn (%emit-length-symbol w (lz-token-length tk))
                 (%emit-distance-symbol w (lz-token-distance tk)))))
    ;; end-of-block symbol 256
    (multiple-value-bind (code len) (%fixed-litlen-code 256) (bw-put-huffman-code w code len))
    (bw-finish w)))

;;; ---- zlib (RFC 1950) wrapper: 2-byte header + DEFLATE body + adler32 ----

(defun adler32 (bytes)
  (let ((a 1) (b 0))
    (loop for byte across bytes do
      (setf a (mod (+ a byte) 65521))
      (setf b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

(defun zlib-compress (bytes &key (strategy :fixed))
  "zlib stream (RFC 1950): CMF/FLG header + DEFLATE body + adler32 trailer,
decodable by rosette-inflate:zlib-decompress."
  (let* ((body (ecase strategy
                 (:stored (deflate-compress-stored bytes))
                 (:fixed (deflate-compress-fixed bytes))))
         (cmf #x78)                     ; CM=8 (deflate), CINFO=7 (32K window)
         (flg (ash 1 6)))               ; FLEVEL=01 (fast), no FDICT
    ;; bump FLG (bits 0-4 are free / no other meaning here) until the header
    ;; check (CMF*256+FLG) mod 31 = 0 holds, per RFC 1950 sec 2.2.
    (loop while (/= (mod (+ (* cmf 256) flg) 31) 0) do (incf flg))
    (let* ((adler (adler32 bytes))
           (out (make-array (+ 2 (length body) 4) :element-type '(unsigned-byte 8))))
      (setf (aref out 0) cmf (aref out 1) flg)
      (replace out body :start1 2)
      (let ((p (+ 2 (length body))))
        (setf (aref out p) (ldb (byte 8 24) adler)
              (aref out (+ p 1)) (ldb (byte 8 16) adler)
              (aref out (+ p 2)) (ldb (byte 8 8) adler)
              (aref out (+ p 3)) (ldb (byte 8 0) adler)))
      out)))
