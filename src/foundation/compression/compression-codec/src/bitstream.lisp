;;;; rosette-compression-codec/src/bitstream.lisp --- LSB-first bit I/O.
;;;;
;;;; Mirrors rosette-inflate's bit-reader convention exactly (see its %getbit):
;;;; bits are consumed from each octet LSB-first.  Multi-bit FIELDS (BFINAL,
;;;; BTYPE, HLIT, extra bits, stored-block LEN, ...) are written LSB-first
;;;; (RFC 1951 sec 3.1.1).  Huffman CODES are packed MSB-first within their
;;;; own bit pattern (RFC 1951 sec 3.2.2) -- i.e. the high-order bit of the
;;;; code is the first bit pushed into the (LSB-first) octet stream.

(in-package #:rosette-compression-codec)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

;;; ---- writer ----

(defstruct (bit-writer (:constructor make-bit-writer ()))
  (bitbuf 0 :type (unsigned-byte 8))
  (bitcnt 0 :type fixnum)
  (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))

(declaim (inline %bw-put-bit))
(defun %bw-put-bit (w bit)
  (setf (bit-writer-bitbuf w) (logior (bit-writer-bitbuf w) (ash (logand bit 1) (bit-writer-bitcnt w))))
  (incf (bit-writer-bitcnt w))
  (when (= (bit-writer-bitcnt w) 8)
    (vector-push-extend (bit-writer-bitbuf w) (bit-writer-out w))
    (setf (bit-writer-bitbuf w) 0 (bit-writer-bitcnt w) 0)))

(defun bw-put-bits (w val n)
  "Write the N low bits of VAL, least-significant bit first."
  (dotimes (i n) (%bw-put-bit w (ldb (byte 1 i) val))))

(defun bw-put-huffman-code (w code len)
  "Write a LEN-bit Huffman CODE, most-significant bit first (RFC 1951 3.2.2)."
  (loop for i from (1- len) downto 0 do (%bw-put-bit w (if (logbitp i code) 1 0))))

(defun bw-finish (w)
  "Pad to a byte boundary with zero bits and return the accumulated octets."
  (when (plusp (bit-writer-bitcnt w))
    (vector-push-extend (bit-writer-bitbuf w) (bit-writer-out w))
    (setf (bit-writer-bitbuf w) 0 (bit-writer-bitcnt w) 0))
  (make-array (fill-pointer (bit-writer-out w)) :element-type '(unsigned-byte 8)
              :initial-contents (bit-writer-out w)))

;;; ---- reader (generic; rosette-inflate has its own for actual DEFLATE decode --
;;;      this one backs the self-contained generic-Huffman round-trip test) ----

(defstruct (bit-reader (:constructor make-bit-reader (src)))
  (src nil :type octets)
  (pos 0 :type fixnum)
  (bitbuf 0 :type (unsigned-byte 8))
  (bitcnt 0 :type fixnum))

(declaim (inline %br-get-bit))
(defun %br-get-bit (r)
  (when (zerop (bit-reader-bitcnt r))
    (setf (bit-reader-bitbuf r) (aref (bit-reader-src r) (bit-reader-pos r)))
    (incf (bit-reader-pos r))
    (setf (bit-reader-bitcnt r) 8))
  (let ((bit (logand (bit-reader-bitbuf r) 1)))
    (setf (bit-reader-bitbuf r) (ash (bit-reader-bitbuf r) -1))
    (decf (bit-reader-bitcnt r))
    bit))

(defun br-get-bits (r n)
  (let ((val 0))
    (dotimes (i n) (setf val (logior val (ash (%br-get-bit r) i))))
    val))

(defun br-get-huffman-code (r)
  "Consume exactly one more bit, MSB-first accumulation convention: returns
the single bit just read (0 or 1). The caller (huffman.lisp) accumulates
CODE = (code<<1)|bit and LEN = LEN+1 across calls, checking a (length . code)
table after each bit, mirroring rosette-inflate's decode-sym walk."
  (%br-get-bit r))
