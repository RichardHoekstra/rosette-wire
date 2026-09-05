;;;; rosette-compression-codec/src/range-coder.lisp --- byte-oriented range coder.
;;;;
;;;; A classic carryless 32-bit range coder (Subbotin-style, as used in
;;;; LZMA/PPMd family codecs): the interval [low, low+range) is narrowed by
;;;; each symbol's (cum, freq, tot); a byte is flushed whenever the top byte
;;;; of LOW and LOW+RANGE-1 agree, or forced out when RANGE underflows
;;;; below BOTTOM (so precision never collapses to zero).  Static order-0
;;;; frequency model: BYTE-FREQUENCY-TABLE builds (cum, freq, tot) for a
;;;; whole buffer; encoder and decoder share that table (transmitted
;;;; out-of-band here, exactly like the LZ77/DEFLATE token stream is
;;;; "transmitted" out-of-band in this codec's own round-trip tests).

(in-package #:rosette-compression-codec)

(defconstant +rc-top+ (ash 1 24))
(defconstant +rc-bottom+ (ash 1 16))
(defconstant +rc-mask+ #xFFFFFFFF)

(defstruct byte-freq-table
  (cum (make-array 257 :element-type 'integer) :type simple-vector)
  (total 0 :type integer))

(defun byte-frequency-table (bytes)
  "Static order-0 model: CUM[b] = # of bytes strictly less than symbol B,
CUM[256] = total count.  A symbol B occupies the half-open frequency
interval [CUM[b], CUM[b+1])."
  (let ((counts (make-array 256 :initial-element 0))
        (cum (make-array 257 :initial-element 0)))
    (loop for b across bytes do (incf (aref counts b)))
    ;; every symbol needs freq >= 1 so freq/tot decoding of a byte that
    ;; happens to be absent from BYTES never occurs in these tests; encode
    ;; only ever calls with symbols present, so zero-freq gaps are fine.
    (loop for i below 256 do (setf (aref cum (1+ i)) (+ (aref cum i) (aref counts i))))
    (make-byte-freq-table :cum cum :total (aref cum 256))))

(defstruct rc-encoder
  (low 0 :type (unsigned-byte 32))
  (range +rc-mask+ :type (unsigned-byte 32))
  (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))

(defun %rc-force-range (low)
  "Unsigned-16-bit wraparound of (-LOW) & (BOTTOM-1); Lisp's LOGAND already
treats negative integers as infinite two's-complement, so this needs no
explicit 32-bit mask.  Never returns 0 (falls back to the full BOTTOM span)."
  (let ((r (logand (- low) (1- +rc-bottom+))))
    (if (zerop r) +rc-bottom+ r)))

(defun %rc-enc-normalize (enc)
  (loop
    (cond
      ((< (logxor (rc-encoder-low enc) (logand (+ (rc-encoder-low enc) (rc-encoder-range enc)) +rc-mask+)) +rc-top+))
      ((< (rc-encoder-range enc) +rc-bottom+)
       (setf (rc-encoder-range enc) (%rc-force-range (rc-encoder-low enc))))
      (t (return)))
    (vector-push-extend (ldb (byte 8 24) (rc-encoder-low enc)) (rc-encoder-out enc))
    (setf (rc-encoder-low enc) (logand (ash (rc-encoder-low enc) 8) +rc-mask+))
    (setf (rc-encoder-range enc) (logand (ash (rc-encoder-range enc) 8) +rc-mask+))
    (when (zerop (rc-encoder-range enc)) (setf (rc-encoder-range enc) +rc-mask+))))

(defun %rc-encode-symbol (enc cum freq tot)
  (setf (rc-encoder-range enc) (truncate (rc-encoder-range enc) tot))
  (setf (rc-encoder-low enc) (logand (+ (rc-encoder-low enc) (* cum (rc-encoder-range enc))) +rc-mask+))
  (setf (rc-encoder-range enc) (* freq (rc-encoder-range enc)))
  (%rc-enc-normalize enc))

(defun range-encode-bytes (bytes table)
  "Encode BYTES under the static order-0 TABLE (from BYTE-FREQUENCY-TABLE).
Returns the compressed octet vector."
  (let ((enc (make-rc-encoder)) (cum (byte-freq-table-cum table)) (tot (byte-freq-table-total table)))
    (loop for b across bytes do
      (%rc-encode-symbol enc (aref cum b) (- (aref cum (1+ b)) (aref cum b)) tot))
    ;; flush: emit 4 more bytes of LOW so the decoder has a full 32-bit window
    (dotimes (i 4)
      (vector-push-extend (ldb (byte 8 24) (rc-encoder-low enc)) (rc-encoder-out enc))
      (setf (rc-encoder-low enc) (logand (ash (rc-encoder-low enc) 8) +rc-mask+)))
    (make-array (fill-pointer (rc-encoder-out enc)) :element-type '(unsigned-byte 8)
                :initial-contents (rc-encoder-out enc))))

(defstruct rc-decoder
  (src nil :type octets)
  (pos 0 :type fixnum)
  (low 0 :type (unsigned-byte 32))
  (range +rc-mask+ :type (unsigned-byte 32))
  (code 0 :type (unsigned-byte 32)))

(defun %rc-next-byte (dec)
  (if (< (rc-decoder-pos dec) (length (rc-decoder-src dec)))
      (prog1 (aref (rc-decoder-src dec) (rc-decoder-pos dec)) (incf (rc-decoder-pos dec)))
      0))

(defun %make-rc-decoder (src)
  (let ((dec (make-rc-decoder :src src)))
    (dotimes (i 4) (setf (rc-decoder-code dec) (logior (ash (rc-decoder-code dec) 8) (%rc-next-byte dec))))
    dec))

(defun %rc-dec-normalize (dec)
  (loop
    (cond
      ((< (logxor (rc-decoder-low dec) (logand (+ (rc-decoder-low dec) (rc-decoder-range dec)) +rc-mask+)) +rc-top+))
      ((< (rc-decoder-range dec) +rc-bottom+)
       (setf (rc-decoder-range dec) (%rc-force-range (rc-decoder-low dec))))
      (t (return)))
    (setf (rc-decoder-code dec) (logand (logior (ash (rc-decoder-code dec) 8) (%rc-next-byte dec)) +rc-mask+))
    (setf (rc-decoder-low dec) (logand (ash (rc-decoder-low dec) 8) +rc-mask+))
    (setf (rc-decoder-range dec) (logand (ash (rc-decoder-range dec) 8) +rc-mask+))
    (when (zerop (rc-decoder-range dec)) (setf (rc-decoder-range dec) +rc-mask+))))

(defun %rc-decode-freq (dec tot)
  (setf (rc-decoder-range dec) (truncate (rc-decoder-range dec) tot))
  (truncate (logand (- (rc-decoder-code dec) (rc-decoder-low dec)) +rc-mask+) (rc-decoder-range dec)))

(defun %rc-decode-update (dec cum freq)
  (setf (rc-decoder-low dec) (logand (+ (rc-decoder-low dec) (* cum (rc-decoder-range dec))) +rc-mask+))
  (setf (rc-decoder-range dec) (* freq (rc-decoder-range dec)))
  (%rc-dec-normalize dec))

(defun %find-symbol (cum target)
  "Binary search CUM (257 entries, CUM[0]=0, non-decreasing) for the symbol
whose half-open interval [CUM[b],CUM[b+1]) contains TARGET."
  (let ((lo 0) (hi 256))
    (loop while (< (1+ lo) hi) do
      (let ((mid (ash (+ lo hi) -1)))
        (if (<= (aref cum mid) target) (setf lo mid) (setf hi mid))))
    lo))

(defun range-decode-bytes (compressed table n)
  "Decode N bytes previously produced by RANGE-ENCODE-BYTES under TABLE."
  (let ((dec (%make-rc-decoder compressed))
        (cum (byte-freq-table-cum table)) (tot (byte-freq-table-total table))
        (out (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n)
      (let* ((target (min (1- tot) (%rc-decode-freq dec tot)))
             (sym (%find-symbol cum target)))
        (setf (aref out i) sym)
        (%rc-decode-update dec (aref cum sym) (- (aref cum (1+ sym)) (aref cum sym)))))
    out))
