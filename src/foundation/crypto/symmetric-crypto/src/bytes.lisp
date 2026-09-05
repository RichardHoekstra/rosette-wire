;;;; bytes.lisp --- byte-vector and hex utilities for rosette-symmetric-crypto.
;;;;
;;;; Self-contained (no dependency on rosette-crypto, which is a Tier-1
;;;; sibling): every primitive in this library operates on
;;;; (simple-array (unsigned-byte 8) (*)) vectors.

(in-package #:rosette-symmetric-crypto)

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector (&optional (n '*)) `(simple-array octet (,n)))

(declaim (inline make-byte-vector))
(defun make-byte-vector (length &key (initial-element 0))
  "Allocate a fresh (unsigned-byte 8) simple-array of LENGTH octets."
  (make-array length :element-type 'octet :initial-element initial-element))

(defun coerce-octets (sequence)
  "Return SEQUENCE as a simple (unsigned-byte 8) vector (copying if needed)."
  (if (typep sequence 'octet-vector)
      sequence
      (let ((out (make-byte-vector (length sequence))))
        (map-into out (lambda (x) (logand x #xff)) sequence)
        out)))

(defun bytes->hex (bytes)
  "Lowercase hex encoding of BYTES (a sequence of octets)."
  (let* ((b (coerce-octets bytes))
         (n (length b))
         (s (make-string (* 2 n)))
         (digits "0123456789abcdef"))
    (dotimes (i n s)
      (let ((byte (aref b i)))
        (setf (char s (* 2 i)) (char digits (ldb (byte 4 4) byte))
              (char s (1+ (* 2 i))) (char digits (ldb (byte 4 0) byte)))))))

(defun hex-digit-value (ch)
  (let ((code (char-code (char-downcase ch))))
    (cond ((<= (char-code #\0) code (char-code #\9)) (- code (char-code #\0)))
          ((<= (char-code #\a) code (char-code #\f)) (+ 10 (- code (char-code #\a))))
          (t (error "rosette-symmetric-crypto: invalid hex digit ~S" ch)))))

(defun hex->bytes (hex)
  "Decode a hex string HEX into an octet vector.  HEX must have even length."
  (let ((n (length hex)))
    (when (oddp n)
      (error "rosette-symmetric-crypto: hex string has odd length ~D" n))
    (let ((out (make-byte-vector (floor n 2))))
      (dotimes (i (length out) out)
        (setf (aref out i)
              (+ (* 16 (hex-digit-value (char hex (* 2 i))))
                 (hex-digit-value (char hex (1+ (* 2 i))))))))))

(defun string->bytes (string)
  "Encode STRING as octets via its character codes (Latin-1 / ASCII)."
  (let ((out (make-byte-vector (length string))))
    (dotimes (i (length string) out)
      (let ((code (char-code (char string i))))
        (when (> code 255)
          (error "rosette-symmetric-crypto: character ~S not representable as a single octet" (char string i)))
        (setf (aref out i) code)))))

(defun xor-bytes (a b)
  "Elementwise XOR of two equal-length octet sequences A and B."
  (let ((a (coerce-octets a)) (b (coerce-octets b)))
    (assert (= (length a) (length b)))
    (let ((out (make-byte-vector (length a))))
      (dotimes (i (length a) out)
        (setf (aref out i) (logxor (aref a i) (aref b i)))))))

(defun constant-time-equal (a b)
  "Compare two octet sequences without early-exit timing leakage.
Shared by AES-GCM and ChaCha20-Poly1305 tag verification."
  (and (= (length a) (length b))
       (zerop (reduce #'logior (map 'vector #'logxor a b)))))
