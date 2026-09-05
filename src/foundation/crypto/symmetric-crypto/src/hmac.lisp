;;;; hmac.lisp --- HMAC-SHA256 (RFC 2104 / RFC 4231).

(in-package #:rosette-symmetric-crypto)

(defconstant +sha256-block-size+ 64)

(defun hmac-sha256 (key message)
  "HMAC-SHA256(KEY, MESSAGE) per RFC 2104, using the SHA-256 in this
library.  KEY and MESSAGE are octet sequences (or strings, coerced).
Returns a 32-octet vector."
  (let* ((key (coerce-octets key))
         (key (if (> (length key) +sha256-block-size+) (sha256 key) key))
         (key-block (make-byte-vector +sha256-block-size+)))
    (replace key-block key)
    (let ((ipad (make-byte-vector +sha256-block-size+))
          (opad (make-byte-vector +sha256-block-size+)))
      (dotimes (i +sha256-block-size+)
        (setf (aref ipad i) (logxor (aref key-block i) #x36)
              (aref opad i) (logxor (aref key-block i) #x5c)))
      (let* ((msg (coerce-octets message))
             (inner (sha256 (concatenate '(vector octet) ipad msg))))
        (sha256 (concatenate '(vector octet) opad inner))))))
