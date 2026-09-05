;;;; hkdf.lisp --- HKDF-SHA256 (RFC 5869): extract-and-expand key derivation.

(in-package #:rosette-symmetric-crypto)

(defconstant +sha256-hash-len+ 32)

(defun hkdf-extract (salt ikm)
  "RFC 5869 step 1: PRK = HMAC-Hash(salt, IKM).  If SALT is empty/NIL,
a HashLen-length zero-filled key is used, per the RFC."
  (let ((salt (if (or (null salt) (zerop (length salt)))
                   (make-byte-vector +sha256-hash-len+)
                   (coerce-octets salt))))
    (hmac-sha256 salt (coerce-octets ikm))))

(defun hkdf-expand (prk info length)
  "RFC 5869 step 2: OKM = T(1) || T(2) || ... truncated to LENGTH octets,
where T(0) = empty, T(i) = HMAC-Hash(PRK, T(i-1) || info || i)."
  (when (> length (* 255 +sha256-hash-len+))
    (error "rosette-symmetric-crypto: HKDF requested length ~D exceeds 255*HashLen" length))
  (let* ((info (coerce-octets info))
         (n (ceiling length +sha256-hash-len+))
         (okm (make-byte-vector (* n +sha256-hash-len+)))
         (tprev (make-byte-vector 0)))
    (dotimes (i n)
      (let* ((counter (make-byte-vector 1 :initial-element (1+ i)))
             (input (concatenate '(vector octet) tprev info counter))
             (tcur (hmac-sha256 prk input)))
        (replace okm tcur :start1 (* i +sha256-hash-len+))
        (setf tprev tcur)))
    (subseq okm 0 length)))

(defun hkdf (salt ikm info length)
  "Full HKDF: extract then expand.  Returns LENGTH octets of key material."
  (hkdf-expand (hkdf-extract salt ikm) info length))
