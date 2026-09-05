;;;; gcm.lisp --- AES-GCM AEAD (NIST SP 800-38D).
;;;;
;;;; Supports the common 96-bit (12-octet) IV case directly (J0 = IV ||
;;;; 0^31 || 1) and the general case (J0 = GHASH_H(IV padded || len(IV)))
;;;; for other IV lengths.  GHASH is GF(2^128) multiplication using the
;;;; bit-reflected convention SP 800-38D specifies (leftmost bit of the
;;;; block is the coefficient of x^0).

(in-package #:rosette-symmetric-crypto)

(define-condition gcm-auth-error (error)
  ((message :initform "AES-GCM authentication tag mismatch: ciphertext or AAD was tampered with"
            :reader gcm-auth-error-message))
  (:report (lambda (c s) (write-string (gcm-auth-error-message c) s))))

(defconstant +gcm-r+ #xe1000000000000000000000000000000
  "The GCM reduction constant 11100001 || 0^120, MSB-first.")

(defun gf128-mul (x y)
  "Multiply two 128-bit integers X, Y in GF(2^128) per SP 800-38D's
bit-reflected convention (bit 127 of the integer = leftmost bit = x^0)."
  (let ((z 0) (v y))
    (dotimes (i 128 z)
      (when (logbitp (- 127 i) x) (setf z (logxor z v)))
      (setf v (if (logbitp 0 v)
                  (logxor (ash v -1) +gcm-r+)
                  (ash v -1))))))

(defun ghash (h data)
  "GHASH_H(DATA): DATA is an octet vector whose length is a multiple
of 16.  H is the 128-bit hash subkey as an integer.  Returns a
128-bit integer."
  (let ((y 0))
    (loop for off from 0 below (length data) by 16
          do (setf y (gf128-mul (logxor y (block->int (subseq data off (+ off 16)))) h)))
    y))

(defun pad16 (bytes)
  "Zero-pad BYTES up to the next multiple of 16 octets (no-op if already so)."
  (let* ((n (length bytes))
         (r (mod n 16)))
    (if (zerop r) (coerce-octets bytes)
        (let ((out (make-byte-vector (+ n (- 16 r)))))
          (replace out (coerce-octets bytes))
          out))))

(defun u64be (n)
  (let ((out (make-byte-vector 8)))
    (dotimes (i 8 out) (setf (aref out (- 7 i)) (ldb (byte 8 (* 8 i)) n)))))

(defun incr32 (block)
  "SP 800-38D incr32: increment only the rightmost 32 bits of BLOCK,
modulo 2^32, leaving the top 96 bits unchanged."
  (let* ((out (copy-seq block))
         (low (logior (ash (aref block 12) 24) (ash (aref block 13) 16)
                       (ash (aref block 14) 8) (aref block 15)))
         (new-low (logand #xffffffff (1+ low))))
    (setf (aref out 12) (ldb (byte 8 24) new-low)
          (aref out 13) (ldb (byte 8 16) new-low)
          (aref out 14) (ldb (byte 8 8) new-low)
          (aref out 15) (ldb (byte 8 0) new-low))
    out))

(defun gctr (w nr icb data)
  "GCM's internal CTR-mode encrypt/decrypt: keystream blocks are
AES-encrypt(ICB), AES-encrypt(incr32(ICB)), ... XORed with DATA."
  (let* ((data (coerce-octets data))
         (n (length data))
         (out (make-byte-vector n))
         (ctr icb))
    (loop for off from 0 below n by 16
          do (let* ((ks (aes-encrypt-block w nr ctr))
                     (chunk-len (min 16 (- n off))))
                (dotimes (i chunk-len)
                  (setf (aref out (+ off i)) (logxor (aref data (+ off i)) (aref ks i))))
                (setf ctr (incr32 ctr))))
    out))

(defun compute-j0 (h iv)
  (let ((iv (coerce-octets iv)))
    (if (= (length iv) 12)
        (let ((j0 (make-byte-vector 16)))
          (replace j0 iv)
          (setf (aref j0 15) 1)
          j0)
        (int->block
         (ghash h (concatenate '(vector octet) (pad16 iv)
                                (make-byte-vector 8)
                                (u64be (* 8 (length iv)))))))))

(defun gcm-tag (h w nr j0 aad ciphertext tag-length)
  (let* ((aad (coerce-octets aad))
         (s-input (concatenate '(vector octet)
                                (pad16 aad) (pad16 ciphertext)
                                (u64be (* 8 (length aad))) (u64be (* 8 (length ciphertext)))))
         (s (ghash h s-input))
         (full-tag (gctr w nr j0 (int->block s))))
    (subseq full-tag 0 tag-length)))

(defun aes-gcm-encrypt (key iv plaintext aad &key (tag-length 16))
  "AES-GCM encrypt.  Returns (values ciphertext tag), both octet
vectors; TAG is TAG-LENGTH octets (default 16 = 128-bit tag)."
  (multiple-value-bind (w nr) (aes-key-schedule key)
    (let* ((h (block->int (aes-encrypt-block w nr (make-byte-vector 16))))
           (j0 (compute-j0 h iv))
           (ciphertext (gctr w nr (incr32 j0) plaintext))
           (tag (gcm-tag h w nr j0 aad ciphertext tag-length)))
      (values ciphertext tag))))

(defun aes-gcm-decrypt (key iv ciphertext aad tag &key (tag-length 16))
  "AES-GCM decrypt + verify.  Signals GCM-AUTH-ERROR if TAG does not
match; otherwise returns the plaintext octet vector."
  (multiple-value-bind (w nr) (aes-key-schedule key)
    (let* ((h (block->int (aes-encrypt-block w nr (make-byte-vector 16))))
           (j0 (compute-j0 h iv))
           (expected-tag (gcm-tag h w nr j0 aad ciphertext tag-length)))
      (unless (constant-time-equal expected-tag (coerce-octets tag))
        (error 'gcm-auth-error))
      (gctr w nr (incr32 j0) ciphertext))))
