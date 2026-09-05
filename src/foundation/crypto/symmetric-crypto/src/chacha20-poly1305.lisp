;;;; chacha20-poly1305.lisp --- AEAD_CHACHA20_POLY1305 (RFC 8439 section 2.8).

(in-package #:rosette-symmetric-crypto)

(defun poly1305-key-gen (key nonce)
  "RFC 8439 section 2.6: derive a one-time Poly1305 key from the
ChaCha20 KEY and NONCE (block counter fixed at 0)."
  (subseq (chacha20-block key 0 nonce) 0 32))

(defun pad16-cp (bytes)
  "Zero-pad BYTES to a multiple of 16 octets (chacha20-poly1305 uses
the same padding rule as GCM; kept local so this file has no ordering
dependency on gcm.lisp)."
  (let* ((n (length bytes)) (r (mod n 16)))
    (if (zerop r) (coerce-octets bytes)
        (let ((out (make-byte-vector (+ n (- 16 r)))))
          (replace out (coerce-octets bytes))
          out))))

(defun chacha20-poly1305-mac-data (aad ciphertext)
  (let ((aad (coerce-octets aad)) (ct (coerce-octets ciphertext)))
    (concatenate '(vector octet)
                 (pad16-cp aad) (pad16-cp ct)
                 (int->le-bytes (length aad) 8)
                 (int->le-bytes (length ct) 8))))

(defun chacha20-poly1305-encrypt (key nonce plaintext aad)
  "AEAD_CHACHA20_POLY1305 encrypt.  KEY is 32 octets, NONCE 12 octets.
Returns (values ciphertext tag), TAG a 16-octet Poly1305 tag."
  (let* ((otk (poly1305-key-gen key nonce))
         (ciphertext (chacha20-crypt key 1 nonce plaintext))
         (tag (poly1305-mac otk (chacha20-poly1305-mac-data aad ciphertext))))
    (values ciphertext tag)))

(defun chacha20-poly1305-decrypt (key nonce ciphertext aad tag)
  "AEAD_CHACHA20_POLY1305 decrypt + verify.  Signals GCM-AUTH-ERROR
(shared condition with AES-GCM) if TAG does not match; otherwise
returns the plaintext octet vector."
  (let* ((otk (poly1305-key-gen key nonce))
         (expected (poly1305-mac otk (chacha20-poly1305-mac-data aad ciphertext))))
    (unless (constant-time-equal expected (coerce-octets tag))
      (error 'gcm-auth-error))
    (chacha20-crypt key 1 nonce ciphertext)))
