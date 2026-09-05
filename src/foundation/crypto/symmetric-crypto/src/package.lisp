;;;; package.lisp --- package definition for rosette-symmetric-crypto.

(defpackage #:rosette-symmetric-crypto
  (:use #:cl)
  (:export
   ;; byte / hex helpers
   #:make-byte-vector
   #:bytes->hex
   #:hex->bytes
   #:string->bytes
   #:xor-bytes
   ;; SHA-256 (self-contained; needed for HMAC/HKDF)
   #:sha256
   #:sha256-hex
   ;; HMAC-SHA256 (RFC 2104 / RFC 4231)
   #:hmac-sha256
   ;; HKDF-SHA256 (RFC 5869)
   #:hkdf-extract
   #:hkdf-expand
   #:hkdf
   ;; AES-128/192/256 block cipher (FIPS-197)
   #:aes-key-schedule
   #:aes-encrypt-block
   #:aes-decrypt-block
   ;; AES-CTR mode (NIST SP 800-38A)
   #:aes-ctr-crypt
   ;; AES-GCM AEAD (NIST SP 800-38D)
   #:aes-gcm-encrypt
   #:aes-gcm-decrypt
   #:gcm-auth-error
   ;; ChaCha20 stream cipher (RFC 8439)
   #:chacha20-block
   #:chacha20-crypt
   ;; Poly1305 one-time authenticator (RFC 8439)
   #:poly1305-mac
   ;; ChaCha20-Poly1305 AEAD (RFC 8439)
   #:chacha20-poly1305-encrypt
   #:chacha20-poly1305-decrypt))

(in-package #:rosette-symmetric-crypto)
