;;;; sha256.lisp --- SHA-256 (FIPS 180-4), self-contained.
;;;;
;;;; Needed internally for HMAC-SHA256 and HKDF-SHA256.  Deliberately
;;;; NOT imported from the rosette-crypto sibling: that library sits at
;;;; the same Tier (01-kernel), so depending on it would break the
;;;; Tier-1 "deps subset of Tier-0" rule.  This is a small (~110
;;;; line), independent, vector-verified re-implementation.

(in-package #:rosette-symmetric-crypto)

(defparameter +sha256-k+
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(deftype u32 () '(unsigned-byte 32))

(declaim (inline rotr32))
(defun rotr32 (x n)
  (declare (type u32 x) (type (integer 0 31) n))
  (logand #xffffffff (logior (ash x (- n)) (ash x (- 32 n)))))

(defun sha256-pad (message)
  "Return MESSAGE (an octet vector) padded to a multiple of 64 octets
per FIPS 180-4 section 5.1.1."
  (let* ((msg (coerce-octets message))
         (ml (length msg))
         (bit-len (* 8 ml))
         ;; total length: ml + 1 (0x80) + k zero bytes + 8 (length) ≡ 0 mod 64
         (pad-len (mod (- 56 (mod (1+ ml) 64)) 64))
         (total (+ ml 1 pad-len 8))
         (out (make-byte-vector total)))
    (replace out msg)
    (setf (aref out ml) #x80)
    (dotimes (i 8)
      (setf (aref out (- total 1 i)) (ldb (byte 8 (* 8 i)) bit-len)))
    out))

(defun sha256 (message)
  "SHA-256 digest of MESSAGE (a sequence of octets); returns a 32-octet vector."
  (let* ((padded (sha256-pad message))
         (nblocks (/ (length padded) 64))
         (h (make-array 8 :element-type 'u32
                        :initial-contents '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                                             #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
         (w (make-array 64 :element-type 'u32)))
    (dotimes (blk nblocks)
      (let ((off (* blk 64)))
        (dotimes (i 16)
          (setf (aref w i)
                (logior (ash (aref padded (+ off (* 4 i))) 24)
                        (ash (aref padded (+ off (* 4 i) 1)) 16)
                        (ash (aref padded (+ off (* 4 i) 2)) 8)
                        (aref padded (+ off (* 4 i) 3)))))
        (loop for i from 16 below 64 do
          (let* ((s0 (logxor (rotr32 (aref w (- i 15)) 7)
                              (rotr32 (aref w (- i 15)) 18)
                              (ash (aref w (- i 15)) -3)))
                 (s1 (logxor (rotr32 (aref w (- i 2)) 17)
                             (rotr32 (aref w (- i 2)) 19)
                             (ash (aref w (- i 2)) -10))))
            (setf (aref w i)
                  (logand #xffffffff
                          (+ (aref w (- i 16)) s0 (aref w (- i 7)) s1)))))
        (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
              (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
          (dotimes (i 64)
            (let* ((s1 (logxor (rotr32 e 6) (rotr32 e 11) (rotr32 e 25)))
                   (ch (logxor (logand e f) (logand (logxor e #xffffffff) g)))
                   (temp1 (logand #xffffffff (+ hh s1 ch (aref +sha256-k+ i) (aref w i))))
                   (s0 (logxor (rotr32 a 2) (rotr32 a 13) (rotr32 a 22)))
                   (maj (logxor (logand a b) (logand a c) (logand b c)))
                   (temp2 (logand #xffffffff (+ s0 maj))))
              (setf hh g g f f e e (logand #xffffffff (+ d temp1))
                    d c c b b a a (logand #xffffffff (+ temp1 temp2)))))
          (setf (aref h 0) (logand #xffffffff (+ (aref h 0) a))
                (aref h 1) (logand #xffffffff (+ (aref h 1) b))
                (aref h 2) (logand #xffffffff (+ (aref h 2) c))
                (aref h 3) (logand #xffffffff (+ (aref h 3) d))
                (aref h 4) (logand #xffffffff (+ (aref h 4) e))
                (aref h 5) (logand #xffffffff (+ (aref h 5) f))
                (aref h 6) (logand #xffffffff (+ (aref h 6) g))
                (aref h 7) (logand #xffffffff (+ (aref h 7) hh))))))
    (let ((out (make-byte-vector 32)))
      (dotimes (i 8 out)
        (dotimes (j 4)
          (setf (aref out (+ (* 4 i) j)) (ldb (byte 8 (* 8 (- 3 j))) (aref h i))))))))

(defun sha256-hex (message)
  "Hex-encoded SHA-256 digest of MESSAGE."
  (bytes->hex (sha256 message)))
