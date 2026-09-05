;;;; aes.lisp --- AES-128/192/256 block cipher (FIPS-197), key-schedule +
;;;; single-block encrypt/decrypt.  Self-contained GF(2^8) arithmetic;
;;;; no external dependency.

(in-package #:rosette-symmetric-crypto)

;;; --- S-box (FIPS-197 Figure 7) -------------------------------------

(defparameter +aes-sbox+
  (make-array 256 :element-type '(unsigned-byte 8) :initial-contents
   '(#x63 #x7c #x77 #x7b #xf2 #x6b #x6f #xc5 #x30 #x01 #x67 #x2b #xfe #xd7 #xab #x76
     #xca #x82 #xc9 #x7d #xfa #x59 #x47 #xf0 #xad #xd4 #xa2 #xaf #x9c #xa4 #x72 #xc0
     #xb7 #xfd #x93 #x26 #x36 #x3f #xf7 #xcc #x34 #xa5 #xe5 #xf1 #x71 #xd8 #x31 #x15
     #x04 #xc7 #x23 #xc3 #x18 #x96 #x05 #x9a #x07 #x12 #x80 #xe2 #xeb #x27 #xb2 #x75
     #x09 #x83 #x2c #x1a #x1b #x6e #x5a #xa0 #x52 #x3b #xd6 #xb3 #x29 #xe3 #x2f #x84
     #x53 #xd1 #x00 #xed #x20 #xfc #xb1 #x5b #x6a #xcb #xbe #x39 #x4a #x4c #x58 #xcf
     #xd0 #xef #xaa #xfb #x43 #x4d #x33 #x85 #x45 #xf9 #x02 #x7f #x50 #x3c #x9f #xa8
     #x51 #xa3 #x40 #x8f #x92 #x9d #x38 #xf5 #xbc #xb6 #xda #x21 #x10 #xff #xf3 #xd2
     #xcd #x0c #x13 #xec #x5f #x97 #x44 #x17 #xc4 #xa7 #x7e #x3d #x64 #x5d #x19 #x73
     #x60 #x81 #x4f #xdc #x22 #x2a #x90 #x88 #x46 #xee #xb8 #x14 #xde #x5e #x0b #xdb
     #xe0 #x32 #x3a #x0a #x49 #x06 #x24 #x5c #xc2 #xd3 #xac #x62 #x91 #x95 #xe4 #x79
     #xe7 #xc8 #x37 #x6d #x8d #xd5 #x4e #xa9 #x6c #x56 #xf4 #xea #x65 #x7a #xae #x08
     #xba #x78 #x25 #x2e #x1c #xa6 #xb4 #xc6 #xe8 #xdd #x74 #x1f #x4b #xbd #x8b #x8a
     #x70 #x3e #xb5 #x66 #x48 #x03 #xf6 #x0e #x61 #x35 #x57 #xb9 #x86 #xc1 #x1d #x9e
     #xe1 #xf8 #x98 #x11 #x69 #xd9 #x8e #x94 #x9b #x1e #x87 #xe9 #xce #x55 #x28 #xdf
     #x8c #xa1 #x89 #x0d #xbf #xe6 #x42 #x68 #x41 #x99 #x2d #x0f #xb0 #x54 #xbb #x16)))

(defparameter +aes-inv-sbox+
  (let ((inv (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256 inv)
      (setf (aref inv (aref +aes-sbox+ i)) i))))

;;; --- GF(2^8) arithmetic (AES reduction polynomial x^8+x^4+x^3+x+1) --

(declaim (inline xtime))
(defun xtime (a)
  (declare (type (unsigned-byte 8) a))
  (let ((s (ash a 1)))
    (if (logbitp 8 s) (logand #xff (logxor s #x1b)) s)))

(defun gmul (a b)
  "Multiply A and B in GF(2^8)."
  (declare (type (unsigned-byte 8) a b))
  (let ((p 0))
    (dotimes (i 8 p)
      (when (logbitp 0 b) (setf p (logxor p a)))
      (setf a (xtime a))
      (setf b (ash b -1)))))

;;; --- key expansion ---------------------------------------------------

(defun word (b0 b1 b2 b3)
  (logior (ash b0 24) (ash b1 16) (ash b2 8) b3))

(defun sub-word (w)
  (word (aref +aes-sbox+ (ldb (byte 8 24) w))
        (aref +aes-sbox+ (ldb (byte 8 16) w))
        (aref +aes-sbox+ (ldb (byte 8 8) w))
        (aref +aes-sbox+ (ldb (byte 8 0) w))))

(defun rot-word (w)
  (word (ldb (byte 8 16) w) (ldb (byte 8 8) w) (ldb (byte 8 0) w) (ldb (byte 8 24) w)))

(defun aes-key-schedule (key)
  "Expand KEY (16/24/32 octets -> AES-128/192/256) into a vector of
round-key words (32-bit integers), length 4*(Nr+1).  Second value is
Nr (number of rounds: 10/12/14)."
  (let* ((key (coerce-octets key))
         (nk (/ (length key) 4))
         (nr (case nk (4 10) (6 12) (8 14)
                   (t (error "rosette-symmetric-crypto: AES key must be 16, 24, or 32 octets, got ~D" (length key)))))
         (total-words (* 4 (1+ nr)))
         (w (make-array total-words)))
    (dotimes (i nk)
      (setf (aref w i) (word (aref key (* 4 i)) (aref key (1+ (* 4 i)))
                              (aref key (+ 2 (* 4 i))) (aref key (+ 3 (* 4 i))))))
    (let ((rcon 1))
      (loop for i from nk below total-words do
        (let ((temp (aref w (1- i))))
          (cond
            ((zerop (mod i nk))
             (setf temp (logxor (sub-word (rot-word temp)) (ash rcon 24)))
             (setf rcon (xtime rcon)))
            ((and (> nk 6) (= 4 (mod i nk)))
             (setf temp (sub-word temp))))
          (setf (aref w i) (logxor (aref w (- i nk)) temp)))))
    (values w nr)))

;;; --- state <-> block helpers ------------------------------------------

(defun add-round-key (state w round)
  "XOR the 4 words w[4*round .. 4*round+3] into the 16-byte STATE
(column-major: state byte (r + 4c) <- word c, byte r)."
  (dotimes (c 4)
    (let ((word (aref w (+ (* 4 round) c))))
      (dotimes (r 4)
        (setf (aref state (+ r (* 4 c)))
              (logxor (aref state (+ r (* 4 c))) (ldb (byte 8 (* 8 (- 3 r))) word)))))))

(defun sub-bytes (state sbox)
  (dotimes (i 16)
    (setf (aref state i) (aref sbox (aref state i)))))

(defun shift-rows (state)
  "Left-rotate row r by r positions (state[r,c] = state[r + 4c])."
  (let ((tmp (copy-seq state)))
    (dotimes (r 4)
      (dotimes (c 4)
        (setf (aref state (+ r (* 4 c))) (aref tmp (+ r (* 4 (mod (+ c r) 4)))))))))

(defun inv-shift-rows (state)
  (let ((tmp (copy-seq state)))
    (dotimes (r 4)
      (dotimes (c 4)
        (setf (aref state (+ r (* 4 c))) (aref tmp (+ r (* 4 (mod (- c r) 4)))))))))

(defun mix-columns (state)
  (dotimes (c 4)
    (let* ((b (+ 0 (* 4 c))))
      (let ((a0 (aref state b)) (a1 (aref state (+ b 1)))
            (a2 (aref state (+ b 2))) (a3 (aref state (+ b 3))))
        (setf (aref state b)       (logxor (gmul a0 2) (gmul a1 3) a2 a3))
        (setf (aref state (+ b 1)) (logxor a0 (gmul a1 2) (gmul a2 3) a3))
        (setf (aref state (+ b 2)) (logxor a0 a1 (gmul a2 2) (gmul a3 3)))
        (setf (aref state (+ b 3)) (logxor (gmul a0 3) a1 a2 (gmul a3 2)))))))

(defun inv-mix-columns (state)
  (dotimes (c 4)
    (let* ((b (* 4 c)))
      (let ((a0 (aref state b)) (a1 (aref state (+ b 1)))
            (a2 (aref state (+ b 2))) (a3 (aref state (+ b 3))))
        (setf (aref state b)       (logxor (gmul a0 14) (gmul a1 11) (gmul a2 13) (gmul a3 9)))
        (setf (aref state (+ b 1)) (logxor (gmul a0 9) (gmul a1 14) (gmul a2 11) (gmul a3 13)))
        (setf (aref state (+ b 2)) (logxor (gmul a0 13) (gmul a1 9) (gmul a2 14) (gmul a3 11)))
        (setf (aref state (+ b 3)) (logxor (gmul a0 11) (gmul a1 13) (gmul a2 9) (gmul a3 14)))))))

(defun aes-encrypt-block (w nr block)
  "Encrypt one 16-octet BLOCK under expanded key W with NR rounds
(values from AES-KEY-SCHEDULE).  Returns a fresh 16-octet vector."
  (let ((state (copy-seq (coerce-octets block))))
    (add-round-key state w 0)
    (loop for round from 1 below nr do
      (sub-bytes state +aes-sbox+)
      (shift-rows state)
      (mix-columns state)
      (add-round-key state w round))
    (sub-bytes state +aes-sbox+)
    (shift-rows state)
    (add-round-key state w nr)
    state))

(defun aes-decrypt-block (w nr block)
  "Decrypt one 16-octet BLOCK under expanded key W with NR rounds."
  (let ((state (copy-seq (coerce-octets block))))
    (add-round-key state w nr)
    (loop for round from (1- nr) downto 1 do
      (inv-shift-rows state)
      (sub-bytes state +aes-inv-sbox+)
      (add-round-key state w round)
      (inv-mix-columns state))
    (inv-shift-rows state)
    (sub-bytes state +aes-inv-sbox+)
    (add-round-key state w 0)
    state))
