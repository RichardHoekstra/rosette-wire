;;;; chacha20.lisp --- ChaCha20 stream cipher (RFC 8439 section 2.3-2.4).

(in-package #:rosette-symmetric-crypto)

(declaim (inline rotl32))
(defun rotl32 (x n)
  (declare (type u32 x) (type (integer 0 31) n))
  (logand #xffffffff (logior (ash x n) (ash x (- n 32)))))

(defmacro qr (state a b c d)
  `(progn
     (setf (aref ,state ,a) (logand #xffffffff (+ (aref ,state ,a) (aref ,state ,b)))
           (aref ,state ,d) (rotl32 (logxor (aref ,state ,d) (aref ,state ,a)) 16)
           (aref ,state ,c) (logand #xffffffff (+ (aref ,state ,c) (aref ,state ,d)))
           (aref ,state ,b) (rotl32 (logxor (aref ,state ,b) (aref ,state ,c)) 12)
           (aref ,state ,a) (logand #xffffffff (+ (aref ,state ,a) (aref ,state ,b)))
           (aref ,state ,d) (rotl32 (logxor (aref ,state ,d) (aref ,state ,a)) 8)
           (aref ,state ,c) (logand #xffffffff (+ (aref ,state ,c) (aref ,state ,d)))
           (aref ,state ,b) (rotl32 (logxor (aref ,state ,b) (aref ,state ,c)) 7))))

(defun le-word (bytes off)
  (logior (aref bytes off) (ash (aref bytes (+ off 1)) 8)
          (ash (aref bytes (+ off 2)) 16) (ash (aref bytes (+ off 3)) 24)))

(defun chacha20-init-state (key counter nonce)
  (let* ((key (coerce-octets key)) (nonce (coerce-octets nonce))
         (s (make-array 16 :element-type 'u32)))
    (setf (aref s 0) #x61707865 (aref s 1) #x3320646e
          (aref s 2) #x79622d32 (aref s 3) #x6b206574)
    (dotimes (i 8) (setf (aref s (+ 4 i)) (le-word key (* 4 i))))
    (setf (aref s 12) counter)
    (dotimes (i 3) (setf (aref s (+ 13 i)) (le-word nonce (* 4 i))))
    s))

(defun chacha20-block (key counter nonce)
  "The ChaCha20 block function: 256-bit KEY, 32-bit COUNTER, 96-bit
NONCE -> 64 octets of keystream."
  (let* ((s (chacha20-init-state key counter nonce))
         (working (copy-seq s)))
    (dotimes (i 10)
      (qr working 0 4 8 12) (qr working 1 5 9 13) (qr working 2 6 10 14) (qr working 3 7 11 15)
      (qr working 0 5 10 15) (qr working 1 6 11 12) (qr working 2 7 8 13) (qr working 3 4 9 14))
    (dotimes (i 16) (setf (aref working i) (logand #xffffffff (+ (aref working i) (aref s i)))))
    (let ((out (make-byte-vector 64)))
      (dotimes (i 16 out)
        (dotimes (j 4)
          (setf (aref out (+ (* 4 i) j)) (ldb (byte 8 (* 8 j)) (aref working i))))))))

(defun chacha20-crypt (key counter nonce data)
  "Encrypt/decrypt (symmetric XOR) DATA with the ChaCha20 keystream
starting at block COUNTER."
  (let* ((data (coerce-octets data))
         (n (length data))
         (out (make-byte-vector n)))
    (loop for off from 0 below n by 64
          for blk from counter
          do (let* ((ks (chacha20-block key blk nonce))
                    (chunk-len (min 64 (- n off))))
               (dotimes (i chunk-len)
                 (setf (aref out (+ off i)) (logxor (aref data (+ off i)) (aref ks i))))))
    out))
