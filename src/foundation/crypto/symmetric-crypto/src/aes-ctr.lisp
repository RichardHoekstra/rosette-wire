;;;; aes-ctr.lisp --- AES-CTR mode (NIST SP 800-38A section 6.5).
;;;;
;;;; The "standard increment function" of SP 800-38A treats the whole
;;;; 128-bit counter block as a single big-endian integer and
;;;; increments it modulo 2^128; that is what this file implements.
;;;; (GCM's internal counter, in gcm.lisp, increments only the low 32
;;;; bits per SP 800-38D and is implemented separately there.)

(in-package #:rosette-symmetric-crypto)

(defun block->int (block)
  (let ((n 0))
    (dotimes (i 16 n) (setf n (logior (ash n 8) (aref block i))))))

(defun int->block (n)
  (let ((out (make-byte-vector 16)))
    (dotimes (i 16 out)
      (setf (aref out (- 15 i)) (ldb (byte 8 (* 8 i)) n)))))

(defun aes-ctr-crypt (key initial-counter data)
  "Encrypt/decrypt (symmetric) DATA under AES-CTR with KEY (16/24/32
octets) and 16-octet INITIAL-COUNTER block.  Returns a fresh octet
vector the same length as DATA."
  (multiple-value-bind (w nr) (aes-key-schedule key)
    (let* ((data (coerce-octets data))
           (n (length data))
           (out (make-byte-vector n))
           (counter (block->int (coerce-octets initial-counter))))
      (loop for off from 0 below n by 16
            do (let* ((ks (aes-encrypt-block w nr (int->block counter)))
                      (chunk-len (min 16 (- n off))))
                 (dotimes (i chunk-len)
                   (setf (aref out (+ off i)) (logxor (aref data (+ off i)) (aref ks i))))
                 (setf counter (logand #xffffffffffffffffffffffffffffffff (1+ counter)))))
      out)))
