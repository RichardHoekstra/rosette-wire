;;;; poly1305.lisp --- Poly1305 one-time authenticator (RFC 8439 section 2.5).

(in-package #:rosette-symmetric-crypto)

(defconstant +poly1305-p+ (- (ash 1 130) 5))

(defun le-bytes->int (bytes)
  (let ((n 0))
    (dotimes (i (length bytes) n) (setf n (logior n (ash (aref bytes i) (* 8 i)))))))

(defun int->le-bytes (n length)
  (let ((out (make-byte-vector length)))
    (dotimes (i length out) (setf (aref out i) (ldb (byte 8 (* 8 i)) n)))))

(defun poly1305-clamp (r)
  (logand r #x0ffffffc0ffffffc0ffffffc0fffffff))

(defun poly1305-mac (key message)
  "Poly1305(KEY, MESSAGE): KEY is a 32-octet one-time key, MESSAGE an
arbitrary-length octet sequence.  Returns a 16-octet tag."
  (let* ((key (coerce-octets key))
         (r (poly1305-clamp (le-bytes->int (subseq key 0 16))))
         (s (le-bytes->int (subseq key 16 32)))
         (msg (coerce-octets message))
         (n (length msg))
         (acc 0))
    (loop for off from 0 below n by 16
          do (let* ((chunk-len (min 16 (- n off)))
                    (block-n (logior (le-bytes->int (subseq msg off (+ off chunk-len)))
                                      (ash 1 (* 8 chunk-len)))))
               (setf acc (mod (* (+ acc block-n) r) +poly1305-p+))))
    (int->le-bytes (logand (+ acc s) #xffffffffffffffffffffffffffffffff) 16)))
