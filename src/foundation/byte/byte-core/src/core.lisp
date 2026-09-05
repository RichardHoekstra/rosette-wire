;;;; core.lisp --- byte-vector coercion primitives.

(in-package #:rosette-byte-core)

(deftype u8 ()
  "An unsigned 8-bit byte."
  '(unsigned-byte 8))

(deftype byte-vector ()
  "A one-dimensional specialized unsigned-byte 8 array."
  '(simple-array u8 (*)))

(defun make-byte-vector (length &key (initial-element 0))
  "Return a specialized unsigned-byte 8 vector of LENGTH."
  (make-array length
              :element-type 'u8
              :initial-element (logand #xff initial-element)))

(defun %copy-byte-source (data reader)
  (let* ((n (length data))
         (out (make-byte-vector n)))
    (loop for i below n
          do (setf (aref out i) (funcall reader data i)))
    out))

(defun %character-byte (character)
  (logand #xff (char-code character)))

(defun %coerce-byte-element (value)
  (etypecase value
    (integer (logand #xff value))
    (character (%character-byte value))))

(defun coerce-byte-vector (data)
  "Return DATA as a fresh BYTE-VECTOR.

Strings are coerced by low 8 bits of CHAR-CODE. Vectors may contain
unsigned bytes or characters; integer vector elements are clipped to
their low 8 bits."
  (etypecase data
    (string
     (%copy-byte-source data
                        (lambda (string i)
                          (%character-byte (char string i)))))
    (byte-vector
     (let ((out (make-byte-vector (length data))))
       (replace out data)
       out))
    (vector
     (%copy-byte-source data
                        (lambda (vector i)
                          (%coerce-byte-element (aref vector i)))))))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "Canonical RFC 4648 Base64 alphabet.")

(defun %base64-value (character)
  (position character +base64-alphabet+ :test #'char=))

(defun %base64-whitespace-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\Page)
          :test #'char=))

(defun base64-encode-bytes (bytes)
  "Encode BYTES as canonical padded RFC 4648 Base64."
  (let* ((input (coerce-byte-vector bytes))
         (n (length input))
         (output (make-string (* 4 (ceiling n 3)))))
    (loop for input-index from 0 below n by 3
          for output-index from 0 by 4
          for remaining = (- n input-index)
          for b0 = (aref input input-index)
          for b1 = (if (> remaining 1) (aref input (1+ input-index)) 0)
          for b2 = (if (> remaining 2) (aref input (+ input-index 2)) 0)
          for triple = (logior (ash b0 16) (ash b1 8) b2)
          do (setf (char output output-index)
                   (char +base64-alphabet+ (ldb (byte 6 18) triple))
                   (char output (1+ output-index))
                   (char +base64-alphabet+ (ldb (byte 6 12) triple))
                   (char output (+ output-index 2))
                   (if (> remaining 1)
                       (char +base64-alphabet+ (ldb (byte 6 6) triple))
                       #\=)
                   (char output (+ output-index 3))
                   (if (> remaining 2)
                       (char +base64-alphabet+ (ldb (byte 6 0) triple))
                       #\=)))
    output))

(defun base64-decode-bytes (string &key (ignore-whitespace t))
  "Decode padded RFC 4648 STRING into a fresh byte vector.

Whitespace is ignored when IGNORE-WHITESPACE is true. Other non-alphabet
characters, misplaced padding, nonzero unused bits, and partial quartets are
rejected."
  (check-type string string)
  (let ((clean (make-array (length string)
                           :element-type 'character
                           :adjustable t
                           :fill-pointer 0)))
    (loop for character across string
          do (cond
               ((or (char= character #\=) (%base64-value character))
                (vector-push-extend character clean))
               ((and ignore-whitespace (%base64-whitespace-p character)))
               (t (error "Invalid Base64 character ~S" character))))
    (let ((n (length clean)))
      (unless (zerop (mod n 4))
        (error "Base64 input length ~D is not a whole quartet" n))
      (let* ((padding
               (cond ((zerop n) 0)
                     ((char/= (aref clean (1- n)) #\=) 0)
                     ((and (> n 1) (char= (aref clean (- n 2)) #\=)) 2)
                     (t 1)))
             (output-length (- (* 3 (/ n 4)) padding))
             (output (make-byte-vector output-length))
             (output-index 0))
        (loop for index from 0 below n by 4
              for final-p = (= (+ index 4) n)
              for c0 = (aref clean index)
              for c1 = (aref clean (1+ index))
              for c2 = (aref clean (+ index 2))
              for c3 = (aref clean (+ index 3))
              for v0 = (%base64-value c0)
              for v1 = (%base64-value c1)
              for v2 = (and (char/= c2 #\=) (%base64-value c2))
              for v3 = (and (char/= c3 #\=) (%base64-value c3))
              do (unless (and v0 v1
                              (or v2 (and final-p (char= c2 #\=)))
                              (or v3 (and final-p (char= c3 #\=))))
                   (error "Malformed Base64 quartet at offset ~D" index))
                 (when (and (char= c2 #\=) (char/= c3 #\=))
                   (error "Malformed Base64 padding at offset ~D" index))
                 (when (and (char= c2 #\=) (not (zerop (logand v1 #x0f))))
                   (error "Nonzero unused Base64 bits at offset ~D" index))
                 (when (and v2 (char= c3 #\=)
                            (not (zerop (logand v2 #x03))))
                   (error "Nonzero unused Base64 bits at offset ~D" index))
                 (let ((triple (logior (ash v0 18)
                                       (ash v1 12)
                                       (ash (or v2 0) 6)
                                       (or v3 0))))
                   (when (< output-index output-length)
                     (setf (aref output output-index)
                           (ldb (byte 8 16) triple))
                     (incf output-index))
                   (when (< output-index output-length)
                     (setf (aref output output-index)
                           (ldb (byte 8 8) triple))
                     (incf output-index))
                   (when (< output-index output-length)
                     (setf (aref output output-index)
                           (ldb (byte 8 0) triple))
                     (incf output-index))))
        output))))

(defun byte-windows (bytes width &key (stride width) drop-tail)
  "Return a vector of fresh byte-vector windows over BYTES.

WIDTH is the maximum window size. STRIDE defaults to WIDTH. When
DROP-TAIL is true, omit the final partial window."
  (let* ((bs (coerce-byte-vector bytes))
         (windows '()))
    (loop for start from 0 below (length bs) by stride
          for end = (min (length bs) (+ start width))
          when (or (= (- end start) width) (not drop-tail))
            do (let ((window (make-byte-vector (- end start))))
                 (loop for i below (- end start)
                       do (setf (aref window i) (aref bs (+ start i))))
                 (push window windows)))
    (coerce (nreverse windows) 'vector)))

(declaim (inline bits-to-bytes bytes-to-bits))

(defun bits-to-bytes (bits)
  "Convert a real-valued bit count to whole bytes, rounding up."
  (declare (type real bits))
  (the (integer 0 *) (ceiling bits 8)))

(defun bytes-to-bits (bytes)
  "Convert a non-negative byte count to bits."
  (declare (type (integer 0 *) bytes))
  (the (integer 0 *) (* bytes 8)))

(declaim (inline low-nibble high-nibble pack-nibbles))

(defun low-nibble (byte)
  "Return BYTE's low 4-bit nibble."
  (declare (type integer byte))
  (the (unsigned-byte 4) (logand byte #x0f)))

(defun high-nibble (byte)
  "Return BYTE's high 4-bit nibble."
  (declare (type integer byte))
  (the (unsigned-byte 4) (ldb (byte 4 4) byte)))

(defun pack-nibbles (low high)
  "Pack LOW and HIGH 4-bit nibbles into one unsigned byte."
  (declare (type integer low high))
  (the u8 (logior (low-nibble low) (ash (low-nibble high) 4))))
