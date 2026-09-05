;;;; rosette-compression-codec/src/lz77.lisp --- LZ77 sliding-window tokenizer.
;;;;
;;;; Tokens are DEFLATE-compatible by construction: match length in
;;;; [MIN-MATCH, MAX-MATCH] (default 3..258, RFC 1951's range) and distance
;;;; in [1, WINDOW-SIZE] (default 32768) so DEFLATE.LISP can lower them
;;;; straight into length/distance Huffman symbols.

(in-package #:rosette-compression-codec)

(defstruct (lz-token (:constructor %make-lit (byte))
                     (:constructor %make-match (length distance)))
  (kind :literal :type (member :literal :match))
  (byte 0 :type (unsigned-byte 8))
  (length 0 :type fixnum)
  (distance 0 :type fixnum))

(defun lz77-token-literal (b) (let ((tk (%make-lit b))) (setf (lz-token-kind tk) :literal) tk))
(defun lz77-token-match (len dist)
  (let ((tk (%make-match len dist))) (setf (lz-token-kind tk) :match) tk))
(defun lz77-token-length (tk) (lz-token-length tk))
(defun lz77-token-distance (tk) (lz-token-distance tk))

(defun %find-longest-match (bytes pos n window-size min-match max-match)
  "Naive backward search for the longest match ending before POS.
Returns (values length distance) with length 0 if no match >= MIN-MATCH."
  (let* ((win-start (max 0 (- pos window-size)))
         (best-len 0) (best-dist 0)
         (max-len (min max-match (- n pos))))
    (when (>= max-len min-match)
      (loop for cand from (1- pos) downto win-start do
        (when (= (aref bytes cand) (aref bytes pos))
          (let ((len 0))
            (loop while (and (< len max-len)
                              (= (aref bytes (+ cand len)) (aref bytes (+ pos len))))
                  do (incf len))
            (when (> len best-len)
              (setf best-len len best-dist (- pos cand))
              (when (= best-len max-len) (return)))))))
    (values best-len best-dist)))

(defun lz77-compress (bytes &key (window-size 32768) (min-match 3) (max-match 258))
  "Tokenize BYTES (an octet vector) into a list of LZ-TOKEN literal/match tokens."
  (let ((n (length bytes)) (pos 0) (tokens nil))
    (loop while (< pos n) do
      (multiple-value-bind (len dist)
          (%find-longest-match bytes pos n window-size min-match max-match)
        (if (>= len min-match)
            (progn (push (lz77-token-match len dist) tokens) (incf pos len))
            (progn (push (lz77-token-literal (aref bytes pos)) tokens) (incf pos)))))
    (nreverse tokens)))

(defun lz77-decompress (tokens &key (length-hint 0))
  "Inverse of LZ77-COMPRESS: expand a token list back into an octet vector."
  (let ((out (make-array length-hint :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0)))
    (dolist (tk tokens)
      (ecase (lz-token-kind tk)
        (:literal (vector-push-extend (lz-token-byte tk) out))
        (:match
         (let ((dist (lz-token-distance tk)) (len (lz-token-length tk)))
           (dotimes (k len)
             (vector-push-extend (aref out (- (fill-pointer out) dist)) out))))))
    (make-array (fill-pointer out) :element-type '(unsigned-byte 8) :initial-contents out)))
