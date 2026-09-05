;;;; rosette-compression-codec/src/huffman.lisp --- canonical Huffman coding.
;;;;
;;;; Two pieces:
;;;;  1. huffman-code-lengths: classic Huffman-tree code-length assignment
;;;;     from a frequency table (min-heap via a sorted list; fine for the
;;;;     small byte alphabets this substrate feeds it).
;;;;  2. huffman-canonical-codes: RFC 1951 sec 3.2.2's canonical-code
;;;;     assignment from lengths alone (the same algorithm rosette-inflate's
;;;;     decoder is built to consume, and what DEFLATE's fixed/dynamic
;;;;     Huffman tables both use).
;;;;
;;;; huffman-compress/huffman-decompress is a small self-contained generic
;;;; byte codec (its own bitstream, not a DEFLATE block) used to verify the
;;;; prefix property, Kraft equality, and exact round-trip of the canonical
;;;; codes this module builds.

(in-package #:rosette-compression-codec)

(defstruct hnode
  (freq 0 :type integer)
  (symbol nil)
  (left nil)
  (right nil))

(defun huffman-code-lengths (freq-alist)
  "FREQ-ALIST is a list of (symbol . count) with count > 0.  Returns an
alist (symbol . code-length).  Symbols with a single entry get length 1
by convention (so the caller can still emit a 1-bit code)."
  (let ((entries (remove-if (lambda (kv) (<= (cdr kv) 0)) freq-alist)))
    (when (null entries) (return-from huffman-code-lengths nil))
    (when (= (length entries) 1)
      (return-from huffman-code-lengths (list (cons (car (first entries)) 1))))
    (let ((heap (mapcar (lambda (kv) (make-hnode :freq (cdr kv) :symbol (car kv))) entries)))
      (setf heap (sort heap #'< :key #'hnode-freq))
      (loop while (> (length heap) 1) do
        (let* ((a (pop heap)) (b (pop heap))
               (merged (make-hnode :freq (+ (hnode-freq a) (hnode-freq b)) :left a :right b)))
          ;; stable insert keeping heap sorted ascending by freq
          (setf heap (merge 'list (list merged) heap #'< :key #'hnode-freq))))
      (let ((lengths nil))
        (labels ((walk (node depth)
                   (cond
                     ((null node))
                     ((and (null (hnode-left node)) (null (hnode-right node)))
                      (push (cons (hnode-symbol node) (max depth 1)) lengths))
                     (t (walk (hnode-left node) (1+ depth))
                        (walk (hnode-right node) (1+ depth))))))
          (walk (first heap) 0))
        lengths))))

(defun huffman-canonical-codes (length-alist)
  "RFC 1951 3.2.2 canonical-code assignment.  LENGTH-ALIST is (symbol . len).
Returns an alist (symbol . (code . len)), MSB-first code values."
  (when (null length-alist) (return-from huffman-canonical-codes nil))
  (let* ((max-len (reduce #'max length-alist :key #'cdr))
         (count (make-array (1+ max-len) :initial-element 0)))
    (dolist (kv length-alist) (incf (aref count (cdr kv))))
    (setf (aref count 0) 0)
    (let ((next-code (make-array (1+ max-len) :initial-element 0))
          (code 0))
      (loop for bits from 1 to max-len do
        (setf code (ash (+ code (aref count (1- bits))) 1))
        (setf (aref next-code bits) code))
      (let ((sorted (sort (copy-list length-alist) #'< :key (lambda (kv) (car kv)))))
        (mapcar (lambda (kv)
                  (let* ((len (cdr kv)) (c (aref next-code len)))
                    (incf (aref next-code len))
                    (cons (car kv) (cons c len))))
                sorted)))))

(defun huffman-kraft-sum (length-alist)
  "Exact (rational) Kraft sum: sum(2^-length) over the code table."
  (reduce #'+ length-alist :key (lambda (kv) (/ 1 (expt 2 (cdr kv)))) :initial-value 0))

(defun huffman-prefix-free-p (code-alist)
  "CODE-ALIST is (symbol . (code . len)).  T iff no code is a bit-prefix of another."
  (let ((codes (coerce (mapcar #'cdr code-alist) 'vector)))
    (loop for i below (length codes) always
      (loop for j below (length codes) always
        (or (= i j)
            (let* ((ci (car (aref codes i))) (li (cdr (aref codes i)))
                   (cj (car (aref codes j))) (lj (cdr (aref codes j)))
                   (m (min li lj)))
              (/= (ash ci (- m li)) (ash cj (- m lj)))))))))

;;; ---- self-contained generic byte codec (own bitstream, own decode) ----

(defun byte-frequencies (bytes)
  (let ((tbl (make-array 256 :initial-element 0)))
    (loop for b across bytes do (incf (aref tbl b)))
    (loop for i below 256 when (plusp (aref tbl i)) collect (cons i (aref tbl i)))))

(defun huffman-compress (bytes)
  "Returns (values code-alist payload-octets bit-count original-length).
CODE-ALIST: (byte . (code . len)), needed by huffman-decompress (a canonical
Huffman stream is only decodable given its code-length table -- exactly the
information a real DEFLATE dynamic block transmits in its header)."
  (let* ((freqs (byte-frequencies bytes))
         (lengths (huffman-code-lengths freqs))
         (codes (huffman-canonical-codes lengths))
         (w (make-bit-writer))
         (code-map (make-array 256)))
    (dolist (kv codes) (setf (aref code-map (car kv)) (cdr kv)))
    (loop for b across bytes do
      (let ((cl (aref code-map b)))
        (bw-put-huffman-code w (car cl) (cdr cl))))
    (let* ((total-bits (loop for kv in codes
                              sum (* (cdr (cdr kv))
                                     (count (car kv) bytes))))
           (payload (bw-finish w)))
      (values codes payload total-bits (length bytes)))))

(defun huffman-decompress (code-alist payload bit-count n)
  "Inverse of HUFFMAN-COMPRESS.  CODE-ALIST: (byte . (code . len))."
  (let ((table (make-hash-table :test 'equal))
        (r (make-bit-reader payload))
        (out (make-array n :element-type '(unsigned-byte 8)))
        (code 0) (len 0) (bits-consumed 0))
    (dolist (kv code-alist) (setf (gethash (cons (cdr (cdr kv)) (car (cdr kv))) table) (car kv)))
    (dotimes (i n)
      (setf code 0 len 0)
      (loop
        (setf code (logior (ash code 1) (br-get-huffman-code r)))
        (incf len) (incf bits-consumed)
        (multiple-value-bind (sym found) (gethash (cons len code) table)
          (when found
            (setf (aref out i) sym)
            (return))
          (when (> bits-consumed (+ bit-count 8)) (error "huffman-decompress: no matching code")))))
    out))
