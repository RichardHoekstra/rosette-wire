;;;; rosette-blc/src/blc2.lisp --- BLC2: Levenshtein-coded de Bruijn indices.
;;;;
;;;; Plain BLC codes a variable index n in UNARY (1^(n+1) 0), so a deep
;;;; variable costs n+2 bits.  BLC2 (John Tromp's gblc.c / parseBLC2) keeps
;;;; the lambda/application structure identical but codes the index with the
;;;; recursive LEVENSHTEIN code, which costs ~2 log2 n bits.  This is the
;;;; principled compact-index metric and the FAIR ruler for deep / DAG-heavy
;;;; terms: deeply-nested shared variables stop being expensive.
;;;;
;;;; Levenshtein code (matching gblc.c levenshtein()):
;;;;   code(0)   = 0
;;;;   code(n)   = 1  code(l)  <l low bits of n below the leading 1>   (n>=1)
;;;;              where l = (bit-length n) - 1
;;;;   e.g. code(1)=10, code(2)=1100, code(5)=1110001
;;;;
;;;; BLC2 term grammar (matching parseBLC2):
;;;;   variable    1  code(n)          ; leading 1, then Levenshtein index
;;;;   abstraction 0 0  enc(body)
;;;;   application 0 1  enc(f) enc(g)
;;;;
;;;; Note plain BLC's variable code 1^(n+1)0 is exactly "1" ++ unary(n);
;;;; BLC2 swaps unary for Levenshtein.  They AGREE for n in {0,1} (so terms
;;;; whose indices are all 0/1 -- I, K, Church numerals -- have identical
;;;; blc-length and blc2-length) and BLC2 wins for large n (crossover n>=7).

(in-package #:rosette-blc)

;;; ------------------------------------------------------------------
;;; The Levenshtein code for natural numbers.
;;; ------------------------------------------------------------------

(defun %binary-bits (n)
  "Big-endian bit list of N >= 1 (leading bit is 1)."
  (let ((bits '()))
    (loop for k = n then (ash k -1) while (plusp k)
          do (push (logand k 1) bits))
    bits))

(defun levenshtein-encode (n)
  "Levenshtein code of the natural number N (>= 0) as a bit list."
  (check-type n (integer 0))
  (if (zerop n)
      (list 0)
      (let* ((bin (%binary-bits n))      ; leading 1
             (l (1- (length bin)))       ; number of bits below the leading 1
             (tail (rest bin)))          ; those l bits
        (append (list 1) (levenshtein-encode l) tail))))

(defun levenshtein-decode (bits)
  "Decode one Levenshtein code from the front of BITS; (values n rest).
Mirrors gblc.c's levenshtein()."
  (when (null bits)
    (error "levenshtein-decode: unexpected end of bits"))
  (if (zerop (first bits))
      (values 0 (rest bits))
      (multiple-value-bind (l rest) (levenshtein-decode (rest bits))
        (let ((x 1))
          (dotimes (i l)
            (when (null rest)
              (error "levenshtein-decode: truncated index payload"))
            (setf x (+ (* 2 x) (first rest))
                  rest (rest rest)))
          (values x rest)))))

(defun levenshtein-length (n)
  "Bit-length of (levenshtein-encode n), computed directly."
  (check-type n (integer 0))
  (if (zerop n)
      1
      (let ((l (1- (integer-length n))))   ; (integer-length n) = bit count
        (+ 1 (levenshtein-length l) l))))

;;; ------------------------------------------------------------------
;;; The BLC2 term encoding.
;;; ------------------------------------------------------------------

(defun blc2-encode (term)
  "Encode TERM in BLC2 (Levenshtein-coded indices) as a bit list."
  (cond
    ((term-var-p term)
     (list* 1 (levenshtein-encode (term-index term))))
    ((term-lam-p term)
     (list* 0 0 (blc2-encode (term-body term))))
    ((term-app-p term)
     (list* 0 1 (append (blc2-encode (term-fun term))
                        (blc2-encode (term-arg term)))))
    (t (error "blc2-encode: not a lambda term: ~S" term))))

(defun blc2-length (term)
  "The BLC2 (Levenshtein-index) bit-length of TERM, computed structurally.

Variable index n contributes 1 + (levenshtein-length n); abstraction adds
2 + len(body); application adds 2 + len(f) + len(g).  This is the FAIR ruler
for deep / DAG-heavy terms (deep variables cost ~2 log2 n, not n)."
  (cond
    ((term-var-p term) (+ 1 (levenshtein-length (term-index term))))
    ((term-lam-p term) (+ 2 (blc2-length (term-body term))))
    ((term-app-p term) (+ 2 (blc2-length (term-fun term))
                          (blc2-length (term-arg term))))
    (t (error "blc2-length: not a lambda term: ~S" term))))

(defun %blc2-parse (bits)
  "Parse one BLC2 term from the front of BITS; (values term rest).
Mirrors gblc.c's parseBLC2."
  (when (null bits)
    (error "blc2-decode: unexpected end of bits"))
  (if (= (first bits) 1)
      ;; variable: 1 then a Levenshtein index
      (multiple-value-bind (n rest) (levenshtein-decode (rest bits))
        (values (tvar n) rest))
      ;; 0 0 -> abstraction ; 0 1 -> application
      (let ((rest (rest bits)))
        (when (null rest)
          (error "blc2-decode: truncated after leading 0"))
        (if (zerop (first rest))
            (multiple-value-bind (body more) (%blc2-parse (rest rest))
              (values (tlam body) more))
            (multiple-value-bind (f more) (%blc2-parse (rest rest))
              (multiple-value-bind (a more2) (%blc2-parse more)
                (values (tapp f a) more2)))))))

(defun blc2-decode (bits)
  "Decode a BLC2 bitstring BITS to a lambda term (rejects trailing bits)."
  (multiple-value-bind (term rest) (%blc2-parse bits)
    (when rest
      (error "blc2-decode: ~D trailing bit(s) after a complete term"
             (length rest)))
    term))

(defun blc2-length-shared (term)
  "BLC2 length AFTER CSE let-factoring -- the compounded ruler: Levenshtein
indices (cheap deep variables) over a shared (DAG) term.  This is the
honest description length for deep, DAG-heavy structure."
  (blc2-length (blc-cse-factor term)))
