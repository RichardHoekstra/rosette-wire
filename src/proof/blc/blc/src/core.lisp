;;;; rosette-blc/src/core.lisp --- Binary Lambda Calculus (John Tromp).
;;;;
;;;; A lambda term in de Bruijn form is one of:
;;;;
;;;;   (:var n)      n >= 0, a bound-variable reference (de Bruijn index)
;;;;   (:lam body)   abstraction  (lambda . body)
;;;;   (:app f a)    application  (f applied to a)
;;;;
;;;; The Binary Lambda Calculus encoding (Tromp 2006) gives every closed
;;;; term an EXACT bit-length -- it is the measurement that makes
;;;; Minimum-Description-Length literal:
;;;;
;;;;   enc((:var n))   = 1^(n+1) 0        ; n+1 ones, then a zero
;;;;   enc((:lam M))   = 00  enc(M)
;;;;   enc((:app M N)) = 01  enc(M) enc(N)
;;;;
;;;; Bitstrings are lists of the integers 0 and 1.  blc-length computes
;;;; the bit count structurally (no string built); it is gated equal to
;;;; (length (blc-encode term)).

(in-package #:rosette-blc)

;;; ------------------------------------------------------------------
;;; Term constructors, predicates, accessors.
;;; ------------------------------------------------------------------

(defun tvar (n)
  "A de Bruijn variable reference, index N >= 0."
  (check-type n (integer 0))
  (list :var n))

(defun tlam (body)
  "An abstraction over BODY."
  (list :lam body))

(defun tapp (f a)
  "Application of F to A."
  (list :app f a))

(defun term-var-p (term)
  (and (consp term) (eq (car term) :var)))
(defun term-lam-p (term)
  (and (consp term) (eq (car term) :lam)))
(defun term-app-p (term)
  (and (consp term) (eq (car term) :app)))

(defun term-index (term) (second term))
(defun term-body  (term) (second term))
(defun term-fun   (term) (second term))
(defun term-arg   (term) (third term))

(defun term-p (term)
  "T iff TERM is a well-formed de-Bruijn lambda term."
  (cond
    ((term-var-p term)
     (and (= (length term) 2)
          (integerp (term-index term))
          (>= (term-index term) 0)))
    ((term-lam-p term)
     (and (= (length term) 2)
          (term-p (term-body term))))
    ((term-app-p term)
     (and (= (length term) 3)
          (term-p (term-fun term))
          (term-p (term-arg term))))
    (t nil)))

;;; ------------------------------------------------------------------
;;; The encoding.
;;; ------------------------------------------------------------------

(defun blc-encode (term)
  "Encode TERM to a bitstring (a list of 0/1 integers), per Tromp's BLC."
  (cond
    ((term-var-p term)
     ;; 1^(n+1) 0
     (append (make-list (1+ (term-index term)) :initial-element 1)
             (list 0)))
    ((term-lam-p term)
     (list* 0 0 (blc-encode (term-body term))))
    ((term-app-p term)
     (append (list 0 1)
             (blc-encode (term-fun term))
             (blc-encode (term-arg term))))
    (t (error "blc-encode: not a lambda term: ~S" term))))

(defun blc-length (term)
  "The exact BLC bit-length of TERM, computed structurally.

Variable index n contributes n+2 bits; an abstraction adds 2 + len(body);
an application adds 2 + len(f) + len(g)."
  (cond
    ((term-var-p term) (+ (term-index term) 2))
    ((term-lam-p term) (+ 2 (blc-length (term-body term))))
    ((term-app-p term) (+ 2
                          (blc-length (term-fun term))
                          (blc-length (term-arg term))))
    (t (error "blc-length: not a lambda term: ~S" term))))

;;; ------------------------------------------------------------------
;;; The decoder (parser) -- for round-trip gating.
;;; ------------------------------------------------------------------

(defun %parse (bits)
  "Parse one term from the front of BITS; return (values term rest)."
  (when (null bits)
    (error "blc-decode: unexpected end of bitstring"))
  (let ((b0 (first bits)))
    (cond
      ;; 0 0 ... -> abstraction ; 0 1 ... -> application
      ((zerop b0)
       (let ((rest (rest bits)))
         (when (null rest)
           (error "blc-decode: truncated after leading 0"))
         (if (zerop (first rest))
             (multiple-value-bind (body more) (%parse (rest rest))
               (values (tlam body) more))
             (multiple-value-bind (f more) (%parse (rest rest))
               (multiple-value-bind (a more2) (%parse more)
                 (values (tapp f a) more2))))))
      ;; 1^(n+1) 0 -> variable n
      (t
       (let ((ones 0)
             (cur bits))
         (loop while (and cur (= (first cur) 1))
               do (incf ones) (setf cur (rest cur)))
         (when (null cur)
           (error "blc-decode: variable not terminated by 0"))
         ;; cur now starts with the terminating 0
         (values (tvar (1- ones)) (rest cur)))))))

(defun blc-decode (bits)
  "Decode a bitstring BITS (list of 0/1) to a lambda term.
Signals an error if there are trailing bits left over."
  (multiple-value-bind (term rest) (%parse bits)
    (when rest
      (error "blc-decode: ~D trailing bit(s) after a complete term" (length rest)))
    term))

;;; ------------------------------------------------------------------
;;; Bitstring <-> "0010" string helpers.
;;; ------------------------------------------------------------------

(defun bits->string (bits)
  "Render a bitstring (list of 0/1) as a string of #\\0 / #\\1."
  (map 'string (lambda (b) (if (zerop b) #\0 #\1)) bits))

(defun string->bits (string)
  "Parse a string of #\\0 / #\\1 into a bitstring (list of 0/1)."
  (map 'list (lambda (c)
               (ecase c (#\0 0) (#\1 1)))
       string))

;;; ------------------------------------------------------------------
;;; The classic combinators and Church numerals, as terms.
;;; ------------------------------------------------------------------

(defun combinator-i ()
  "I = \\x.x  =  (:lam (:var 0))."
  (tlam (tvar 0)))

(defun combinator-k ()
  "K = \\x.\\y.x  =  (:lam (:lam (:var 1)))."
  (tlam (tlam (tvar 1))))

(defun combinator-s ()
  "S = \\x.\\y.\\z. (x z)(y z)."
  (tlam (tlam (tlam
    (tapp (tapp (tvar 2) (tvar 0))
          (tapp (tvar 1) (tvar 0)))))))

(defun church-numeral (n)
  "The Church numeral N = \\f.\\x. f^n x, in de Bruijn form (f=var 1, x=var 0)."
  (check-type n (integer 0))
  (tlam (tlam
    (let ((body (tvar 0)))
      (dotimes (i n body)
        (setf body (tapp (tvar 1) body)))))))
