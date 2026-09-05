;;;; complex.lisp --- the abstract chain complex object + the d^2 = 0 law.
;;;;
;;;; A chain complex is a sequence of free groups/spaces C_0, ..., C_N with
;;;; boundary maps d_n : C_n -> C_{n-1} (n = 1..N) such that
;;;;
;;;;     d_{n-1} d_n = 0      (the boundary of a boundary is zero).
;;;;
;;;; We store the boundary matrices as a list (d_1 d_2 ... d_N), each a
;;;; list-of-rows integer matrix with rows indexed by (n-1)-chains and
;;;; columns by n-chains -- exactly rosette-smith-normal-form's convention, so
;;;; the Z homology back-end consumes this object directly.

(in-package #:rosette-chain-complex)

(defstruct (chain-complex (:constructor %make-chain-complex))
  "An abstract chain complex C_0 <-d_1- C_1 <-d_2- ... <-d_N- C_N.
BOUNDARIES = (d_1 ... d_N), each d_n a list-of-rows matrix d_n : C_n -> C_{n-1}
  (NIL stands for the zero map).
DIMS       = (dim C_0 ... dim C_N), the rank of each chain group."
  (boundaries '() :type list)
  (dims       '() :type list))

(defun make-chain-complex (boundaries dims)
  "Build a chain complex from BOUNDARIES = (d_1 ... d_N) and DIMS =
(dim C_0 ... dim C_N).  Validates the shapes: d_n must have DIM C_{n-1} rows
and DIM C_n columns (a zero map may be given as NIL)."
  (let ((n (length boundaries)))
    (assert (= (length dims) (1+ n)) ()
            "DIMS must have one more entry than BOUNDARIES (C_0..C_N).")
    (loop for d in boundaries
          for k from 1
          for rows = (nth (1- k) dims)   ; dim C_{k-1}
          for cols = (nth k dims)         ; dim C_k
          do (when d
               (assert (= (%rows d) rows) ()
                       "d_~D must have ~D rows (= dim C_~D), got ~D."
                       k rows (1- k) (%rows d))
               (when (plusp (%rows d))
                 (assert (= (%cols d) cols) ()
                         "d_~D must have ~D columns (= dim C_~D), got ~D."
                         k cols k (%cols d)))))
    (%make-chain-complex :boundaries boundaries :dims dims)))

;; defstruct already provides CHAIN-COMPLEX-BOUNDARIES and CHAIN-COMPLEX-DIMS.

(defun chain-complex-top (cc)
  "The top dimension N (so the chain groups are C_0 .. C_N)."
  (1- (length (chain-complex-dims cc))))

(defun boundary-matrix (cc n)
  "The boundary matrix d_n : C_n -> C_{n-1} of CC (NIL if absent / zero)."
  (when (and (>= n 1) (<= n (length (chain-complex-boundaries cc))))
    (nth (1- n) (chain-complex-boundaries cc))))

;;; ------------------------------------------------------------------
;;; integer matrix product (list-of-rows), zero-map aware
;;; ------------------------------------------------------------------

(defun %matmul (a b)
  "Integer/rational matrix product A B for list-of-rows matrices.
A NIL factor (the zero map) yields NIL.  Inner dimensions must match."
  (when (or (null a) (null b)) (return-from %matmul nil))
  (let* ((bt (apply #'mapcar #'list b)))     ; columns of B
    (mapcar (lambda (arow)
              (mapcar (lambda (bcol)
                        (reduce #'+ (mapcar #'* arow bcol) :initial-value 0))
                      bt))
            a)))

(defun %zero-matrix-p (m)
  "T iff M is NIL or every entry is zero."
  (or (null m) (every (lambda (row) (every #'zerop row)) m)))

;;; ------------------------------------------------------------------
;;; the defining law:  d_{n-1} d_n = 0
;;; ------------------------------------------------------------------

(defun d-squared-residual (cc)
  "The composites d_{n-1} d_n for n = 2..N (each a list-of-rows matrix, or
NIL for a zero composite).  The defining law d^2 = 0 holds iff every one is
the zero matrix -- this returns the WITNESS, not just a boolean."
  (let ((bs (chain-complex-boundaries cc)))
    (loop for n from 2 to (length bs)
          for d-n   = (nth (1- n) bs)        ; d_n
          for d-n-1 = (nth (- n 2) bs)       ; d_{n-1}
          collect (cons n (%matmul d-n-1 d-n)))))

(defun d-squared-zero-p (cc)
  "T iff d_{n-1} d_n = 0 for every n -- the boundary of a boundary is zero.
This is the property that makes ker d_n >= im d_{n+1}, so the quotient
homology H_n = ker d_n / im d_{n+1} is well defined."
  (every (lambda (pair) (%zero-matrix-p (cdr pair)))
         (d-squared-residual cc)))
