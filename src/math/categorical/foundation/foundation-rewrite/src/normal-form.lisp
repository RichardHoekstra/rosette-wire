;;;; normal-form.lisp --- (Q, N) normal-form decomposition.
;;;;
;;;; Lean source: NormalForm.lean.
;;;;
;;;; Theorem (NormalForm.lean): every finite-dimensional pair (Q, N)
;;;; with N² = 0 decomposes uniquely as J₂^r ⊕ J₁^h, where J₂ is the
;;;; 2-dimensional nilpotent Jordan block and J₁ is the 1-dimensional
;;;; trivial block.  The complete invariant is the pair (q, h) with
;;;; q = 2r + h = dim Q.
;;;;
;;;; Operational form: we represent N as a Q-by-Q matrix of rationals
;;;; (or floats; we only need linear-algebra rank).  Then
;;;;   r = rank(N)               -- dimension of im(N) = #J₂ blocks
;;;;   h = q - 2r                -- dimension of ker(N) - im(N) = #J₁ blocks
;;;;   q = dim Q                 -- the row count of the matrix
;;;;
;;;; Invariant: rank(N²) = 0 ⇒ im(N) ⊆ ker(N) ⇒ rank(N) ≤ q - rank(N),
;;;; i.e. r ≤ q - r, equivalently 2r ≤ q, so h ≥ 0.

(in-package #:rosette-foundation-rewrite)

(defun matrix-dim (m)
  "Return (rows . cols) of matrix M (a 2D array)."
  (cons (array-dimension m 0) (array-dimension m 1)))

(defun zero-matrix-p (m &key (tol 1d-12))
  "Return T if every entry of matrix M is within TOL of zero."
  (let ((rows (array-dimension m 0))
        (cols (array-dimension m 1)))
    (dotimes (i rows t)
      (dotimes (j cols)
        (unless (<= (abs (coerce (aref m i j) 'double-float)) tol)
          (return-from zero-matrix-p nil))))))

(defun matrix-mul (a b)
  "Compute A·B for 2D arrays A and B, returning a fresh 2D array.
Coerces every entry to double-float."
  (let* ((m (array-dimension a 0))
         (k (array-dimension a 1))
         (k2 (array-dimension b 0))
         (n (array-dimension b 1)))
    (unless (= k k2)
      (error 'validate-failed
             :reason :matrix-mul-shape-mismatch
             :datum (list :a (cons m k) :b (cons k2 n))))
    (let ((c (make-array (list m n)
                         :element-type 'double-float
                         :initial-element 0d0)))
      (dotimes (i m)
        (dotimes (j n)
          (let ((acc 0d0))
            (dotimes (p k)
              (incf acc (* (coerce (aref a i p) 'double-float)
                           (coerce (aref b p j) 'double-float))))
            (setf (aref c i j) acc))))
      c)))

(defun matrix-rank (m &key (tol 1d-9))
  "Return the rank of M via plain Gaussian elimination over double-float
arithmetic.  TOL is the pivot-zero threshold.  Used for normal-form
decomposition where M is small (Lean examples are q ≤ 7)."
  (let* ((rows (array-dimension m 0))
         (cols (array-dimension m 1))
         (a (make-array (list rows cols) :element-type 'double-float)))
    ;; Copy M into a working double-float matrix.
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref a i j) (coerce (aref m i j) 'double-float))))
    (let ((rank 0)
          (col 0))
      (loop while (and (< rank rows) (< col cols))
            do (let ((pivot rank))
                 ;; Partial pivot.
                 (loop for r from rank below rows
                       when (> (abs (aref a r col)) (abs (aref a pivot col)))
                         do (setf pivot r))
                 (cond
                   ((<= (abs (aref a pivot col)) tol)
                    (incf col))
                   (t
                    ;; Swap rows RANK and PIVOT.
                    (unless (= pivot rank)
                      (loop for c from 0 below cols
                            for tmp = (aref a rank c)
                            do (setf (aref a rank c) (aref a pivot c))
                               (setf (aref a pivot c) tmp)))
                    ;; Eliminate below.
                    (loop for r from (1+ rank) below rows
                          for factor = (/ (aref a r col)
                                          (aref a rank col))
                          do (loop for c from col below cols
                                   do (decf (aref a r c)
                                            (* factor (aref a rank c)))))
                    (incf rank)
                    (incf col)))))
      rank)))

(defun normal-form-decompose (n-matrix)
  "Decompose the pair (Q, N) where N is given as the square matrix
N-MATRIX of size q×q.  Q itself is implicit: Q = R^q.

Returns two values (J2-RANK J1-HEIGHT) such that Q ≅ J₂^J2-RANK ⊕
J₁^J1-HEIGHT, with q = 2·J2-RANK + J1-HEIGHT.

Signals VALIDATE-FAILED if N is not square or N² ≠ 0.

Reference: NormalForm.lean, dim_formula and range_le_ker."
  (let* ((dim (matrix-dim n-matrix))
         (q (car dim)))
    (unless (= q (cdr dim))
      (error 'validate-failed
             :reason :n-not-square
             :datum dim))
    (let ((nn (matrix-mul n-matrix n-matrix)))
      (unless (zero-matrix-p nn)
        (error 'validate-failed
               :reason :n-squared-not-zero
               :datum nil)))
    (let* ((r (matrix-rank n-matrix))
           (h (- q (* 2 r))))
      (unless (>= h 0)
        (error 'validate-failed
               :reason (list :negative-h :q q :r r :h h)))
      (values r h))))

(defun normal-form-invariant (n-matrix)
  "Return the complete invariant (q . h) of the pair (Q, N), where q is
the dimension of Q and h is the cohomology dimension dim(ker N / im N).

By NormalForm.lean::dim_formula, q = 2r + h and the cell-count
invariant is exactly (q, h)."
  (multiple-value-bind (r h) (normal-form-decompose n-matrix)
    (cons (+ (* 2 r) h) h)))

(defun normal-form-equivalent-p (n1 n2)
  "Return T iff (Q₁, N₁) and (Q₂, N₂) have the same normal-form
invariant (q, h), i.e. they are NormalForm-equivalent.

This is the runtime witness of the Lean uniqueness theorem: two
square-zero modules are isomorphic exactly when (q, h) coincide."
  (equal (normal-form-invariant n1) (normal-form-invariant n2)))
