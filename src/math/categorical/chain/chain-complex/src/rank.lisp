;;;; rank.lisp --- exact rational rank and nullity over a field.
;;;;
;;;; Homology over a FIELD is rank-nullity (pure linear algebra): the Betti
;;;; number b_n = dim ker(d_n) - rank(d_{n+1}) = (dim C_n - rank d_n) -
;;;; rank d_{n+1}.  We compute the rank by exact rational Gaussian
;;;; elimination -- no floating tolerance, so the Betti numbers of an
;;;; integer complex are EXACT (the circle/sphere/torus come out as integers
;;;; with no rounding).  Over Q this rank also equals the integer SNF rank,
;;;; so the free part agrees with rosette-smith-normal-form by construction.
;;;;
;;;; Matrices here are LISTS OF ROWS (matching rosette-smith-normal-form's
;;;; convention), each row a list of rationals/integers.  NIL or the empty
;;;; matrix is the zero map (rank 0).

(in-package #:rosette-chain-complex)

(defun %rows (d) (length d))
(defun %cols (d) (if (or (null d) (null (first d))) 0 (length (first d))))

(defun field-rank (d)
  "Exact rational row-rank of the list-of-rows matrix D (a field-rank, by
Gaussian elimination over Q).  NIL / an empty matrix has rank 0.  This is
the rank that makes the Betti numbers exact for an integer complex; over Q
it equals the integer Smith-normal-form rank, so the FREE part agrees with
rosette-smith-normal-form."
  (let* ((rows (%rows d))
         (cols (%cols d)))
    (when (or (zerop rows) (zerop cols))
      (return-from field-rank 0))
    ;; mutable rational copy
    (let ((a (make-array rows)))
      (loop for r in d for i from 0
            do (setf (aref a i) (coerce-row r)))
      (let ((rank 0))
        (loop for col below cols
              while (< rank rows)
              do (let ((pivot nil))
                   ;; find a nonzero pivot at/below the current rank row
                   (loop for row from rank below rows
                         when (not (zerop (aref (aref a row) col)))
                           do (setf pivot row) (return))
                   (when pivot
                     (rotatef (aref a pivot) (aref a rank))
                     (let* ((prow (aref a rank))
                            (lead (aref prow col)))
                       ;; eliminate this column from every other row
                       (loop for row below rows
                             unless (= row rank)
                               do (let* ((rrow (aref a row))
                                         (factor (aref rrow col)))
                                    (unless (zerop factor)
                                      (let ((mult (/ factor lead)))
                                        (loop for j from col below cols
                                              do (decf (aref rrow j)
                                                       (* mult (aref prow j)))))))))
                     (incf rank))))
        rank))))

(defun coerce-row (row)
  "A simple-vector of exact rationals from a list ROW."
  (let ((v (make-array (length row))))
    (loop for x in row for j from 0
          do (setf (aref v j) (if (rationalp x) x (rationalize x))))
    v))

(defun field-nullity (d n-cols)
  "Dimension of ker(D) over a field, where D has N-COLS columns:
nullity = N-COLS - rank(D).  N-COLS is the dimension of the source space
(needed because a NIL / zero map carries no column count of its own)."
  (- n-cols (field-rank d)))
