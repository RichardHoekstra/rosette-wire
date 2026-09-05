;;;; smith.lisp --- exact integer Smith normal form.
;;;;
;;;; Given an integer matrix A (m x n), compute unimodular U (m x m) and
;;;; V (n x n) and a diagonal D (m x n) with
;;;;
;;;;     D = U A V ,
;;;;
;;;; where D = diag(d_1, d_2, ..., d_r, 0, ..., 0) with each d_i a positive
;;;; integer and d_1 | d_2 | ... | d_r.  The d_i are the INVARIANT FACTORS;
;;;; r is the rank of A over Q.
;;;;
;;;; Algorithm: integer row/column reduction with gcd pivoting (the classic
;;;; Smith reduction).  Pure exact-integer (bignum) arithmetic; entry growth
;;;; is the well-known cost, so this is for SMALL matrices / small complexes.
;;;; O(n^3) elementary ops with bignum entries.
;;;;
;;;; This generalizes the inline integer SNF written by the HoTT-taste
;;;; witness (scratch-hott-taste.lisp), which returned only the invariant
;;;; factors; here we additionally accumulate the unimodular transforms U, V
;;;; (so D = U A V is checkable) and normalize the divisibility chain.

(in-package #:rosette-smith-normal-form)

;;; ------------------------------------------------------------------
;;; matrix representation: a simple 2D integer array (list-of-rows in / out)
;;; ------------------------------------------------------------------

(defun rows->array (rows)
  "ROWS is a list of equal-length integer lists.  Return a 2D integer array."
  (let* ((m (length rows))
         (n (if (zerop m) 0 (length (first rows))))
         (a (make-array (list m n) :element-type 'integer :initial-element 0)))
    (loop for r below m for row in rows do
      (loop for c below n for x in row do (setf (aref a r c) x)))
    a))

(defun array->rows (a)
  "2D integer array -> list of rows (each a list of integers)."
  (destructuring-bind (m n) (array-dimensions a)
    (loop for r below m collect
      (loop for c below n collect (aref a r c)))))

(defun identity-array (n)
  (let ((a (make-array (list n n) :element-type 'integer :initial-element 0)))
    (dotimes (i n) (setf (aref a i i) 1))
    a))

;;; exact integer matrix multiply (works on 2D integer arrays)
(defun mat-mul (a b)
  "Exact integer matrix product A B.  A is (m x k), B is (k x n).
Accepts list-of-rows or 2D arrays; returns a list-of-rows."
  (let ((aa (if (arrayp a) a (rows->array a)))
        (bb (if (arrayp b) b (rows->array b))))
    (destructuring-bind (m k) (array-dimensions aa)
      (destructuring-bind (k2 n) (array-dimensions bb)
        (assert (= k k2) () "mat-mul: inner dimensions ~D /= ~D" k k2)
        (let ((c (make-array (list m n) :element-type 'integer
                                        :initial-element 0)))
          (dotimes (i m)
            (dotimes (j n)
              (let ((s 0))
                (dotimes (p k) (incf s (* (aref aa i p) (aref bb p j))))
                (setf (aref c i j) s))))
          (array->rows c))))))

;;; exact integer determinant via fraction-free (Bareiss) elimination.
(defun mat-det (a)
  "Exact integer determinant of a square integer matrix (Bareiss algorithm)."
  (let ((m (if (arrayp a) a (rows->array a))))
    (destructuring-bind (n n2) (array-dimensions m)
      (assert (= n n2) () "mat-det: not square (~Dx~D)" n n2)
      (when (zerop n) (return-from mat-det 1))
      ;; work on a copy
      (let ((b (make-array (list n n) :element-type 'integer))
            (sign 1) (prev 1))
        (dotimes (i n) (dotimes (j n) (setf (aref b i j) (aref m i j))))
        (dotimes (k n)
          ;; pivot
          (when (zerop (aref b k k))
            (let ((p (loop for r from (1+ k) below n
                           when (not (zerop (aref b r k))) return r)))
              (unless p (return-from mat-det 0))
              (dotimes (c n) (rotatef (aref b k c) (aref b p c)))
              (setf sign (- sign))))
          (loop for i from (1+ k) below n do
            (loop for j from (1+ k) below n do
              (setf (aref b i j)
                    (truncate (- (* (aref b i j) (aref b k k))
                                 (* (aref b i k) (aref b k j)))
                              prev))))
          (setf prev (aref b k k)))
        (* sign (aref b (1- n) (1- n)))))))

(defun unimodular-p (a)
  "T iff A is a square integer matrix with determinant +/-1."
  (let ((aa (if (arrayp a) a (rows->array a))))
    (destructuring-bind (m n) (array-dimensions aa)
      (and (= m n) (member (mat-det aa) '(1 -1))))))

(defun diagonal-p (a)
  "T iff every off-diagonal entry of A (rectangular allowed) is zero."
  (let ((aa (if (arrayp a) a (rows->array a))))
    (destructuring-bind (m n) (array-dimensions aa)
      (dotimes (i m t)
        (dotimes (j n)
          (when (and (/= i j) (not (zerop (aref aa i j))))
            (return-from diagonal-p nil)))))))

(defun divisibility-chain-p (factors)
  "T iff FACTORS = (d_1 d_2 ...) satisfies d_1 | d_2 | ... (each divides next)."
  (loop for (d e) on factors
        when (and e (not (zerop (mod e d)))) do (return nil)
        finally (return t)))

;;; ------------------------------------------------------------------
;;; the SNF result
;;; ------------------------------------------------------------------

(defstruct (snf-result (:constructor %make-snf) (:conc-name snf-))
  "Result of SMITH-NORMAL-FORM.  D = U A V, U/V unimodular, D diagonal.
D, U, V are list-of-rows integer matrices.  INVARIANT-FACTORS is the list
of positive diagonal entries d_1 | d_2 | ... | d_r; RANK is r."
  d u v rank invariant-factors)

;;; ------------------------------------------------------------------
;;; the reduction
;;; ------------------------------------------------------------------

(defun smith-normal-form (matrix)
  "Compute the exact integer Smith normal form of MATRIX (a list of integer
rows).  Returns an SNF-RESULT with D = U A V, U/V unimodular, D diagonal with
invariant factors d_1 | d_2 | ... | d_r > 0.

Pure exact-integer (bignum) arithmetic.  O(n^3) elementary operations with
bignum entry growth -- intended for small matrices / small chain complexes."
  (let* ((a (rows->array matrix)))
    (destructuring-bind (m n) (array-dimensions a)
      (let ((u (identity-array m))     ; row ops accumulate here:  u A v = d
            (v (identity-array n)))     ; col ops accumulate here
        (labels ((swap-rows (i j)
                   (when (/= i j)
                     (dotimes (c n) (rotatef (aref a i c) (aref a j c)))
                     (dotimes (c m) (rotatef (aref u i c) (aref u j c)))))
                 (swap-cols (i j)
                   (when (/= i j)
                     (dotimes (r m) (rotatef (aref a r i) (aref a r j)))
                     (dotimes (r n) (rotatef (aref v r i) (aref v r j)))))
                 (negate-row (i)
                   (dotimes (c n) (setf (aref a i c) (- (aref a i c))))
                   (dotimes (c m) (setf (aref u i c) (- (aref u i c)))))
                 ;; row_dst += k * row_src   (left-multiply elementary)
                 (addmul-row (dst src k)
                   (dotimes (c n) (incf (aref a dst c) (* k (aref a src c))))
                   (dotimes (c m) (incf (aref u dst c) (* k (aref u src c)))))
                 ;; col_dst += k * col_src   (right-multiply elementary)
                 (addmul-col (dst src k)
                   (dotimes (r m) (incf (aref a r dst) (* k (aref a r src))))
                   (dotimes (r n) (incf (aref v r dst) (* k (aref v r src))))))
          (let ((rank 0)
                (lim (min m n)))
            ;; Phase 1: diagonalize via gcd pivoting on the lower-right block.
            (loop while (< rank lim) do
              ;; find any nonzero pivot in block [rank..m) x [rank..n)
              (let ((pr nil) (pc nil))
                (loop named find for r from rank below m do
                  (loop for c from rank below n do
                    (unless (zerop (aref a r c))
                      (setf pr r pc c) (return-from find))))
                (when (null pr) (return))      ; rest is all zero -> done
                (swap-rows rank pr)
                (swap-cols rank pc)
                ;; reduce pivot row/col by repeated gcd-style elimination
                ;; until the pivot divides everything in its row and column.
                (loop
                  (let ((done t))
                    ;; clear column `rank` using row `rank`
                    (loop for r below m do
                      (when (and (/= r rank) (not (zerop (aref a r rank))))
                        (let ((q (truncate (aref a r rank) (aref a rank rank))))
                          (addmul-row r rank (- q)))
                        (unless (zerop (aref a r rank))
                          (swap-rows rank r) (setf done nil))))
                    ;; clear row `rank` using column `rank`
                    (loop for c below n do
                      (when (and (/= c rank) (not (zerop (aref a rank c))))
                        (let ((q (truncate (aref a rank c) (aref a rank rank))))
                          (addmul-col c rank (- q)))
                        (unless (zerop (aref a rank c))
                          (swap-cols rank c) (setf done nil))))
                    (when done (return))))
                (when (minusp (aref a rank rank)) (negate-row rank))
                (incf rank)))
            ;; Phase 2: normalize the divisibility chain d_1 | d_2 | ... .
            ;; For adjacent diagonal entries (a,b) not satisfying a|b, fold
            ;; them so the pair becomes (gcd, lcm); repeat to a fixpoint.
            (let ((changed t))
              (loop while changed do
                (setf changed nil)
                (loop for i from 0 below (1- rank) do
                  (let ((a1 (aref a i i)) (a2 (aref a (1+ i) (1+ i))))
                    (unless (zerop (mod a2 a1))
                      ;; bring a2 into the i-th column, then re-run gcd
                      ;; elimination on the 2x2 block (i,i),(i+1,i+1).
                      (addmul-col i (1+ i) 1)   ; col_i += col_{i+1}
                      ;; now (i,i)=a1, (i+1,i)=a2 ; eliminate to gcd
                      (loop
                        (let ((done t))
                          (loop for r in (list i (1+ i)) do
                            (when (and (/= r i)
                                       (not (zerop (aref a r i))))
                              (let ((q (truncate (aref a r i)
                                                 (aref a i i))))
                                (addmul-row r i (- q)))
                              (unless (zerop (aref a r i))
                                (swap-rows i r) (setf done nil))))
                          (loop for c in (list i (1+ i)) do
                            (when (and (/= c i)
                                       (not (zerop (aref a i c))))
                              (let ((q (truncate (aref a i c)
                                                 (aref a i i))))
                                (addmul-col c i (- q)))
                              (unless (zerop (aref a i c))
                                (swap-cols i c) (setf done nil))))
                          (when done (return))))
                      (when (minusp (aref a i i)) (negate-row i))
                      (when (minusp (aref a (1+ i) (1+ i)))
                        (negate-row (1+ i)))
                      (setf changed t))))))
            (let ((factors (loop for i below rank collect (aref a i i))))
              (%make-snf :d (array->rows a)
                         :u (array->rows u)
                         :v (array->rows v)
                         :rank rank
                         :invariant-factors factors))))))))

(defun invariant-factors (matrix)
  "Convenience: just the invariant-factor list d_1 | d_2 | ... | d_r of MATRIX
(each a positive integer).  Drops the unimodular transforms."
  (snf-invariant-factors (smith-normal-form matrix)))
