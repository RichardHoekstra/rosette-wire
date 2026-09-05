;;;; homology.lisp --- integer homology with torsion, via Smith normal form.
;;;;
;;;; A chain complex
;;;;
;;;;     ... --d_{n+1}--> C_n --d_n--> C_{n-1} --> ... --d_1--> C_0 --> 0
;;;;
;;;; is given by its integer boundary matrices d_n (a list, index 0 = d_1,
;;;; index k = d_{k+1}).  Each d_n is an integer matrix with rows indexed by
;;;; the (n-1)-chains and columns by the n-chains: d_n : C_n -> C_{n-1}.
;;;;
;;;; The n-th integer homology is
;;;;
;;;;     H_n = ker(d_n) / im(d_{n+1})
;;;;         = Z^{beta_n}  (+)  Z/t_1 (+) ... (+) Z/t_s ,
;;;;
;;;; where beta_n = dim ker(d_n) - rank(d_{n+1}) is the n-th Betti number
;;;; (the FREE rank, exactly what the rational/float matrix-rank homology in
;;;; rosette-cech-cochain computes) and the torsion coefficients t_i > 1 are the
;;;; invariant factors of d_{n+1} that exceed 1.
;;;;
;;;; The TORSION is the headline: the rational/Betti homology MISSES it.
;;;;   - Klein bottle: H_1 = Z (+) Z/2   (beta_1 = 1 only over Q)
;;;;   - RP^2:         H_1 = Z/2         (beta_1 = 0 over Q -- pure torsion!)

(in-package #:rosette-smith-normal-form)

;;; ------------------------------------------------------------------
;;; a homology group:  Z^free-rank  (+)  (+)_i Z/t_i
;;; ------------------------------------------------------------------

(defstruct (homology-group (:constructor %make-hom))
  "An f.g. abelian group  Z^FREE-RANK (+) (+)_i Z/t_i.
TORSION is the list of torsion coefficients t_i > 1 (with multiplicity)."
  (free-rank 0 :type integer)
  (torsion '() :type list))

(defun hom-free-rank (g) (homology-group-free-rank g))
(defun hom-torsion (g)   (homology-group-torsion g))
(defun hom-betti (g)
  "The Betti number = free rank (what rational/float homology reports)."
  (homology-group-free-rank g))

(defun format-homology-group (g &optional (stream nil))
  "Pretty: Z^r (+) Z/t1 (+) ...  ('0' for the trivial group)."
  (let* ((r (hom-free-rank g)) (tors (hom-torsion g))
         (parts (append
                 (cond ((= r 0) '())
                       ((= r 1) (list "Z"))
                       (t (list (format nil "Z^~D" r))))
                 (mapcar (lambda (tk) (format nil "Z/~D" tk)) tors)))
         (s (if parts (format nil "~{~A~^ (+) ~}" parts) "0")))
    (if stream (write-string s stream) s)))

;;; ------------------------------------------------------------------
;;; ranks / nullities over Z (via SNF rank, which equals the Q-rank)
;;; ------------------------------------------------------------------

(defun %matrix-rows (d)
  "Rows count of a list-of-rows matrix D (0 if D is NIL / empty)."
  (length d))

(defun %matrix-cols (d)
  "Columns of a list-of-rows matrix D (0 if D is NIL / empty)."
  (if (or (null d) (null (first d))) 0 (length (first d))))

(defun %snf-of (d)
  "SNF result of D, or NIL when D has no entries."
  (when (and d (plusp (%matrix-cols d)) (plusp (%matrix-rows d)))
    (smith-normal-form d)))

;;; ------------------------------------------------------------------
;;; homology of one degree from (d_n, d_{n+1}, dim C_n)
;;; ------------------------------------------------------------------

(defun homology-group-at (d-n d-n+1 dim-cn)
  "The n-th integer homology group H_n = ker(d_n)/im(d_{n+1}).

D-N      = boundary matrix d_n : C_n -> C_{n-1}, or NIL if n = 0 (d_0 = 0).
D-N+1    = boundary matrix d_{n+1} : C_{n+1} -> C_n, or NIL if none.
DIM-CN   = number of n-chains (rank of the free group C_n).

Free rank = dim ker(d_n) - rank(d_{n+1}) = (DIM-CN - rank d_n) - rank d_{n+1}.
Torsion  = invariant factors of d_{n+1} that exceed 1 (the d_{n+1} factors;
the >1 invariant factors of the boundary map landing in C_n)."
  (let* ((snf-n   (%snf-of d-n))
         (snf-n+1 (%snf-of d-n+1))
         (rank-dn   (if snf-n   (snf-rank snf-n)   0))
         (rank-dn+1 (if snf-n+1 (snf-rank snf-n+1) 0))
         (free (- (- dim-cn rank-dn) rank-dn+1))
         (torsion (when snf-n+1
                    (remove-if (lambda (x) (<= x 1))
                               (snf-invariant-factors snf-n+1)))))
    (%make-hom :free-rank free :torsion torsion)))

;;; ------------------------------------------------------------------
;;; whole-complex integer homology
;;; ------------------------------------------------------------------

(defun integer-homology (boundary-matrices chain-dims)
  "Integer homology of a chain complex with TORSION.

BOUNDARY-MATRICES = (d_1 d_2 ... d_N), each a list-of-rows integer matrix,
d_k : C_k -> C_{k-1}.  Use NIL for a zero boundary map.
CHAIN-DIMS = (dim C_0  dim C_1 ... dim C_N), the rank of each chain group.

Returns a list (H_0 H_1 ... H_N) of HOMOLOGY-GROUP structs.  H_n carries the
free rank (= Betti number beta_n) and the torsion coefficients (> 1) -- the
torsion the rational/Betti homology of rosette-cech-cochain cannot see."
  (let* ((top (1- (length chain-dims))))
    (loop for n from 0 to top collect
      (let* ((d-n   (when (>= n 1) (nth (1- n) boundary-matrices)))   ; d_n
             (d-n+1 (when (<= (1+ n) top) (nth n boundary-matrices))) ; d_{n+1}
             (dim-cn (nth n chain-dims)))
        (homology-group-at d-n d-n+1 dim-cn)))))
