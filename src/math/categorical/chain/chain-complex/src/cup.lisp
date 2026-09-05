;;;; cup.lisp --- the simplicial CUP PRODUCT: cohomology as a graded RING.
;;;;
;;;; rosette-chain-complex computes the cohomology GROUPS H^n (the additive
;;;; invariant -- Betti numbers).  This file installs the missing
;;;; MULTIPLICATIVE structure: the simplicial cup product
;;;;
;;;;     cup : C^p x C^q -> C^{p+q},
;;;;
;;;; given by the ALEXANDER-WHITNEY formula on an ORDERED simplicial complex.
;;;; For a p-cochain a, a q-cochain b, and a (p+q)-simplex
;;;; sigma = [v_0 ... v_{p+q}] (vertices in increasing order):
;;;;
;;;;     (a cup b)(sigma) = a([v_0 .. v_p]) * b([v_p .. v_{p+q}])
;;;;
;;;; -- the FRONT p-face times the BACK q-face, sharing the middle vertex v_p.
;;;; The cup product is a cochain map (delta(a cup b) = delta a cup b +/-
;;;; a cup delta b), so it descends to a product on cohomology classes
;;;;
;;;;     cup : H^p x H^q -> H^{p+q},
;;;;
;;;; making H^*(X) a GRADED RING.  That ring is a finer invariant than the
;;;; Betti numbers: on the torus T^2 the two H^1 generators cup to the H^2
;;;; top class (nonzero), while on the wedge S^1 v S^1 v S^2 -- which has the
;;;; SAME Betti numbers (1,2,1) -- every product of 1-classes VANISHES.  The
;;;; ring distinguishes spaces the additive invariant cannot.
;;;;
;;;; SCOPE: the cup arithmetic here is over GF(2) (the field F_2).  This is
;;;; orientation-sign-free and already separates T^2 from the wedge; over
;;;; GF(2) the graded-commutativity sign (-1)^{pq} is +1, so [a cup b] =
;;;; [b cup a] as classes.  An exact-rational (Q) cup product is a clean
;;;; follow-up (it needs the Alexander-Whitney signs threaded through the
;;;; rational cocycle/coboundary spaces -- see the note at the foot).
;;;;
;;;; This promotes the light Alexander-Whitney implementation of
;;;; scratch-witness-compositionality-cup.lisp into the library, reusing the
;;;; lib's own oriented-simplex machinery (builders.lisp) rather than a
;;;; private re-implementation.

(in-package #:rosette-chain-complex)

;;; ==================================================================
;;; PART 1 -- the ORDERED simplicial cochain object.
;;;
;;; The chain-complex object stores only boundary MATRICES; the cup product
;;; needs the oriented SIMPLICES themselves (front/back faces of a tuple).
;;; SIMPLICIAL-COCHAINS retains, per dimension k, the ordered vector of
;;; canonical (sorted) k-simplices and a tuple->column index, alongside the
;;; chain complex (so Betti/cohomology dimensions come from the same object).
;;; ==================================================================

(defstruct (simplicial-cochains (:constructor %make-simplicial-cochains))
  "An oriented simplicial complex carrying BOTH its chain complex and the
ordered simplices the cup product acts on.
COMPLEX  = the CHAIN-COMPLEX (boundary matrices, dims) -- for cohomology dims.
ORDERED  = vector indexed by dimension k: ORDERED[k] is a vector of the
           canonical (sorted) k-simplex tuples, in the column order of C_k.
INDEX    = vector indexed by k: INDEX[k] maps a sorted k-tuple -> column.
MAXDIM   = the top dimension."
  (complex nil)
  (ordered #() :type simple-vector)
  (index   #() :type simple-vector)
  (maxdim  0   :type fixnum))

(defun simplicial-cochains (top-simplices)
  "Build the oriented simplicial complex of TOP-SIMPLICES (each a vertex
tuple) as a SIMPLICIAL-COCHAINS object: the same oriented complex that
SIMPLICIAL-COMPLEX builds, but retaining the ordered simplices the cup
product needs.  The embedded CHAIN-COMPLEX gives Betti/cohomology dimensions
identical to (SIMPLICIAL-COMPLEX TOP-SIMPLICES)."
  ;; close under faces, bucket by dimension -- mirrors SIMPLICIAL-COMPLEX.
  (let ((by-dim (make-hash-table :test 'eql)))
    (labels ((ensure-dim (k)
               (or (gethash k by-dim)
                   (setf (gethash k by-dim) (make-hash-table :test 'equal))))
             (add (s)
               (let* ((c (%canon s)) (k (1- (length c))))
                 (when (>= k 0)
                   (unless (gethash c (ensure-dim k))
                     (setf (gethash c (ensure-dim k)) t)
                     (when (>= k 1)
                       (dolist (sf (%faces c)) (add (cdr sf)))))))))
      (dolist (s top-simplices) (add s)))
    (let* ((max-dim (loop for k being the hash-keys of by-dim maximize k))
           (ordered (make-array (1+ max-dim) :initial-element #()))
           (index   (make-array (1+ max-dim) :initial-element nil)))
      (loop for k from 0 to max-dim
            for tbl = (gethash k by-dim)
            for simps = (when tbl
                          (sort (loop for s being the hash-keys of tbl collect s)
                                #'list<))
            do (let ((ix (make-hash-table :test 'equal))
                     (vec (coerce simps 'vector)))
                 (loop for s across vec for j from 0 do (setf (gethash s ix) j))
                 (setf (aref ordered k) vec
                       (aref index   k) ix)))
      ;; dims + boundary matrices, exactly as SIMPLICIAL-COMPLEX does.
      (let* ((dims (loop for k from 0 to max-dim
                         collect (length (aref ordered k))))
             (boundaries
               (loop for k from 1 to max-dim
                     collect (%boundary-matrix
                              (coerce (aref ordered k) 'list)
                              (aref index (1- k))
                              (length (aref ordered (1- k)))))))
        (%make-simplicial-cochains
         :complex (make-chain-complex boundaries dims)
         :ordered ordered :index index :maxdim max-dim)))))

(defun sc-complex (sc) "The embedded CHAIN-COMPLEX." (simplicial-cochains-complex sc))

(defun sc-n-simplices (sc k)
  "Number of k-simplices (= dim C_k)."
  (if (<= 0 k (simplicial-cochains-maxdim sc))
      (length (aref (simplicial-cochains-ordered sc) k))
      0))

(defun sc-simplex-col (sc tuple)
  "Column of the canonical k-simplex TUPLE (sorted), or NIL if absent."
  (let ((k (1- (length tuple))))
    (when (<= 0 k (simplicial-cochains-maxdim sc))
      (gethash tuple (aref (simplicial-cochains-index sc) k)))))

;;; ==================================================================
;;; PART 2 -- GF(2) cochains and the cocycle / coboundary spaces.
;;;
;;; A degree-k cochain is a bit-list over the k-simplices (length = dim C_k).
;;; The coboundary delta^k : C^k -> C^{k+1} is the TRANSPOSE of d_{k+1}; a
;;; class is nonzero in H^k iff its representative is a cocycle (delta = 0)
;;; that is not a coboundary.  We reuse the lib's boundary matrices and a
;;; small self-contained GF(2) reducer (no external deps).
;;; ==================================================================

(defun %coboundary-rows (sc k)
  "delta^k : C^k -> C^{k+1} as a list of rows (one per (k+1)-simplex),
each row of length (dim C_k), over GF(2).  This is the transpose of
d_{k+1}.  NIL if there are no (k+1)-simplices."
  (let ((d (boundary-matrix (sc-complex sc) (1+ k))))  ; d_{k+1}: C_{k+1}->C_k
    (when d
      ;; d_{k+1} has rows = k-simplices, cols = (k+1)-simplices; delta^k is
      ;; its transpose: rows = (k+1)-simplices, cols = k-simplices.
      (let ((dt (%transpose d)))
        (mapcar (lambda (row) (mapcar (lambda (x) (mod x 2)) row)) dt)))))

;;; --- a minimal GF(2) RREF reducer (pivot-col -> reduced bit-list) -----
(defun %g2-make-reducer () (make-hash-table :test 'eql))

(defun %g2-reduce (red v)
  "Residual of bit-list V against reducer RED."
  (let ((w (copy-list v)))
    (loop for p = (position 1 w)
          while p
          for b = (gethash p red)
          while b
          do (setf w (mapcar #'logxor w b)))
    w))

(defun %g2-add! (red v)
  "Add V to RED; return the new pivot column if independent, else NIL."
  (let* ((w (%g2-reduce red v))
         (p (position 1 w)))
    (when p
      (maphash (lambda (q b)
                 (when (= 1 (nth p b))
                   (setf (gethash q red) (mapcar #'logxor b w))))
               red)
      (setf (gethash p red) w)
      p)))

(defun %g2-rank (red) (hash-table-count red))

(defun %g2-build (vectors)
  (let ((red (%g2-make-reducer)))
    (dolist (v vectors) (%g2-add! red v))
    red))

(defun %g2-in-span-p (red v)
  "T iff bit-list V lies in the span captured by RED."
  (every #'zerop (%g2-reduce red v)))

;;; --- null space of a GF(2) matrix (the cocycle space ker delta) -------
(defun %g2-null-space (m ncol)
  "Basis (list of bit-lists, length NCOL) of ker(M) over GF(2).
M = list-of-rows already mod 2 (or NIL = zero map: kernel = all of F_2^ncol)."
  (when (or (null m) (zerop ncol))
    (return-from %g2-null-space
      (loop for j below ncol
            collect (let ((e (make-list ncol :initial-element 0)))
                      (setf (nth j e) 1) e))))
  (let* ((a (mapcar #'copy-list m))
         (nrow (length a))
         (where (make-array ncol :initial-element -1))
         (row 0))
    (loop for col below ncol while (< row nrow)
          do (let ((sel nil))
               (loop for i from row below nrow
                     when (= 1 (nth col (nth i a))) do (setf sel i) (return))
               (when sel
                 (rotatef (nth sel a) (nth row a))
                 (loop for i below nrow unless (= i row)
                       when (= 1 (nth col (nth i a)))
                         do (setf (nth i a) (mapcar #'logxor (nth i a) (nth row a))))
                 (setf (aref where col) row)
                 (incf row))))
    (let ((basis '()))
      (loop for fc below ncol
            when (= -1 (aref where fc))
              do (let ((v (make-list ncol :initial-element 0)))
                   (setf (nth fc v) 1)
                   (loop for col below ncol for prow = (aref where col)
                         when (and (>= prow 0) (= 1 (nth fc (nth prow a))))
                           do (setf (nth col v) 1))
                   (push v basis)))
      (nreverse basis))))

(defun %cocycle-space (sc k)
  "Basis of the k-cocycles Z^k = ker delta^k over GF(2) (bit-lists)."
  (%g2-null-space (%coboundary-rows sc k) (sc-n-simplices sc k)))

(defun %coboundary-space (sc k)
  "Reducer holding the k-coboundaries B^k = im delta^{k-1} over GF(2).
B^k is the COLUMN space of delta^{k-1} (a coboundary is delta(alpha), a
vector over the k-simplices).  B^0 = 0."
  (let ((m (%coboundary-rows sc (1- k))))
    (if (null m)
        (%g2-make-reducer)
        (%g2-build (apply #'mapcar #'list m)))))  ; columns of delta^{k-1}

;;; ==================================================================
;;; PART 3 -- cohomology dimensions and a generator basis (GF(2)).
;;; ==================================================================

(defun gf2-betti (sc k)
  "dim H^k over GF(2) = dim Z^k - dim B^k.  (Over a field with characteristic
2 this matches the rational Betti number for the standard test spaces.)"
  (- (length (%cocycle-space sc k))
     (%g2-rank (%coboundary-space sc k))))

(defun cohomology-generators (sc k)
  "Representative k-cocycles (bit-lists) whose CLASSES form a basis of H^k
over GF(2): seed a reducer with B^k, then add Z^k vectors; each one that
raises the rank is a fresh generator representative."
  (let ((red (%coboundary-space sc k))   ; seeded with B^k
        (reps '()))
    (dolist (z (%cocycle-space sc k))
      (when (%g2-add! red z)             ; independent of B^k + earlier reps
        (push z reps)))
    (nreverse reps)))

;;; ==================================================================
;;; PART 4 -- THE CUP PRODUCT (Alexander-Whitney), over GF(2).
;;; ==================================================================

(defun %cochain-value (cochain sc k tuple)
  "Value of degree-k COCHAIN (bit-list) on the sorted k-TUPLE (0 if absent)."
  (let ((col (gethash tuple (aref (simplicial-cochains-index sc) k))))
    (if col (nth col cochain) 0)))

(defun cochain-cup (sc alpha p beta q)
  "The cochain-level cup product (ALPHA cup BETA): a (p+q)-cochain (bit-list)
over GF(2).  ALPHA is a p-cochain, BETA a q-cochain (bit-lists over the p-
and q-simplices of SC).  Alexander-Whitney: on a (p+q)-simplex
sigma = [v_0..v_{p+q}], the value is ALPHA(front p-face) * BETA(back q-face),
front = [v_0..v_p], back = [v_p..v_{p+q}]."
  (let* ((deg (+ p q))
         (n (sc-n-simplices sc deg))
         (out (make-list n :initial-element 0)))
    (when (plusp n)
      (loop for sigma across (aref (simplicial-cochains-ordered sc) deg)
            for col = (gethash sigma (aref (simplicial-cochains-index sc) deg))
            do (let* ((front (subseq sigma 0 (1+ p)))   ; v_0..v_p
                      (back  (subseq sigma p))          ; v_p..v_{p+q}
                      (av (%cochain-value alpha sc p front))
                      (bv (%cochain-value beta  sc q back)))
                 (setf (nth col out) (mod (* av bv) 2)))))
    out))

(defun cochain-cocycle-p (sc cochain k)
  "T iff the k-COCHAIN is a cocycle (delta^k cochain = 0) over GF(2)."
  (let ((m (%coboundary-rows sc k)))
    (or (null m)
        (every (lambda (row)
                 (zerop (mod (reduce #'+ (mapcar #'* cochain row)) 2)))
               m))))

(defun cochain-coboundary-p (sc cochain k)
  "T iff the k-COCHAIN is a coboundary (lies in B^k) over GF(2)."
  (%g2-in-span-p (%coboundary-space sc k) cochain))

(defun cohomology-nonzero-p (sc cochain k)
  "T iff the k-COCHAIN represents a NON-ZERO class in H^k over GF(2):
a cocycle that is not a coboundary."
  (and (cochain-cocycle-p sc cochain k)
       (not (cochain-coboundary-p sc cochain k))))

(defun cohomology-cohomologous-p (sc a b k)
  "T iff k-cochains A and B represent the SAME class in H^k over GF(2)
(their difference, = XOR over GF(2), is a coboundary)."
  (cochain-coboundary-p sc (mapcar #'logxor a b) k))

(defun cohomology-cup (sc alpha p beta q)
  "The induced RING product on cohomology classes: returns a (p+q)-cochain
representing [ALPHA] cup [BETA] in H^{p+q}, and -- as a second value -- T iff
that class is NON-ZERO in H^{p+q}.  ALPHA, BETA should be cocycle
representatives (e.g. from COHOMOLOGY-GENERATORS); the cup of cocycles is a
cocycle, and its class depends only on the classes of ALPHA, BETA."
  (let ((prod (cochain-cup sc alpha p beta q)))
    (values prod (cohomology-nonzero-p sc prod (+ p q)))))

;;; ==================================================================
;;; PART 5 -- the cohomology RING as a graded structure.
;;;
;;; A finite presentation of the GF(2) cohomology ring: per degree, the
;;; generator representatives and dim H^k, plus the cup-form
;;;     M[i][j] = [g_i cup g_j] in H^{deg(g_i)+deg(g_j)]
;;; on the degree-1 generators -- the structure constants that distinguish
;;; T^2 (nonzero form) from the wedge (zero form).
;;; ==================================================================

(defstruct (cohomology-ring (:constructor %make-cohomology-ring))
  "The GF(2) cohomology ring of a SIMPLICIAL-COCHAINS object.
COCHAINS    = the underlying simplicial complex.
DIMENSIONS  = (dim H^0 dim H^1 ... dim H^N) over GF(2).
GENERATORS  = vector indexed by k: representative k-cocycles for a basis of H^k.
H1-CUP-FORM = matrix M[i][j] (0/1) = whether [g_i cup g_j] is nonzero in H^2,
              over the degree-1 generators."
  (cochains nil)
  (dimensions '() :type list)
  (generators #() :type simple-vector)
  (h1-cup-form '() :type list))

(defun cohomology-ring (sc)
  "Build the graded GF(2) COHOMOLOGY-RING of SC: the per-degree dimensions
and generator representatives, and the H^1 x H^1 -> H^2 cup-form whose
non-vanishing is the multiplicative invariant beyond the Betti numbers."
  (let* ((maxdim (simplicial-cochains-maxdim sc))
         (gens (make-array (1+ maxdim)))
         (dims '()))
    (loop for k from 0 to maxdim
          do (setf (aref gens k) (cohomology-generators sc k))
             (push (gf2-betti sc k) dims))
    (let* ((g1 (aref gens 1))
           (form (loop for gi in g1
                       collect (loop for gj in g1
                                     collect (if (nth-value 1 (cohomology-cup sc gi 1 gj 1))
                                                 1 0)))))
      (%make-cohomology-ring
       :cochains sc
       :dimensions (nreverse dims)
       :generators gens
       :h1-cup-form form))))

(defun cohomology-ring-generators-in-degree (ring k)
  "The representative k-cocycle generators of RING in degree K."
  (let ((g (cohomology-ring-generators ring)))
    (if (<= 0 k (1- (length g))) (aref g k) '())))

(defun cup-form-nonzero-p (form)
  "T iff the cup-form FORM (a 0/1 matrix) has any nonzero entry -- i.e. some
product of degree-1 generators is a nonzero higher class."
  (some (lambda (r) (some (lambda (x) (= x 1)) r)) form))

;;; ==================================================================
;;; FOLLOW-UP (Q / rational cup product).  The Alexander-Whitney formula is
;;; defined over any coefficient ring, and the lib's boundary matrices are
;;; exact-integer, so a rational cup product is structurally available: build
;;; Z^k / B^k over Q (the lib already has FIELD-RANK / FIELD-NULLITY), thread
;;; the AW value with signs through %COCHAIN-VALUE, and replace the GF(2)
;;; reducer with rational RREF.  Over Q the graded-commutativity sign
;;; (-1)^{pq} becomes visible (a cup b = -(b cup a) in degree 1), which GF(2)
;;; collapses.  Left as a clean extension; GF(2) already realises the ring
;;; and the T^2-vs-wedge discrimination.
;;; ==================================================================
