;;;; builders.lisp --- spaces become chain complexes.
;;;;
;;;; SIMPLICIAL-COMPLEX builds the abstract simplicial chain complex from a
;;;; set of top simplices (each a tuple of vertex labels): it generates all
;;;; faces, orients them canonically (sorted vertex order), and fills the
;;;; boundary matrices with the standard alternating-sign incidence
;;;;
;;;;     d [v_0 ... v_k] = sum_i (-1)^i [v_0 ... v_i^ ... v_k].
;;;;
;;;; GRAPH-COMPLEX is the 1-dimensional case (vertices + edges): b_0 = number
;;;; of connected components, b_1 = number of independent cycles -- the graph
;;;; homology rosette-federation-homology and the Cech 1-skeleton report.
;;;;
;;;; CECH-1-COCHAIN-COMPLEX exhibits a Cech 1-skeleton (vertices V, edges E,
;;;; triangles T) -- the structure rosette-cech-cochain works with -- as a chain
;;;; complex, so its H^1 (the obstruction / inconsistency dimension) is the
;;;; first cohomology of THIS abstract complex.  This is the unification.

(in-package #:rosette-chain-complex)

;;; ------------------------------------------------------------------
;;; oriented simplicial complex from a set of top simplices
;;; ------------------------------------------------------------------

(defun %canon (simplex)
  "Canonical (sorted, deduplicated) vertex tuple for an oriented simplex."
  (sort (remove-duplicates (copy-list simplex)) #'< :key #'%vkey))

(defvar *vkey-table* nil)
(defun %vkey (v)
  "A stable integer key for a vertex label V (so non-numeric labels sort)."
  (if (realp v) v
      (let ((h (sxhash v))) h)))

(defun %faces (simplex)
  "The codimension-1 faces of an oriented k-simplex (a sorted tuple),
each paired with its sign (-1)^i: omit vertex i in turn."
  (loop for i from 0 below (length simplex)
        collect (cons (expt -1 i)
                      (append (subseq simplex 0 i)
                              (subseq simplex (1+ i))))))

(defun simplicial-complex (top-simplices)
  "Build the oriented simplicial chain complex of the complex whose maximal
simplices are TOP-SIMPLICES (each a list of vertex labels).  All faces are
generated and oriented by sorted vertex order; the boundary matrices carry
the alternating-sign incidence.  Returns a CHAIN-COMPLEX.

Example: ((a b c)) is a filled triangle (b_0=1, b_1=0, b_2=0); the three
edges without the face is a circle (b_0=1, b_1=1)."
  ;; 1. close under faces; bucket simplices by dimension.
  (let ((by-dim (make-hash-table :test 'eql)))   ; dim -> hash-set of canon tuples
    (labels ((ensure-dim (k)
               (or (gethash k by-dim)
                   (setf (gethash k by-dim)
                         (make-hash-table :test 'equal))))
             (add (s)
               (let* ((c (%canon s)) (k (1- (length c))))
                 (when (>= k 0)
                   (unless (gethash c (ensure-dim k))
                     (setf (gethash c (ensure-dim k)) t)
                     ;; recurse into faces
                     (when (>= k 1)
                       (dolist (sf (%faces c)) (add (cdr sf)))))))))
      (dolist (s top-simplices) (add s)))
    ;; 2. order simplices in each dimension; build index maps.
    (let* ((max-dim (loop for k being the hash-keys of by-dim maximize k))
           (ordered (make-array (1+ max-dim) :initial-element nil))
           (index   (make-array (1+ max-dim) :initial-element nil)))
      (loop for k from 0 to max-dim
            for tbl = (gethash k by-dim)
            for simps = (when tbl
                          (sort (loop for s being the hash-keys of tbl collect s)
                                #'list< ))
            do (setf (aref ordered k) simps)
               (let ((ix (make-hash-table :test 'equal)))
                 (loop for s in simps for j from 0 do (setf (gethash s ix) j))
                 (setf (aref index k) ix)))
      ;; 3. dims and boundary matrices.
      (let* ((dims (loop for k from 0 to max-dim
                         collect (length (aref ordered k))))
             (boundaries
               (loop for k from 1 to max-dim
                     collect (%boundary-matrix
                              (aref ordered k) (aref index (1- k))
                              (length (aref ordered (1- k)))))))
        (make-chain-complex boundaries dims)))))

(defun list< (a b)
  "Lexicographic order on vertex-key tuples A, B."
  (loop for x in a for y in b
        for kx = (%vkey x) for ky = (%vkey y)
        do (cond ((< kx ky) (return t))
                 ((> kx ky) (return nil)))
        finally (return (< (length a) (length b)))))

(defun %boundary-matrix (k-simps lower-index n-lower)
  "The boundary matrix d_k : C_k -> C_{k-1} as a list of N-LOWER rows, one
column per simplex in K-SIMPS.  Entry (face, simplex) = (-1)^i for the i-th
omitted-vertex face.  LOWER-INDEX maps a (k-1)-simplex to its row."
  (let* ((ncol (length k-simps))
         (rows (loop repeat n-lower collect (make-list ncol :initial-element 0)))
         (rowv (coerce rows 'vector)))
    (loop for s in k-simps for col from 0
          do (dolist (sf (%faces s))
               (let* ((sign (car sf))
                      (face (%canon (cdr sf)))
                      (row  (gethash face lower-index)))
                 (when row
                   (setf (nth col (aref rowv row))
                         (+ (nth col (aref rowv row)) sign))))))
    (coerce rowv 'list)))

;;; ------------------------------------------------------------------
;;; graph complex (the 1-skeleton): b_0 components, b_1 cycles
;;; ------------------------------------------------------------------

(defun graph-complex (vertices edges)
  "The chain complex of a graph: C_0 = VERTICES, C_1 = EDGES (each edge a
pair (u v) of vertex labels).  d_1 sends edge (u v) to v - u.  Then
b_0 = number of connected components, b_1 = number of independent cycles.
This is rosette-federation-homology's b_0/b_1 and rosette-graph-core's component
count, as one chain complex."
  (let* ((vidx (make-hash-table :test 'equal)))
    (loop for v in vertices for i from 0 do (setf (gethash v vidx) i))
    (let* ((nv (length vertices))
           (d1 (loop repeat nv collect (make-list (length edges) :initial-element 0)))
           (d1v (coerce d1 'vector)))
      (loop for (u v) in edges for col from 0
            for ui = (gethash u vidx) for vi = (gethash v vidx)
            do (when ui (incf (nth col (aref d1v ui)) -1))   ; -u
               (when vi (incf (nth col (aref d1v vi)) +1)))  ; +v
      (make-chain-complex (list (coerce d1v 'list))
                          (list nv (length edges))))))

;;; ------------------------------------------------------------------
;;; the Cech 1-skeleton adapter -- the unification
;;; ------------------------------------------------------------------

(defun cech-1-cochain-complex (vertices edges triangles)
  "A Cech 1-skeleton -- VERTICES, EDGES (pairs), TRIANGLES (vertex triples) --
as a chain complex (C_0 vertices, C_1 edges, C_2 triangles) with the
simplicial boundary.  Its cohomology H^1 = ker delta^1 / im delta^0 is the
first Cech cohomology that rosette-cech-cochain and rosette-knowledge-sheaf compute
as dim H^1: a non-zero H^1 is the obstruction / inconsistency certificate.

This is the unification: rosette-cech-cochain's signed coboundary H^1, the
knowledge-sheaf's inconsistency, and a simplicial complex's homology are the
SAME abstract (co)chain structure.  H^1 of this complex = b_1 (over a field,
H^1 = H_1)."
  (declare (ignorable vertices))
  ;; reuse the oriented simplicial machinery: edges are 1-simplices,
  ;; triangles are 2-simplices, isolated vertices are 0-simplices.
  (simplicial-complex
   (append (mapcar #'list vertices)
           (mapcar (lambda (e) (copy-list e)) edges)
           (mapcar (lambda (tr) (copy-list tr)) triangles))))
