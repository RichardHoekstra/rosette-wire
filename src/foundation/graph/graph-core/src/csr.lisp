;;;; csr.lisp --- compressed sparse row adjacency representation.
;;;;
;;;; A CSR-GRAPH stores the out-adjacency of a (directed or undirected)
;;;; graph on N vertices indexed 0..N-1 using two parallel arrays:
;;;;
;;;;   row-offsets : (simple-array (unsigned-byte 32) (N+1))
;;;;   col-indices : (simple-array (unsigned-byte 32) (M))
;;;;
;;;; Edge i (0 <= i < M) goes from u to col-indices[i] where u is the
;;;; unique vertex such that row-offsets[u] <= i < row-offsets[u+1].
;;;; Out-neighbors of u therefore live in col-indices on the half-open
;;;; range [row-offsets[u], row-offsets[u+1]).
;;;;
;;;; Optional edge weights live in a parallel double-float array
;;;; aligned with col-indices.  Undirected graphs are stored as
;;;; symmetric directed graphs (each undirected edge appears twice).

(in-package #:rosette-graph-core)

(deftype index-array () '(simple-array (unsigned-byte 32) (*)))

(declaim (inline %u32))
(defun %u32 (x)
  "Coerce X to an (unsigned-byte 32) index."
  (the (unsigned-byte 32) (logand #xFFFFFFFF (the integer x))))

(defun %make-u32-array (length &key (initial-element 0))
  (make-array length :element-type '(unsigned-byte 32)
                     :initial-element (%u32 initial-element)))

(defstruct (csr-graph
            (:conc-name csr-)
            (:constructor %make-csr-graph)
            (:copier nil))
  "Compressed-sparse-row adjacency primitive.

NUM-VERTICES   : non-negative integer N
ROW-OFFSETS    : (unsigned-byte 32) vector of length N+1
COL-INDICES    : (unsigned-byte 32) vector of length M = num-edges
EDGE-WEIGHTS   : NIL or double-float vector of length M
DIRECTED-P     : T for digraph, NIL for symmetric undirected storage"
  (num-vertices 0 :type (unsigned-byte 32))
  (row-offsets  (%make-u32-array 1) :type index-array)
  (col-indices  (%make-u32-array 0) :type index-array)
  (edge-weights nil :type (or null (simple-array double-float (*))))
  (directed-p   t   :type boolean))

(defun make-csr-graph (num-vertices row-offsets col-indices
                       &key edge-weights (directed-p t))
  "Wrap user-supplied CSR vectors into a CSR-GRAPH.

The arrays are coerced to the canonical typed-array shapes.  No
structural validation is performed beyond length agreement; callers
typically construct via CSR-FROM-EDGES or CSR-FROM-ADJACENCY instead."
  (let* ((n   (%u32 num-vertices))
         (ro  (coerce row-offsets 'index-array))
         (ci  (coerce col-indices 'index-array))
         (ew  (when edge-weights
                (coerce edge-weights '(simple-array double-float (*))))))
    (unless (= (length ro) (1+ n))
      (error "row-offsets length ~D /= num-vertices+1 ~D" (length ro) (1+ n)))
    (when (and ew (/= (length ew) (length ci)))
      (error "edge-weights length ~D /= col-indices length ~D"
             (length ew) (length ci)))
    (%make-csr-graph :num-vertices n
                     :row-offsets ro
                     :col-indices ci
                     :edge-weights ew
                     :directed-p directed-p)))

(defun csr-num-edges (graph)
  "Number of stored edges (twice the undirected count for undirected graphs)."
  (length (csr-col-indices graph)))

(defun csr-out-degree (graph u)
  "Number of out-neighbors of vertex U."
  (let ((ro (csr-row-offsets graph)))
    (- (aref ro (1+ u)) (aref ro u))))

(defun csr-neighbors (graph u)
  "Return out-neighbors of U as a fresh simple-vector of fixnum indices."
  (let* ((ro (csr-row-offsets graph))
         (ci (csr-col-indices graph))
         (start (aref ro u))
         (end   (aref ro (1+ u)))
         (out (make-array (- end start))))
    (loop for k from start below end
          for j from 0
          do (setf (svref out j) (aref ci k)))
    out))

(defun csr-edge-weight (graph edge-index)
  "Return the weight of stored edge EDGE-INDEX, or 1d0 if unweighted."
  (let ((ew (csr-edge-weights graph)))
    (if ew (aref ew edge-index) 1d0)))

(defmacro csr-do-neighbors ((neighbor-var graph u
                             &key (edge-index-var nil)
                                  (weight-var     nil))
                            &body body)
  "Iterate BODY over out-neighbors of U in GRAPH.

NEIGHBOR-VAR is bound to each neighbor index in turn.  When
EDGE-INDEX-VAR is supplied it is bound to the edge slot in
COL-INDICES; WEIGHT-VAR is bound to the corresponding edge weight
(1d0 for unweighted graphs)."
  (let ((g-sym  (gensym "GRAPH"))
        (ro-sym (gensym "RO"))
        (ci-sym (gensym "CI"))
        (ew-sym (gensym "EW"))
        (k-sym  (or edge-index-var (gensym "K")))
        (u-sym  (gensym "U"))
        (lo-sym (gensym "LO"))
        (hi-sym (gensym "HI")))
    `(let* ((,g-sym  ,graph)
            (,u-sym  ,u)
            (,ro-sym (csr-row-offsets ,g-sym))
            (,ci-sym (csr-col-indices ,g-sym))
            (,ew-sym (csr-edge-weights ,g-sym))
            (,lo-sym (aref ,ro-sym ,u-sym))
            (,hi-sym (aref ,ro-sym (1+ ,u-sym))))
       (declare (ignorable ,ew-sym))
       (loop for ,k-sym from ,lo-sym below ,hi-sym
             do (let ((,neighbor-var (aref ,ci-sym ,k-sym))
                      ,@(when weight-var
                          `((,weight-var (if ,ew-sym
                                             (aref ,ew-sym ,k-sym)
                                             1d0)))))
                  ,@body)))))

;;;; -------------------------------------------------------------- builders

(defun %edges->csr (n edges weights directed-p)
  "Counting-sort edges into CSR layout.

EDGES is a sequence whose elements are (u . v) cons cells or 2-lists
or 2-vectors.  WEIGHTS is NIL or a sequence parallel to EDGES.  For
undirected graphs the reverse edge is added automatically."
  (let* ((m-in (length edges))
         (m    (if directed-p m-in (* 2 m-in)))
         (deg  (%make-u32-array n))
         (ro   (%make-u32-array (1+ n)))
         (ci   (%make-u32-array m))
         (ew   (when weights
                 (make-array m :element-type 'double-float
                               :initial-element 0d0))))
    (labels ((edge-uv (e)
               (etypecase e
                 (cons (if (consp (cdr e))
                           (values (first e) (second e))
                           (values (car e) (cdr e))))
                 (vector (values (aref e 0) (aref e 1)))))
             (bump (u)
               (setf (aref deg u) (%u32 (1+ (aref deg u))))))
      (map nil (lambda (e)
                 (multiple-value-bind (u v) (edge-uv e)
                   (bump u)
                   (unless directed-p (bump v))))
           edges)
      (let ((acc 0))
        (dotimes (u n)
          (setf (aref ro u) (%u32 acc))
          (incf acc (aref deg u)))
        (setf (aref ro n) (%u32 acc)))
      ;; cursor[u] = next free slot
      (let ((cursor (copy-seq ro)))
        (flet ((emit (u v w)
                 (let ((slot (aref cursor u)))
                   (setf (aref ci slot) (%u32 v))
                   (when ew (setf (aref ew slot) (coerce w 'double-float)))
                   (setf (aref cursor u) (%u32 (1+ slot))))))
          (if weights
              (map nil (lambda (e w)
                         (multiple-value-bind (u v) (edge-uv e)
                           (emit u v w)
                           (unless directed-p (emit v u w))))
                   edges weights)
              (map nil (lambda (e)
                         (multiple-value-bind (u v) (edge-uv e)
                           (emit u v 1d0)
                           (unless directed-p (emit v u 1d0))))
                   edges))))
      (values ro ci ew))))

(defun csr-from-edges (num-vertices edges
                       &key weights (directed-p t))
  "Build a CSR-GRAPH on NUM-VERTICES from an EDGES sequence.

Each edge is (u . v), (u v), or #(u v).  WEIGHTS, when supplied, is a
parallel sequence of real numbers, stored as double-float."
  (multiple-value-bind (ro ci ew)
      (%edges->csr num-vertices edges weights directed-p)
    (%make-csr-graph :num-vertices (%u32 num-vertices)
                     :row-offsets ro
                     :col-indices ci
                     :edge-weights ew
                     :directed-p directed-p)))

(defun csr-from-adjacency (adjacency &key weights (directed-p t))
  "Build a CSR-GRAPH from per-vertex out-neighbor lists.

ADJACENCY is a sequence of length N whose i-th element is a sequence
of vertex indices that are out-neighbors of i.  WEIGHTS, when
supplied, is a same-shaped sequence of edge weights aligned to
ADJACENCY.  For undirected graphs each row is treated as already
listing the half-edges; the reverse half is added automatically.
The resulting CSR has the same per-row order as ADJACENCY."
  (let* ((n (length adjacency))
         (edges '())
         (ws    (when weights '())))
    (loop for u from 0
          for row across (coerce adjacency 'vector)
          for wrow = (when weights (elt (coerce weights 'vector) u))
          do (loop for v in (coerce row 'list)
                   for k from 0
                   do (push (cons u v) edges)
                      (when weights
                        (push (elt (coerce wrow 'vector) k) ws))))
    (csr-from-edges n (nreverse edges)
                    :weights (when weights (nreverse ws))
                    :directed-p directed-p)))

;;;; ---------------------------------------------------------------- transpose

(defun csr-transpose (graph)
  "Return the CSR-GRAPH with all directed edges reversed.

For an undirected (symmetric) graph this is structurally identical to
the input.  Edge weights are carried over; the new graph keeps the
DIRECTED-P flag of the input."
  (let* ((n  (csr-num-vertices graph))
         (ro (csr-row-offsets graph))
         (ci (csr-col-indices graph))
         (ew (csr-edge-weights graph))
         (m  (length ci))
         (in-deg (%make-u32-array n))
         (t-ro   (%make-u32-array (1+ n)))
         (t-ci   (%make-u32-array m))
         (t-ew   (when ew (make-array m :element-type 'double-float
                                        :initial-element 0d0))))
    (dotimes (k m)
      (let ((v (aref ci k)))
        (setf (aref in-deg v) (%u32 (1+ (aref in-deg v))))))
    (let ((acc 0))
      (dotimes (v n)
        (setf (aref t-ro v) (%u32 acc))
        (incf acc (aref in-deg v)))
      (setf (aref t-ro n) (%u32 acc)))
    (let ((cursor (copy-seq t-ro)))
      (dotimes (u n)
        (loop for k from (aref ro u) below (aref ro (1+ u))
              for v = (aref ci k)
              for slot = (aref cursor v)
              do (setf (aref t-ci slot) (%u32 u))
                 (when t-ew (setf (aref t-ew slot) (aref ew k)))
                 (setf (aref cursor v) (%u32 (1+ slot))))))
    (%make-csr-graph :num-vertices n
                     :row-offsets t-ro
                     :col-indices t-ci
                     :edge-weights t-ew
                     :directed-p (csr-directed-p graph))))
