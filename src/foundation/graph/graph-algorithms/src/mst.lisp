;;;; mst.lisp --- Minimum spanning tree: Kruskal (union-find) and Prim
;;;; (array-based) over rosette-graph-core CSR adjacency.
;;;;
;;;; Both assume GRAPH is undirected (edges stored symmetrically, as
;;;; produced by CSR-FROM-EDGES :directed-p nil) and connected; when the
;;;; graph is disconnected each returns a minimum SPANNING FOREST (one tree
;;;; per component) and the total weight of that forest.

(in-package #:rosette-graph-algorithms)

(defun %undirected-edge-list (graph)
  "Return the undirected edge set of GRAPH as a list of (weight u v),
each undirected edge appearing exactly once (u < v)."
  (let ((edges '()))
    (dotimes (u (csr-num-vertices graph))
      (csr-do-neighbors (v graph u :weight-var w)
        (when (< u v)
          (push (list w u v) edges))))
    (nreverse edges)))

(defun kruskal-mst (graph)
  "Kruskal's algorithm on undirected CSR GRAPH.

Return (values EDGES TOTAL-WEIGHT) where EDGES is a list of (u v weight)
triples forming a minimum spanning forest, in the order they were added,
and TOTAL-WEIGHT is their double-float sum."
  (let* ((n (csr-num-vertices graph))
         (uf (make-union-find n))
         (candidates (sort (%undirected-edge-list graph) #'< :key #'first))
         (chosen '())
         (total 0d0))
    (dolist (e candidates)
      (destructuring-bind (w u v) e
        (when (uf-union uf u v)
          (push (list u v w) chosen)
          (incf total w))))
    (values (nreverse chosen) total)))

(defun prim-mst (graph &optional (source 0))
  "Prim's algorithm on undirected CSR GRAPH starting from SOURCE.

Array-based O(V^2) selection (no external heap dependency).  Return
(values EDGES TOTAL-WEIGHT) as for KRUSKAL-MST; vertices unreachable from
SOURCE are handled by restarting Prim in each remaining component, so the
result is a minimum spanning forest across all components."
  (let* ((n (csr-num-vertices graph))
         (in-tree (make-array n :element-type 'bit :initial-element 0))
         (key (make-array n :element-type 'double-float
                            :initial-element
                            #.(coerce most-positive-fixnum 'double-float)))
         (parent (make-array n :initial-element nil))
         (chosen '())
         (total 0d0)
         (visited-count 0))
    (flet ((%prim-from (start)
             (setf (aref key start) 0d0)
             (dotimes (iter (- n visited-count))
               (declare (ignorable iter))
               ;; pick the unvisited vertex with smallest key
               (let ((best -1) (best-key #.(coerce most-positive-fixnum 'double-float)))
                 (dotimes (v n)
                   (when (and (zerop (aref in-tree v))
                              (< (aref key v) best-key))
                     (setf best v best-key (aref key v))))
                 (when (< best 0) (return))
                 (setf (aref in-tree best) 1)
                 (incf visited-count)
                 (when (aref parent best)
                   (push (list (aref parent best) best (aref key best)) chosen)
                   (incf total (aref key best)))
                 (csr-do-neighbors (v graph best :weight-var w)
                   (when (and (zerop (aref in-tree v)) (< w (aref key v)))
                     (setf (aref key v) w
                           (aref parent v) best)))))))
      ;; run Prim from SOURCE, then mop up any other components.
      (%prim-from source)
      (dotimes (u n)
        (when (zerop (aref in-tree u))
          (%prim-from u))))
    (values (nreverse chosen) total)))
