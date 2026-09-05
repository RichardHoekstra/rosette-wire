;;;; topological-sort.lisp --- Kahn's algorithm over rosette-graph-core CSR
;;;; adjacency.
;;;;
;;;; A topological order exists iff the digraph is acyclic (DAG).  Kahn's
;;;; algorithm repeatedly removes a zero-in-degree vertex; if it drains
;;;; fewer than N vertices before the frontier empties, the remaining
;;;; vertices participate in a cycle and no order exists.

(in-package #:rosette-graph-algorithms)

(defun %in-degrees (graph)
  "Return a fresh (unsigned-byte 32) vector of in-degrees of GRAPH."
  (let* ((n (csr-num-vertices graph))
         (indeg (make-array n :element-type '(unsigned-byte 32)
                              :initial-element 0)))
    (dotimes (u n)
      (csr-do-neighbors (v graph u)
        (incf (aref indeg v))))
    indeg))

(defun topological-sort (graph)
  "Return (values ORDER CYCLE-P) for a directed CSR GRAPH.

ORDER is a list of vertex indices in topological order (every edge u->v
has u before v).  CYCLE-P is T iff the graph is NOT a DAG, in which case
ORDER contains only the acyclic prefix Kahn's algorithm managed to drain
(strictly fewer than N vertices)."
  (let* ((n (csr-num-vertices graph))
         (indeg (%in-degrees graph))
         (queue (make-array n :fill-pointer 0 :adjustable t))
         (order '()))
    (dotimes (u n)
      (when (zerop (aref indeg u))
        (vector-push-extend u queue)))
    (let ((head 0))
      (loop while (< head (fill-pointer queue))
            do (let ((u (aref queue head)))
                 (incf head)
                 (push u order)
                 (csr-do-neighbors (v graph u)
                   (when (zerop (decf (aref indeg v)))
                     (vector-push-extend v queue))))))
    (setf order (nreverse order))
    (values order (< (length order) n))))

(defun dag-p (graph)
  "T iff directed CSR GRAPH has no cycle (a full topological order exists)."
  (multiple-value-bind (order cycle-p) (topological-sort graph)
    (declare (ignore order))
    (not cycle-p)))
