;;;; centrality.lisp --- degree, betweenness (Brandes), and PageRank
;;;; centrality over rosette-graph-core CSR adjacency.

(in-package #:rosette-graph-algorithms)

(defun degree-centrality (graph &key (normalize t))
  "Return a double-float vector of out-degree centrality of CSR GRAPH.
NORMALIZE (default T) divides by (n-1), the maximum possible degree."
  (let* ((n (csr-num-vertices graph))
         (out (make-array n :element-type 'double-float :initial-element 0d0))
         (denom (if (and normalize (> n 1)) (coerce (1- n) 'double-float) 1d0)))
    (dotimes (u n)
      (setf (aref out u) (/ (coerce (csr-out-degree graph u) 'double-float) denom)))
    out))

(defun in-degree-centrality (graph &key (normalize t))
  "Return a double-float vector of in-degree centrality of CSR GRAPH
(computed via the transpose adjacency)."
  (degree-centrality (csr-transpose graph) :normalize normalize))

(defun betweenness-centrality (graph &key (normalize t))
  "Brandes' algorithm: exact shortest-path betweenness centrality of every
vertex in unweighted CSR GRAPH (edge weights, if any, are ignored -- every
edge has hop-length 1).

Return a double-float vector.  When NORMALIZE (default T), undirected
graphs are divided by (n-1)(n-2)/2 and directed graphs by (n-1)(n-2), the
maximum possible number of shortest paths a vertex could sit on, so
values land in [0, 1]."
  (let* ((n (csr-num-vertices graph))
         (bc (make-array n :element-type 'double-float :initial-element 0d0)))
    (dotimes (s n)
      (let ((sigma (make-array n :element-type 'double-float :initial-element 0d0))
            (dist  (make-array n :initial-element -1))
            (preds (make-array n :initial-element nil))
            (stack '())
            (queue (make-array n :fill-pointer 0 :adjustable t)))
        (setf (aref sigma s) 1d0
              (aref dist s) 0)
        (vector-push-extend s queue)
        (let ((head 0))
          (loop while (< head (fill-pointer queue))
                do (let ((v (aref queue head)))
                     (incf head)
                     (push v stack)
                     (csr-do-neighbors (w graph v)
                       (when (< (aref dist w) 0)
                         (setf (aref dist w) (1+ (aref dist v)))
                         (vector-push-extend w queue))
                       (when (= (aref dist w) (1+ (aref dist v)))
                         (incf (aref sigma w) (aref sigma v))
                         (push v (aref preds w)))))))
        (let ((delta (make-array n :element-type 'double-float :initial-element 0d0)))
          (dolist (w stack)
            (dolist (v (aref preds w))
              (incf (aref delta v)
                    (* (/ (aref sigma v) (aref sigma w)) (1+ (aref delta w)))))
            (unless (= w s)
              (incf (aref bc w) (aref delta w)))))))
    (unless (csr-directed-p graph)
      (dotimes (u n) (setf (aref bc u) (/ (aref bc u) 2d0))))
    (when (and normalize (> n 2))
      (let ((denom (coerce (* (1- n) (- n 2)) 'double-float)))
        (unless (csr-directed-p graph) (setf denom (/ denom 2d0)))
        (dotimes (u n) (setf (aref bc u) (/ (aref bc u) denom)))))
    bc))

(defun pagerank (graph &key (damping 0.85d0) (tol 1d-10) (max-iterations 200))
  "PageRank of CSR GRAPH by power iteration on the Google matrix (damping
factor DAMPING, uniform teleport, dangling out-degree-0 nodes redistribute
their mass uniformly).  Return a double-float vector summing to 1.

Convergence: iterate until the L1 change between successive rank vectors
is below TOL, or MAX-ITERATIONS is reached."
  (let* ((n (csr-num-vertices graph))
         (transpose (csr-transpose graph))
         (out-deg (make-array n :initial-element 0))
         (rank (make-array n :element-type 'double-float
                             :initial-element (/ 1d0 n)))
         (teleport (/ (- 1d0 damping) n)))
    (dotimes (u n) (setf (aref out-deg u) (csr-out-degree graph u)))
    (dotimes (iter max-iterations)
      (declare (ignorable iter))
      (let ((dangling-mass 0d0))
        (dotimes (u n)
          (when (zerop (aref out-deg u))
            (incf dangling-mass (aref rank u))))
        (let ((new-rank (make-array n :element-type 'double-float
                                      :initial-element (+ teleport (/ (* damping dangling-mass) n))))
              (delta 0d0))
          (dotimes (v n)
            (csr-do-neighbors (u transpose v)
              ;; U is an in-neighbor of V in the original graph
              (when (plusp (aref out-deg u))
                (incf (aref new-rank v)
                      (/ (* damping (aref rank u)) (aref out-deg u))))))
          (dotimes (u n) (incf delta (abs (- (aref new-rank u) (aref rank u)))))
          (setf rank new-rank)
          (when (< delta tol) (return)))))
    rank))
