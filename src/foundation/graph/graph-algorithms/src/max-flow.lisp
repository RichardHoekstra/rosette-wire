;;;; max-flow.lisp --- Edmonds-Karp max-flow / min-cut.
;;;;
;;;; This library OWNS max-flow/min-cut in the substrate.  A FLOW-NETWORK
;;;; is its own small mutable adjacency-list structure (edges paired with a
;;;; zero-capacity reverse twin, the standard residual-graph trick) rather
;;;; than a CSR-GRAPH: CSR's row/col arrays are fixed-shape and max-flow
;;;; needs per-edge residual capacity that mutates every augmentation.
;;;;
;;;; Edmonds-Karp: repeatedly BFS (fewest hops) for an augmenting path with
;;;; positive residual capacity from SOURCE to SINK, push the path's
;;;; bottleneck capacity, until no augmenting path remains.  Max-flow =
;;;; min-cut (LP duality / Ford-Fulkerson theorem): the total pushed flow
;;;; equals the capacity of the minimum s-t cut, which this file also
;;;; extracts from the residual graph's source-reachable set.

(in-package #:rosette-graph-algorithms)

(defstruct (flow-edge (:conc-name fe-))
  (to 0 :type fixnum)
  (cap 0d0 :type double-float)
  (flow 0d0 :type double-float))

(defstruct (flow-network (:conc-name fn-) (:constructor %make-flow-network))
  (num-vertices 0 :type fixnum)
  ;; adjacency[u] = adjustable vector of indices into EDGES incident from u
  (adjacency (make-array 0) :type simple-vector)
  ;; EDGES stored as forward/reverse pairs: edge 2k is (u->v), edge 2k+1 is
  ;; the reverse residual twin (v->u), added together by FLOW-ADD-EDGE.
  (edges (make-array 0 :adjustable t :fill-pointer 0) :type vector))

(defun make-flow-network (num-vertices)
  "Construct an empty flow network on NUM-VERTICES vertices 0..N-1."
  (%make-flow-network :num-vertices num-vertices
                       :adjacency (map-into (make-array num-vertices)
                                             (lambda () (make-array 0 :adjustable t :fill-pointer 0)))
                       :edges (make-array 0 :adjustable t :fill-pointer 0)))

(defun flow-add-edge (network u v capacity &key (bidirectional nil))
  "Add a directed edge U->V with residual CAPACITY to NETWORK, plus its
zero-capacity reverse residual twin.  BIDIRECTIONAL adds an equal-capacity
V->U edge (and its own residual twin) instead of a zero-capacity one --
i.e. models an undirected edge that can carry flow either way."
  (let* ((edges (fn-edges network))
         (adjacency (fn-adjacency network))
         (fwd-idx (fill-pointer edges))
         (rev-idx (1+ fwd-idx)))
    (vector-push-extend (make-flow-edge :to v :cap (coerce capacity 'double-float)) edges)
    (vector-push-extend (make-flow-edge :to u :cap (if bidirectional (coerce capacity 'double-float) 0d0)) edges)
    (vector-push-extend fwd-idx (aref adjacency u))
    (vector-push-extend rev-idx (aref adjacency v))
    (values fwd-idx rev-idx)))

(declaim (inline %residual))
(defun %residual (edge)
  (- (fe-cap edge) (fe-flow edge)))

(defun %bfs-augmenting-path (network source sink)
  "BFS for an augmenting path.  Return the parent-edge-index vector (NIL
entries for unreached vertices, T sentinel would be ambiguous so SOURCE's
slot stays NIL too) or NIL if SINK is unreachable."
  (let* ((n (fn-num-vertices network))
         (edges (fn-edges network))
         (adjacency (fn-adjacency network))
         (parent-edge (make-array n :initial-element nil))
         (visited (make-array n :element-type 'bit :initial-element 0))
         (queue (make-array n :fill-pointer 0 :adjustable t)))
    (setf (aref visited source) 1)
    (vector-push-extend source queue)
    (let ((head 0))
      (loop while (< head (fill-pointer queue))
            do (let ((u (aref queue head)))
                 (incf head)
                 (when (= u sink) (return-from %bfs-augmenting-path parent-edge))
                 (loop for eidx across (aref adjacency u)
                       for e = (aref edges eidx)
                       when (and (zerop (aref visited (fe-to e)))
                                 (> (%residual e) 1d-15))
                         do (setf (aref visited (fe-to e)) 1
                                  (aref parent-edge (fe-to e)) eidx)
                            (vector-push-extend (fe-to e) queue)))))
    (if (plusp (aref visited sink)) parent-edge nil)))

(defun edmonds-karp-max-flow (network source sink)
  "Edmonds-Karp maximum flow from SOURCE to SINK in NETWORK.  Return the
double-float max-flow value; NETWORK's edge flows are left at a maximum
flow assignment (so MIN-CUT can be read off the residual graph)."
  (let ((edges (fn-edges network))
        (total 0d0))
    (loop
      (let ((parent-edge (%bfs-augmenting-path network source sink)))
        (unless parent-edge (return total))
        ;; walk sink back to source, finding the bottleneck residual
        (let ((bottleneck #.(coerce most-positive-fixnum 'double-float))
              (v sink))
          (loop while (/= v source)
                do (let* ((eidx (aref parent-edge v))
                          (e (aref edges eidx)))
                     (setf bottleneck (min bottleneck (%residual e)))
                     (setf v (fe-to (aref edges (logxor eidx 1))))))
          ;; push BOTTLENECK along the path
          (setf v sink)
          (loop while (/= v source)
                do (let* ((eidx (aref parent-edge v))
                          (e (aref edges eidx))
                          (r (aref edges (logxor eidx 1))))
                     (incf (fe-flow e) bottleneck)
                     (decf (fe-flow r) bottleneck)
                     (setf v (fe-to r))))
          (incf total bottleneck))))))

(defun min-cut (network source sink)
  "After EDMONDS-KARP-MAX-FLOW has saturated NETWORK, return (values
CUT-EDGES CUT-VALUE): CUT-EDGES is a list of (u v capacity) original edges
crossing from the SOURCE side to the SINK side of the residual graph's
reachable partition, and CUT-VALUE is their double-float capacity sum
(equal to the max-flow value by the max-flow/min-cut theorem)."
  (declare (ignorable sink))
  (let* ((n (fn-num-vertices network))
         (edges (fn-edges network))
         (adjacency (fn-adjacency network))
         (reachable (make-array n :element-type 'bit :initial-element 0))
         (queue (make-array n :fill-pointer 0 :adjustable t)))
    (setf (aref reachable source) 1)
    (vector-push-extend source queue)
    (let ((head 0))
      (loop while (< head (fill-pointer queue))
            do (let ((u (aref queue head)))
                 (incf head)
                 (loop for eidx across (aref adjacency u)
                       for e = (aref edges eidx)
                       when (and (zerop (aref reachable (fe-to e)))
                                 (> (%residual e) 1d-15))
                         do (setf (aref reachable (fe-to e)) 1)
                            (vector-push-extend (fe-to e) queue)))))
    (let ((cut '()) (value 0d0))
      (dotimes (u n)
        (when (plusp (aref reachable u))
          (loop for eidx across (aref adjacency u)
                for e = (aref edges eidx)
                ;; forward edges (even index) with positive original
                ;; capacity crossing to the non-reachable side
                when (and (evenp eidx)
                          (zerop (aref reachable (fe-to e)))
                          (> (fe-cap e) 0d0))
                  do (push (list u (fe-to e) (fe-cap e)) cut)
                     (incf value (fe-cap e)))))
      (values (nreverse cut) value))))
