;;;; traversal.lisp --- BFS and DFS on CSR-GRAPHs.

(in-package #:rosette-graph-core)

(defconstant +csr-unvisited+ #xFFFFFFFF
  "Sentinel value in parent / level vectors meaning a vertex was never reached.")

(defun csr-bfs (graph source)
  "Breadth-first traversal of GRAPH from SOURCE.

Returns three values:
  PARENTS  : (unsigned-byte 32) vector of length N;
             parents[u] = predecessor of u on the BFS tree, or
             +csr-unvisited+ for unreached vertices.  parents[source]=source.
  LEVELS   : (unsigned-byte 32) vector of length N;
             levels[u]  = hop distance from source, or +csr-unvisited+.
  ORDER    : simple-vector of visited vertex indices, BFS-ordered."
  (let* ((n        (csr-num-vertices graph))
         (parents  (%make-u32-array n :initial-element +csr-unvisited+))
         (levels   (%make-u32-array n :initial-element +csr-unvisited+))
         (queue    (make-array n :element-type '(unsigned-byte 32)
                                 :initial-element 0))
         (head 0) (tail 0))
    (setf (aref parents source) (%u32 source)
          (aref levels  source) 0
          (aref queue   tail)   (%u32 source)
          tail (1+ tail))
    (loop while (< head tail)
          for u = (aref queue head)
          do (incf head)
             (csr-do-neighbors (v graph u)
               (when (= (aref parents v) +csr-unvisited+)
                 (setf (aref parents v) (%u32 u)
                       (aref levels  v) (%u32 (1+ (aref levels u)))
                       (aref queue   tail) (%u32 v))
                 (incf tail))))
    (let ((order (make-array tail)))
      (dotimes (i tail)
        (setf (svref order i) (aref queue i)))
      (values parents levels order))))

(defun csr-bfs-layers (graph source)
  "BFS from SOURCE returned as a list of vertex lists, one per layer.

The first element is (SOURCE), the second its neighbors, and so on."
  (multiple-value-bind (parents levels order) (csr-bfs graph source)
    (declare (ignore parents))
    (let ((max-level 0)
          (visited (length order)))
      (dotimes (i visited)
        (let ((v (svref order i)))
          (when (> (aref levels v) max-level)
            (setf max-level (aref levels v)))))
      (let ((layers (make-array (1+ max-level) :initial-element nil)))
        ;; iterate in reverse to obtain natural per-layer order after PUSH
        (loop for i from (1- visited) downto 0
              for v = (svref order i)
              for l = (aref levels v)
              do (push v (aref layers l)))
        (coerce layers 'list)))))

(defun csr-dfs (graph source &key (preorder-fn nil) (postorder-fn nil))
  "Iterative DFS from SOURCE.

Returns two simple-vectors of visited vertex indices: the preorder
(discovery) sequence and the postorder (finish) sequence.

If PREORDER-FN or POSTORDER-FN are supplied they are invoked on each
vertex at the corresponding moment, in addition to being recorded."
  (let* ((n        (csr-num-vertices graph))
         (visited  (make-array n :element-type 'bit :initial-element 0))
         (preord   '())
         (postord  '())
         ;; stack frames are (vertex . next-edge-index)
         (stack    (list (cons source (aref (csr-row-offsets graph) source)))))
    (setf (sbit visited source) 1)
    (push source preord)
    (when preorder-fn (funcall preorder-fn source))
    (loop while stack
          for frame = (first stack)
          for u = (car frame)
          for k = (cdr frame)
          for hi = (aref (csr-row-offsets graph) (1+ u))
          do (cond
               ((>= k hi)
                ;; finish u
                (pop stack)
                (push u postord)
                (when postorder-fn (funcall postorder-fn u)))
               (t
                (let ((v (aref (csr-col-indices graph) k)))
                  (setf (cdr frame) (1+ k))
                  (when (zerop (sbit visited v))
                    (setf (sbit visited v) 1)
                    (push v preord)
                    (when preorder-fn (funcall preorder-fn v))
                    (push (cons v (aref (csr-row-offsets graph) v))
                          stack))))))
    (values (coerce (nreverse preord)  'simple-vector)
            (coerce (nreverse postord) 'simple-vector))))
