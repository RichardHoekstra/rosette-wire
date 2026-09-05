;;;; scc.lisp --- strongly connected components (iterative Tarjan).

(in-package #:rosette-graph-core)

(defun csr-strongly-connected-components (graph)
  "Compute strongly connected components of GRAPH (Tarjan's algorithm).

Returns two values:
  COMPONENT-ID : (unsigned-byte 32) vector of length N giving the
                 component index for each vertex.  Component ids are
                 assigned in REVERSE topological order of the
                 component DAG (Tarjan's natural output).
  COMPONENTS   : list of components, each a list of vertex indices,
                 in the same order as COMPONENT-ID indexes them.

Edge directions are honored regardless of DIRECTED-P; for symmetric
storage every weakly-connected component is also strongly connected,
and the function returns the same partition as connected components."
  (let* ((n          (csr-num-vertices graph))
         (ro         (csr-row-offsets graph))
         (ci         (csr-col-indices graph))
         (index-of   (%make-u32-array n :initial-element +csr-unvisited+))
         (lowlink    (%make-u32-array n))
         (on-stack   (make-array n :element-type 'bit :initial-element 0))
         (comp-id    (%make-u32-array n :initial-element +csr-unvisited+))
         (stack      '())                ; SCC stack (vertices)
         (next-index 0)
         (next-comp  0)
         (components '()))
    (labels
        ((strongconnect (root)
           ;; Iterative DFS frame: (u . edge-cursor)
           (let ((work (list (cons root (aref ro root)))))
             (setf (aref index-of root) (%u32 next-index)
                   (aref lowlink  root) (%u32 next-index))
             (incf next-index)
             (push root stack)
             (setf (sbit on-stack root) 1)
             (loop while work
                   for frame = (first work)
                   for u  = (car frame)
                   for k  = (cdr frame)
                   for hi = (aref ro (1+ u))
                   do (cond
                        ((>= k hi)
                         ;; finished u: root-of-SCC check
                         (pop work)
                         (when work
                           ;; propagate lowlink up to caller
                           (let ((parent (car (first work))))
                             (when (< (aref lowlink u) (aref lowlink parent))
                               (setf (aref lowlink parent) (aref lowlink u)))))
                         (when (= (aref lowlink u) (aref index-of u))
                           (let ((comp '()))
                             (loop for w = (pop stack)
                                   do (setf (sbit on-stack w) 0
                                            (aref comp-id w) (%u32 next-comp))
                                      (push w comp)
                                      until (= w u))
                             (push (nreverse comp) components)
                             (incf next-comp))))
                        (t
                         (let ((v (aref ci k)))
                           (setf (cdr frame) (1+ k))
                           (cond
                             ((= (aref index-of v) +csr-unvisited+)
                              (setf (aref index-of v) (%u32 next-index)
                                    (aref lowlink  v) (%u32 next-index))
                              (incf next-index)
                              (push v stack)
                              (setf (sbit on-stack v) 1)
                              (push (cons v (aref ro v)) work))
                             ((= (sbit on-stack v) 1)
                              (when (< (aref index-of v) (aref lowlink u))
                                (setf (aref lowlink u) (aref index-of v))))))))))))
      (dotimes (v n)
        (when (= (aref index-of v) +csr-unvisited+)
          (strongconnect v))))
    (values comp-id (nreverse components))))
