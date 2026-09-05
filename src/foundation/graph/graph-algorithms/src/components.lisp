;;;; components.lisp --- connected components over rosette-graph-core CSR
;;;; adjacency.
;;;;
;;;; CONNECTED-COMPONENTS treats every edge as undirected (the weak
;;;; connectivity relation: u ~ v iff u reaches v ignoring direction) via
;;;; rosette-union-find-core.  STRONGLY-CONNECTED-COMPONENTS is a thin
;;;; re-export of rosette-graph-core's Tarjan-style SCC so callers of this
;;;; library get the full connectivity family from one package.

(in-package #:rosette-graph-algorithms)

(defun connected-components (graph)
  "Return the (weakly, i.e. edge-direction-ignored) connected components of
CSR GRAPH as a list of lists of vertex indices."
  (let* ((n (csr-num-vertices graph))
         (uf (make-union-find n)))
    (dotimes (u n)
      (csr-do-neighbors (v graph u)
        (uf-union uf u v)))
    (uf-components uf)))

(defun connected-p (graph)
  "T iff CSR GRAPH is a single (weakly) connected component, or has <= 1
vertex."
  (let ((n (csr-num-vertices graph)))
    (or (<= n 1)
        (= 1 (length (connected-components graph))))))

(defun strongly-connected-components (graph)
  "Re-export of rosette-graph-core:csr-strongly-connected-components: the
strongly connected components of directed CSR GRAPH, as a list of lists
of vertex indices."
  (nth-value 1 (csr-strongly-connected-components graph)))
