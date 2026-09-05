;;;; package.lisp --- package definition for rosette-graph-core.

(defpackage #:rosette-graph-core
  (:use #:cl)
  (:import-from #:rosette-array-core
                #:make-f-array
                #:f64-array)
  (:export
   ;; CSR graph type and constructors
   #:csr-graph
   #:csr-graph-p
   #:make-csr-graph
   #:csr-from-edges
   #:csr-from-adjacency
   ;; accessors
   #:csr-num-vertices
   #:csr-num-edges
   #:csr-row-offsets
   #:csr-col-indices
   #:csr-edge-weights
   #:csr-directed-p
   #:csr-neighbors
   #:csr-out-degree
   #:csr-edge-weight
   #:csr-do-neighbors
   ;; structural
   #:csr-transpose
   ;; traversal
   #:+csr-unvisited+
   #:csr-bfs
   #:csr-bfs-layers
   #:csr-dfs
   ;; SCC
   #:csr-strongly-connected-components))

(in-package #:rosette-graph-core)
