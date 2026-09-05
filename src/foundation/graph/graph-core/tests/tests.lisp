;;;; tests.lisp --- correctness tests for rosette-graph-core.

(in-package #:rosette-graph-core/tests)

(defun %sorted-neighbors (g u)
  (sort (coerce (csr-neighbors g u) 'list) #'<))

(defun run-all-tests ()
  (with-test-run (run "rosette-graph-core")

    ;; ---------------------------------------------------------- CSR shape
    ;; Directed line 0 -> 1 -> 2 -> 3
    (let ((g (csr-from-edges 4 '((0 . 1) (1 . 2) (2 . 3)))))
      (check run (= (csr-num-vertices g) 4)         "line: vertex count")
      (check run (= (csr-num-edges g)    3)         "line: edge count")
      (check run (equal (%sorted-neighbors g 0) '(1)) "line: neighbors[0]")
      (check run (equal (%sorted-neighbors g 3) '())  "line: neighbors[3]")
      (check run (= (csr-out-degree g 1) 1)         "line: out-degree"))

    ;; Undirected duplication (each user edge produces 2 stored half-edges)
    (let ((g (csr-from-edges 3 '((0 . 1) (1 . 2)) :directed-p nil)))
      (check run (= (csr-num-edges g) 4)          "undirected: doubled")
      (check run (equal (%sorted-neighbors g 1) '(0 2)) "undirected: symmetry"))

    ;; ------------------------------------------------------------ weights
    (let ((g (csr-from-edges 3 '((0 . 1) (0 . 2) (1 . 2))
                              :weights '(1.5 2.5 3.5))))
      (let ((seen 0d0))
        (csr-do-neighbors (v g 0 :weight-var w)
          (declare (ignore v))
          (incf seen w))
        (check run (= seen 4d0) "edge-weight sum out of 0 = 1.5+2.5"))
      ;; iterate over vertex 1: single edge of weight 3.5
      (let ((seen 0d0))
        (csr-do-neighbors (v g 1 :weight-var w)
          (declare (ignore v))
          (incf seen w))
        (check run (= seen 3.5d0) "edge-weight at vertex 1")))

    ;; ------------------------------------------------------------ BFS
    ;; Square + diagonal:
    ;;   0 - 1
    ;;   |   |
    ;;   2 - 3
    ;; plus 0 - 3
    (let ((g (csr-from-edges 4 '((0 . 1) (0 . 2) (0 . 3) (1 . 3) (2 . 3))
                              :directed-p nil)))
      (multiple-value-bind (parents levels order) (csr-bfs g 0)
        (declare (ignore parents))
        (check run (= (length order) 4)        "bfs: all reached")
        (check run (= (aref levels 0) 0)       "bfs: source at level 0")
        (check run (= (aref levels 1) 1)       "bfs: neighbor at level 1")
        (check run (= (aref levels 2) 1)       "bfs: neighbor at level 1")
        (check run (= (aref levels 3) 1)       "bfs: diag at level 1")))

    ;; BFS reachability (disconnected component)
    (let ((g (csr-from-edges 4 '((0 . 1)) :directed-p nil)))
      (multiple-value-bind (parents levels order) (csr-bfs g 0)
        (declare (ignore parents))
        (check run (= (length order) 2)              "bfs: only reachable")
        (check run (= (aref levels 2) +csr-unvisited+)
               "bfs: unreached marked")))

    ;; ------------------------------------------------------------ BFS layers
    (let* ((g (csr-from-edges 5 '((0 . 1) (0 . 2) (1 . 3) (2 . 4))))
           (layers (csr-bfs-layers g 0)))
      (check run (= (length layers) 3)                    "bfs-layers: depth")
      (check run (equal (first layers) '(0))              "bfs-layers: source")
      (check run (= (length (second layers)) 2)           "bfs-layers: level 1")
      (check run (= (length (third  layers)) 2)           "bfs-layers: level 2"))

    ;; ------------------------------------------------------------ DFS
    (let ((g (csr-from-edges 4 '((0 . 1) (0 . 2) (1 . 3)))))
      (multiple-value-bind (pre post) (csr-dfs g 0)
        (check run (= (length pre)  4)         "dfs: preorder length")
        (check run (= (length post) 4)         "dfs: postorder length")
        (check run (eql (svref pre  0) 0)      "dfs: source first preorder")
        (check run (eql (svref post 3) 0)      "dfs: source last postorder")))

    ;; ------------------------------------------------------------ SCC
    ;; Classic Tarjan example: a 3-cycle {0,1,2}, a 3-cycle {3,4,5},
    ;; and a bridge 2 -> 3 making two distinct SCCs.
    (let ((g (csr-from-edges
              6 '((0 . 1) (1 . 2) (2 . 0)
                  (2 . 3)
                  (3 . 4) (4 . 5) (5 . 3)))))
      (multiple-value-bind (comp-id components)
          (csr-strongly-connected-components g)
        (check run (= (length components) 2)
               "scc: two components")
        (check run (= (aref comp-id 0) (aref comp-id 1))
               "scc: 0,1 share component")
        (check run (= (aref comp-id 1) (aref comp-id 2))
               "scc: 0,1,2 share component")
        (check run (= (aref comp-id 3) (aref comp-id 4))
               "scc: 3,4 share component")
        (check run (/= (aref comp-id 2) (aref comp-id 3))
               "scc: bridge separates components")))

    ;; Single big SCC across a 4-cycle
    (let ((g (csr-from-edges 4 '((0 . 1) (1 . 2) (2 . 3) (3 . 0)))))
      (multiple-value-bind (comp-id components)
          (csr-strongly-connected-components g)
        (check run (= (length components) 1) "scc: cycle is one component")
        (check run (every (lambda (v) (= v (aref comp-id 0)))
                          (coerce comp-id 'list))
               "scc: cycle component uniform")))

    ;; ------------------------------------------------------------ transpose
    (let* ((g  (csr-from-edges 3 '((0 . 1) (0 . 2) (1 . 2))))
           (gt (csr-transpose g)))
      (check run (= (csr-num-edges gt) 3)               "T: edge count preserved")
      (check run (equal (%sorted-neighbors gt 2) '(0 1)) "T: in-nbrs of 2")
      (check run (equal (%sorted-neighbors gt 0) '())   "T: in-nbrs of 0"))

    ;; ------------------------------------------------------------ adjacency
    (let ((g (csr-from-adjacency '((1 2) (2) ()))))
      (check run (= (csr-num-vertices g) 3) "adj: vertex count")
      (check run (equal (%sorted-neighbors g 0) '(1 2)) "adj: row 0")
      (check run (equal (%sorted-neighbors g 1) '(2))   "adj: row 1"))))
