;;;; tests/tests.lisp --- rosette-graph-algorithms test suite.
;;;;
;;;; Coverage against hand-computed / textbook known-answer instances:
;;;;   1. Kahn topological sort respects edge order on a DAG; detects a cycle.
;;;;   2. Undirected connected components on a two-component graph.
;;;;   3. Strongly connected components on a 6-vertex digraph (two SCCs).
;;;;   4. Kruskal MST matches a hand-computed total weight (6.0) on a
;;;;      4-vertex weighted graph, and Prim agrees with Kruskal.
;;;;   5. Edmonds-Karp max-flow on the CLRS Fig. 26.1 network: max flow 23,
;;;;      and min-cut value equals the max-flow value.
;;;;   6. Degree centrality on a star graph; betweenness centrality on a
;;;;      3-vertex path (middle vertex normalized betweenness = 1.0);
;;;;      PageRank sums to 1 and is uniform (0.5/0.5) on a symmetric 2-cycle.

(defpackage #:rosette-graph-algorithms/tests
  (:use #:cl #:rosette-graph-algorithms)
  (:import-from #:rosette-graph-core #:csr-from-edges #:csr-num-vertices)
  (:export #:run-all-tests))

(in-package #:rosette-graph-algorithms/tests)

(defvar *test-count* 0)
(defvar *failure-count* 0)

(defun reset-counters ()
  (setf *test-count* 0
        *failure-count* 0))

(defmacro is (form &optional (message "assertion failed"))
  `(progn
     (incf *test-count*)
     (unless ,form
       (incf *failure-count*)
       (format t "  FAIL: ~A~%       form: ~S~%" ,message ',form))))

(defmacro test-case (name &body body)
  `(progn
     (format t "~&~A~%" ,name)
     ,@body))

(defun approx= (a b &optional (eps 1d-6))
  (< (abs (- a b)) eps))

;;;; --- 1. topological sort ------------------------------------------------

(defun test-topological-sort ()
  (test-case "Kahn topological sort: DAG respects edge order; cycle detected"
    (let ((dag (csr-from-edges 6 '((5 . 2) (5 . 0) (4 . 0) (4 . 1) (2 . 3) (3 . 1))
                                :directed-p t)))
      (multiple-value-bind (order cycle-p) (topological-sort dag)
        (is (not cycle-p) "DAG has no cycle")
        (is (= (length order) 6) "order contains all 6 vertices")
        ;; every edge u->v must have u appear before v in ORDER
        (let ((pos (make-array 6)))
          (loop for u in order for i from 0 do (setf (aref pos u) i))
          (dolist (e '((5 . 2) (5 . 0) (4 . 0) (4 . 1) (2 . 3) (3 . 1)))
            (is (< (aref pos (car e)) (aref pos (cdr e)))
                (format nil "edge ~A respected in topological order" e))))
        (is (dag-p dag) "dag-p agrees: acyclic")))
    (let ((cyclic (csr-from-edges 3 '((0 . 1) (1 . 2) (2 . 0)) :directed-p t)))
      (multiple-value-bind (order cycle-p) (topological-sort cyclic)
        (is cycle-p "3-cycle is correctly flagged as non-DAG")
        (is (< (length order) 3) "partial order shorter than N on a cycle"))
      (is (not (dag-p cyclic)) "dag-p agrees: cyclic"))))

;;;; --- 2. connected components (undirected) -------------------------------

(defun test-connected-components ()
  (test-case "Undirected connected components: two components {0,1,2} {3,4}"
    (let ((g (csr-from-edges 5 '((0 . 1) (1 . 2) (3 . 4)) :directed-p nil)))
      (let ((comps (mapcar (lambda (c) (sort (copy-list c) #'<))
                            (connected-components g))))
        (is (= (length comps) 2) "exactly two components")
        (is (member '(0 1 2) comps :test #'equal) "component {0,1,2} present")
        (is (member '(3 4) comps :test #'equal) "component {3,4} present")
        (is (not (connected-p g)) "connected-p is false for a 2-component graph")))
    (let ((full (csr-from-edges 4 '((0 . 1) (1 . 2) (2 . 3)) :directed-p nil)))
      (is (connected-p full) "connected-p is true for a connected path"))))

;;;; --- 3. strongly connected components -----------------------------------

(defun test-scc ()
  (test-case "Strongly connected components: 6-vertex digraph, two SCCs {0,1,2} {3,4,5}"
    (let* ((g (csr-from-edges 6 '((0 . 1) (1 . 2) (2 . 0) (2 . 3) (3 . 4) (4 . 5) (5 . 3))
                               :directed-p t))
           (comps (mapcar (lambda (c) (sort (copy-list c) #'<))
                           (strongly-connected-components g))))
      (is (= (length comps) 2) "exactly two SCCs")
      (is (member '(0 1 2) comps :test #'equal) "SCC {0,1,2} present")
      (is (member '(3 4 5) comps :test #'equal) "SCC {3,4,5} present"))))

;;;; --- 4. minimum spanning tree (Kruskal + Prim) ---------------------------

(defun test-mst ()
  (test-case "Kruskal / Prim MST: 4-vertex graph, hand-computed total weight 6.0"
    ;; edges: (0,1,1) (0,2,4) (1,2,2) (1,3,6) (2,3,3)
    ;; MST (unique, distinct weights): (0,1,1)+(1,2,2)+(2,3,3) = 6
    (let ((g (csr-from-edges 4 '((0 . 1) (0 . 2) (1 . 2) (1 . 3) (2 . 3))
                              :weights '(1d0 4d0 2d0 6d0 3d0)
                              :directed-p nil)))
      (multiple-value-bind (kedges ktotal) (kruskal-mst g)
        (is (approx= ktotal 6d0) "Kruskal total weight = 6.0")
        (is (= (length kedges) 3) "Kruskal MST has n-1 = 3 edges"))
      (multiple-value-bind (pedges ptotal) (prim-mst g 0)
        (is (approx= ptotal 6d0) "Prim total weight = 6.0")
        (is (= (length pedges) 3) "Prim MST has n-1 = 3 edges"))
      (multiple-value-bind (k1 kt) (kruskal-mst g)
        (declare (ignore k1))
        (multiple-value-bind (p1 pt) (prim-mst g 0)
          (declare (ignore p1))
          (is (approx= kt pt) "Kruskal and Prim agree on total MST weight"))))))

;;;; --- 5. max-flow / min-cut (Edmonds-Karp) --------------------------------

(defun test-max-flow ()
  (test-case "Edmonds-Karp max-flow: CLRS Fig. 26.1 network, max flow = 23"
    ;; s=0 v1=1 v2=2 v3=3 v4=4 t=5
    (let ((net (make-flow-network 6)))
      (flow-add-edge net 0 1 16d0)
      (flow-add-edge net 0 2 13d0)
      (flow-add-edge net 1 3 12d0)
      (flow-add-edge net 2 1 4d0)
      (flow-add-edge net 2 4 14d0)
      (flow-add-edge net 3 2 9d0)
      (flow-add-edge net 3 5 20d0)
      (flow-add-edge net 4 3 7d0)
      (flow-add-edge net 4 5 4d0)
      (let ((maxflow (edmonds-karp-max-flow net 0 5)))
        (is (approx= maxflow 23d0) "max flow from s to t is 23")
        (multiple-value-bind (cut-edges cut-value) (min-cut net 0 5)
          (is (approx= cut-value maxflow) "min-cut value equals max-flow value")
          (is (> (length cut-edges) 0) "min-cut returns at least one crossing edge")))))
  (test-case "Edmonds-Karp max-flow: single bottleneck edge caps the flow"
    (let ((net (make-flow-network 4)))
      (flow-add-edge net 0 1 10d0)
      (flow-add-edge net 1 2 2d0)   ; bottleneck
      (flow-add-edge net 2 3 10d0)
      (is (approx= (edmonds-karp-max-flow net 0 3) 2d0)
          "flow is capped by the single bottleneck edge capacity"))))

;;;; --- 6. centrality --------------------------------------------------------

(defun test-centrality ()
  (test-case "Degree centrality: star graph, center = 1.0, leaves = 0.25"
    (let ((star (csr-from-edges 5 '((0 . 1) (0 . 2) (0 . 3) (0 . 4)) :directed-p nil)))
      (let ((dc (degree-centrality star)))
        (is (approx= (aref dc 0) 1.0d0) "center degree centrality = 1.0")
        (dotimes (leaf 4)
          (is (approx= (aref dc (1+ leaf)) 0.25d0)
              (format nil "leaf ~A degree centrality = 0.25" (1+ leaf)))))))
  (test-case "Betweenness centrality: 3-vertex path 0-1-2, middle vertex normalized = 1.0"
    (let ((path (csr-from-edges 3 '((0 . 1) (1 . 2)) :directed-p nil)))
      (let ((bc (betweenness-centrality path)))
        (is (approx= (aref bc 1) 1.0d0) "middle vertex betweenness = 1.0 (on every shortest path)")
        (is (approx= (aref bc 0) 0.0d0) "endpoint betweenness = 0.0")
        (is (approx= (aref bc 2) 0.0d0) "endpoint betweenness = 0.0"))))
  (test-case "PageRank: symmetric 2-cycle converges to uniform 0.5/0.5, sums to 1"
    (let ((cycle2 (csr-from-edges 2 '((0 . 1) (1 . 0)) :directed-p t)))
      (let ((pr (pagerank cycle2)))
        (is (approx= (+ (aref pr 0) (aref pr 1)) 1.0d0 1d-8) "PageRank sums to 1")
        (is (approx= (aref pr 0) 0.5d0 1d-6) "symmetric 2-cycle: rank(0) = 0.5")
        (is (approx= (aref pr 1) 0.5d0 1d-6) "symmetric 2-cycle: rank(1) = 0.5"))))
  (test-case "PageRank: hub receiving two in-links outranks a dangling leaf"
    ;; 0 -> 1, 0 -> 2, 1 -> 2  (2 is a sink/dangling node, receives from 0 and 1)
    (let ((hub (csr-from-edges 3 '((0 . 1) (0 . 2) (1 . 2)) :directed-p t)))
      (let ((pr (pagerank hub)))
        (is (approx= (+ (aref pr 0) (aref pr 1) (aref pr 2)) 1.0d0 1d-8)
            "PageRank sums to 1 on the hub graph")
        (is (> (aref pr 2) (aref pr 0)) "vertex 2 (two in-links) outranks source vertex 0")
        (is (> (aref pr 2) (aref pr 1)) "vertex 2 (two in-links) outranks vertex 1")))))

;;;; --- entry point ------------------------------------------------------

(defun run-all-tests ()
  (reset-counters)
  (format t "~%running rosette-graph-algorithms tests...~%")
  (test-topological-sort)
  (test-connected-components)
  (test-scc)
  (test-mst)
  (test-max-flow)
  (test-centrality)
  (format t "~%~A test assertions, ~A failures.~%"
          *test-count* *failure-count*)
  (when (plusp *failure-count*)
    (error "rosette-graph-algorithms tests failed: ~A failures" *failure-count*))
  (values *test-count* *failure-count*))
