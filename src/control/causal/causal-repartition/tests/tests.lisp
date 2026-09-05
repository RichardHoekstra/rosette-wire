;;;; tests/tests.lisp --- min-cut on causal load, the phase transition, hysteresis.
;;;;
;;;; The hot sub-domain is a BARBELL: two dense triangles (each internal load 30)
;;;; joined by one bridge of weight w.  At latency-ratio lambda the bridge cut has
;;;; net gain 30 - (lambda-1)*w, so the split/keep boundary sits at w = 30/(lambda-1).

(in-package #:rosette-causal-repartition/tests)

(defun barbell (bridge)
  (make-interaction-graph
   '("a1" "a2" "a3" "b1" "b2" "b3")
   (list '("a1" "a2" 10) '("a1" "a3" 10) '("a2" "a3" 10)
         '("b1" "b2" 10) '("b1" "b3" 10) '("b2" "b3" 10)
         (list "a1" "b1" bridge))))

(defparameter +a-cluster+ '("a1" "a2" "a3"))

(defun run-all-tests ()
  (with-test-run (run "rosette-causal-repartition")

    ;; --- graph measurements
    (let ((g (barbell 2)))
      (check run (= 30 (internal-weight g +a-cluster+)) "a-cluster internal load = 30")
      (check run (= 2 (cut-weight g +a-cluster+)) "the bridge cut weighs 2")
      (check run (= 10 (edge-weight g "a1" "a2")) "edge weight is symmetric/summed"))

    ;; --- min-cut finds the thin bridge and splits it
    (let ((g (barbell 2)))
      (multiple-value-bind (subset gain) (best-cut g 3)
        (check run (= 26 gain) "thin-bridge barbell: best gain = 30 - 2*2 = 26")
        (check run (= 3 (length subset)) "the best cut is the 3-node cluster (the bridge)")
        (check run (should-split-p g 3) "a thin bridge should be split")))

    ;; --- a thick bridge is NOT worth splitting
    (let ((g (barbell 20)))
      (check run (not (should-split-p g 3))
             "a thick bridge (w=20 > 15) should stay merged"))

    ;; --- a dense clique has no thin cut and is never split
    (let ((clique (make-interaction-graph
                   '("c1" "c2" "c3" "c4")
                   '(("c1" "c2" 10) ("c1" "c3" 10) ("c1" "c4" 10)
                     ("c2" "c3" 10) ("c2" "c4" 10) ("c3" "c4" 10)))))
      (check run (not (should-split-p clique 3)) "a dense clique is not split"))

    ;; --- the phase transition: located, monotone, and sharpened by lambda
    (let ((g10 (barbell 10)) (g14 (barbell 14)))
      (check run (> (net-gain g10 +a-cluster+ 3) (net-gain g14 +a-cluster+ 3))
             "net gain falls monotonically as the bridge thickens"))
    (check run (= 15 (critical-bridge 30 3)) "boundary at lambda=3 is w=15")
    (check run (= 10 (critical-bridge 30 4)) "boundary at lambda=4 is w=10")
    (check run (< (critical-bridge 30 4) (critical-bridge 30 3))
           "a dearer network (larger lambda) sharpens the transition -- cut thinner to win")
    (let ((g (barbell 12)))
      (check run (should-split-p g 3) "w=12 splits when the network is cheap (lambda=3)")
      (check run (not (should-split-p g 4))
             "the SAME seam stays merged when the network is dear (lambda=4)"))

    ;; --- measurement is LOCAL: far-away load does not move this seam's decision
    (let ((near (barbell 2))
          (far (make-interaction-graph
                '("a1" "a2" "a3" "b1" "b2" "b3" "u1" "u2")
                (list '("a1" "a2" 10) '("a1" "a3" 10) '("a2" "a3" 10)
                      '("b1" "b2" 10) '("b1" "b3" 10) '("b2" "b3" 10)
                      '("a1" "b1" 2) '("u1" "u2" 100)))))
      (check run (= (net-gain near +a-cluster+ 3) (net-gain far +a-cluster+ 3))
             "adding a disconnected 100-weight cluster does not change this seam's gain"))

    ;; --- hysteresis collapses the flapping a single threshold suffers
    (let* ((gains '(5 25 15 12 18 11 19 13 22 9))
           (hlcs (loop for i from 1 to 10 collect (cf:make-hlc i 0)))
           (ctrl (make-repartition-controller :theta-hi 20 :theta-lo 10))
           (events (run-controller ctrl gains hlcs)))
      (check run (= 2 (length events))
             "hysteresis fires exactly two switches (split at 25, merge at 9)")
      (check run (eq :split (repartition-event-kind (first events))) "first switch is a split")
      (check run (eq :merge (repartition-event-kind (second events))) "second switch is a merge")
      (check run (= 25 (repartition-event-gain (first events))) "split fired on gain 25")
      (check run (>= (count-naive-flaps 15 gains) 8)
             "a naive single threshold flaps >= 8 times on the same trace")
      (check run (< (length events) (count-naive-flaps 15 gains))
             "hysteresis strictly reduces switching versus a single threshold"))

    ;; --- the physical layer is itself replayable + causally stamped
    (let* ((gains '(5 25 15 12 18 11 19 13 22 9))
           (hlcs (loop for i from 1 to 10 collect (cf:make-hlc i 0)))
           (e1 (run-controller (make-repartition-controller :theta-hi 20 :theta-lo 10)
                               gains hlcs))
           (e2 (run-controller (make-repartition-controller :theta-hi 20 :theta-lo 10)
                               gains hlcs)))
      (check run (equal (mapcar #'repartition-event-kind e1)
                        (mapcar #'repartition-event-kind e2))
             "the repartition event stream is deterministic (replayable)")
      (check run (cf:hlc< (repartition-event-hlc (first e1))
                          (repartition-event-hlc (second e1)))
             "repartition events are HLC-stamped in causal order"))

    ;; --- a controller with no dead band is rejected
    (check run (handler-case (progn (make-repartition-controller :theta-hi 5 :theta-lo 5) nil)
                 (error () t))
           "theta-hi must exceed theta-lo (a real dead band)")))
