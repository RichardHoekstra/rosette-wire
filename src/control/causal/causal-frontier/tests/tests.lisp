;;;; tests/tests.lisp --- the two laws of the causal frontier.
;;;;
;;;; Scenario: two independent cells, "vault" and "dock".  The vault carries a
;;;; deliberately NON-COMMUTATIVE op stream (an :add, a :set, another :add) so
;;;; that order matters -- otherwise partition-invariance would be trivial.
;;;;
;;;;   e1: add  +10                       (deps none)
;;;;   e2: set  100   depends-on e1       (e1 -> e2)
;;;;   e3: add  +5    depends-on e1       (e1 -> e3, concurrent with e2)
;;;;   e4: add  +7    depends-on e2,e3    (after both)
;;;;
;;;; Canonical order: e1, then the concurrent pair {e2,e3} settled by tiebreak
;;;; (hlc e2=(2,0) < e3=(3,0) so e2 first), then e4:
;;;;   0 +10 -> 10 ; set 100 -> 100 ; +5 -> 105 ; +7 -> 112.  vault = 112.
;;;;   dock: f1 +3 -> f2 +4 = 7.

(in-package #:rosette-causal-frontier/tests)

(defun scenario ()
  (list
   (make-event :id "e1" :actor "a" :hlc (make-hlc 1 0) :deps '()
               :target "vault" :kind :add :value 10)
   (make-event :id "e2" :actor "a" :hlc (make-hlc 2 0) :deps '("e1")
               :target "vault" :kind :set :value 100)
   (make-event :id "e3" :actor "b" :hlc (make-hlc 3 0) :deps '("e1")
               :target "vault" :kind :add :value 5)
   (make-event :id "e4" :actor "a" :hlc (make-hlc 4 0) :deps '("e2" "e3")
               :target "vault" :kind :add :value 7)
   (make-event :id "f1" :actor "c" :hlc (make-hlc 1 0) :deps '()
               :target "dock" :kind :add :value 3)
   (make-event :id "f2" :actor "c" :hlc (make-hlc 2 0) :deps '("f1")
               :target "dock" :kind :add :value 4)))

(defun by-id (events id) (find id events :key #'event-id :test #'string=))

;;; A deliberately WRONG evaluation: split ONE cell's ops across two arbiters,
;;; let each apply its half, then concatenate.  This is the membrane-through-an-
;;; arbiter hazard, and it must diverge from the correct result.
(defun broken-split-vault (events)
  (let* ((vault (remove "vault" events :key #'event-target :test-not #'string=))
         (group-a (remove-if-not (lambda (e) (member (event-id e) '("e1" "e3")
                                                     :test #'string=))
                                 vault))
         (group-b (remove-if-not (lambda (e) (member (event-id e) '("e2" "e4")
                                                     :test #'string=))
                                 vault))
         (state 0))
    (dolist (e (linearize group-a :closed nil)) (setf state (%apply-vault state e)))
    (dolist (e (linearize group-b :closed nil)) (setf state (%apply-vault state e)))
    state))

(defun %apply-vault (v e)
  (ecase (event-kind e) (:add (+ v (event-value e))) (:set (event-value e))))

(defun run-all-tests ()
  (with-test-run (run "rosette-causal-frontier")
    (let* ((evs (scenario))
           (e1 (by-id evs "e1")) (e2 (by-id evs "e2"))
           (e3 (by-id evs "e3")) (e4 (by-id evs "e4")))

      ;; --- HLC order
      (check run (hlc< (make-hlc 1 5) (make-hlc 2 0)) "hlc< orders on logical")
      (check run (hlc< (make-hlc 2 0) (make-hlc 2 1)) "hlc< breaks ties on counter")
      (check run (not (hlc< (make-hlc 2 1) (make-hlc 2 1))) "hlc< is irreflexive")
      (check run (= 7 (hlc-logical (hlc-tick (make-hlc 3 9) 7)))
             "hlc-tick jumps to a newer physical time")
      (check run (= 10 (hlc-counter (hlc-tick (make-hlc 5 9) 3)))
             "hlc-tick bumps the counter when physical time did not advance")

      ;; --- the partial order
      (check run (happens-before-p e1 e4 evs) "e1 -> e4 transitively")
      (check run (not (happens-before-p e4 e1 evs)) "e4 does not precede e1")
      (check run (concurrent-p e2 e3 evs) "e2 and e3 are genuinely concurrent")
      (check run (not (concurrent-p e1 e4 evs)) "causally ordered events are not concurrent")

      ;; --- deterministic linearization respecting deps
      (let ((lin (mapcar #'event-id (linearize evs))))
        (check run (equal lin (mapcar #'event-id (linearize evs)))
               "linearize is deterministic")
        (check run (< (position "e1" lin :test #'string=)
                      (position "e2" lin :test #'string=))
               "linearize puts a cause before its effect (e1 before e2)")
        (check run (< (position "e2" lin :test #'string=)
                      (position "e3" lin :test #'string=))
               "concurrent e2,e3 settled by tiebreak: e2 (hlc 2) before e3 (hlc 3)")
        (check run (< (position "e3" lin :test #'string=)
                      (position "e4" lin :test #'string=))
               "e4 follows both its causes"))

      ;; --- the computed state
      (let ((s (causal-final-state evs)))
        (check run (= 112 (cell-state s "vault")) "vault resolves to 112")
        (check run (= 7 (cell-state s "dock")) "dock resolves to 7"))

      ;; --- LAW 1: QUOTIENT (causal adjudication == total-order run-game)
      (check run (quotient-consistent-p evs)
             "QUOTIENT: run-game over the linearization == causal-final-state")

      ;; --- LAW 2: PARTITION-INVARIANCE (repartition is a gauge)
      (let ((coarse (lambda (target) (declare (ignore target)) "root"))
            (fine   (lambda (target) target))                     ; per-cell
            (split  (lambda (target)                              ; vault<->dock apart
                      (if (string= target "vault") "node-A" "node-B"))))
        (check run (partition-invariant-p evs coarse fine)
               "coarse one-arbiter == fine per-cell")
        (check run (partition-invariant-p evs fine split)
               "any two cell-respecting partitions agree")
        (check run (= (state-hash (adjudicate evs coarse))
                      (state-hash (causal-final-state evs)))
               "adjudication under one global arbiter == the causal final state"))

      ;; --- the precondition, and the falsifier that motivates the SWITCH
      (check run (one-target-one-arbiter-p (lambda (tgt) tgt) evs)
             "a cell-respecting partition satisfies one-target-one-arbiter")
      (let ((correct (cell-state (causal-final-state evs) "vault"))
            (broken (broken-split-vault evs)))
        (check run (/= correct broken)
               "splitting one cell across arbiters (a membrane through an arbiter) changes the result")
        (check run (= correct 112) "the correct vault value is order-dependent (112)")))))
