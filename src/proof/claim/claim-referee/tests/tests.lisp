;;;; tests.lisp --- law-verified tests for rosette-claim-referee.
;;;;
;;;; rosette-claim-referee is a Tier-1 kernel, so its API is pinned by laws,
;;;; not spot checks.  The laws encode the honest-accounting contract:
;;;;
;;;;   L1  a true point claim confirms with the right measured value/margin;
;;;;   L2  a false point claim refutes, carrying measured value and margin;
;;;;   L3  a "10x speedup" measured at 2x refutes WITH factor 2.0 reported
;;;;       (the GeoRefine / over-optimism pattern);
;;;;   L4  a >= claim AT the boundary (measured == claimed) confirms;
;;;;   L5  a <= claim mirror-images L4;
;;;;   L6  a composite with 2/3 sub-claims holding is :partial with exactly
;;;;       those 2 flagged (the fusion ignition/Q>1/plant-positive split);
;;;;   L7  an all-holding composite is :confirmed; an all-failing one :refuted;
;;;;   L8  a measurement thunk that ERRORS yields :untestable, never crashes;
;;;;   L9  a non-number measurement is also :untestable (skeptical default);
;;;;   L10 a composite whose sub-claims are all untestable is :untestable;
;;;;   L11 determinism: the same deterministic claim re-adjudicates identically.
;;;;
;;;; All data is deterministic; there is no Math.random in the harness.

(defpackage #:rosette-claim-referee/tests
  (:use #:cl #:rosette-claim-referee))

(in-package #:rosette-claim-referee/tests)

(defmacro is (condition message)
  `(unless ,condition (error "FAIL: ~A" ,message)))

(defun ~= (a b &optional (eps 1d-9))
  (and (realp a) (realp b) (< (abs (- a b)) eps)))

(defvar *zero* 0
  "A runtime zero the compiler cannot constant-fold, used to force a clean
DIVISION-BY-ZERO that exercises the referee's error path deterministically.")

(defun run-all-tests ()
  (let ((assertions 0))
    (flet ((ok (condition message)
             (incf assertions)
             (is condition message)))

      ;; ---- L1: true point claim confirms ----
      (let ((v (adjudicate
                (point-claim "pi-ish" 3.14159d0 (lambda () 3.14159d0)
                             :tolerance 1d-6))))
        (ok (eq (verdict-status v) :confirmed) "L1 true point -> :confirmed")
        (ok (~= (verdict-measured v) 3.14159d0) "L1 carries measured value")
        (ok (~= (verdict-margin v) 0d0) "L1 margin ~ 0"))

      ;; ---- L2: false point claim refutes with measured + margin ----
      (let ((v (adjudicate
                (point-claim "claimed-10" 10d0 (lambda () 7d0)
                             :tolerance 1d-3))))
        (ok (eq (verdict-status v) :refuted) "L2 false point -> :refuted")
        (ok (~= (verdict-measured v) 7d0) "L2 carries measured 7")
        (ok (~= (verdict-margin v) -3d0) "L2 margin = measured - claimed = -3"))

      ;; ---- L3: '10x speedup' measured at 2x -> refuted, factor 2.0 ----
      (let ((v (adjudicate
                (lower-bound-claim "10x speedup" 10d0 (lambda () 2d0)))))
        (ok (eq (verdict-status v) :refuted) "L3 2x vs >=10x -> :refuted")
        (ok (~= (verdict-measured v) 2d0) "L3 carries measured 2x")
        (ok (~= (verdict-ratio v) 0.2d0) "L3 ratio measured/claimed = 0.2")
        ;; The actual factor of speedup achieved is measured value 2.0.
        (ok (~= (verdict-measured v) 2.0d0) "L3 reports the actual 2.0x factor")
        (ok (search "2" (verdict-reason v)) "L3 reason names the shortfall"))

      ;; ---- L4: >= claim at the boundary confirms (documented convention) ----
      (let ((v (adjudicate
                (lower-bound-claim "boundary >=" 5d0 (lambda () 5d0)))))
        (ok (eq (verdict-status v) :confirmed)
            "L4 measured == claimed confirms (>= boundary)"))
      ;; just below the boundary refutes
      (let ((v (adjudicate
                (lower-bound-claim "just-under >=" 5d0 (lambda () 4.999d0)))))
        (ok (eq (verdict-status v) :refuted) "L4 just below >= boundary refutes"))
      ;; comfortably above confirms
      (let ((v (adjudicate
                (lower-bound-claim "above >=" 5d0 (lambda () 12d0)))))
        (ok (eq (verdict-status v) :confirmed) "L4 above >= confirms")
        (ok (~= (verdict-ratio v) 2.4d0) "L4 ratio 12/5 = 2.4"))

      ;; ---- L5: <= claim mirror image ----
      (let ((v (adjudicate
                (upper-bound-claim "latency <= 5ms" 5d0 (lambda () 5d0)))))
        (ok (eq (verdict-status v) :confirmed) "L5 measured == claimed confirms (<=)"))
      (let ((v (adjudicate
                (upper-bound-claim "latency <= 5ms" 5d0 (lambda () 8d0)))))
        (ok (eq (verdict-status v) :refuted) "L5 over <= bound refutes"))

      ;; ---- L6: composite 2/3 holding -> :partial with exactly those 2 ----
      ;; The fusion triple: triple-product ignition vs Q_fus>1 vs plant-positive.
      (let* ((c (composite-claim
                 "NIF shot"
                 (list
                  ;; ignition (lab gain >= 1): NIF 2022 achieved ~1.5 -> holds
                  (lower-bound-claim "ignition Q_sci>=1" 1d0 (lambda () 1.5d0))
                  ;; Q_fus>1 at the target: also holds at the scientific gain
                  (lower-bound-claim "Q_fus>=1" 1d0 (lambda () 1.5d0))
                  ;; plant-positive (G_plant >= 1): NIF ~0.056 -> fails hard
                  (lower-bound-claim "plant-positive G>=1" 1d0 (lambda () 0.056d0)))))
             (v (adjudicate c)))
        (ok (eq (verdict-status v) :partial) "L6 2/3 holding -> :partial")
        (ok (= (verdict-holding-count v) 2) "L6 exactly 2 sub-claims confirmed")
        (ok (= (length (verdict-subverdicts v)) 3) "L6 all 3 sub-verdicts present")
        (let ((statuses (mapcar #'verdict-status (verdict-subverdicts v))))
          (ok (equal statuses '(:confirmed :confirmed :refuted))
              "L6 exactly the first two hold, plant-positive refuted")))

      ;; ---- L7: all-holding -> :confirmed; all-failing -> :refuted ----
      (let ((v (adjudicate
                (composite-claim
                 "all-hold"
                 (list (lower-bound-claim "a" 1d0 (lambda () 2d0))
                       (lower-bound-claim "b" 1d0 (lambda () 3d0)))))))
        (ok (eq (verdict-status v) :confirmed) "L7 all sub-claims hold -> :confirmed")
        (ok (= (verdict-holding-count v) 2) "L7 both confirmed"))
      (let ((v (adjudicate
                (composite-claim
                 "all-fail"
                 (list (lower-bound-claim "a" 10d0 (lambda () 1d0))
                       (lower-bound-claim "b" 10d0 (lambda () 2d0)))))))
        (ok (eq (verdict-status v) :refuted) "L7 all sub-claims fail -> :refuted")
        (ok (= (verdict-holding-count v) 0) "L7 none confirmed"))

      ;; ---- L8: erroring thunk -> :untestable, no crash ----
      (let ((v (adjudicate
                (point-claim "boom" 1d0 (lambda () (error "kaboom"))))))
        (ok (eq (verdict-status v) :untestable) "L8 erroring thunk -> :untestable")
        (ok (null (verdict-measured v)) "L8 no measured value recorded")
        (ok (not (eq (verdict-status v) :confirmed))
            "L8 never :confirmed on failure (skeptical default)"))
      ;; an erroring lower-bound is also caught
      (let ((v (adjudicate
                (lower-bound-claim "boom2" 1d0
                                   (lambda () (/ 1 *zero*))))))
        (ok (eq (verdict-status v) :untestable) "L8 erroring bound -> :untestable"))

      ;; ---- L9: non-number measurement -> :untestable ----
      (let ((v (adjudicate
                (point-claim "string-result" 1d0 (lambda () "not a number")))))
        (ok (eq (verdict-status v) :untestable) "L9 non-number -> :untestable")
        (ok (not (eq (verdict-status v) :confirmed)) "L9 never confirmed"))
      (let ((v (adjudicate
                (point-claim "nil-result" 1d0 (lambda () nil)))))
        (ok (eq (verdict-status v) :untestable) "L9 nil result -> :untestable"))

      ;; ---- L10: composite all-untestable -> :untestable ----
      (let ((v (adjudicate
                (composite-claim
                 "all-untestable"
                 (list (point-claim "x" 1d0 (lambda () (error "no")))
                       (point-claim "y" 1d0 (lambda () nil)))))))
        (ok (eq (verdict-status v) :untestable)
            "L10 every sub-claim untestable -> :untestable")
        (ok (= (verdict-holding-count v) 0) "L10 none confirmed"))
      ;; a composite with one holding and one untestable is :partial
      (let ((v (adjudicate
                (composite-claim
                 "mixed-untestable"
                 (list (lower-bound-claim "ok" 1d0 (lambda () 2d0))
                       (point-claim "bad" 1d0 (lambda () (error "no"))))))))
        (ok (eq (verdict-status v) :partial)
            "L10 one holds, one untestable -> :partial")
        (ok (= (verdict-holding-count v) 1) "L10 exactly one confirmed"))

      ;; ---- L11: determinism ----
      (let* ((mk (lambda ()
                   (composite-claim
                    "det"
                    (list (point-claim "p" 2d0 (lambda () 2d0) :tolerance 1d-9)
                          (lower-bound-claim "q" 10d0 (lambda () 3d0))))))
             (v1 (adjudicate (funcall mk)))
             (v2 (adjudicate (funcall mk))))
        (ok (eq (verdict-status v1) (verdict-status v2)) "L11 status deterministic")
        (ok (= (verdict-holding-count v1) (verdict-holding-count v2))
            "L11 holding-count deterministic")
        (ok (equal (mapcar #'verdict-status (verdict-subverdicts v1))
                   (mapcar #'verdict-status (verdict-subverdicts v2)))
            "L11 sub-verdict statuses deterministic"))

      ;; ---- L12-L14: the permutation negative control ----
      ;; Deterministic xorshift so the shuffle null is reproducible (no Math.random).
      (let ((seed 88172645463325252))
        (flet ((rnd ()                          ; xorshift64 -> [0,1)
                 (setf seed (logand (logxor seed (ash seed 13)) #xFFFFFFFFFFFFFFFF))
                 (setf seed (logxor seed (ash seed -7)))
                 (setf seed (logand (logxor seed (ash seed 17)) #xFFFFFFFFFFFFFFFF))
                 (/ (float seed 1d0) #x10000000000000000)))
          ;; L12: a REAL effect (obs far above the shuffle null) confirms.
          ;; effect-fn returns a fixed large value; resample-fn returns small noise
          ;; around 0 -> z >> 2.
          (let ((v (adjudicate
                    (permutation-control-claim "real-effect"
                                               (lambda () 5d0)
                                               (lambda () (- (rnd) 0.5d0))
                                               :perms 200 :threshold 2d0))))
            (ok (eq (verdict-status v) :confirmed)
                "L12 effect far above its shuffle null -> :confirmed")
            (ok (> (verdict-measured v) 2d0) "L12 permutation z above threshold"))
          ;; L13: a NULL effect (obs drawn from the SAME distribution as the
          ;; shuffle null) does NOT stand out -> refuted.
          (let ((v (adjudicate
                    (permutation-control-claim "null-effect"
                                               (lambda () (- (rnd) 0.5d0))
                                               (lambda () (- (rnd) 0.5d0))
                                               :perms 200 :threshold 2d0))))
            (ok (eq (verdict-status v) :refuted)
                "L13 effect indistinguishable from its shuffle null -> :refuted"))
          ;; L14: skeptical default -- an erroring effect-fn yields :untestable.
          (let ((v (adjudicate
                    (permutation-control-claim "boom"
                                               (lambda () (/ 1 *zero*))
                                               (lambda () (- (rnd) 0.5d0))))))
            (ok (eq (verdict-status v) :untestable)
                "L14 erroring control measurement -> :untestable, never crashes"))))

      ;; ---- Taxonomy sanity ----
      (ok (equal (verdict-keywords) '(:confirmed :refuted :partial :untestable))
          "taxonomy is the documented closed set")
      (ok (verdict-keyword-p :confirmed) "keyword-p recognises :confirmed")
      (ok (not (verdict-keyword-p :closed-numerical))
          "keyword-p rejects an obligation keyword (orthogonal taxonomy)")

      ;; ---- Rendering does not crash and reflects the status ----
      (let* ((v (adjudicate (lower-bound-claim "10x speedup" 10d0 (lambda () 2d0))))
             (row (verdict-row-string v)))
        (ok (and (stringp row) (search "REFUTED" row))
            "row-string renders REFUTED tag"))
      (let* ((c (composite-claim
                 "NIF shot"
                 (list (lower-bound-claim "ignition" 1d0 (lambda () 1.5d0))
                       (lower-bound-claim "plant" 1d0 (lambda () 0.056d0)))))
             (tbl (verdict-table-string (adjudicate c))))
        (ok (and (stringp tbl) (search "PARTIAL" tbl) (search "ignition" tbl))
            "table-string renders composite with sub-rows"))

      (format t "~&rosette-claim-referee: ~D assertions passed.~%" assertions)
      assertions)))
