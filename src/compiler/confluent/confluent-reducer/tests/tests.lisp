;;;; tests.lisp --- rosette-confluent-reducer mechanism-proof gate.
;;;;
;;;; ONE core-term with BOTH confluent runs and non-confluent `:set` frontiers.
;;;; We PROVE, on that term:
;;;;   (a) the confluent redexes are correctly identified and fire in ANY order
;;;;       to the SAME normal form (confluence => parallel-safe);
;;;;   (b) the non-confluent frontier is correctly located (the serial residue);
;;;;   (c) the parallel-schedule value == the serial normal form, sealed by an
;;;;       rosette-causal-frontier partition-invariance GAUGE certificate;
;;;;   (d) the reduced value == rosette-core-term's own EVAL of the same term;
;;;; the master certificate re-runs to T and tampering is CAUGHT; and we report
;;;; the measured PARALLEL WIDTH.

(defpackage #:rosette-confluent-reducer/tests
  (:use #:cl #:rosette-confluent-reducer)
  (:local-nicknames (#:ct  #:rosette-core-term)
                    (#:cf  #:rosette-confluence-frontier)
                    (#:pw  #:rosette-proof-witness)))
(in-package #:rosette-confluent-reducer/tests)

(defvar *fails* 0)
(defun expect (label got want)
  (if (equal got want)
      (format t "  ok   ~a = ~a~%" label got)
      (progn (incf *fails*) (format t "  FAIL ~a: got ~s want ~s~%" label got want))))
(defun expect-true (label got)
  (if got (format t "  ok   ~a~%" label)
      (progn (incf *fails*) (format t "  FAIL ~a: got NIL~%" label))))

;;; --- the witness term ------------------------------------------------------
;;; Two INDEPENDENT accumulator cells A and B, each: an init :set, a confluent
;;; run of :adds, a forgetting :set (the non-confluent frontier), a second
;;; confluent run, combined at the end by (+ a b).  Interleaved to exercise the
;;; cross-cell concurrency.
;;;
;;;   A: set 0 | add 3 add 4 add 5 | set 100 | add 6 add 7      -> 113
;;;   B: set 0 | add 10 add 20     | set 50  | add 8            ->  58
;;;   main = (+ a b) = 171
(defparameter *main*
  '(let a 0
    (let b 0
     (let a (+ a 3)
      (let b (+ b 10)
       (let a (+ a 4)
        (let b (+ b 20)
         (let a (+ a 5)
          (let a 100
           (let b 50
            (let a (+ a 6)
             (let b (+ b 8)
              (let a (+ a 7)
               (+ a b)))))))))))))
  "A core-term accumulator let-chain with confluent :add runs and forgetting
:set frontiers on two independent cells.")

(defparameter *program* (ct:make-core-program :funs nil :main *main*))

(defun %a-ops ()
  "All of cell A's ops in program order (recovered from the witness term)."
  (multiple-value-bind (ops body) (extract-ops *main*)
    (declare (ignore body))
    (remove "a" ops :test-not #'string= :key #'cred-op-cell)))

(defun run-all-tests ()
  (setf *fails* 0)

  ;; === reduce == eval (ground truth) =======================================
  (format t "~%-- (d) reduce == eval ==~%")
  (multiple-value-bind (pval plans ops body) (confluent-reduce *main*)
    (let ((evalv (ct:program-eval *program*)))
      (expect "core-term EVAL(main)" evalv 171)
      (expect "confluent-reduce parallel value" pval 171)
      (expect-true "reduce == eval" (eql pval evalv))
      ;; also == core-term's own NORMALIZER normal form
      (expect-true "reduce == normalize"
                   (eql pval (nth-value 0 (ct:program-normalize *program*)))))

    ;; === (a) confluent redexes: identified + order-independent ==============
    (format t "~%-- (a) confluent redexes fire in ANY order, same result --~%")
    (let* ((plan-a (find "a" plans :key #'reduction-plan-cell :test #'string=))
           (batches-a (reduction-plan-batches plan-a))
           ;; the two multi-op confluent runs on A
           (run1 (find 3 batches-a :key #'length))   ; add 3,4,5
           (run2 (find 2 batches-a :key #'length)))  ; add 6,7
      (expect-true "A has a width-3 confluent batch (adds 3,4,5)" run1)
      (expect-true "A has a width-2 confluent batch (adds 6,7)" run2)
      ;; rosette-confluence-frontier confirms each run is order-FREE...
      (flet ((specs (run) (mapcar (lambda (o) (cf:op (cred-op-kind o) (cred-op-value o)))
                                  run)))
        (expect-true "run1 CONFLUENT-P (rosette-confluence-frontier)"
                     (cf:confluent-p (specs run1)))
        (expect-true "run1 ORDER-INDEPENDENT-P (every permutation same)"
                     (cf:order-independent-p (specs run1)))
        (expect-true "run1 needs NO measure (symbolic/verify-only)"
                     (not (cf:measurement-needed-p (specs run1))))
        ;; ...and actually FIRING it in different orders reaches one value.
        (let* ((s (specs run1))
               (u (cf:unique-result s))
               (rev (cf:order-result (reverse s))))
          (expect "run1 unique-result == its reverse-order result" u rev)
          (expect-true "verify-resolution accepts a scrambled order"
                       (cf:verify-resolution s (reverse s) u)))))

    ;; === (b) the non-confluent frontier is located (serial residue) =========
    (format t "~%-- (b) non-confluent frontier located --~%")
    (let* ((plan-a (find "a" plans :key #'reduction-plan-cell :test #'string=))
           (frontier-a (non-confluent-frontier plan-a)))
      ;; A's frontier is exactly its two :set ops (init 0, reset 100).
      (expect "A frontier size (the serial residue)" (length frontier-a) 2)
      (expect-true "every A frontier op is a :SET (forgetting)"
                   (every (lambda (o) (eq (cred-op-kind o) :set)) frontier-a))
      ;; the WHOLE cell-A stream is order-DEPENDENT: a measure/serialization is
      ;; irreducibly required across the frontier (rosette-confluence-frontier).
      (let ((all-a (mapcar (lambda (o) (cf:op (cred-op-kind o) (cred-op-value o)))
                           (%a-ops))))
        (expect-true "whole cell-A op-stream MEASUREMENT-NEEDED-P (order-dependent)"
                     (cf:measurement-needed-p all-a))))

    ;; === (c) parallel schedule == serial NF, sealed by the gauge cert =======
    (format t "~%-- (c) parallel schedule == serial NF (partition-invariance gauge) --~%")
    (expect "serial normal form" (serial-normal-form ops body) 171)
    (expect "parallel-schedule value" (parallel-schedule-value ops body) 171)
    (expect-true "parallel == serial" (eql (parallel-schedule-value ops body)
                                           (serial-normal-form ops body)))
    (expect-true "partition-invariance GAUGE certificate holds"
                 (partition-gauge-certificate ops))

    ;; the FALSIFIER with teeth: reordering ACROSS the frontier changes the
    ;; value (you may parallelize confluent batches, NOT cross the :set).
    (format t "~%-- falsifier: crossing the non-confluent frontier breaks it --~%")
    (let* ((a-ops (%a-ops))
           (specs (mapcar (lambda (o) (cf:op (cred-op-kind o) (cred-op-value o))) a-ops))
           (serial (cf:order-result specs))
           ;; move the reset :set to the front -- a non-confluent reorder
           (set-op (find :set specs :key (lambda (s) (getf s :kind))
                                    :from-end t))
           (scrambled (cons set-op (remove set-op specs :count 1 :test #'equal))))
      (expect "cell-A serial fold" serial 113)
      (expect-true "crossing the frontier CHANGES the result (non-confluent)"
                   (/= serial (cf:order-result scrambled))))

    ;; === rosette-causal-repartition: cross-cell parallel width ==================
    (format t "~%-- rosette-causal-repartition: cross-cell parallel width --~%")
    (multiple-value-bind (split? subset gain xwidth) (repartition-report ops body)
      (declare (ignore subset))
      (expect "cross-cell width (independent cells)" xwidth 2)
      (expect-true "min-cut SHOULD-SPLIT the cells across arbiters (net-gain>0)"
                   split?)
      (format t "  info measured net-gain at lambda=3/2: ~a~%" gain)))

  ;; === the master certificate: re-runs to T, tampering caught ==============
  (format t "~%-- master certificate --~%")
  (let ((cert (confluent-reduce-certificate *program*)))
    (expect-true "certificate PASSES" (pw:certificate-passed cert))
    (expect-true "certificate RE-RUNS to T" (verify-confluent-reduce-certificate cert))
    (let ((w (getf (pw:certificate-payload cert) :parallel-width)))
      (format t "  info measured PARALLEL WIDTH (peak concurrent redexes): ~a~%" w)
      ;; peak wave: A's width-3 first run + B's width-2 first run, concurrent.
      (expect "measured parallel width" w 5))
    ;; NEGATIVE: tamper a FRESH certificate's sealed value in place (its own
    ;; witness re-reads the payload at verify time), so the falsifier has teeth.
    (let ((forged (confluent-reduce-certificate *program*)))
      (setf (getf (pw:certificate-payload forged) :parallel-value) 999)
      (expect-true "FORGED (tampered value) certificate FAILS re-run"
                   (not (verify-confluent-reduce-certificate forged)))
      ;; and a tampered WIDTH is caught too.
      (let ((forged2 (confluent-reduce-certificate *program*)))
        (setf (getf (pw:certificate-payload forged2) :parallel-width) 99)
        (expect-true "FORGED (tampered width) certificate FAILS re-run"
                     (not (verify-confluent-reduce-certificate forged2))))))

  (if (zerop *fails*)
      (progn (format t "~%rosette-confluent-reducer: ALL TESTS PASS~%") t)
      (error "rosette-confluent-reducer: ~a test(s) failed" *fails*)))
