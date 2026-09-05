;;;; tests/tests.lisp --- the confluence frontier is the symbolic/neuro boundary.
;;;;
;;;; Add-only ops commute -> confluent -> order-free -> SYMBOLIC (a verifier
;;;; decides the unique result, no tiebreak).  Mixing a :set breaks commutation
;;;; -> non-confluent -> order-dependent -> NEURO (a measure must choose the
;;;; branch; different measures diverge; the verifier can still check a chosen
;;;; branch but could not have derived it).

(in-package #:rosette-confluence-frontier/tests)

(defun run-all-tests ()
  (with-test-run (run "rosette-confluence-frontier")
    (let ((adds (list (op :add 10) (op :add 5) (op :add 7)))       ; confluent
          (mixed (list (op :add 10) (op :set 100) (op :add 5))))   ; non-confluent

      ;; --- SYMBOLIC side: add-only is confluent and order-free
      (check run (commuting-p (op :add 10) (op :add 5)) "two adds commute")
      (check run (confluent-p adds) "add-only redex is confluent")
      (check run (order-independent-p adds) "confluent => order-free")
      (check run (not (measurement-needed-p adds))
             "a confluent redex needs no measure (symbolic)")
      (check run (= 22 (unique-result adds)) "the unique result is 22, tiebreak-free")
      ;; two genuinely different orders agree -> the tiebreak is irrelevant
      (check run (= (order-result adds) (order-result (reverse adds)))
             "any order of a confluent redex agrees")

      ;; --- NEURO side: mixing a :set breaks commutation
      (check run (not (commuting-p (op :add 10) (op :set 100)))
             "an add and a set do not commute")
      (check run (not (confluent-p mixed)) "the mixed redex is non-confluent")
      (check run (not (order-independent-p mixed)) "non-confluent => order-dependent")
      (check run (measurement-needed-p mixed)
             "a non-confluent redex REQUIRES a measure (neuro)")
      (check run (handler-case (progn (unique-result mixed) nil) (error () t))
             "unique-result refuses a non-confluent redex -- there is no unique value")

      ;; --- the tiebreak is load-bearing exactly on the neuro side
      (check run (/= (order-result mixed) (order-result (reverse mixed)))
             "two orders of the non-confluent redex give different results")

      ;; --- a MEASURE collapses the superposition; different measures diverge
      (let ((keep   (lambda (ops) ops))            ; sampler A: as-given
            (flip   (lambda (ops) (reverse ops)))) ; sampler B: reversed
        (multiple-value-bind (ra oa) (resolve-with-measure mixed keep)
          (multiple-value-bind (rb ob) (resolve-with-measure mixed flip)
            (check run (/= ra rb)
                   "two measures collapse the non-confluent redex to different outcomes")
            ;; ... but on the symbolic side every measure agrees
            (check run (= (resolve-with-measure adds keep)
                          (resolve-with-measure adds flip))
                   "on a confluent redex every measure gives the same outcome")
            ;; --- the verifier gates a chosen branch even where it could not choose it
            (check run (verify-resolution mixed oa ra)
                   "verify accepts a correctly-resolved branch")
            (check run (verify-resolution mixed ob rb)
                   "verify accepts the other branch too -- checking is confluent")
            (check run (not (verify-resolution mixed oa (1+ ra)))
                   "verify rejects a wrong claimed result")
            (check run (not (verify-resolution mixed (list (op :add 999)) ra))
                   "verify rejects an order that is not a permutation of the ops")))))))
