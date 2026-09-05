;;;; rewrite.lisp --- Stage 3b (OPTIMIZE, live code): certified rewriting.
;;;;
;;;; The tree-shake (reduce.lisp) drops DEAD code; ddmin (ddmin.lisp) drops
;;;; forms not needed for the entrypoint. Neither makes the LIVE code cheaper.
;;;; This layer does, by composing the substrate's certified rewriter,
;;;; `rosette-sound-self-rewrite:improve` -- an equality-saturation loop (egraph-
;;;; backed via egraph-proves-equivalent-p) that rewrites an expression to a
;;;; strictly smaller EQUIVALENT one and hands back the certificate chain: every
;;;; step is verified on a battery, so the result is behaviourally equal by
;;;; construction, not by faith.
;;;;
;;;; HONEST SCOPE (the wall). `improve` optimises ARITHMETIC op-trees over
;;;; {+ - * sq}. So this shrinks live *arithmetic* code and is a no-op on
;;;; everything else. It is the seed of general live-code rewriting -- exposed
;;;; and certificate-carrying today; extending the rewrite IR beyond arithmetic
;;;; (so it fires on real library bodies) is the open frontier. The parity
;;;; gauge-check remains the backstop wherever a rewrite is applied to shipped
;;;; source.

(in-package #:rosette-ship)

(defun certified-simplify (expr &key battery (max-steps 200))
  "Simplify arithmetic EXPR to a certified-equivalent, strictly smaller form
via rosette-sound-self-rewrite:improve. Returns (values SIMPLIFIED STEPS
IMPROVEMENT): STEPS is the certificate chain (each step proven equivalent on a
battery and strictly smaller); IMPROVEMENT is the op-count removed. A form with
no applicable rule returns unchanged with IMPROVEMENT 0 -- a safe no-op."
  (multiple-value-bind (opt steps)
      (rosette-sound-self-rewrite:improve expr :battery battery :max-steps max-steps)
    (values opt steps (rosette-sound-self-rewrite:improvement-total steps))))

(defun certified-simplify-report (expr &key battery)
  "A plist summary of CERTIFIED-SIMPLIFY on EXPR: the input, the certified
smaller output, sizes, and the op-count improvement. The rewrite is sound by
the certificate chain; report it, don't assert it."
  (multiple-value-bind (opt steps improvement)
      (certified-simplify expr :battery battery)
    ;; improve's contract: every returned STEP is a :CONFIRMED verdict
    ;; (equivalent on the battery AND strictly smaller). So a non-empty step
    ;; chain IS the certificate that the rewrite is sound.
    (list :input expr
          :output opt
          :size-before (rosette-sound-self-rewrite:program-size expr)
          :size-after (rosette-sound-self-rewrite:program-size opt)
          :improvement improvement
          :certified (and steps (plusp improvement) t))))
