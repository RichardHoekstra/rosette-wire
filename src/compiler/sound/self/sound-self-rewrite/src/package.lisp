;;;; package.lisp --- package definition for rosette-sound-self-rewrite.

(in-package #:cl-user)

(defpackage #:rosette-sound-self-rewrite
  (:use #:cl)
  (:local-nicknames (#:u   #:rosette-unification)
                    (#:es  #:rosette-egraph-saturate)
                    (#:ref #:rosette-claim-referee))
  (:documentation
   "A CLOSED, SOUND self-improvement loop over s-expression programs.  The program
being improved AND the rewriter are both s-exprs: a rewrite rule is the datum
(NAME LHS RHS) with ?variables, applied by UNIFICATION at every position -- a
metacircular rewriter, no compiled match code.  Each proposed rewrite is gated by
TWO certificates adjudicated by rosette-claim-referee: a re-runnable BEHAVIOURAL
EQUIVALENCE theorem (rosette-egraph-saturate's battery-gated certify-merge, i.e. the
e-graph merge gate) AND a STRICT op-count improvement.  Only a verdict of
:CONFIRMED is accepted; the loop runs to fixpoint.  A behaviour-changing rewrite
never certifies and is provably refused -- correctness is preserved at every
brick.")
  (:export
   ;; rules (s-expr data) + the metacircular rewriter
   #:*simplify-rules* #:rule-name #:rule-lhs #:rule-rhs
   #:rewrite-at-root #:rewrite-candidates #:rules->functions
   ;; metrics
   #:program-size #:program-free-vars #:program-battery #:eval-cost
   ;; the sound per-step verifier
   #:rewrite-step #:rewrite-step-p
   #:rewrite-step-rule #:rewrite-step-before #:rewrite-step-after
   #:rewrite-step-size-before #:rewrite-step-size-after
   #:rewrite-step-certificate #:rewrite-step-verdict
   #:verify-rewrite #:certified-improvement-p
   ;; the closed loop
   #:improve #:improvement-total
   ;; e-graph corroboration
   #:egraph-proves-equivalent-p))
