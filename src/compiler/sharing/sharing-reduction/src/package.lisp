;;;; package.lisp --- public API for rosette-sharing-reduction.

(in-package #:cl-user)

(defpackage #:rosette-sharing-reduction
  (:use #:cl)
  (:nicknames #:rosette-inet)
  (:local-nicknames (#:blc #:rosette-blc))
  ;; The reference TLC AST + evaluator (rung: rosette-foundation-rewrite) is the
  ;; source-term type and the correctness oracle.  Import its API explicitly
  ;; (not :use) to avoid the DAG-NODE-COUNT clash with rosette-sexpr-dag.
  (:import-from #:rosette-foundation-rewrite
   #:tlc-term #:tlc-term-kind
   #:tlc-var #:tlc-lam #:tlc-app
   #:tlc-var-name #:tlc-lam-param #:tlc-lam-body
   #:tlc-app-fn #:tlc-app-arg #:tlc-equal-p
   #:beta-step #:normalize #:*normalize-max-iter*)
  (:export
   ;; net arena + nodes
   #:make-net
   #:net-p
   #:net-node-count
   #:net-interactions
   #:net-rewrites
   #:net-annihilations
   #:net-commutations
   #:net-trace
   #:alloc-node
   #:node-symbol
   #:node-label
   #:free-node
   ;; ports + wiring
   #:port
   #:make-port
   #:port-node
   #:port-slot
   #:connect
   #:disconnect
   #:enter
   ;; symbols (the interaction combinators)
   #:+con+
   #:+dup+
   #:+era+
   ;; reduction
   #:active-pairs
   #:reduce-step
   #:reduce-selected-step
   #:reduce-up-to-budget
   #:reduce-all
   #:reduce-parallel-round
   #:normalize-net
   ;; lambda <-> net
   #:lam->net
   #:net->term
   #:church-numeral
   #:church-add
   #:church-mul
   #:church-exp
   #:church-count
   #:apply*
   #:net-of-term
   #:run-term
   #:term->sexp
   ;; geometry / dag
   #:net->sexp
   #:net-tree-size
   #:net-dag-size
   ;; certificates + verdict
   #:schedule-gauge-certificate
   #:schedule-gauge-certificate-p
   #:schedule-gauge-certificate-source-form
   #:schedule-gauge-certificate-sequential-normal-form
   #:schedule-gauge-certificate-parallel-normal-form
   #:schedule-gauge-certificate-sequential-interactions
   #:schedule-gauge-certificate-parallel-interactions
   #:certify-schedule-gauge
   #:verify-schedule-gauge-certificate
   #:certify
   #:reduction-cost-profile
   #:reduction-cost-profile-bounded
   #:reduction-cost-scaling
   #:reduction-cost-phase-sweep
   #:make-constructed-phase-corpus
   #:verify-constructed-phase-certificate
   #:verify-constructed-phase-corpus
   #:reduction-cost-constructed-phase-sweep
   #:reduction-trace-blc-description
   #:certificate-report
   #:print-verdict))

(in-package #:rosette-sharing-reduction)
