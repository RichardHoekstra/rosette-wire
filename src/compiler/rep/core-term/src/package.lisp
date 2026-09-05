;;;; package.lisp --- public API for rosette-core-term.

(defpackage #:rosette-core-term
  (:use #:cl)
  (:import-from #:rosette-proof-carrier-core
                #:make-proof-node #:proof-leaf #:proof-branch
                #:proof-node #:proof-node-p #:proof-node-kind
                #:proof-node-payload #:proof-node-children #:proof-node-label
                #:proof-node->plist #:proof-node-size #:proof-node-depth)
  (:import-from #:rosette-proof-witness
                #:make-certificate #:certificate #:certificate-p
                #:certificate-passed #:certificate-payload #:certificate-witness
                #:make-lean-witness #:runtime-verify)
  (:import-from #:rosette-lisp-codegen
                #:compile-program #:run
                #:program-code #:program-entry #:program-fn-entry
                #:program-fn-arity)
  (:import-from #:rosette-gpu-kernel-dsl
                #:make-kernel-spec)
  (:import-from #:rosette-gpu-kernel-dsl/cpu
                #:interpret-kernel-spec)
  (:export
   ;; --- the ONE object -----------------------------------------------------
   #:core-program #:core-program-p #:make-core-program
   #:core-program-funs #:core-program-main
   #:term-check #:core-type-error
   ;; --- coincidence 1: EVAL ------------------------------------------------
   #:term-eval #:program-eval
   ;; --- coincidence 2: PROVE (normalize emits the certificate) -------------
   #:term-normalize #:program-normalize
   #:normalization-certificate #:verify-normalization-certificate
   #:definitionally-equal-p #:definitional-equality-certificate
   ;; certificate accessors re-exported for callers
   #:certificate-passed #:certificate-payload #:certificate-witness
   ;; --- coincidence 3: COMPILE (bridge to silicon IR) ----------------------
   #:lower-to-integer-lisp #:lower-to-program #:oracle-run
   #:battery-check #:*core-vm-spec*
   #:make-core-vm-invocation #:core-vm-invocation-result
   ;; i32 tagged-fixnum oracle: faithful on [-2^28, 2^28), else refused
   #:core-vm-i32-range-error #:+core-vm-value-limit+
   ;; --- STRETCH (d): typed-lambda layer + metacircular self-eval -----------
   #:core-closure #:core-closure-p
   #:encode-e #:decode-tag #:self-interp-funs #:self-eval #:direct-eval-e
   #:self-eval-certificate
   ;; --- (e): F-fragment self-interp (let + multivar + first-order calls) ---
   #:encode-f #:self-interp-funs-f #:self-eval-f #:self-eval-f-raw
   #:direct-eval-f #:self-eval-f-certificate
   ;; reflective-fixpoint walls, made falsifiable
   #:f-code-fits-as-value-p #:f-heap-max-index
   ;; --- reusable proof-carrying reduction seam -----------------------------
   #:certified-reduce #:reduction-trace-fingerprint
   #:verify-reduction-certificate #:reduction-normal-form))

(in-package #:rosette-core-term)
