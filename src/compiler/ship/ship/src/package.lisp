;;;; package.lisp --- public API for rosette-ship.

(defpackage #:rosette-ship
  (:use #:cl)
  (:export
   ;; --- repo anchor --------------------------------------------------------
   #:*repo-root*
   #:count-primary-systems
   ;; --- REDUCE (stage 2): reachable dependency closure --------------------
   #:dep-name
   #:asdf-system-deps
   #:library-closure
   #:slice #:slice-p #:make-slice #:compute-slice
   #:slice-root #:slice-order #:slice-missing #:slice-lib-count
   ;; --- EMIT: reduced self-contained single file --------------------------
   #:system-source-files
   #:emit-reduced-file
   #:reduction-report #:reduction-report-p #:make-reduction-report
   #:reduction-report-whole #:reduction-report-kept
   #:reduction-report-files #:reduction-report-lines
   #:reduction-report-missing #:reduction-report-ratio
   #:reduction-report-form-level
   #:reduction-report->plist
   ;; --- the build GAUGE: what a reproducibility verdict is quantified over
   #:build-toolchain #:toolchain-id #:toolchain-fingerprint #:toolchain-match-p
   ;; --- OPTIMIZE (stage 3): form-level tree-shake (the optimal reduced lisp)
   #:form-level-reduce #:parse-slice-forms #:emit-kept-forms
   ;; --- OPTIMIZE (stage 3+): ddmin -- the provable minimum via parity oracle
   #:ddmin #:ddmin-reduce #:make-load-run-oracle
   ;; --- #7: ddmin as a substrate bug-shrinker (same engine, failing oracle)
   #:shrink-to-property #:make-property-oracle #:minimal-reproducer
   ;; --- OPTIMIZE (stage 3b): certified live-code rewriting (egraph-backed)
   #:certified-simplify #:certified-simplify-report
   ;; --- #1: general (non-arithmetic) rewrite seed -- e-graph equivalence
   #:forms-equivalent-p #:prove-simplification
   ;; --- LOWER (stage 4): pluggable delivery floors ------------------------
   #:lower-to-floor
   #:floor-artifact #:floor-artifact-p #:make-floor-artifact
   #:floor-artifact-floor #:floor-artifact-reduced-file
   #:floor-artifact-driver #:floor-artifact-output
   #:floor-artifact-build-command #:floor-artifact-metadata
   #:supported-floors
   ;; --- CERTIFY (stage 5): parity gauge-check -----------------------------
   #:make-parity-certificate #:parity-passed-p
   ;; --- #4: all-floors-agree (deployment's one-IR-many-floors moat) -------
   #:floors-agree-p #:cross-floor-report
   ;; --- #5/#6: the live compiler earns its keep (self-optimize + hot-patch)
   #:runtime-simplify #:apply-patch
   ;; --- IDENTITY (stage 6): self-describing, content-addressed manifest ---
   #:ship-manifest #:write-ship-manifest #:manifest-from-run
   #:verify-ship-manifest
   ;; --- #2: content-addressed reproducible-build cache --------------------
   #:reduced-content-id #:cache-hit-p #:cache-record
   ;; --- the verb ----------------------------------------------------------
   #:ship #:ship-ddmin #:ship-report
   ;; --- the self-sufficient entrypoint (rosette-ship ships itself) ------------
   #:main #:run-ship #:parse-ship-args #:ensure-registry
   ;; --- the production substrate: Deliverables (.ship) + locks (.lock) ----
   #:read-ship-spec #:build-deliverable #:deliverable-lock #:deliverable-view
   #:write-lock #:compare-lock #:*audiences*
   #:deliver-file #:deliver-all #:find-ship-specs
   #:write-deliverables-status
   ;; --- external parity: rosette vs third-party (verdict-as-distribution) -----
   #:parity-agree-p #:read-parity-suites #:run-parity-suite #:run-parity-suites
   #:parity-suite-summary))

(in-package #:rosette-ship)
