;;;; package.lisp --- public API for replayable Wire diagnostics.

(defpackage #:rosette-wire-diagnostics
  (:use #:cl)
  (:local-nicknames (#:cid #:rosette-content-identity)
                    (#:esh #:rosette-front-door/eshkol)
                    (#:front #:rosette-front-door)
                    (#:json #:rosette-json)
                    (#:qir #:rosette-quantum-ir)
                    (#:qvm #:rosette-quantum-vm)
                    (#:rw #:rosette-wire))
  (:export
   #:make-eshkol-campaign
   #:eshkol-campaign
   #:eshkol-campaign-p
   #:eshkol-campaign-source
   #:eshkol-campaign-toolchain-id
   #:eshkol-campaign-id
   #:run-eshkol-campaign
   #:replay-diagnostic-bundle
   #:verify-diagnostic-bundle
   #:diagnostic-bundle->form
   #:diagnostic-bundle
   #:diagnostic-bundle-p
   #:diagnostic-bundle-id
   #:diagnostic-bundle-campaign-id
   #:diagnostic-bundle-source
   #:diagnostic-bundle-program-id
   #:diagnostic-bundle-toolchain-id
   #:diagnostic-bundle-verdict
   #:diagnostic-bundle-outcome
   #:diagnostic-bundle-earliest-boundary
   #:diagnostic-bundle-observations
   #:diagnostic-bundle-disagreements
   #:diagnostic-bundle-error-status
   #:eshkol-diagnostic-bundle->wire-value
   #:eshkol-diagnostic-bundle-from-wire-value
   #:make-eshkol-diagnostic-wire-graph
   #:register-eshkol-diagnostic-adapter

   #:make-moonlab-campaign
   #:moonlab-campaign
   #:moonlab-campaign-p
   #:moonlab-campaign-circuit
   #:moonlab-campaign-implementation-id
   #:moonlab-campaign-required-abi
   #:moonlab-campaign-tolerance
   #:moonlab-campaign-id
   #:run-moonlab-campaign
   #:replay-moonlab-diagnostic-bundle
   #:verify-moonlab-diagnostic-bundle
   #:moonlab-diagnostic-bundle->form
   #:moonlab-diagnostic-bundle
   #:moonlab-diagnostic-bundle-p
   #:moonlab-diagnostic-bundle-id
   #:moonlab-diagnostic-bundle-campaign-id
   #:moonlab-diagnostic-bundle-circuit
   #:moonlab-diagnostic-bundle-circuit-id
   #:moonlab-diagnostic-bundle-implementation-id
   #:moonlab-diagnostic-bundle-required-abi
   #:moonlab-diagnostic-bundle-observed-abi
   #:moonlab-diagnostic-bundle-tolerance
   #:moonlab-diagnostic-bundle-verdict
   #:moonlab-diagnostic-bundle-outcome
   #:moonlab-diagnostic-bundle-earliest-boundary
   #:moonlab-diagnostic-bundle-observations
   #:moonlab-diagnostic-bundle-error-status
   #:moonlab-diagnostic-bundle->wire-value
   #:moonlab-diagnostic-bundle-from-wire-value
   #:make-moonlab-diagnostic-wire-graph
   #:register-moonlab-diagnostic-adapter

   ;; Generated campaigns and the wider Moonlab public-consumer surface.
   #:make-moonlab-surface-campaign
   #:moonlab-surface-campaign
   #:moonlab-surface-campaign-p
   #:moonlab-surface-campaign-spec
   #:moonlab-surface-campaign-id
   #:run-moonlab-surface-campaign
   #:verify-moonlab-surface-diagnostic-bundle
   #:replay-moonlab-surface-diagnostic-bundle
   #:moonlab-surface-diagnostic-bundle->form
   #:moonlab-surface-diagnostic-bundle
   #:moonlab-surface-diagnostic-bundle-p
   #:moonlab-surface-diagnostic-bundle-id
   #:moonlab-surface-diagnostic-bundle-campaign-id
   #:moonlab-surface-diagnostic-bundle-spec
   #:moonlab-surface-diagnostic-bundle-implementation-id
   #:moonlab-surface-diagnostic-bundle-verdict
   #:moonlab-surface-diagnostic-bundle-outcome
   #:moonlab-surface-diagnostic-bundle-earliest-boundary
   #:moonlab-surface-diagnostic-bundle-observations
   #:moonlab-surface-diagnostic-bundle-error-status
   #:make-eshkol-source-generator
   #:shrink-eshkol-source
   #:make-moonlab-surface-spec-generator
   #:shrink-moonlab-surface-spec
   #:run-generated-eshkol-campaign
   #:run-generated-moonlab-campaign
   #:generated-diagnostic-report
   #:generated-diagnostic-report-p
   #:generated-diagnostic-report-id
   #:generated-diagnostic-report-kind
   #:generated-diagnostic-report-parameters
   #:generated-diagnostic-report-seed
   #:generated-diagnostic-report-requested-trials
   #:generated-diagnostic-report-trials
   #:generated-diagnostic-report-verdict
   #:generated-diagnostic-report-counterexample
   #:generated-diagnostic-report-minimized
   #:generated-diagnostic-report-shrink-steps
   #:generated-diagnostic-report-evidence
   #:generated-diagnostic-report->form
   #:verify-generated-diagnostic-report

   #:make-cross-boundary-diagnostic-wire-graph
   #:register-cross-boundary-diagnostic-adapter
   #:cross-boundary-diagnostic-bundle
   #:cross-boundary-diagnostic-bundle-p
   #:cross-boundary-diagnostic-bundle-id
   #:cross-boundary-diagnostic-bundle-verdict
   #:cross-boundary-diagnostic-bundle-earliest-boundary
   #:cross-boundary-diagnostic-bundle-eshkol-bundle
   #:cross-boundary-diagnostic-bundle-moonlab-bundle
   #:cross-boundary-diagnostic-bundle-spec
   #:cross-boundary-diagnostic-bundle->form
   #:verify-cross-boundary-diagnostic-bundle))
