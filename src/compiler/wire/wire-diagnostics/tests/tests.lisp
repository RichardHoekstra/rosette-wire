;;;; tests.lisp --- executable obligations for Wire diagnostics.

(defpackage #:rosette-wire-diagnostics/tests
  (:use #:cl #:rosette-wire-diagnostics)
  (:local-nicknames (#:esh #:rosette-front-door/eshkol)
                    (#:front #:rosette-front-door)
                    (#:json #:rosette-json)
                    (#:rw #:rosette-wire))
  (:import-from #:rosette-assert-core #:assert-true)
  (:export #:run-all-tests))

(in-package #:rosette-wire-diagnostics/tests)

(defparameter *surface* '((main 7)))
(defparameter *moonlab-circuit*
  '(:qubits 2 :basis-index 0 :gates (("h" 0) ("cnot" 0 1))))

(defun fake-runner-path ()
  (uiop:native-namestring
   (asdf:system-relative-pathname
    :front-door/eshkol/tests "tests/fixtures/fake-eshkol.sh")))

(defun fake-moonlab-path ()
  (uiop:native-namestring
   (asdf:system-relative-pathname
    :wire-diagnostics "tests/fixtures/fake-moonlab.sh")))

(defun program-id (surface)
  (nth-value
   1
   (esh:emit-eshkol-source (front:surface->program-ir surface))))

(defun fake-command (surface value &key (mode "ok") wrong-program-id)
  (list "env"
        (format nil "ROSETTE_FAKE_ESHKOL_MODE=~A" mode)
        (format nil "ROSETTE_FAKE_ESHKOL_PROGRAM_ID=~A"
                (if wrong-program-id
                    "wrong-program-id"
                    (program-id surface)))
        (format nil "ROSETTE_FAKE_ESHKOL_VALUE=~D" value)
        "sh" (fake-runner-path)))

(defun signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun run-all-tests ()
  (let* ((campaign
           (make-eshkol-campaign *surface* :toolchain-id "fake-eshkol-1"))
         (same
           (make-eshkol-campaign *surface* :toolchain-id "fake-eshkol-1"))
         (changed-toolchain
           (make-eshkol-campaign *surface* :toolchain-id "fake-eshkol-2")))
    (assert-true (string= (eshkol-campaign-id campaign)
                          (eshkol-campaign-id same))
                 "same campaign has the same durable identity")
    (assert-true (not (string= (eshkol-campaign-id campaign)
                               (eshkol-campaign-id changed-toolchain)))
                 "implementation identity drift changes the campaign")
    (assert-true
     (signals-error-p
      (lambda ()
        (make-eshkol-campaign *surface* :toolchain-id "/private/tool")))
     "toolchain identity refuses path-shaped private configuration")

    (let* ((command (fake-command *surface* 7))
           (bundle
             (run-eshkol-campaign campaign :eshkol-command command)))
      (assert-true (eq :pass (diagnostic-bundle-verdict bundle))
                   "equal direct/kernel/JIT/AOT values pass")
      (assert-true (eq :complete
                       (diagnostic-bundle-earliest-boundary bundle))
                   "passing campaign reaches the complete boundary")
      (assert-true (verify-diagnostic-bundle bundle)
                   "fresh bundle verifies by recomputing both identities")
      (assert-true
       (not (search (fake-runner-path)
                    (prin1-to-string (diagnostic-bundle->form bundle))))
       "serialized evidence excludes the local runner command")
      (multiple-value-bind (fresh identical-p)
          (replay-diagnostic-bundle bundle :eshkol-command command)
        (assert-true identical-p
                     "replay of unchanged code and floors preserves identity")
        (assert-true (verify-diagnostic-bundle fresh)
                     "the replayed bundle independently verifies"))
      (let ((tampered
              (rosette-wire-diagnostics::copy-diagnostic-bundle bundle)))
        (setf (diagnostic-bundle-verdict tampered) :fail)
        (assert-true (not (verify-diagnostic-bundle tampered))
                     "changed verdict invalidates the content identity")))

    (let ((bundle
            (run-eshkol-campaign
             campaign :eshkol-command (fake-command *surface* 8))))
      (assert-true (eq :fail (diagnostic-bundle-verdict bundle))
                   "an executed floor disagreement fails")
      (assert-true (eq :disagreement (diagnostic-bundle-outcome bundle))
                   "semantic mismatch remains distinct from runner failure")
      (assert-true (eq :eshkol-jit
                       (diagnostic-bundle-earliest-boundary bundle))
                   "the first observed mismatch is localized to JIT")
      (assert-true (= 2 (length (diagnostic-bundle-disagreements bundle)))
                   "both disagreeing Eshkol floors remain visible")
      (assert-true (verify-diagnostic-bundle bundle)
                   "failing evidence is still an internally valid bundle"))

    (let ((bundle
            (run-eshkol-campaign
             campaign
             :eshkol-command
             (fake-command *surface* 7 :wrong-program-id t))))
      (assert-true (eq :fail (diagnostic-bundle-verdict bundle))
                   "a receipt bound to the wrong program fails")
      (assert-true (eq :jit (diagnostic-bundle-earliest-boundary bundle))
                   "malformed program binding is localized to JIT intake")
      (assert-true (eq :malformed-receipt
                       (diagnostic-bundle-error-status bundle))
                   "the structured refusal preserves its machine status")
      (assert-true (verify-diagnostic-bundle bundle)
                   "the malformed-binding failure is replayable evidence"))

    (let ((bundle
            (run-eshkol-campaign
             campaign
             :eshkol-command
             (fake-command *surface* 7 :mode "availability-fail"))))
      (assert-true (eq :unavailable (diagnostic-bundle-verdict bundle))
                   "missing execution authority is never a pass")
      (assert-true (eq :availability
                       (diagnostic-bundle-earliest-boundary bundle))
                   "unavailability is localized before execution")
      (assert-true (verify-diagnostic-bundle bundle)
                   "unavailability also has a valid receipt")))

  (let* ((campaign
           (make-eshkol-campaign '((main (+ 1)))
                                 :toolchain-id "fake-eshkol-1"))
         (bundle
           (run-eshkol-campaign
            campaign :eshkol-command "definitely-not-executed")))
    (assert-true (eq :refused (diagnostic-bundle-outcome bundle))
                 "invalid source is refused before tool execution")
    (assert-true (eq :front-door-admission
                     (diagnostic-bundle-earliest-boundary bundle))
                 "source refusal names the earliest public boundary")
    (assert-true (verify-diagnostic-bundle bundle)
                 "admission refusal is content-bound evidence"))

  ;; Opt-in real-toolchain rung. The public ID is deliberately distinct from
  ;; the executable path, which never enters the diagnostic bundle.
  (let ((binary (uiop:getenv "ROSETTE_ESHKOL_BIN"))
        (toolchain-id (uiop:getenv "ROSETTE_ESHKOL_ID")))
    (when (and binary (plusp (length binary)))
      (assert-true (and toolchain-id (plusp (length toolchain-id)))
                   "a real Eshkol run requires a public ROSETTE_ESHKOL_ID")
      (let* ((campaign
               (make-eshkol-campaign
                '((main (+ (* 6 7) (% 11 3))))
                :toolchain-id toolchain-id))
             (command (list binary))
             (bundle
               (run-eshkol-campaign campaign :eshkol-command command)))
        (format t "~&  INFO  real Wire diagnostic via ~A~%" toolchain-id)
        (assert-true (eq :pass (diagnostic-bundle-verdict bundle))
                     "real Eshkol direct/kernel/JIT/AOT campaign passes")
        (assert-true (verify-diagnostic-bundle bundle)
                     "real Eshkol evidence verifies")
        (assert-true
         (not (search binary
                      (prin1-to-string (diagnostic-bundle->form bundle))))
         "real executable path is absent from portable evidence")
        (multiple-value-bind (fresh identical-p)
            (replay-diagnostic-bundle bundle :eshkol-command command)
          (assert-true identical-p
                       "real Eshkol replay preserves the bundle identity")
          (assert-true (verify-diagnostic-bundle fresh)
                       "real replay evidence independently verifies")))))

  (let* ((campaign
           (make-moonlab-campaign
            *moonlab-circuit* :implementation-id "fake-moonlab-1.2.0"))
         (same
           (make-moonlab-campaign
            *moonlab-circuit* :implementation-id "fake-moonlab-1.2.0"))
         (drifted
           (make-moonlab-campaign
            *moonlab-circuit* :implementation-id "fake-moonlab-1.2.1"))
         (command (list "sh" (fake-moonlab-path))))
    (assert-true (string= (moonlab-campaign-id campaign)
                          (moonlab-campaign-id same))
                 "same Moonlab campaign has stable identity")
    (assert-true (not (string= (moonlab-campaign-id campaign)
                               (moonlab-campaign-id drifted)))
                 "Moonlab implementation drift changes campaign identity")
    (let ((bundle (run-moonlab-campaign
                   campaign :moonlab-command command :moonlab-library "/fake/lib")))
      (assert-true (eq :pass (moonlab-diagnostic-bundle-verdict bundle))
                   "quantum-vm and fake Moonlab Bell states agree")
      (assert-true (verify-moonlab-diagnostic-bundle bundle)
                   "Moonlab pass recomputes state, fidelity, and identities")
      (assert-true
       (not (search (fake-moonlab-path)
                    (prin1-to-string (moonlab-diagnostic-bundle->form bundle))))
       "Moonlab probe path is excluded from portable evidence")
      (assert-true
       (not (search "/fake/lib"
                    (prin1-to-string (moonlab-diagnostic-bundle->form bundle))))
       "Moonlab library path is excluded from portable evidence")
      (multiple-value-bind (fresh identical-p)
          (replay-moonlab-diagnostic-bundle
           bundle :moonlab-command command :moonlab-library "/fake/lib")
        (assert-true identical-p "Moonlab replay preserves exact bundle identity")
        (assert-true (verify-moonlab-diagnostic-bundle fresh)
                     "replayed Moonlab evidence independently verifies"))
      (let ((tampered
              (rosette-wire-diagnostics::copy-moonlab-diagnostic-bundle bundle)))
        (setf (moonlab-diagnostic-bundle-observed-abi tampered) '(9 9 9))
        (assert-true (not (verify-moonlab-diagnostic-bundle tampered))
                     "Moonlab ABI tampering invalidates the bundle")))
    (let* ((mismatch-command
             (list "env" "ROSETTE_FAKE_MOONLAB_MODE=mismatch"
                   "sh" (fake-moonlab-path)))
           (bundle (run-moonlab-campaign
                    campaign :moonlab-command mismatch-command
                    :moonlab-library "/fake/lib")))
      (assert-true (eq :fail (moonlab-diagnostic-bundle-verdict bundle))
                   "Moonlab state mismatch fails")
      (assert-true (eq :moonlab-computation
                       (moonlab-diagnostic-bundle-earliest-boundary bundle))
                   "state mismatch localizes after native transfer")
      (assert-true (verify-moonlab-diagnostic-bundle bundle)
                   "Moonlab mismatch remains valid evidence"))
    (let* ((old-command
             (list "env" "ROSETTE_FAKE_MOONLAB_MODE=old-abi"
                   "sh" (fake-moonlab-path)))
           (bundle (run-moonlab-campaign
                    campaign :moonlab-command old-command
                    :moonlab-library "/fake/lib")))
      (assert-true (eq :refused (moonlab-diagnostic-bundle-outcome bundle))
                   "an old Moonlab ABI is refused, not executed")
      (assert-true (verify-moonlab-diagnostic-bundle bundle)
                   "ABI refusal is content-bound evidence"))
    (let ((bundle (run-moonlab-campaign campaign)))
      (assert-true (eq :unavailable (moonlab-diagnostic-bundle-verdict bundle))
                   "absent Moonlab is unavailable, never pass")
      (assert-true (verify-moonlab-diagnostic-bundle bundle)
                   "Moonlab unavailability verifies")))

  ;; The wider probe is still an isolated downstream consumer.  It covers
  ;; measurement/collapse, Kraus completeness, a caller-owned QGT callback,
  ;; the stable exact-gradient entry, ownership failures, and an optional GPU
  ;; rung whose CPU-only result is explicitly :UNAVAILABLE rather than PASS.
  (let* ((spec '(:measurement-step 3 :channel-step 7 :qgt-phase 2
                 :gradient-a 3 :gradient-b -2))
         (campaign (make-moonlab-surface-campaign
                    spec :implementation-id "fake-moonlab-surfaces-1"))
         (command (list "sh" (fake-moonlab-path)))
         (bundle (run-moonlab-surface-campaign
                  campaign :moonlab-command command
                  :moonlab-library "/fake/lib")))
    (assert-true (eq :pass
                     (moonlab-surface-diagnostic-bundle-verdict bundle))
                 "all required Moonlab public surfaces pass")
    (assert-true
     (eq :unavailable
         (getf (getf (moonlab-surface-diagnostic-bundle-observations bundle)
                     :gpu)
               :verdict))
     "the optional CPU-only GPU rung is unavailable, never called pass")
    (assert-true (verify-moonlab-surface-diagnostic-bundle bundle)
                 "surface evidence independently recomputes every law")
    (assert-true
     (not (search (fake-moonlab-path)
                  (prin1-to-string
                   (moonlab-surface-diagnostic-bundle->form bundle))))
     "surface evidence excludes the private probe command")
    (multiple-value-bind (fresh identical-p)
        (replay-moonlab-surface-diagnostic-bundle
         bundle :moonlab-command command :moonlab-library "/fake/lib")
      (assert-true identical-p "surface replay preserves exact identity")
      (assert-true (verify-moonlab-surface-diagnostic-bundle fresh)
                   "surface replay verifies"))
    (let* ((bad-command
             (list "env" "ROSETTE_FAKE_MOONLAB_MODE=surface-gradient-mismatch"
                   "sh" (fake-moonlab-path)))
           (bad (run-moonlab-surface-campaign
                 campaign :moonlab-command bad-command
                 :moonlab-library "/fake/lib")))
      (assert-true (eq :moonlab-native-gradient
                       (moonlab-surface-diagnostic-bundle-earliest-boundary bad))
                   "gradient disagreement is located at the native-gradient seam")
      (assert-true (verify-moonlab-surface-diagnostic-bundle bad)
                   "a surface disagreement remains valid evidence")))

  ;; Generated Eshkol semantics: CHECK owns the seeded trial stream and greedy
  ;; shrink. Every passing trial bundle is retained, and report verification
  ;; regenerates the inputs before re-running each bundle law.
  (labels ((eshkol-runner (delta)
             (lambda (source)
               (let ((value (front:eval source)))
                 (run-eshkol-campaign
                  (make-eshkol-campaign source
                                        :toolchain-id "fake-generated-eshkol")
                  :eshkol-command (fake-command source (+ value delta)))))))
    (let* ((one (run-generated-eshkol-campaign
                 :seed 731 :trials 10 :max-depth 2
                 :runner (eshkol-runner 0)))
           (two (run-generated-eshkol-campaign
                 :seed 731 :trials 10 :max-depth 2
                 :runner (eshkol-runner 0))))
      (assert-true (eq :pass (generated-diagnostic-report-verdict one))
                   "seeded generated Eshkol semantics pass")
      (assert-true (= 10 (length (generated-diagnostic-report-evidence one)))
                   "every passing Eshkol trial retains a verified bundle")
      (assert-true (string= (generated-diagnostic-report-id one)
                            (generated-diagnostic-report-id two))
                   "same Eshkol seed produces byte-identical report identity")
      (assert-true (verify-generated-diagnostic-report one)
                   "Eshkol report regenerates inputs and verifies all bundles"))
    (let ((failure (run-generated-eshkol-campaign
                    :seed 17 :trials 6 :max-depth 2
                    :runner (eshkol-runner 1))))
      (assert-true (eq :counterexample
                       (generated-diagnostic-report-verdict failure))
                   "a planted Eshkol semantic fault is found")
      (assert-true (equal '((:main 0))
                          (generated-diagnostic-report-minimized failure))
                   "the Eshkol fault deterministically shrinks to MAIN 0")
      (assert-true (verify-generated-diagnostic-report failure)
                   "the minimized Eshkol counterexample report verifies")))

  (let* ((command (list "sh" (fake-moonlab-path)))
         (runner
           (lambda (spec)
             (run-moonlab-surface-campaign
              (make-moonlab-surface-campaign
               spec :implementation-id "fake-generated-moonlab")
              :moonlab-command command :moonlab-library "/fake/lib")))
         (one (run-generated-moonlab-campaign
               :seed 991 :trials 8 :runner runner))
         (two (run-generated-moonlab-campaign
               :seed 991 :trials 8 :runner runner)))
    (assert-true (eq :pass (generated-diagnostic-report-verdict one))
                 "seeded generated Moonlab public surfaces pass")
    (assert-true (string= (generated-diagnostic-report-id one)
                          (generated-diagnostic-report-id two))
                 "same Moonlab seed produces the same report identity")
    (assert-true (verify-generated-diagnostic-report one)
                 "Moonlab report regenerates specs and verifies every bundle")
    (let* ((bad-command
             (list "env" "ROSETTE_FAKE_MOONLAB_MODE=surface-qgt-mismatch"
                   "sh" (fake-moonlab-path)))
           (failure
             (run-generated-moonlab-campaign
              :seed 19 :trials 6
              :runner
              (lambda (spec)
                (run-moonlab-surface-campaign
                 (make-moonlab-surface-campaign
                  spec :implementation-id "fake-generated-moonlab")
                 :moonlab-command bad-command
                 :moonlab-library "/fake/lib")))))
      (assert-true (eq :counterexample
                       (generated-diagnostic-report-verdict failure))
                   "a planted QGT callback fault is found and shrunk")
      (assert-true (verify-generated-diagnostic-report failure)
                   "the minimized Moonlab counterexample verifies")))

  ;; The existing campaigns are ordinary immutable Wire graphs. Their runner
  ;; closures own private executable configuration; graph values and receipts
  ;; carry only canonical campaign/bundle data and public artifact identities.
  (let* ((campaign
           (make-eshkol-campaign *surface* :toolchain-id "fake-eshkol-1"))
         (dependency-set-id (rw:canonical-id '(("lock" . "test-v1"))))
         (graph (make-eshkol-diagnostic-wire-graph
                 campaign :dependency-set-id dependency-set-id))
         (drifted-graph
           (make-eshkol-diagnostic-wire-graph
            (make-eshkol-campaign *surface* :toolchain-id "fake-eshkol-2")
            :dependency-set-id dependency-set-id))
         (runner (rw:make-wire-runner))
         (command (fake-command *surface* 7)))
    (register-eshkol-diagnostic-adapter runner :eshkol-command command)
    (assert-true
     (eq :pass (rw:wire-receipt-verdict (rw:validate-wire-graph graph)))
     "Eshkol diagnostic graph validates as an ordinary Wire graph")
    (assert-true (not (string= (rw:wire-graph-id graph)
                               (rw:wire-graph-id drifted-graph)))
                 "public Eshkol implementation drift changes graph identity")
    (let* ((execution (rw:run-wire-graph graph runner nil))
           (verification (rw:verify-wire-receipt graph execution runner))
           (certification (rw:certify-wire-graph graph runner nil))
           (output (cdr (assoc "diagnostic"
                               (rw:wire-receipt-outputs execution)
                               :test #'string=)))
           (wire-value (json:json-parse (cdr (assoc "bundle" output
                                                    :test #'string=))))
           (bundle (eshkol-diagnostic-bundle-from-wire-value wire-value)))
      (assert-true (eq :pass (rw:wire-receipt-verdict execution))
                   "Eshkol graph executes within the one-step bound")
      (assert-true (eq :pass (rw:wire-receipt-verdict verification))
                   "Wire verifier reconstructs and reruns Eshkol bundle laws")
      (assert-true (eq :pass (rw:wire-receipt-verdict certification))
                   "dependency-locked Eshkol graph certifies")
      (assert-true (verify-diagnostic-bundle bundle)
                   "Eshkol receipt reconstructs the exact verified bundle")
      (assert-true
       (string= (diagnostic-bundle-id bundle)
                (cdr (assoc "bundleId" output :test #'string=)))
       "Eshkol receipt metadata is bound to the reconstructed bundle")
      (assert-true
       (not (search (fake-runner-path)
                    (rw:canonical-json (rw:wire-receipt->value execution))))
       "Eshkol Wire receipt excludes the private runner path")
      (let ((tampered (copy-tree wire-value)))
        (setf (cdr (assoc "schema" tampered :test #'string=)) "wrong")
        (assert-true
         (signals-error-p
          (lambda () (eshkol-diagnostic-bundle-from-wire-value tampered)))
         "Eshkol wire bundle schema drift is refused"))))

  (let* ((campaign
           (make-moonlab-campaign
            *moonlab-circuit* :implementation-id "fake-moonlab-1.2.0"))
         (graph (make-moonlab-diagnostic-wire-graph campaign))
         (drifted-graph
           (make-moonlab-diagnostic-wire-graph
            (make-moonlab-campaign
             *moonlab-circuit* :implementation-id "fake-moonlab-1.2.1")))
         (runner (rw:make-wire-runner))
         (command (list "sh" (fake-moonlab-path))))
    (register-moonlab-diagnostic-adapter
     runner :moonlab-command command :moonlab-library "/fake/lib")
    (assert-true
     (eq :pass (rw:wire-receipt-verdict (rw:validate-wire-graph graph)))
     "Moonlab diagnostic graph validates as an ordinary Wire graph")
    (assert-true (not (string= (rw:wire-graph-id graph)
                               (rw:wire-graph-id drifted-graph)))
                 "public Moonlab implementation drift changes graph identity")
    (let* ((execution (rw:run-wire-graph graph runner nil))
           (verification (rw:verify-wire-receipt graph execution runner))
           (output (cdr (assoc "diagnostic"
                               (rw:wire-receipt-outputs execution)
                               :test #'string=)))
           (wire-value (json:json-parse (cdr (assoc "bundle" output
                                                    :test #'string=))))
           (bundle (moonlab-diagnostic-bundle-from-wire-value wire-value)))
      (assert-true (eq :pass (rw:wire-receipt-verdict execution))
                   "Moonlab graph executes within the one-step bound")
      (assert-true (eq :pass (rw:wire-receipt-verdict verification))
                   "Wire verifier reconstructs and reruns Moonlab bundle laws")
      (assert-true (verify-moonlab-diagnostic-bundle bundle)
                   "Moonlab receipt reconstructs exact floating evidence")
      (assert-true
       (string= (moonlab-diagnostic-bundle-id bundle)
                (cdr (assoc "bundleId" output :test #'string=)))
       "Moonlab receipt metadata is bound to the reconstructed bundle")
      (let ((receipt-json
              (rw:canonical-json (rw:wire-receipt->value execution))))
        (assert-true (not (search (fake-moonlab-path) receipt-json))
                     "Moonlab Wire receipt excludes the probe path")
        (assert-true (not (search "/fake/lib" receipt-json))
                     "Moonlab Wire receipt excludes the library path"))
      (let* ((tampered (copy-tree wire-value))
             (form (cdr (assoc "form" tampered :test #'string=))))
        (setf (cdr (assoc "kind" form :test #'string=)) "unknown")
        (assert-true
         (signals-error-p
          (lambda () (moonlab-diagnostic-bundle-from-wire-value tampered)))
         "Moonlab tagged evidence drift is refused"))))

  ;; One graph now carries the whole diagnosis chain.  The transfer step uses
  ;; the verified Eshkol integer to derive the Moonlab lattice request; the
  ;; final component and receipt verifier independently reconstruct both child
  ;; bundles and reject a binding that was not so derived.
  (let* ((eshkol
           (make-eshkol-campaign '((main 7))
                                 :toolchain-id "fake-cross-eshkol"))
         (graph
           (make-cross-boundary-diagnostic-wire-graph
            eshkol :moonlab-implementation-id "fake-cross-moonlab"))
         (drifted
           (make-cross-boundary-diagnostic-wire-graph
            eshkol :moonlab-implementation-id "fake-cross-moonlab-2"))
         (runner (rw:make-wire-runner)))
    (register-cross-boundary-diagnostic-adapter
     runner :eshkol-command (fake-command '((main 7)) 7)
     :moonlab-command (list "sh" (fake-moonlab-path))
     :moonlab-library "/fake/lib")
    (let ((validation (rw:validate-wire-graph graph)))
      (assert-true (eq :pass (rw:wire-receipt-verdict validation))
                   "the four-step cross-boundary graph validates")
      (assert-true
       (equal '("diagnose-eshkol" "bind-abi" "diagnose-moonlab"
                "verify-boundary")
              (rw:wire-receipt-step-order validation))
       "the diagnosis order is graph binding, Eshkol, ABI, Moonlab, verifier"))
    (assert-true (not (string= (rw:wire-graph-id graph)
                               (rw:wire-graph-id drifted)))
                 "Moonlab implementation drift changes cross-graph identity")
    (let* ((execution (rw:run-wire-graph graph runner nil))
           (verification (rw:verify-wire-receipt graph execution runner))
           (output (cdr (assoc "diagnostic"
                               (rw:wire-receipt-outputs execution)
                               :test #'string=)))
           (bundle
             (rosette-wire-diagnostics::%cross-boundary-bundle-from-wire-value
              (json:json-parse (cdr (assoc "bundle" output
                                           :test #'string=))))))
      (assert-true (eq :pass (rw:wire-receipt-verdict execution))
                   "the bounded four-step boundary graph executes")
      (assert-true (eq :pass (rw:wire-receipt-verdict verification))
                   "the receipt verifier reconstructs the complete chain")
      (assert-true (verify-cross-boundary-diagnostic-bundle bundle)
                   "the cross-boundary bundle independently verifies")
      (assert-true
       (equal (generated-diagnostic-report-minimized
               (run-generated-eshkol-campaign
                :seed 17 :trials 1 :max-depth 0
                :runner
                (lambda (source)
                  (run-eshkol-campaign
                   (make-eshkol-campaign source
                                         :toolchain-id "cross-test-mutant")
                   :eshkol-command
                   (fake-command source (1+ (front:eval source)))))))
              '((:main 0)))
       "the cross test retains the generated-shrinker seam")
      (assert-true
       (equal '(:measurement-step 7 :channel-step 7 :qgt-phase 3
                :gradient-a -9 :gradient-b -16)
              (cross-boundary-diagnostic-bundle-spec bundle))
       "the Moonlab request is deterministically derived from Eshkol value 7")
      (let ((tampered
              (rosette-wire-diagnostics::copy-cross-boundary-diagnostic-bundle
               bundle)))
        (setf (cross-boundary-diagnostic-bundle-spec tampered)
              '(:measurement-step 0 :channel-step 0 :qgt-phase 0
                :gradient-a 0 :gradient-b 0))
        (assert-true (not (verify-cross-boundary-diagnostic-bundle tampered))
                     "cross-boundary binding tampering is refused"))))

  (let ((probe (uiop:getenv "ROSETTE_MOONLAB_PROBE"))
        (library (uiop:getenv "ROSETTE_MOONLAB_LIB"))
        (implementation-id (uiop:getenv "ROSETTE_MOONLAB_ID")))
    (when (and probe (plusp (length probe)))
      (assert-true (and library (plusp (length library))
                        implementation-id (plusp (length implementation-id)))
                   "real Moonlab requires library path and public identity")
      (let* ((campaign
               (make-moonlab-campaign
                '(:qubits 2 :basis-index 1
                  :gates (("h" 0) ("ry" 0.37d0 1)
                          ("cnot" 0 1) ("rz" -0.22d0 0)))
                :implementation-id implementation-id))
             (bundle (run-moonlab-campaign
                      campaign :moonlab-command (list probe)
                      :moonlab-library library)))
        (format t "~&  INFO  real Moonlab diagnostic via ~A~%" implementation-id)
        (assert-true (eq :pass (moonlab-diagnostic-bundle-verdict bundle))
                     "real Moonlab and quantum-vm states agree")
        (assert-true (verify-moonlab-diagnostic-bundle bundle)
                     "real Moonlab evidence verifies")
        (multiple-value-bind (fresh identical-p)
            (replay-moonlab-diagnostic-bundle
             bundle :moonlab-command (list probe) :moonlab-library library)
          (assert-true identical-p "real Moonlab replay preserves identity")
          (assert-true (verify-moonlab-diagnostic-bundle fresh)
                       "real Moonlab replay verifies")))
      (let* ((campaign
               (make-moonlab-surface-campaign
                '(:measurement-step 3 :channel-step 7 :qgt-phase 2
                  :gradient-a 3 :gradient-b -2)
                :implementation-id implementation-id))
             (bundle
               (run-moonlab-surface-campaign
                campaign :moonlab-command (list probe)
                :moonlab-library library)))
        (format t "~&  INFO  real Moonlab surface diagnostic via ~A~%"
                implementation-id)
        (assert-true
         (or (eq :pass
                 (moonlab-surface-diagnostic-bundle-verdict bundle))
             (and (eq :fail
                      (moonlab-surface-diagnostic-bundle-verdict bundle))
                  (eq :disagreement
                      (moonlab-surface-diagnostic-bundle-outcome bundle))
                  (eq :moonlab-qgt-callback
                      (moonlab-surface-diagnostic-bundle-earliest-boundary
                       bundle))
                  (eq :qgt-law
                      (moonlab-surface-diagnostic-bundle-error-status bundle))))
         "real Moonlab surfaces pass or expose the known located QGT callback disagreement")
        (assert-true (verify-moonlab-surface-diagnostic-bundle bundle)
                     "real Moonlab surface evidence verifies"))))

  t)
