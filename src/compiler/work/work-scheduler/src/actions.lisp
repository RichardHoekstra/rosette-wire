;;;; actions.lisp --- work-action scoring, minimization, and ensembles.

(in-package #:rosette-work-scheduler)

(defun score-work-action (items &key (compute-weight 1d0)
                                     (critical-path-weight 1d0)
                                     (communication-weight 1d0)
                                     (parallelism-pressure-weight 1d0)
                                     metadata)
  "Return a scalar action score for ITEMS.

The score is a deterministic weighted sum of total compute cost, critical path
cost, read/write communication footprint, and parallelism pressure.  Smaller
scores represent cheaper execution plans under the supplied weights."
  (let* ((analysis (analyze-work-dag items))
         (compute (work-dag-analysis-total-cost analysis))
         (critical (work-dag-analysis-critical-path-cost analysis))
         (communication (%communication-footprint-cost items))
         (pressure (%parallelism-pressure analysis))
         (weights (%action-weight-plist compute-weight
                                        critical-path-weight
                                        communication-weight
                                        parallelism-pressure-weight))
         (value (+ (* compute-weight compute)
                   (* critical-path-weight critical)
                   (* communication-weight communication)
                   (* parallelism-pressure-weight pressure))))
    (%make-work-action-score
     :value value
     :compute-cost compute
     :critical-path-cost critical
     :communication-cost communication
     :parallelism-pressure pressure
     :weights weights
     :metadata metadata)))

(defun score-causal-cone-action (items targets &rest score-args)
  "Return an action score for only the causal cone required by TARGETS.

Returns two values: the WORK-ACTION-SCORE and the WORK-CAUSAL-CONE."
  (let ((cone (causal-cone items targets)))
    (values (apply #'score-work-action
                   (work-causal-cone-tasks cone)
                   score-args)
            cone)))

(defun rank-work-actions (candidates &key (compute-weight 1d0)
                                         (critical-path-weight 1d0)
                                         (communication-weight 1d0)
                                         (parallelism-pressure-weight 1d0))
  "Return CANDIDATES ranked by increasing action score.

Each candidate is a plist with `:NAME`, `:ITEMS`, and optional `:METADATA`.
The returned `work-action-choice` values retain the full score object so
callers can audit the winning decision."
  (let ((choices nil))
    (dolist (candidate candidates)
      (let ((name (getf candidate :name))
            (items (getf candidate :items))
            (metadata (getf candidate :metadata)))
        (unless name
          (error "rank-work-actions: candidate is missing :NAME."))
        (unless items
          (error "rank-work-actions: candidate ~S is missing :ITEMS." name))
        (push (%make-work-action-choice
               :name name
               :items items
               :score (score-work-action
                       items
                       :compute-weight compute-weight
                       :critical-path-weight critical-path-weight
                       :communication-weight communication-weight
                       :parallelism-pressure-weight parallelism-pressure-weight
                       :metadata metadata)
               :metadata metadata)
              choices)))
    (sort (nreverse choices) #'%work-action-choice<)))

(defun minimize-work-action (candidates &rest score-args)
  "Return the lowest-action candidate from RANK-WORK-ACTIONS."
  (first (apply #'rank-work-actions candidates score-args)))

(defun rank-causal-cone-actions (items targets &key (compute-weight 1d0)
                                            (critical-path-weight 1d0)
                                            (communication-weight 1d0)
                                            (parallelism-pressure-weight 1d0))
  "Return TARGETS ranked by their causal-cone action scores.

Each returned `work-action-choice` uses the target name as `NAME`, stores the
causal cone under `CONE`, and stores only the cone tasks under `ITEMS`."
  (let ((choices nil))
    (dolist (target (%target-list targets))
      (multiple-value-bind (score cone)
          (score-causal-cone-action
           items target
           :compute-weight compute-weight
           :critical-path-weight critical-path-weight
           :communication-weight communication-weight
           :parallelism-pressure-weight parallelism-pressure-weight
           :metadata (list :target target))
        (push (%make-work-action-choice
               :name target
               :score score
               :items (work-causal-cone-tasks cone)
               :cone cone
               :metadata (list :target target))
              choices)))
    (sort (nreverse choices) #'%work-action-choice<)))

(defun minimize-causal-cone-action (items targets &rest score-args)
  "Return the cheapest target choice from RANK-CAUSAL-CONE-ACTIONS."
  (first (apply #'rank-causal-cone-actions items targets score-args)))

(defun work-action-ensemble (candidates &key (temperature 1d0)
                                             metadata
                                             (compute-weight 1d0)
                                             (critical-path-weight 1d0)
                                             (communication-weight 1d0)
                                             (parallelism-pressure-weight 1d0))
  "Return a Boltzmann ensemble over candidate work actions.

The existing action score is treated as an energy.  TEMPERATURE controls how
hard the decision is: small values concentrate probability on the minimum
action candidate, while larger values keep near-optimal candidates visible for
sampling, exploration, or diagnostics."
  (%assert-temperature temperature 'work-action-ensemble)
  (let ((choices (rank-work-actions
                  candidates
                  :compute-weight compute-weight
                  :critical-path-weight critical-path-weight
                  :communication-weight communication-weight
                  :parallelism-pressure-weight parallelism-pressure-weight)))
    (%choices->ensemble choices temperature metadata)))

(defun causal-cone-action-ensemble (items targets &key (temperature 1d0)
                                                   metadata
                                                   (compute-weight 1d0)
                                                   (critical-path-weight 1d0)
                                                   (communication-weight 1d0)
                                                   (parallelism-pressure-weight 1d0))
  "Return a Boltzmann ensemble over target causal cones."
  (%assert-temperature temperature 'causal-cone-action-ensemble)
  (let ((choices (rank-causal-cone-actions
                  items targets
                  :compute-weight compute-weight
                  :critical-path-weight critical-path-weight
                  :communication-weight communication-weight
                  :parallelism-pressure-weight parallelism-pressure-weight)))
    (%choices->ensemble choices temperature metadata)))

(defun action-choice-certificate (candidates &key (name :work-action-choice)
                                                   metadata
                                                   (compute-weight 1d0)
                                                   (critical-path-weight 1d0)
                                                   (communication-weight 1d0)
                                                   (parallelism-pressure-weight 1d0))
  "Return a proof-witness certificate for the lowest-action candidate.

The certificate records candidate work signatures, score weights, all ranked
scores, and the winning candidate name.  Verification recomputes the ranking
from current descriptors; the stored winner is treated as a claim, not proof."
  (let* ((ranked (rank-work-actions
                  candidates
                  :compute-weight compute-weight
                  :critical-path-weight critical-path-weight
                  :communication-weight communication-weight
                  :parallelism-pressure-weight parallelism-pressure-weight))
         (winner (first ranked))
         (payload (list :candidates (%candidate-signatures candidates)
                        :weights (%action-weight-plist compute-weight
                                                       critical-path-weight
                                                       communication-weight
                                                       parallelism-pressure-weight)
                        :winner (and winner (work-action-choice-name winner))
                        :scores (%action-choices->score-rows ranked))))
    (rosette-proof-witness:make-certificate
     :name name
     :kind :work-action-choice
     :claim :minimum-action-candidate
     :payload payload
     :passed (not (null winner))
     :metadata metadata)))

(defun verify-action-choice-certificate (candidates certificate)
  "Recheck an action-choice certificate against CANDIDATES.

Returns two values: a boolean and a list of verifier reasons.  The verifier
requires the same candidate names and work-item signatures, then recomputes
the action ranking under the stored weights."
  (check-type certificate rosette-proof-witness:certificate)
  (let* ((payload (rosette-proof-witness:certificate-payload certificate))
         (weights (getf payload :weights))
         (stored-winner (getf payload :winner)))
    (unless stored-winner
      (return-from verify-action-choice-certificate
        (values nil (list (list :kind :missing-payload :field :winner)))))
    (multiple-value-bind (signature-ok signature-reasons)
        (%verify-candidate-signatures candidates (getf payload :candidates))
      (unless signature-ok
        (return-from verify-action-choice-certificate
          (values nil signature-reasons))))
    (let* ((ranked (rank-work-actions
                    candidates
                    :compute-weight (getf weights :compute 1d0)
                    :critical-path-weight (getf weights :critical-path 1d0)
                    :communication-weight (getf weights :communication 1d0)
                    :parallelism-pressure-weight (getf weights :parallelism-pressure 1d0)))
           (winner (first ranked))
           (winner-name (and winner (work-action-choice-name winner))))
      (if (equal stored-winner winner-name)
          (values t nil)
          (values nil (list (list :kind :winner-mismatch
                                  :expected stored-winner
                                  :actual winner-name)))))))

(defun action-ensemble-certificate (candidates &key (name :work-action-ensemble)
                                                    metadata
                                                    (temperature 1d0)
                                                    (compute-weight 1d0)
                                                    (critical-path-weight 1d0)
                                                    (communication-weight 1d0)
                                                    (parallelism-pressure-weight 1d0))
  "Return a proof-witness certificate for a finite-temperature action ensemble.

The payload records candidate signatures, score weights, temperature,
probability rows, free energy, log partition, and the winner.  Verification
recomputes the ensemble from current descriptors and treats the stored payload
as the claim."
  (let* ((ensemble (work-action-ensemble
                   candidates
                   :temperature temperature
                   :compute-weight compute-weight
                   :critical-path-weight critical-path-weight
                   :communication-weight communication-weight
                   :parallelism-pressure-weight parallelism-pressure-weight))
         (payload (%action-ensemble-payload candidates ensemble
                                            compute-weight
                                            critical-path-weight
                                            communication-weight
                                            parallelism-pressure-weight)))
    (rosette-proof-witness:make-certificate
     :name name
     :kind :work-action-ensemble
     :claim :finite-temperature-action-ensemble
     :payload payload
     :passed (not (null (work-action-ensemble-winner ensemble)))
     :metadata metadata)))

(defun verify-action-ensemble-certificate (candidates certificate)
  "Recheck an action ensemble certificate against CANDIDATES.

Returns two values: a boolean and a list of verifier reasons.  The verifier
requires unchanged candidate descriptors, then recomputes the Boltzmann
probabilities, free energy, log partition, and winner under the stored
temperature and score weights."
  (check-type certificate rosette-proof-witness:certificate)
  (let* ((payload (rosette-proof-witness:certificate-payload certificate))
         (temperature (getf payload :temperature))
         (weights (getf payload :weights)))
    (unless temperature
      (return-from verify-action-ensemble-certificate
        (values nil (list (list :kind :missing-payload :field :temperature)))))
    (multiple-value-bind (signature-ok signature-reasons)
        (%verify-candidate-signatures candidates (getf payload :candidates))
      (unless signature-ok
        (return-from verify-action-ensemble-certificate
          (values nil signature-reasons))))
    (let* ((ensemble (work-action-ensemble
                      candidates
                      :temperature temperature
                      :compute-weight (getf weights :compute 1d0)
                      :critical-path-weight (getf weights :critical-path 1d0)
                      :communication-weight (getf weights :communication 1d0)
                      :parallelism-pressure-weight
                      (getf weights :parallelism-pressure 1d0)))
           (actual (%action-ensemble-payload
                    candidates ensemble
                    (getf weights :compute 1d0)
                    (getf weights :critical-path 1d0)
                    (getf weights :communication 1d0)
                    (getf weights :parallelism-pressure 1d0))))
      (%verify-action-ensemble-payload payload actual))))

