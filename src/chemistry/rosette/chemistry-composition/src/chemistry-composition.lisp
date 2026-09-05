;;;; chemistry-composition.lisp --- chemistry-composition implementation.

(in-package #:chemistry-composition)

(define-condition chemistry-composition-refusal (error)
  ((code :initarg :code :reader chemistry-composition-refusal-code)
   (detail :initarg :detail :reader chemistry-composition-refusal-detail))
  (:report (lambda (condition stream)
             (format stream "Chemistry Composition refused (~A): ~A"
                     (chemistry-composition-refusal-code condition)
                     (chemistry-composition-refusal-detail condition)))))

(defun %refuse (code control &rest arguments)
  (error 'chemistry-composition-refusal :code code
         :detail (apply #'format nil control arguments)))

(defun %object-ref (object key &optional default)
  (let ((entry (and (listp object) (assoc key object :test #'string=))))
    (if entry (cdr entry) default)))

(defun molecular-evidence-wire-type ()
  (let ((element
          (record-type
           (make-component-field "count" (scalar-type :u64))
           (make-component-field "symbol" (scalar-type :string)))))
    (record-type
     (make-component-field "elements" (list-type element))
     (make-component-field "formula" (scalar-type :string))
     (make-component-field "molecularWeightMilli" (scalar-type :u64)))))

(defun reaction-evidence-wire-type ()
  (list-type (expert-fact-wire-type)))

(defun %molecular-handler ()
  (lambda (arguments services context)
    (declare (ignore services context))
    (let ((formula (%object-ref arguments "formula" :missing)))
      (unless (stringp formula)
        (%refuse :invalid-formula "formula input must be a string"))
      (let ((elements (parse-formula formula))
            (weight (formula-molecular-weight formula)))
        `(("elements" .
           ,(mapcar (lambda (entry)
                      `(("count" . ,(cdr entry)) ("symbol" . ,(car entry))))
                    elements))
          ("formula" . ,formula)
          ("molecularWeightMilli" . ,(round (* 1000d0 weight))))))))

(defun %same-formula-p (left right)
  (equal (parse-formula left) (parse-formula right)))

(defun %evidence-fact (predicate &rest arguments)
  (make-expert-fact predicate arguments))

(defun %facts->value (facts)
  (mapcar #'expert-fact->value
          (sort facts #'string< :key #'expert-fact-id)))

(defun %reaction-evidence (molecular)
  (let ((formula (%object-ref molecular "formula" :missing))
        (weight (%object-ref molecular "molecularWeightMilli" :missing)))
    (unless (and (stringp formula) (integerp weight))
      (%refuse :invalid-molecular-evidence
               "molecular stage output is missing formula or mass"))
    (unless (%same-formula-p formula "CH3OH")
      (%refuse :product-mismatch
               "flagship expects methanol formula CH3OH, got ~A" formula))
    (let* ((balance
             (balance-reaction "methanol synthesis"
                               '(:co :h2) '(:methanol)))
           (reaction (reaction-balance-reaction balance))
           (coefficients (reaction-balance-coefficients balance))
           (step (make-reaction-step "methanol reactor" reaction
                                     :conversion 0.5d0))
           (network (make-reaction-network "methanol witness" (list step)))
           ;; Exactly one kmol/day CO and two kmol/day H2 on the catalog basis.
           (feed
             (make-stream
              "stoichiometric syngas"
              (list (cons :co (/ (catalog-molecular-weight
                                  *default-component-catalog* :co)
                                 1000d0))
                    (cons :h2 (/ (* 2d0 (catalog-molecular-weight
                                         *default-component-catalog* :h2))
                                 1000d0)))))
           (report
             (run-reaction-network-report "methanol outlet" feed network))
           (mass-closed
             (mass-balance-closed-p
              (reaction-network-report-mass-balance report)))
           (elements-closed
             (element-balance-closed-p
              (reaction-network-report-element-balance report)))
           (network-balanced (reaction-network-balanced-p network))
           (enthalpy (round (reaction-standard-enthalpy reaction)))
           (gibbs (round (reaction-standard-gibbs reaction)))
           (facts
             (list
              (%evidence-fact "molecule-formula" "methanol" formula)
              (%evidence-fact "molecular-weight-milli"
                              "methanol" (format nil "~D" weight))
              (%evidence-fact
               "reaction-stoichiometry" "methanol-synthesis"
               (format nil "~D" (first coefficients))
               (format nil "~D" (second coefficients))
               (format nil "~D" (third coefficients)))
              (%evidence-fact "standard-enthalpy-kj-per-kmol"
                              "methanol-synthesis" (format nil "~D" enthalpy))
              (%evidence-fact "standard-gibbs-kj-per-kmol"
                              "methanol-synthesis" (format nil "~D" gibbs)))))
      (when (reaction-balance-balanced-p balance)
        (push (%evidence-fact "exact-stoichiometry" "methanol-synthesis") facts))
      (when network-balanced
        (push (%evidence-fact "network-balanced" "methanol-synthesis") facts))
      (when mass-closed
        (push (%evidence-fact "mass-closed" "methanol-synthesis") facts))
      (when elements-closed
        (push (%evidence-fact "elements-closed" "methanol-synthesis") facts))
      (when (minusp enthalpy)
        (push (%evidence-fact "exothermic" "methanol-synthesis") facts))
      (when (minusp gibbs)
        (push (%evidence-fact "standard-gibbs-favorable"
                             "methanol-synthesis") facts))
      (%facts->value facts))))

(defun %reaction-handler ()
  (lambda (arguments services context)
    (declare (ignore services context))
    (%reaction-evidence (%object-ref arguments "molecular" nil))))

(defun chemistry-expert-system ()
  (make-expert-system
   "org.rosette/chemistry-evidence-expert"
   (list
    (make-expert-rule
     "characterize-molecule"
     (list (make-expert-fact "molecule-formula" '("?molecule" "?formula")
                             :pattern t)
           (make-expert-fact "molecular-weight-milli"
                             '("?molecule" "?weight") :pattern t))
     (make-expert-fact "molecule-characterized" '("?molecule") :pattern t))
    (make-expert-rule
     "certify-conservation"
     (list (make-expert-fact "exact-stoichiometry" '("?reaction") :pattern t)
           (make-expert-fact "network-balanced" '("?reaction") :pattern t)
           (make-expert-fact "mass-closed" '("?reaction") :pattern t)
           (make-expert-fact "elements-closed" '("?reaction") :pattern t))
     (make-expert-fact "conservation-certified" '("?reaction") :pattern t))
    (make-expert-rule
     "characterize-thermodynamics"
     (list (make-expert-fact "exothermic" '("?reaction") :pattern t)
           (make-expert-fact "standard-gibbs-favorable"
                             '("?reaction") :pattern t))
     (make-expert-fact "thermodynamics-characterized"
                       '("?reaction") :pattern t))
    (make-expert-rule
     "close-chemistry-vertical"
     (list (make-expert-fact "molecule-characterized" '("methanol")
                             :pattern t)
           (make-expert-fact "conservation-certified"
                             '("methanol-synthesis") :pattern t)
           (make-expert-fact "thermodynamics-characterized"
                             '("methanol-synthesis") :pattern t))
     (make-expert-fact "chemistry-vertical-complete"
                       '("methanol-synthesis") :pattern t)))))

(defun %molecular-component ()
  (make-component-descriptor
   :name "org.rosette/chemistry-molecular" :version "1.0.0"
   :imports nil
   :exports
   (list
    (make-port
     "molecular"
     (list
      (make-component-operation
       "analyze-formula"
       (list (make-component-field "formula" (scalar-type :string)))
       (molecular-evidence-wire-type)))))
   :effects '(:pure) :capabilities nil
   :adapter '(("kind" . "rosette-chemical-formula-v1"))
   :verifiers '("chemistry-vertical/v1")))

(defun %reaction-component ()
  (make-component-descriptor
   :name "org.rosette/chemistry-reaction" :version "1.0.0"
   :imports nil
   :exports
   (list
    (make-port
     "reaction"
     (list
      (make-component-operation
       "methanol-evidence"
       (list (make-component-field "molecular"
                                   (molecular-evidence-wire-type)))
       (reaction-evidence-wire-type)))))
   :effects '(:pure) :capabilities nil
   :adapter
   '(("kind" . "rosette-chemistry-stack-v1")
     ("libraries" . ("reaction-balancer" "reaction-network"
                      "chemical-thermo")))
   :verifiers '("chemistry-vertical/v1")))

(defun make-chemistry-composition ()
  (let* ((expert-system (chemistry-expert-system))
         (molecular (%molecular-component))
         (reaction (%reaction-component))
         (expert (make-expert-component expert-system))
         (molecular-node
           (make-component-node
            "molecular" molecular
            (canonical-id '("chemistry-composition" "molecular" "1.0.0"))))
         (reaction-node
           (make-component-node
            "reaction" reaction
            (canonical-id '("chemistry-composition" "reaction" "1.0.0"))))
         (expert-node
           (make-component-node
            "expert" expert
            (canonical-id '("chemistry-composition" "expert" "1.0.0")))))
    (make-composition
     :name "org.rosette/flagship-methanol-chemistry"
     :nodes (list molecular-node reaction-node expert-node)
     :services nil
     :steps
     (list
      (make-wire-step
       :id "molecule" :node-id "molecular" :port "molecular"
       :operation "analyze-formula"
       :bindings (list (make-data-binding "formula" (input-source "formula"))))
      (make-wire-step
       :id "reaction" :node-id "reaction" :port "reaction"
       :operation "methanol-evidence"
       :bindings
       (list (make-data-binding "molecular" (step-source "molecule"))))
      (make-wire-step
       :id "expert" :node-id "expert" :port "expert" :operation "infer"
       :bindings (list (make-data-binding "facts" (step-source "reaction")))))
     :inputs (list (make-component-field "formula" (scalar-type :string)))
     :outputs
     (list (make-wire-output "molecule" "molecule")
           (make-wire-output "reactionEvidence" "reaction")
           (make-wire-output "expertProof" "expert"))
     :capability-grants nil
     :required-evidence
     '("chemistry-vertical/v1" "expert-derivation-replay/v1")
     :limits '(("maxSteps" . 3) ("maxOutputBytes" . 262144)))))

(defun %receipt-output (receipt name)
  (cdr (assoc name (wire-receipt-outputs receipt) :test #'string=)))

(defun make-chemistry-runner ()
  (let* ((runner (make-composition-runner))
         (system (chemistry-expert-system))
         (molecular-handler (%molecular-handler))
         (reaction-handler (%reaction-handler)))
    (register-component-handler runner "molecular" "molecular"
                                "analyze-formula" molecular-handler)
    (register-component-handler runner "reaction" "reaction"
                                "methanol-evidence" reaction-handler)
    (register-component-handler runner "expert" "expert" "infer"
                                (make-expert-handler system))
    (register-component-verifier
     runner "chemistry-vertical/v1"
     (lambda (composition receipt)
       (declare (ignore composition))
       (let* ((actual-molecular (%receipt-output receipt "molecule"))
              (actual-reaction (%receipt-output receipt "reactionEvidence"))
              (formula (%object-ref actual-molecular "formula" nil))
              (expected-molecular
                (and formula
                     (funcall molecular-handler
                              (list (cons "formula" formula)) nil nil)))
              (expected-reaction
                (and expected-molecular
                     (funcall reaction-handler
                              (list (cons "molecular" expected-molecular))
                              nil nil)))
              (pass (and (equal actual-molecular expected-molecular)
                         (equal actual-reaction expected-reaction))))
         (values pass
                 `(("molecularReplay" . ,(if pass "pass" "fail"))
                   ("reactionReplay" . ,(if pass "pass" "fail")))))))
    (register-component-verifier
     runner "expert-derivation-replay/v1"
     (lambda (composition receipt)
       (declare (ignore composition))
       (let* ((facts-value (%receipt-output receipt "reactionEvidence"))
              (actual (%receipt-output receipt "expertProof"))
              (facts (and facts-value
                          (mapcar #'expert-fact-from-value facts-value)))
              (expected (and facts
                             (expert-run->value
                              (run-expert-system system facts))))
              (pass (and actual expected (equal actual expected))))
         (values pass
                 `(("expertReplay" . ,(if pass "pass" "fail")))))))
    runner))

(defun run-chemistry-flagship (&key (formula "CH3OH"))
  (run-composition (make-chemistry-composition) (make-chemistry-runner)
                   (list (cons "formula" formula))))

(defun verify-chemistry-receipt (receipt)
  (unless (wire-receipt-p receipt)
    (%refuse :invalid-receipt "expected a Rosette Wire receipt"))
  (verify-composition-receipt
   (make-chemistry-composition) receipt (make-chemistry-runner)))
