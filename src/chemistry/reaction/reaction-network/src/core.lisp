;;;; core.lisp --- Chemical reaction-network construction.

(in-package #:rosette-reaction-network)

(defstruct (reaction-step (:constructor %make-reaction-step))
  (name "" :type string)
  reaction
  (conversion 1d0 :type real)
  note)

(defun make-reaction-step (name reaction &key (conversion 1d0) note)
  "Construct one executable stoichiometric reaction-network step."
  (check-type name string)
  (check-type reaction pe:reaction)
  (unless (and (realp conversion) (<= 0d0 conversion 1d0))
    (error "Conversion must be in [0, 1]: ~S" conversion))
  (%make-reaction-step :name name
                       :reaction reaction
                       :conversion (float conversion 1d0)
                       :note note))

(defun reaction-step->plist (step)
  "Return a stable plist for STEP."
  (check-type step reaction-step)
  (list :name (reaction-step-name step)
        :reaction (pe:reaction->plist (reaction-step-reaction step))
        :conversion (reaction-step-conversion step)
        :note (reaction-step-note step)))

(defstruct (reaction-network (:constructor %make-reaction-network))
  (name "" :type string)
  (steps nil :type list)
  note)

(defun make-reaction-network (name steps &key note)
  "Construct an ordered chemical reaction network."
  (check-type name string)
  (assert (every #'reaction-step-p steps) (steps)
          "Reaction network steps must be reaction-step records.")
  (%make-reaction-network :name name :steps (copy-list steps) :note note))

(defun reaction-network-reactions (network)
  "Return the reactions used by NETWORK in step order."
  (check-type network reaction-network)
  (mapcar #'reaction-step-reaction (reaction-network-steps network)))

(defun reaction-step-balanced-p
    (step &key (catalog pe:*default-component-catalog*)
       (tolerance pe:+default-balance-tolerance+))
  "T iff STEP's native reaction is mass- and element-balanced."
  (check-type step reaction-step)
  (pe:reaction-balanced-p (reaction-step-reaction step)
                          :catalog catalog
                          :tolerance tolerance))

(defun reaction-network-balanced-p
    (network &key (catalog pe:*default-component-catalog*)
       (tolerance pe:+default-balance-tolerance+))
  "T iff every native reaction in NETWORK is balanced."
  (check-type network reaction-network)
  (every (lambda (step)
           (reaction-step-balanced-p step
                                     :catalog catalog
                                     :tolerance tolerance))
         (reaction-network-steps network)))

(defun reaction-network-invalid-steps
    (network &key (catalog pe:*default-component-catalog*)
       (tolerance pe:+default-balance-tolerance+))
  "Return step names whose native reactions are not balanced."
  (check-type network reaction-network)
  (loop for step in (reaction-network-steps network)
        unless (reaction-step-balanced-p step
                                        :catalog catalog
                                        :tolerance tolerance)
          collect (reaction-step-name step)))

(defun reaction-network-balance-report
    (network &key (catalog pe:*default-component-catalog*)
       (tolerance pe:+default-balance-tolerance+))
  "Return a stable reaction-preflight report for NETWORK."
  (check-type network reaction-network)
  (list :balanced-p
        (reaction-network-balanced-p network
                                     :catalog catalog
                                     :tolerance tolerance)
        :invalid-steps
        (reaction-network-invalid-steps network
                                        :catalog catalog
                                        :tolerance tolerance)
        :steps
        (mapcar (lambda (step)
                  (list :name (reaction-step-name step)
                        :reaction
                        (pe:reaction->plist (reaction-step-reaction step)
                                            :catalog catalog)
                        :balanced-p
                        (reaction-step-balanced-p step
                                                  :catalog catalog
                                                  :tolerance tolerance)))
                (reaction-network-steps network))))

(defun reaction-network->plist (network)
  "Return a stable plist for NETWORK."
  (check-type network reaction-network)
  (list :name (reaction-network-name network)
        :steps (mapcar #'reaction-step->plist
                       (reaction-network-steps network))
        :reaction-balance (reaction-network-balance-report network)
        :note (reaction-network-note network)))

(defstruct (reaction-step-report (:constructor %make-reaction-step-report))
  (name "" :type string)
  step inlet outlet limiting-reactant limiting-extent theoretical-yields
  mass-balance element-balance note)

(defstruct (reaction-network-report (:constructor %make-reaction-network-report))
  (name "" :type string)
  network inlet outlet step-reports mass-balance element-balance note)

(defun %theoretical-yields (inlet step catalog)
  (let ((reaction (reaction-step-reaction step)))
    (mapcar (lambda (pair)
              (let ((product (car pair)))
                (cons product
                      (pe:reaction-product-theoretical-yield
                       inlet reaction product
                       :conversion (reaction-step-conversion step)
                       :catalog catalog))))
            (pe:reaction-products reaction))))

(defun %run-step-report
    (inlet step &key catalog on-missing-formula)
  (multiple-value-bind (extent limiting)
      (pe:reaction-extent-limit inlet
                                (reaction-step-reaction step)
                                :catalog catalog)
    (let* ((outlet (pe:stoichiometric-reactor
                    (reaction-step-name step)
                    inlet
                    (reaction-step-reaction step)
                    (reaction-step-conversion step)
                    :catalog catalog
                    :note (reaction-step-note step)))
           (mass (pe:make-mass-balance (reaction-step-name step)
                                        (list inlet)
                                        (list outlet)))
           (elements (make-element-balance (reaction-step-name step)
                                           (list inlet)
                                           (list outlet)
                                           :catalog catalog
                                           :on-missing-formula
                                           on-missing-formula)))
      (%make-reaction-step-report
       :name (reaction-step-name step)
       :step step
       :inlet inlet
       :outlet outlet
       :limiting-reactant limiting
       :limiting-extent extent
       :theoretical-yields (%theoretical-yields inlet step catalog)
       :mass-balance mass
       :element-balance elements
       :note (reaction-step-note step)))))

(defun run-reaction-network-report
    (name inlet network &key
       (catalog pe:*default-component-catalog*)
       (on-missing-formula :error)
       note)
  "Run NETWORK and return a report with per-step conservation diagnostics."
  (check-type name string)
  (check-type inlet pe:material-stream)
  (check-type network reaction-network)
  (let ((stream inlet)
        (reports nil))
    (dolist (step (reaction-network-steps network))
      (let ((report (%run-step-report stream step
                                      :catalog catalog
                                      :on-missing-formula
                                      on-missing-formula)))
        (push report reports)
        (setf stream (reaction-step-report-outlet report))))
    (let* ((ordered (nreverse reports))
           (outlet (pe:make-stream name
                                   (pe:stream-flows stream)
                                   :temperature (pe:stream-temperature stream)
                                   :pressure (pe:stream-pressure stream)
                                   :note note))
           (mass (pe:make-mass-balance name (list inlet) (list outlet)))
           (elements (make-element-balance name
                                           (list inlet)
                                           (list outlet)
                                           :catalog catalog
                                           :on-missing-formula
                                           on-missing-formula)))
      (%make-reaction-network-report
       :name name
       :network network
       :inlet inlet
       :outlet outlet
       :step-reports ordered
       :mass-balance mass
       :element-balance elements
       :note note))))

(defun run-reaction-network
    (name inlet network &key (catalog pe:*default-component-catalog*) note)
  "Run NETWORK sequentially over INLET and return the outlet stream."
  (reaction-network-report-outlet
   (run-reaction-network-report name inlet network
                                :catalog catalog
                                :on-missing-formula :ignore
                                :note note)))

(defun %add-flow (key value alist &key (test #'equal))
  (let ((slot (assoc key alist :test test)))
    (if slot
        (incf (cdr slot) value)
        (push (cons key value) alist)))
  alist)

(defun %component-formula (component)
  (pe:component-formula component))

(defun %element-weight (element)
  (pe:formula-molecular-weight (list (cons element 1))))

(defun %catalog-component-or-nil (catalog key on-missing-formula)
  (handler-case
      (pe:catalog-component catalog key)
    (error (condition)
      (if (eq on-missing-formula :ignore)
          nil
          (error condition)))))

(defun stream-element-flows
    (stream &key (catalog pe:*default-component-catalog*)
       (on-missing-formula :error))
  "Return element mass flows for STREAM as (element . tonnes/day).
Stream mass units are tonnes/day; component molecular weights are kg/kmol."
  (check-type stream pe:material-stream)
  (let ((acc nil))
    (dolist (pair (pe:stream-flows stream))
      (let* ((component (%catalog-component-or-nil catalog (car pair)
                                                   on-missing-formula))
             (formula (and component (%component-formula component))))
        (cond
          (formula
           (let* ((mw (pe:component-molecular-weight component))
                  (kmol (/ (* (cdr pair) 1000d0) mw)))
             (dolist (element formula)
               (setf acc
                     (%add-flow (car element)
                                (/ (* kmol (cdr element)
                                      (%element-weight (car element)))
                                   1000d0)
                                acc :test #'string=)))))
          ((null component))
          ((eq on-missing-formula :ignore))
          (t
           (error "Component ~S has no formula." (pe:component-key component))))))
    (sort acc #'string< :key #'car)))

(defstruct (element-balance (:constructor %make-element-balance))
  (name "" :type string)
  (input-elements nil :type list)
  (output-elements nil :type list)
  (residuals nil :type list)
  (tolerance pe:+default-balance-tolerance+ :type real)
  note)

(defun %merge-element-flows (streams catalog on-missing-formula)
  (let ((acc nil))
    (dolist (stream streams)
      (dolist (pair (stream-element-flows stream
                                          :catalog catalog
                                          :on-missing-formula on-missing-formula))
        (setf acc (%add-flow (car pair) (cdr pair) acc :test #'string=))))
    (sort acc #'string< :key #'car)))

(defun %element-keys (left right)
  (sort (remove-duplicates (append (mapcar #'car left) (mapcar #'car right))
                           :test #'string=)
        #'string<))

(defun %element-flow (flows element)
  (or (cdr (assoc element flows :test #'string=)) 0d0))

(defun make-element-balance
    (name inputs outputs &key
       (catalog pe:*default-component-catalog*)
       (tolerance pe:+default-balance-tolerance+)
       (on-missing-formula :error)
       note)
  "Construct an element-balance check over input and output streams."
  (check-type name string)
  (assert (every #'pe:stream-p inputs) (inputs)
          "Element-balance inputs must be streams.")
  (assert (every #'pe:stream-p outputs) (outputs)
          "Element-balance outputs must be streams.")
  (let* ((in (%merge-element-flows inputs catalog on-missing-formula))
         (out (%merge-element-flows outputs catalog on-missing-formula))
         (residuals
           (mapcar (lambda (element)
                     (cons element (- (%element-flow out element)
                                      (%element-flow in element))))
                   (%element-keys in out))))
    (%make-element-balance :name name
                           :input-elements in
                           :output-elements out
                           :residuals residuals
                           :tolerance (float tolerance 1d0)
                           :note note)))

(defun element-balance-residual (balance element)
  "Return output-minus-input element residual for BALANCE."
  (check-type balance element-balance)
  (%element-flow (element-balance-residuals balance) element))

(defun element-balance-closed-p (balance &optional element)
  "T iff element residuals are within BALANCE tolerance."
  (check-type balance element-balance)
  (if element
      (<= (abs (element-balance-residual balance element))
          (element-balance-tolerance balance))
      (every (lambda (pair)
               (<= (abs (cdr pair)) (element-balance-tolerance balance)))
             (element-balance-residuals balance))))

(defun element-balance->plist (balance)
  "Return a stable plist for BALANCE."
  (check-type balance element-balance)
  (list :name (element-balance-name balance)
        :input-elements (copy-tree (element-balance-input-elements balance))
        :output-elements (copy-tree (element-balance-output-elements balance))
        :residuals (copy-tree (element-balance-residuals balance))
        :closed-p (element-balance-closed-p balance)
        :tolerance (element-balance-tolerance balance)
        :note (element-balance-note balance)))

(defun reaction-step-report->plist (report)
  "Return a stable plist for REPORT."
  (check-type report reaction-step-report)
  (list :name (reaction-step-report-name report)
        :step (reaction-step->plist (reaction-step-report-step report))
        :inlet (pe:stream->plist (reaction-step-report-inlet report))
        :outlet (pe:stream->plist (reaction-step-report-outlet report))
        :limiting-reactant (reaction-step-report-limiting-reactant report)
        :limiting-extent (reaction-step-report-limiting-extent report)
        :theoretical-yields
        (copy-tree (reaction-step-report-theoretical-yields report))
        :mass-balance
        (pe:mass-balance->plist (reaction-step-report-mass-balance report))
        :element-balance
        (element-balance->plist (reaction-step-report-element-balance report))
        :note (reaction-step-report-note report)))

(defun reaction-network-report->plist (report)
  "Return a stable plist for REPORT."
  (check-type report reaction-network-report)
  (list :name (reaction-network-report-name report)
        :network (reaction-network->plist
                  (reaction-network-report-network report))
        :inlet (pe:stream->plist (reaction-network-report-inlet report))
        :outlet (pe:stream->plist (reaction-network-report-outlet report))
        :step-reports
        (mapcar #'reaction-step-report->plist
                (reaction-network-report-step-reports report))
        :mass-balance
        (pe:mass-balance->plist (reaction-network-report-mass-balance report))
        :element-balance
        (element-balance->plist
         (reaction-network-report-element-balance report))
        :note (reaction-network-report-note report)))
