;;;; core.lisp --- Process-engineering bookkeeping kernel.

(in-package #:rosette-process-engineering)

(defparameter +default-balance-tolerance+ 1d-9
  "Default absolute mass-balance tolerance in caller-chosen flow units.")

(defun %nonnegative-real-p (x)
  (and (realp x) (not (minusp x))))

(defun %component-key< (a b)
  (string< (prin1-to-string a) (prin1-to-string b)))

(defun %normalize-flow-pair (pair)
  (cond
    ((and (consp pair) (symbolp (car pair)) (realp (cdr pair)))
     (cons (car pair) (float (cdr pair) 1d0)))
    ((and (consp pair) (symbolp (first pair)) (realp (second pair)))
     (cons (first pair) (float (second pair) 1d0)))
    (t
     (error "Flow entry must be (component . nonnegative-real): ~S" pair))))

(defun %canonicalize-flows (flows)
  (let ((acc nil))
    (dolist (raw flows)
      (let* ((pair (%normalize-flow-pair raw))
             (component (car pair))
             (flow (cdr pair)))
        (unless (%nonnegative-real-p flow)
          (error "Negative flow for ~S: ~S" component flow))
        (let ((slot (assoc component acc)))
          (if slot
              (incf (cdr slot) flow)
              (push (cons component flow) acc)))))
    (sort (remove-if (lambda (pair)
                       (<= (abs (cdr pair)) +default-balance-tolerance+))
                     acc)
          #'%component-key<
          :key #'car)))

(defstruct (material-stream (:constructor %make-stream)
                            (:conc-name stream-))
  (name "" :type string)
  (flows nil :type list)
  temperature
  pressure
  note)

(defun stream-p (object)
  "T iff OBJECT is a material stream."
  (material-stream-p object))

(defun make-stream (name flows &key temperature pressure note)
  "Construct a material stream with component mass rates.
FLOWS is an alist of (component . nonnegative-real) entries.  Duplicate
components are summed and the stored order is deterministic."
  (check-type name string)
  (%make-stream :name name
                :flows (%canonicalize-flows flows)
                :temperature temperature
                :pressure pressure
                :note note))

(defun stream-component-flow (stream component)
  "Return COMPONENT's flow in STREAM, or 0 when absent."
  (check-type stream material-stream)
  (let ((pair (assoc component (stream-flows stream))))
    (if pair (cdr pair) 0d0)))

(defun stream-components (stream)
  "Return STREAM component keys in deterministic order."
  (check-type stream material-stream)
  (mapcar #'car (stream-flows stream)))

(defun stream-total-flow (stream)
  "Return the total component mass flow in STREAM."
  (check-type stream material-stream)
  (reduce #'+ (stream-flows stream) :key #'cdr :initial-value 0d0))

(defun stream-scale (stream factor &key name note)
  "Return STREAM with every component flow multiplied by FACTOR."
  (check-type stream material-stream)
  (unless (%nonnegative-real-p factor)
    (error "Scale factor must be nonnegative: ~S" factor))
  (make-stream (or name (stream-name stream))
               (mapcar (lambda (pair)
                         (cons (car pair) (* (cdr pair) factor)))
                       (stream-flows stream))
               :temperature (stream-temperature stream)
               :pressure (stream-pressure stream)
               :note (or note (stream-note stream))))

(defun stream-with-flow (stream component flow &key name note)
  "Return STREAM with COMPONENT set to FLOW."
  (check-type stream material-stream)
  (unless (symbolp component)
    (error "Component must be a symbol: ~S" component))
  (unless (%nonnegative-real-p flow)
    (error "Flow must be nonnegative: ~S" flow))
  (let ((flows (remove component (copy-list (stream-flows stream)) :key #'car)))
    (make-stream (or name (stream-name stream))
                 (acons component flow flows)
                 :temperature (stream-temperature stream)
                 :pressure (stream-pressure stream)
                 :note (or note (stream-note stream)))))

(defun stream->plist (stream)
  "Return a stable property-list representation of STREAM."
  (check-type stream material-stream)
  (list :name (stream-name stream)
        :flows (copy-tree (stream-flows stream))
        :total-flow (stream-total-flow stream)
        :temperature (stream-temperature stream)
        :pressure (stream-pressure stream)
        :note (stream-note stream)))

(defun mix-streams (name streams &key note)
  "Return one stream whose component flows are the sum of STREAMS."
  (check-type name string)
  (assert (every #'stream-p streams) (streams)
          "MIX-STREAMS expects stream records.")
  (make-stream name
               (loop for stream in streams append (copy-list (stream-flows stream)))
               :note note))

(defun split-stream (stream fraction &key first-name second-name)
  "Split STREAM into two streams at FRACTION and 1-FRACTION.
Returns two values."
  (check-type stream material-stream)
  (unless (and (realp fraction) (<= 0d0 fraction 1d0))
    (error "Split fraction must be in [0, 1]: ~S" fraction))
  (values (stream-scale stream fraction
                        :name (or first-name
                                  (format nil "~A split A" (stream-name stream))))
          (stream-scale stream (- 1d0 fraction)
                        :name (or second-name
                                  (format nil "~A split B" (stream-name stream))))))

;;; ----------------------------------------------------------------------
;;; Lightweight component chemistry
;;; ----------------------------------------------------------------------

(defstruct (component (:constructor %make-component))
  key
  formula
  molecular-weight
  phase
  hhv
  lhv
  hazard
  note)

(defun make-component (key &key formula molecular-weight phase hhv lhv hazard note)
  "Construct a process component record.
MOLECULAR-WEIGHT is kg/kmol. When FORMULA is supplied and MOLECULAR-WEIGHT is
omitted, the molecular weight is computed from the built-in atomic table."
  (unless (symbolp key)
    (error "Component key must be a symbol: ~S" key))
  (let ((parsed (when formula
                  (if (stringp formula) (parse-formula formula) formula))))
    (%make-component :key key
                     :formula parsed
                     :molecular-weight (or molecular-weight
                                           (when parsed
                                             (formula-molecular-weight parsed)))
                     :phase phase
                     :hhv hhv
                     :lhv lhv
                     :hazard hazard
                     :note note)))

(defun component->plist (component)
  "Return a stable property-list representation of COMPONENT."
  (check-type component component)
  (list :key (component-key component)
        :formula (copy-tree (component-formula component))
        :molecular-weight (component-molecular-weight component)
        :phase (component-phase component)
        :hhv (component-hhv component)
        :lhv (component-lhv component)
        :hazard (component-hazard component)
        :note (component-note component)))

(defstruct (component-catalog (:constructor %make-component-catalog))
  (components nil :type list))

(defun make-component-catalog (components)
  "Construct a component catalog."
  (assert (every #'component-p components) (components)
          "Component catalog entries must be component records.")
  (%make-component-catalog :components (copy-list components)))

(defun component-catalog-with (catalog components)
  "Return CATALOG with COMPONENTS added or replacing entries with the same key."
  (check-type catalog component-catalog)
  (assert (every #'component-p components) (components)
          "Catalog additions must be component records.")
  (let ((keys (mapcar #'component-key components)))
    (make-component-catalog
     (append (copy-list components)
             (remove-if (lambda (component)
                          (member (component-key component) keys))
                        (component-catalog-components catalog))))))

(defun catalog-component (catalog key)
  "Return component KEY from CATALOG, or signal if absent."
  (check-type catalog component-catalog)
  (or (find key (component-catalog-components catalog)
            :key #'component-key)
      (error "No component ~S in catalog." key)))

(defun catalog-molecular-weight (catalog key)
  "Return component KEY molecular weight in kg/kmol."
  (let ((mw (component-molecular-weight (catalog-component catalog key))))
    (unless (and (realp mw) (plusp mw))
      (error "Component ~S has no positive molecular weight." key))
    mw))

(defparameter *default-component-catalog*
  (make-component-catalog
   (list
    (make-component :h2 :formula "H2" :phase :gas :lhv 120d0)
    (make-component :co :formula "CO" :phase :gas :lhv 10.1d0)
    (make-component :co2 :formula "CO2" :phase :gas)
    (make-component :ch4 :formula "CH4" :phase :gas :lhv 50d0)
    (make-component :water :formula "H2O" :phase :liquid)
    (make-component :methanol :formula "CH3OH" :phase :liquid :lhv 19.9d0)
    (make-component :caco3 :formula "CaCO3" :phase :solid)
    (make-component :cao :formula "CaO" :phase :solid)
    (make-component :biochar :molecular-weight 12.0107d0
                    :phase :solid :note "modelled as carbon-equivalent")
    (make-component :volatile-solids :molecular-weight 24d0
                    :phase :solid :note "lumped biomass volatile solids")
    (make-component :inerts :molecular-weight 28d0
                    :phase :gas :note "lumped inert gas")))
  "Default component catalog for first-order process witnesses.")

(defstruct (reaction (:constructor %make-reaction))
  (name "" :type string)
  (stoich nil :type list)
  note)

(defun %canonicalize-stoich (stoich)
  (sort (mapcar (lambda (pair)
                  (unless (and (consp pair) (symbolp (car pair)) (realp (cdr pair)))
                    (error "Stoichiometry entry must be (component . coefficient): ~S" pair))
                  (cons (car pair) (float (cdr pair) 1d0)))
                stoich)
        #'%component-key<
        :key #'car))

(defun make-reaction (name stoich &key note)
  "Construct a stoichiometric reaction.
Coefficients are molar: negative for reactants, positive for products."
  (check-type name string)
  (%make-reaction :name name
                  :stoich (%canonicalize-stoich stoich)
                  :note note))

(defun reaction-reactants (reaction)
  "Return negative stoichiometric entries for REACTION."
  (check-type reaction reaction)
  (remove-if-not #'minusp (reaction-stoich reaction) :key #'cdr))

(defun reaction-products (reaction)
  "Return positive stoichiometric entries for REACTION."
  (check-type reaction reaction)
  (remove-if-not #'plusp (reaction-stoich reaction) :key #'cdr))

(defun %reaction-coefficient (reaction component)
  (cdr (assoc component (reaction-stoich reaction))))

(defun %stream-component-kmol (stream component catalog)
  (/ (* (stream-component-flow stream component) 1000d0)
     (catalog-molecular-weight catalog component)))

(defun reaction-reactant-extent
    (stream reaction component &key (catalog *default-component-catalog*))
  "Return available reaction extent in kmol/day from COMPONENT in STREAM."
  (check-type stream material-stream)
  (check-type reaction reaction)
  (let ((nu (%reaction-coefficient reaction component)))
    (unless (and nu (minusp nu))
      (error "Component ~S is not a reactant in ~S."
             component (reaction-name reaction)))
    (/ (%stream-component-kmol stream component catalog) (- nu))))

(defun reaction-extent-limit
    (stream reaction &key (catalog *default-component-catalog*))
  "Return limiting extent and limiting reactant as two values."
  (check-type reaction reaction)
  (let ((reactants (reaction-reactants reaction))
        best-component
        best-extent)
    (unless reactants
      (error "Reaction ~S has no reactants." (reaction-name reaction)))
    (dolist (pair reactants)
      (let* ((component (car pair))
             (extent (reaction-reactant-extent stream reaction component
                                               :catalog catalog)))
        (when (or (null best-extent) (< extent best-extent))
          (setf best-component component
                best-extent extent))))
    (values best-extent best-component)))

(defun reaction-limiting-reactant
    (stream reaction &key (catalog *default-component-catalog*))
  "Return the component key for the limiting reactant."
  (nth-value 1 (reaction-extent-limit stream reaction :catalog catalog)))

(defun reaction-product-theoretical-yield
    (stream reaction product &key (conversion 1d0)
       (catalog *default-component-catalog*))
  "Return theoretical PRODUCT mass in tonnes/day from STREAM and REACTION."
  (unless (and (realp conversion) (<= 0d0 conversion 1d0))
    (error "Conversion must be in [0, 1]: ~S" conversion))
  (let ((nu (%reaction-coefficient reaction product)))
    (unless (and nu (plusp nu))
      (error "Component ~S is not a product in ~S."
             product (reaction-name reaction)))
    (multiple-value-bind (extent limiting-reactant)
        (reaction-extent-limit stream reaction :catalog catalog)
      (declare (ignore limiting-reactant))
      (/ (* conversion extent nu
            (catalog-molecular-weight catalog product))
         1000d0))))

(defun reaction-balanced-mass-residual
    (reaction &key (catalog *default-component-catalog*))
  "Return sum(nu_i MW_i) for REACTION. A mass-balanced reaction is near zero."
  (check-type reaction reaction)
  (reduce #'+ (reaction-stoich reaction)
          :key (lambda (pair)
                 (* (cdr pair)
                    (catalog-molecular-weight catalog (car pair))))
          :initial-value 0d0))

(defun reaction-element-residuals
    (reaction &key (catalog *default-component-catalog*))
  "Return element residuals for REACTION as output-minus-input atom counts."
  (check-type reaction reaction)
  (let ((acc nil))
    (dolist (pair (reaction-stoich reaction))
      (let* ((component (catalog-component catalog (car pair)))
             (formula (component-formula component)))
        (unless formula
          (error "Component ~S has no formula." (car pair)))
        (dolist (element formula)
          (let ((slot (assoc (car element) acc :test #'string=))
                (delta (* (cdr pair) (cdr element))))
            (if slot
                (incf (cdr slot) delta)
                (push (cons (car element) delta) acc))))))
    (sort acc #'string< :key #'car)))

(defun reaction-element-balanced-p
    (reaction &key (catalog *default-component-catalog*)
       (tolerance +default-balance-tolerance+))
  "T iff every reaction element residual is within TOLERANCE."
  (every (lambda (pair) (<= (abs (cdr pair)) tolerance))
         (reaction-element-residuals reaction :catalog catalog)))

(defun reaction-balanced-p
    (reaction &key (catalog *default-component-catalog*)
       (tolerance +default-balance-tolerance+))
  "T iff REACTION is mass- and element-balanced within TOLERANCE."
  (and (<= (abs (reaction-balanced-mass-residual reaction :catalog catalog))
           tolerance)
       (reaction-element-balanced-p reaction
                                    :catalog catalog
                                    :tolerance tolerance)))

(defun reaction->plist (reaction &key (catalog *default-component-catalog*))
  "Return a stable property-list representation of REACTION."
  (check-type reaction reaction)
  (list :name (reaction-name reaction)
        :stoich (copy-tree (reaction-stoich reaction))
        :mass-residual (reaction-balanced-mass-residual reaction
                                                        :catalog catalog)
        :element-residuals (reaction-element-residuals reaction
                                                       :catalog catalog)
        :balanced-p (reaction-balanced-p reaction :catalog catalog)
        :note (reaction-note reaction)))

(defun stoichiometric-reactor
    (name inlet reaction conversion &key
       (catalog *default-component-catalog*)
       note)
  "Run REACTION over INLET at fractional CONVERSION of the limiting reactant.
Stream mass units are assumed tonnes/day; molecular weights are kg/kmol."
  (check-type name string)
  (check-type inlet material-stream)
  (check-type reaction reaction)
  (unless (and (realp conversion) (<= 0d0 conversion 1d0))
    (error "Conversion must be in [0, 1]: ~S" conversion))
  (let ((limiting-extent (reaction-extent-limit inlet reaction
                                                :catalog catalog)))
    (let ((extent (* conversion limiting-extent))
          (flows (copy-list (stream-flows inlet))))
      (dolist (pair (reaction-stoich reaction))
        (let* ((component (car pair))
               (nu (cdr pair))
               (mw (catalog-molecular-weight catalog component))
               (delta-mass (/ (* nu extent mw) 1000d0))
               (old (or (cdr (assoc component flows)) 0d0))
               (new (+ old delta-mass)))
          (when (and (minusp new)
                     (> (abs new) +default-balance-tolerance+))
            (error "Reaction drove component ~S negative: ~S." component new))
          (setf flows (acons component (max 0d0 new)
                             (remove component flows :key #'car)))))
      (make-stream name flows
                   :temperature (stream-temperature inlet)
                   :pressure (stream-pressure inlet)
                   :note note))))

;;; ----------------------------------------------------------------------
;;; Biomass and gas quality helpers
;;; ----------------------------------------------------------------------

(defstruct (biomass-analysis (:constructor %make-biomass-analysis))
  (moisture 0d0 :type real)
  (ash 0d0 :type real)
  (volatile-matter 0d0 :type real)
  (fixed-carbon 0d0 :type real)
  carbon hydrogen oxygen nitrogen sulfur hhv lhv note)

(defun %fraction-p (x)
  (and (realp x) (<= 0d0 x 1d0)))

(defun make-biomass-analysis (&key
                                (moisture 0d0)
                                (ash 0d0)
                                (volatile-matter 0d0)
                                (fixed-carbon 0d0)
                                carbon hydrogen oxygen nitrogen sulfur
                                hhv lhv note)
  "Construct proximate/ultimate biomass analysis fractions."
  (dolist (value (list moisture ash volatile-matter fixed-carbon))
    (unless (%fraction-p value)
      (error "Biomass proximate value must be in [0, 1]: ~S" value)))
  (when (> (+ moisture ash volatile-matter fixed-carbon)
           (+ 1d0 +default-balance-tolerance+))
    (error "Biomass proximate fractions exceed one."))
  (%make-biomass-analysis :moisture (float moisture 1d0)
                          :ash (float ash 1d0)
                          :volatile-matter (float volatile-matter 1d0)
                          :fixed-carbon (float fixed-carbon 1d0)
                          :carbon carbon :hydrogen hydrogen
                          :oxygen oxygen :nitrogen nitrogen :sulfur sulfur
                          :hhv hhv :lhv lhv :note note))

(defun biomass-dry-fraction (analysis)
  "Return dry-matter fraction for ANALYSIS."
  (check-type analysis biomass-analysis)
  (- 1d0 (biomass-analysis-moisture analysis)))

(defun biomass-combustible-fraction (analysis)
  "Return volatile + fixed-carbon fraction for ANALYSIS."
  (check-type analysis biomass-analysis)
  (+ (biomass-analysis-volatile-matter analysis)
     (biomass-analysis-fixed-carbon analysis)))

(defun biomass-stream-from-analysis (name tonnes-per-day analysis)
  "Construct a material stream from proximate biomass analysis."
  (check-type name string)
  (check-type analysis biomass-analysis)
  (unless (%nonnegative-real-p tonnes-per-day)
    (error "Biomass flow must be nonnegative: ~S" tonnes-per-day))
  (make-stream name
               `((:water . ,(* tonnes-per-day
                                (biomass-analysis-moisture analysis)))
                 (:ash . ,(* tonnes-per-day
                              (biomass-analysis-ash analysis)))
                 (:volatile-matter . ,(* tonnes-per-day
                                           (biomass-analysis-volatile-matter analysis)))
                 (:fixed-carbon . ,(* tonnes-per-day
                                       (biomass-analysis-fixed-carbon analysis))))))

(defun biomass-analysis->plist (analysis)
  "Return a stable property-list representation of ANALYSIS."
  (check-type analysis biomass-analysis)
  (list :moisture (biomass-analysis-moisture analysis)
        :ash (biomass-analysis-ash analysis)
        :volatile-matter (biomass-analysis-volatile-matter analysis)
        :fixed-carbon (biomass-analysis-fixed-carbon analysis)
        :dry-fraction (biomass-dry-fraction analysis)
        :combustible-fraction (biomass-combustible-fraction analysis)
        :carbon (biomass-analysis-carbon analysis)
        :hydrogen (biomass-analysis-hydrogen analysis)
        :oxygen (biomass-analysis-oxygen analysis)
        :nitrogen (biomass-analysis-nitrogen analysis)
        :sulfur (biomass-analysis-sulfur analysis)
        :hhv (biomass-analysis-hhv analysis)
        :lhv (biomass-analysis-lhv analysis)
        :note (biomass-analysis-note analysis)))

(defstruct (gas-quality (:constructor %make-gas-quality))
  (h2 0d0 :type real)
  (co 0d0 :type real)
  (co2 0d0 :type real)
  (ch4 0d0 :type real)
  (inerts 0d0 :type real))

(defun %component-kmol (stream component catalog)
  (%stream-component-kmol stream component catalog))

(defun gas-quality-from-stream (stream &key (catalog *default-component-catalog*))
  "Return molar syngas quality metrics from STREAM.
Molar flows are kmol/day when stream mass is tonnes/day."
  (check-type stream material-stream)
  (let* ((h2 (%component-kmol stream :h2 catalog))
         (co (%component-kmol stream :co catalog))
         (co2 (%component-kmol stream :co2 catalog))
         (ch4 (%component-kmol stream :ch4 catalog))
         (known '(:h2 :co :co2 :ch4))
         (inerts (reduce #'+ (remove-if (lambda (pair)
                                          (member (car pair) known))
                                        (stream-flows stream))
                         :key (lambda (pair)
                                (/ (* (cdr pair) 1000d0)
                                   (catalog-molecular-weight catalog (car pair))))
                         :initial-value 0d0)))
    (%make-gas-quality :h2 h2 :co co :co2 co2 :ch4 ch4 :inerts inerts)))

(defun gas-quality-total (quality)
  "Return total molar flow for QUALITY."
  (check-type quality gas-quality)
  (+ (gas-quality-h2 quality)
     (gas-quality-co quality)
     (gas-quality-co2 quality)
     (gas-quality-ch4 quality)
     (gas-quality-inerts quality)))

(defun syngas-module (quality)
  "Return methanol stoichiometric number M = (H2 - CO2)/(CO + CO2)."
  (check-type quality gas-quality)
  (let ((den (+ (gas-quality-co quality) (gas-quality-co2 quality))))
    (if (zerop den)
        0d0
        (/ (- (gas-quality-h2 quality) (gas-quality-co2 quality)) den))))

(defun h2-co-ratio (quality)
  "Return H2/CO molar ratio, or 0 when CO is absent."
  (check-type quality gas-quality)
  (if (zerop (gas-quality-co quality))
      0d0
      (/ (gas-quality-h2 quality) (gas-quality-co quality))))

(defun co2-fraction (quality)
  "Return molar CO2 fraction."
  (check-type quality gas-quality)
  (let ((total (gas-quality-total quality)))
    (if (zerop total) 0d0 (/ (gas-quality-co2 quality) total))))

(defun inerts-fraction (quality)
  "Return molar inert fraction."
  (check-type quality gas-quality)
  (let ((total (gas-quality-total quality)))
    (if (zerop total) 0d0 (/ (gas-quality-inerts quality) total))))

(defun methanol-theoretical-max
    (quality &key (catalog *default-component-catalog*))
  "Return optimistic methanol maximum in tonnes/day from syngas QUALITY.
This upper bound treats hydrogen as the limiting reagent at 2 H2 per methanol
and carbon oxides as one carbon per methanol."
  (check-type quality gas-quality)
  (let* ((carbon-kmol (+ (gas-quality-co quality) (gas-quality-co2 quality)))
         (h2-kmol (/ (gas-quality-h2 quality) 2d0))
         (methanol-kmol (min carbon-kmol h2-kmol)))
    (/ (* methanol-kmol (catalog-molecular-weight catalog :methanol)) 1000d0)))

(defun gas-quality->plist (quality)
  "Return a stable property-list representation of QUALITY."
  (check-type quality gas-quality)
  (list :h2 (gas-quality-h2 quality)
        :co (gas-quality-co quality)
        :co2 (gas-quality-co2 quality)
        :ch4 (gas-quality-ch4 quality)
        :inerts (gas-quality-inerts quality)
        :total (gas-quality-total quality)
        :syngas-module (syngas-module quality)
        :h2-co-ratio (h2-co-ratio quality)
        :co2-fraction (co2-fraction quality)
        :inerts-fraction (inerts-fraction quality)
        :methanol-theoretical-max (methanol-theoretical-max quality)))

;;; ----------------------------------------------------------------------
;;; Simple separations, drying, and uncertainty
;;; ----------------------------------------------------------------------

(defun dryer-duty (name inlet target-moisture &key
                        (latent-heat-mj-per-t-water 2257d0)
                        (sensible-heat-mj-per-t-water 0d0)
                        (unit "MW")
                        note)
  "Return heat duty required to dry INLET to TARGET-MOISTURE.
TARGET-MOISTURE is final water mass fraction. Stream mass is tonnes/day."
  (check-type name string)
  (check-type inlet material-stream)
  (unless (and (realp target-moisture) (<= 0d0 target-moisture)
               (< target-moisture 1d0))
    (error "Target moisture must be in [0, 1): ~S" target-moisture))
  (let* ((water (stream-component-flow inlet :water))
         (dry (- (stream-total-flow inlet) water))
         (target-water (if (zerop dry)
                           0d0
                           (/ (* target-moisture dry)
                              (- 1d0 target-moisture))))
         (removed (max 0d0 (- water target-water)))
         (mj-per-day (* removed
                        (+ latent-heat-mj-per-t-water
                           sensible-heat-mj-per-t-water)))
         (mw (/ mj-per-day 86400d0)))
    (make-heat-duty name mw :unit unit :note note)))

(defun component-separator (name inlet recoveries &key
                                 recovered-name residue-name note)
  "Split INLET by component recovery fractions.
RECOVERIES is an alist of (component . fraction-to-recovered). Returns two
streams: recovered and residue."
  (check-type name string)
  (check-type inlet material-stream)
  (let ((recovered nil)
        (residue nil))
    (dolist (pair (stream-flows inlet))
      (let* ((component (car pair))
             (flow (cdr pair))
             (fraction (or (cdr (assoc component recoveries)) 0d0)))
        (unless (and (realp fraction) (<= 0d0 fraction 1d0))
          (error "Recovery fraction must be in [0, 1]: ~S" fraction))
        (push (cons component (* flow fraction)) recovered)
        (push (cons component (* flow (- 1d0 fraction))) residue)))
    (values (make-stream (or recovered-name (format nil "~A recovered" name))
                         recovered
                         :note note)
            (make-stream (or residue-name (format nil "~A residue" name))
                         residue
                         :note note))))

(defun dryer-output-stream (name inlet target-moisture &key note)
  "Return dried stream and water-vapor stream for INLET at TARGET-MOISTURE."
  (let* ((water (stream-component-flow inlet :water))
         (dry (- (stream-total-flow inlet) water))
         (target-water (if (zerop dry)
                           0d0
                           (/ (* target-moisture dry)
                              (- 1d0 target-moisture))))
         (removed (max 0d0 (- water target-water)))
         (fraction (if (zerop water) 0d0 (/ removed water))))
    (multiple-value-bind (vapor dried)
        (component-separator name inlet `((:water . ,fraction))
                             :recovered-name (format nil "~A vapor" name)
                             :residue-name (format nil "~A dried" name)
                             :note note)
      (values dried vapor))))

(defun condenser (name inlet condensables &key (fraction 1d0) note)
  "Recover CONDENSABLES from INLET into a condensed stream."
  (component-separator name inlet
                       (mapcar (lambda (component)
                                 (cons component fraction))
                               condensables)
                       :recovered-name (format nil "~A condensate" name)
                       :residue-name (format nil "~A gas" name)
                       :note note))

(defun gas-splitter (name inlet fraction &key note)
  "Split gas INLET into two fractions. Returns two streams."
  (declare (ignore note))
  (split-stream inlet fraction
                :first-name (format nil "~A branch A" name)
                :second-name (format nil "~A branch B" name)))

(defun solids-separator (name inlet solids &key (fraction 1d0) note)
  "Recover SOLIDS from INLET into a solids stream."
  (component-separator name inlet
                       (mapcar (lambda (component)
                                 (cons component fraction))
                               solids)
                       :recovered-name (format nil "~A solids" name)
                       :residue-name (format nil "~A liquid" name)
                       :note note))

(defun distillation-cut (name inlet light-components &key (fraction 1d0) note)
  "Recover LIGHT-COMPONENTS into a distillate stream."
  (component-separator name inlet
                       (mapcar (lambda (component)
                                 (cons component fraction))
                               light-components)
                       :recovered-name (format nil "~A distillate" name)
                       :residue-name (format nil "~A bottoms" name)
                       :note note))

(defun purge-recycle-split (name inlet purge-fraction &key note)
  "Split INLET into purge and recycle streams."
  (declare (ignore note))
  (split-stream inlet purge-fraction
                :first-name (format nil "~A purge" name)
                :second-name (format nil "~A recycle" name)))

(defstruct (uncertain-parameter (:constructor %make-uncertain-parameter))
  name low base high unit note)

(defun make-uncertain-parameter (name low base high &key unit note)
  "Construct a low/base/high parameter range."
  (unless (symbolp name)
    (error "Uncertain parameter name must be a symbol: ~S" name))
  (unless (and (realp low) (realp base) (realp high) (<= low base high))
    (error "Expected LOW <= BASE <= HIGH, got ~S ~S ~S." low base high))
  (%make-uncertain-parameter :name name
                             :low (float low 1d0)
                             :base (float base 1d0)
                             :high (float high 1d0)
                             :unit unit
                             :note note))

(defun uncertain-parameter-value (parameter case)
  "Return PARAMETER value for CASE (:pessimistic, :base, :optimistic)."
  (check-type parameter uncertain-parameter)
  (ecase case
    (:pessimistic (uncertain-parameter-low parameter))
    (:base (uncertain-parameter-base parameter))
    (:optimistic (uncertain-parameter-high parameter))))

(defun uncertain-parameter->plist (parameter)
  "Return a stable property-list representation of PARAMETER."
  (check-type parameter uncertain-parameter)
  (list :name (uncertain-parameter-name parameter)
        :low (uncertain-parameter-low parameter)
        :base (uncertain-parameter-base parameter)
        :high (uncertain-parameter-high parameter)
        :unit (uncertain-parameter-unit parameter)
        :note (uncertain-parameter-note parameter)))

(defun uncertainty-cases (parameters)
  "Return pessimistic/base/optimistic plists for PARAMETERS."
  (assert (every #'uncertain-parameter-p parameters) (parameters)
          "Expected uncertain-parameter records.")
  (mapcar (lambda (case)
            (list :case case
                  :values (loop for parameter in parameters append
                                (list (uncertain-parameter-name parameter)
                                      (uncertain-parameter-value parameter
                                                                 case)))))
          '(:pessimistic :base :optimistic)))

(defun conversion-reactor (name inlet reactant conversion products &key note)
  "Consume REACTANT from INLET and add product yields.
PRODUCTS is an alist of (component . mass-yield-per-mass-reactant-consumed).
Mass closure is intentionally not forced; use MAKE-MASS-BALANCE to inspect the
residual implied by empirical yields."
  (check-type name string)
  (check-type inlet material-stream)
  (unless (and (realp conversion) (<= 0d0 conversion 1d0))
    (error "Conversion must be in [0, 1]: ~S" conversion))
  (let* ((reactant-flow (stream-component-flow inlet reactant))
         (consumed (* reactant-flow conversion))
         (base (stream-with-flow inlet reactant (- reactant-flow consumed)
                                 :name name :note note)))
    (make-stream name
                 (append (stream-flows base)
                         (mapcar (lambda (pair)
                                   (let ((spec (%normalize-flow-pair pair)))
                                     (unless (%nonnegative-real-p (cdr spec))
                                       (error "Product yield must be nonnegative: ~S" pair))
                                     (cons (car spec) (* consumed (cdr spec)))))
                                 products))
                 :temperature (stream-temperature inlet)
                 :pressure (stream-pressure inlet)
                 :note note)))

(defun yield-reactor (name inlet yields &key (basis :total-input) note)
  "Return an output stream from empirical mass yields.
With BASIS :TOTAL-INPUT, each yield is multiplied by the total inlet flow.  With
BASIS naming a component, yields are multiplied by that component's inlet flow."
  (check-type name string)
  (check-type inlet material-stream)
  (let ((basis-flow (if (eq basis :total-input)
                        (stream-total-flow inlet)
                        (stream-component-flow inlet basis))))
    (make-stream name
                 (mapcar (lambda (pair)
                           (let ((spec (%normalize-flow-pair pair)))
                             (unless (%nonnegative-real-p (cdr spec))
                               (error "Yield must be nonnegative: ~S" pair))
                             (cons (car spec) (* basis-flow (cdr spec)))))
                         yields)
                 :note note)))

(defstruct (heat-duty (:constructor %make-heat-duty))
  (name "" :type string)
  (value 0d0 :type real)
  (unit "MW" :type string)
  note)

(defun make-heat-duty (name value &key (unit "MW") note)
  "Construct a signed heat-duty record.
Positive values mean heat required by convention; negative values mean heat
available."
  (check-type name string)
  (check-type unit string)
  (check-type value real)
  (%make-heat-duty :name name :value (float value 1d0) :unit unit :note note))

(defun heat-duty->plist (duty)
  "Return a stable property-list representation of DUTY."
  (check-type duty heat-duty)
  (list :name (heat-duty-name duty)
        :value (heat-duty-value duty)
        :unit (heat-duty-unit duty)
        :note (heat-duty-note duty)))

(defstruct (heat-balance (:constructor %make-heat-balance))
  (name "" :type string)
  (duties nil :type list)
  (unit "MW" :type string)
  (tolerance +default-balance-tolerance+ :type real)
  note)

(defun make-heat-balance (name duties &key
                               (unit "MW")
                               (tolerance +default-balance-tolerance+)
                               note)
  "Construct a signed heat/utility balance over DUTIES.
Positive duties are heat demands; negative duties are heat sources.  All duties
must carry the same UNIT label as the balance."
  (check-type name string)
  (check-type unit string)
  (assert (every #'heat-duty-p duties) (duties)
          "Heat-balance duties must be heat-duty records.")
  (unless (%nonnegative-real-p tolerance)
    (error "Tolerance must be nonnegative: ~S" tolerance))
  (dolist (duty duties)
    (unless (string= (heat-duty-unit duty) unit)
      (error "Heat duty ~S has unit ~S, expected ~S."
             (heat-duty-name duty) (heat-duty-unit duty) unit)))
  (%make-heat-balance :name name
                      :duties (copy-list duties)
                      :unit unit
                      :tolerance (float tolerance 1d0)
                      :note note))

(defun heat-balance-net-duty (balance)
  "Return signed net heat duty for BALANCE.
Positive means net heat demand remains; negative means heat surplus."
  (check-type balance heat-balance)
  (reduce #'+ (heat-balance-duties balance)
          :key #'heat-duty-value
          :initial-value 0d0))

(defun heat-balance-demand-total (balance)
  "Return the sum of positive heat demands in BALANCE."
  (check-type balance heat-balance)
  (reduce #'+ (remove-if-not #'plusp (heat-balance-duties balance)
                             :key #'heat-duty-value)
          :key #'heat-duty-value
          :initial-value 0d0))

(defun heat-balance-source-total (balance)
  "Return the sum of negative heat sources in BALANCE."
  (check-type balance heat-balance)
  (reduce #'+ (remove-if-not #'minusp (heat-balance-duties balance)
                             :key #'heat-duty-value)
          :key #'heat-duty-value
          :initial-value 0d0))

(defun heat-balance-closed-p (balance)
  "T iff BALANCE net duty is within its absolute tolerance."
  (check-type balance heat-balance)
  (approx= (heat-balance-net-duty balance)
           0d0
           (heat-balance-tolerance balance)))

(defun heat-balance->plist (balance)
  "Return a stable property-list representation of BALANCE."
  (check-type balance heat-balance)
  (list :name (heat-balance-name balance)
        :net-duty (heat-balance-net-duty balance)
        :demand-total (heat-balance-demand-total balance)
        :source-total (heat-balance-source-total balance)
        :closed-p (heat-balance-closed-p balance)
        :unit (heat-balance-unit balance)
        :tolerance (heat-balance-tolerance balance)
        :duties (mapcar #'heat-duty->plist (heat-balance-duties balance))
        :note (heat-balance-note balance)))

;;; ----------------------------------------------------------------------
;;; Carbon and economic ledgers
;;; ----------------------------------------------------------------------

(defparameter +carbon-entry-roles+
  '(:emission :sequestration :avoidance :stock :input :output :adjustment)
  "Canonical carbon-entry roles.
Carbon values are signed: positive increases atmospheric burden; negative is
removal, durable storage, or avoided burden.")

(defun carbon-entry-role-p (role)
  "T iff ROLE is a canonical carbon-entry role."
  (and (member role +carbon-entry-roles+) t))

(defstruct (carbon-entry (:constructor %make-carbon-entry))
  (name "" :type string)
  (value 0d0 :type real)
  (unit "tCO2e/d" :type string)
  (role :adjustment :type keyword)
  note)

(defun make-carbon-entry (name value &key
                               (unit "tCO2e/d")
                               (role :adjustment)
                               note)
  "Construct a signed carbon-accounting entry.
Positive VALUE increases atmospheric burden; negative VALUE reduces it or stores
carbon durably.  UNIT is a label and must match the containing ledger."
  (check-type name string)
  (check-type unit string)
  (check-type value real)
  (unless (carbon-entry-role-p role)
    (error "Unknown carbon-entry role: ~S" role))
  (%make-carbon-entry :name name
                      :value (float value 1d0)
                      :unit unit
                      :role role
                      :note note))

(defun carbon-entry->plist (entry)
  "Return a stable property-list representation of ENTRY."
  (check-type entry carbon-entry)
  (list :name (carbon-entry-name entry)
        :value (carbon-entry-value entry)
        :unit (carbon-entry-unit entry)
        :role (carbon-entry-role entry)
        :note (carbon-entry-note entry)))

(defstruct (carbon-ledger (:constructor %make-carbon-ledger))
  (name "" :type string)
  (entries nil :type list)
  (unit "tCO2e/d" :type string)
  (tolerance +default-balance-tolerance+ :type real)
  note)

(defun make-carbon-ledger (name entries &key
                                (unit "tCO2e/d")
                                (tolerance +default-balance-tolerance+)
                                note)
  "Construct a carbon ledger from signed entries."
  (check-type name string)
  (check-type unit string)
  (assert (every #'carbon-entry-p entries) (entries)
          "Carbon-ledger entries must be carbon-entry records.")
  (unless (%nonnegative-real-p tolerance)
    (error "Tolerance must be nonnegative: ~S" tolerance))
  (dolist (entry entries)
    (unless (string= (carbon-entry-unit entry) unit)
      (error "Carbon entry ~S has unit ~S, expected ~S."
             (carbon-entry-name entry) (carbon-entry-unit entry) unit)))
  (%make-carbon-ledger :name name
                       :entries (copy-list entries)
                       :unit unit
                       :tolerance (float tolerance 1d0)
                       :note note))

(defun carbon-ledger-net-burden (ledger)
  "Return signed net atmospheric burden for LEDGER.
Positive is worse; zero or negative is burden-neutral/improving."
  (check-type ledger carbon-ledger)
  (reduce #'+ (carbon-ledger-entries ledger)
          :key #'carbon-entry-value
          :initial-value 0d0))

(defun carbon-ledger-role-total (ledger role)
  "Return the signed total for ROLE in LEDGER."
  (check-type ledger carbon-ledger)
  (unless (carbon-entry-role-p role)
    (error "Unknown carbon-entry role: ~S" role))
  (reduce #'+ (remove role (carbon-ledger-entries ledger)
                      :key #'carbon-entry-role
                      :test-not #'eql)
          :key #'carbon-entry-value
          :initial-value 0d0))

(defun carbon-ledger-role-breakdown (ledger)
  "Return signed totals by canonical carbon-entry role."
  (check-type ledger carbon-ledger)
  (mapcar (lambda (role)
            (cons role (carbon-ledger-role-total ledger role)))
          +carbon-entry-roles+))

(defun carbon-ledger-improves-p (ledger)
  "T iff LEDGER is carbon-neutral or better within tolerance."
  (check-type ledger carbon-ledger)
  (<= (carbon-ledger-net-burden ledger)
      (carbon-ledger-tolerance ledger)))

(defun carbon-ledger->plist (ledger)
  "Return a stable property-list representation of LEDGER."
  (check-type ledger carbon-ledger)
  (list :name (carbon-ledger-name ledger)
        :net-burden (carbon-ledger-net-burden ledger)
        :role-breakdown (carbon-ledger-role-breakdown ledger)
        :improves-p (carbon-ledger-improves-p ledger)
        :unit (carbon-ledger-unit ledger)
        :tolerance (carbon-ledger-tolerance ledger)
        :entries (mapcar #'carbon-entry->plist (carbon-ledger-entries ledger))
        :note (carbon-ledger-note ledger)))

(defparameter +economic-entry-roles+
  '(:capex :opex :revenue :avoided-cost :grant :salvage :credit :other)
  "Canonical economic-entry roles.
Economic values are signed: positive improves project value; negative consumes
project value.")

(defun economic-entry-role-p (role)
  "T iff ROLE is a canonical economic-entry role."
  (and (member role +economic-entry-roles+) t))

(defstruct (economic-entry (:constructor %make-economic-entry))
  (name "" :type string)
  (value 0d0 :type real)
  (unit "kEUR/d" :type string)
  (role :other :type keyword)
  note)

(defun make-economic-entry (name value &key
                                 (unit "kEUR/d")
                                 (role :other)
                                 note)
  "Construct a signed economic entry.
Positive VALUE improves project value; negative VALUE is a cost or burden."
  (check-type name string)
  (check-type unit string)
  (check-type value real)
  (unless (economic-entry-role-p role)
    (error "Unknown economic-entry role: ~S" role))
  (%make-economic-entry :name name
                        :value (float value 1d0)
                        :unit unit
                        :role role
                        :note note))

(defun economic-entry->plist (entry)
  "Return a stable property-list representation of ENTRY."
  (check-type entry economic-entry)
  (list :name (economic-entry-name entry)
        :value (economic-entry-value entry)
        :unit (economic-entry-unit entry)
        :role (economic-entry-role entry)
        :note (economic-entry-note entry)))

(defstruct (economic-ledger (:constructor %make-economic-ledger))
  (name "" :type string)
  (entries nil :type list)
  (unit "kEUR/d" :type string)
  (tolerance +default-balance-tolerance+ :type real)
  note)

(defun make-economic-ledger (name entries &key
                                  (unit "kEUR/d")
                                  (tolerance +default-balance-tolerance+)
                                  note)
  "Construct an economic ledger from signed entries."
  (check-type name string)
  (check-type unit string)
  (assert (every #'economic-entry-p entries) (entries)
          "Economic-ledger entries must be economic-entry records.")
  (unless (%nonnegative-real-p tolerance)
    (error "Tolerance must be nonnegative: ~S" tolerance))
  (dolist (entry entries)
    (unless (string= (economic-entry-unit entry) unit)
      (error "Economic entry ~S has unit ~S, expected ~S."
             (economic-entry-name entry) (economic-entry-unit entry) unit)))
  (%make-economic-ledger :name name
                         :entries (copy-list entries)
                         :unit unit
                         :tolerance (float tolerance 1d0)
                         :note note))

(defun economic-ledger-net-value (ledger)
  "Return signed net project value for LEDGER.
Positive is viable/improving; negative is value-destructive."
  (check-type ledger economic-ledger)
  (reduce #'+ (economic-ledger-entries ledger)
          :key #'economic-entry-value
          :initial-value 0d0))

(defun economic-ledger-role-total (ledger role)
  "Return the signed total for ROLE in LEDGER."
  (check-type ledger economic-ledger)
  (unless (economic-entry-role-p role)
    (error "Unknown economic-entry role: ~S" role))
  (reduce #'+ (remove role (economic-ledger-entries ledger)
                      :key #'economic-entry-role
                      :test-not #'eql)
          :key #'economic-entry-value
          :initial-value 0d0))

(defun economic-ledger-role-breakdown (ledger)
  "Return signed totals by canonical economic-entry role."
  (check-type ledger economic-ledger)
  (mapcar (lambda (role)
            (cons role (economic-ledger-role-total ledger role)))
          +economic-entry-roles+))

(defun economic-ledger-viable-p (ledger)
  "T iff LEDGER is non-negative within tolerance."
  (check-type ledger economic-ledger)
  (>= (economic-ledger-net-value ledger)
      (- (economic-ledger-tolerance ledger))))

(defun economic-ledger->plist (ledger)
  "Return a stable property-list representation of LEDGER."
  (check-type ledger economic-ledger)
  (list :name (economic-ledger-name ledger)
        :net-value (economic-ledger-net-value ledger)
        :role-breakdown (economic-ledger-role-breakdown ledger)
        :viable-p (economic-ledger-viable-p ledger)
        :unit (economic-ledger-unit ledger)
        :tolerance (economic-ledger-tolerance ledger)
        :entries (mapcar #'economic-entry->plist
                         (economic-ledger-entries ledger))
        :note (economic-ledger-note ledger)))

(defstruct (mass-balance (:constructor %make-mass-balance))
  (name "" :type string)
  (inputs nil :type list)
  (outputs nil :type list)
  (tolerance +default-balance-tolerance+ :type real)
  note)

(defun make-mass-balance (name inputs outputs &key
                               (tolerance +default-balance-tolerance+)
                               note)
  "Construct a mass-balance check over input and output streams."
  (check-type name string)
  (assert (every #'stream-p inputs) (inputs)
          "Mass-balance inputs must be streams.")
  (assert (every #'stream-p outputs) (outputs)
          "Mass-balance outputs must be streams.")
  (unless (%nonnegative-real-p tolerance)
    (error "Tolerance must be nonnegative: ~S" tolerance))
  (%make-mass-balance :name name
                      :inputs (copy-list inputs)
                      :outputs (copy-list outputs)
                      :tolerance (float tolerance 1d0)
                      :note note))

(defun %stream-list-total (streams)
  (reduce #'+ streams :key #'stream-total-flow :initial-value 0d0))

(defun %stream-list-component-flow (streams component)
  (reduce #'+ streams
          :key (lambda (stream)
                 (stream-component-flow stream component))
          :initial-value 0d0))

(defun mass-balance-input-total (balance)
  "Return total input flow for BALANCE."
  (check-type balance mass-balance)
  (%stream-list-total (mass-balance-inputs balance)))

(defun mass-balance-output-total (balance)
  "Return total output flow for BALANCE."
  (check-type balance mass-balance)
  (%stream-list-total (mass-balance-outputs balance)))

(defun mass-balance-residual (balance)
  "Return output total minus input total for BALANCE."
  (check-type balance mass-balance)
  (- (mass-balance-output-total balance)
     (mass-balance-input-total balance)))

(defun mass-balance-components (balance)
  "Return all component keys observed in BALANCE inputs or outputs."
  (check-type balance mass-balance)
  (let ((components nil))
    (dolist (stream (append (mass-balance-inputs balance)
                            (mass-balance-outputs balance)))
      (dolist (component (stream-components stream))
        (pushnew component components)))
    (sort components #'%component-key<)))

(defun mass-balance-component-input-flow (balance component)
  "Return total input flow for COMPONENT in BALANCE."
  (check-type balance mass-balance)
  (%stream-list-component-flow (mass-balance-inputs balance) component))

(defun mass-balance-component-output-flow (balance component)
  "Return total output flow for COMPONENT in BALANCE."
  (check-type balance mass-balance)
  (%stream-list-component-flow (mass-balance-outputs balance) component))

(defun mass-balance-component-residual (balance component)
  "Return COMPONENT output flow minus input flow for BALANCE."
  (check-type balance mass-balance)
  (- (mass-balance-component-output-flow balance component)
     (mass-balance-component-input-flow balance component)))

(defun mass-balance-component-residuals (balance)
  "Return an alist of (component . output-minus-input) residuals."
  (check-type balance mass-balance)
  (mapcar (lambda (component)
            (cons component
                  (mass-balance-component-residual balance component)))
          (mass-balance-components balance)))

(defun mass-balance-component-closed-p (balance &optional component)
  "T iff component residuals are within BALANCE tolerance.
When COMPONENT is supplied, check only that component; otherwise check every
component observed in the balance.  This is a diagnostic for non-reactive
operations; reactive operations may close total mass while intentionally moving
mass between component labels."
  (check-type balance mass-balance)
  (if component
      (approx= (mass-balance-component-residual balance component)
               0d0
               (mass-balance-tolerance balance))
      (every (lambda (pair)
               (approx= (cdr pair) 0d0 (mass-balance-tolerance balance)))
             (mass-balance-component-residuals balance))))

(defun mass-balance-closed-p (balance)
  "T iff BALANCE residual is within its absolute tolerance."
  (check-type balance mass-balance)
  (approx= (mass-balance-residual balance)
           0d0
           (mass-balance-tolerance balance)))

(defun mass-balance->plist (balance)
  "Return a stable property-list representation of BALANCE."
  (check-type balance mass-balance)
  (list :name (mass-balance-name balance)
        :input-total (mass-balance-input-total balance)
        :output-total (mass-balance-output-total balance)
        :residual (mass-balance-residual balance)
        :component-residuals (mass-balance-component-residuals balance)
        :components-closed-p (mass-balance-component-closed-p balance)
        :closed-p (mass-balance-closed-p balance)
        :tolerance (mass-balance-tolerance balance)
        :inputs (mapcar #'stream->plist (mass-balance-inputs balance))
        :outputs (mapcar #'stream->plist (mass-balance-outputs balance))
        :note (mass-balance-note balance)))
