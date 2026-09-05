;;;; core.lisp --- Chemical thermochemistry evidence.

(in-package #:rosette-chemical-thermo)

(defstruct (component-thermo (:constructor %make-component-thermo))
  key
  (formation-enthalpy nil :type (or null real))
  (formation-gibbs nil :type (or null real))
  (specific-heat nil :type (or null real))
  note)

(defun make-component-thermo
    (key &key formation-enthalpy formation-gibbs specific-heat note)
  "Construct component thermochemistry data.

FORMATION-ENTHALPY and FORMATION-GIBBS are standard formation properties in
kJ/kmol. SPECIFIC-HEAT is mass specific heat in kJ/tonne/K, compatible with
process-engineering stream flows in tonnes/day."
  (when formation-enthalpy
    (check-type formation-enthalpy real))
  (when specific-heat
    (check-type specific-heat real))
  (when formation-gibbs
    (check-type formation-gibbs real))
  (%make-component-thermo
   :key key
   :formation-enthalpy (and formation-enthalpy
                            (float formation-enthalpy 1d0))
   :formation-gibbs (and formation-gibbs
                         (float formation-gibbs 1d0))
   :specific-heat (and specific-heat (float specific-heat 1d0))
   :note note))

(defun component-thermo->plist (entry)
  "Return a stable plist for component thermochemistry ENTRY."
  (check-type entry component-thermo)
  (list :key (component-thermo-key entry)
        :formation-enthalpy (component-thermo-formation-enthalpy entry)
        :formation-enthalpy-unit "kJ/kmol"
        :formation-gibbs (component-thermo-formation-gibbs entry)
        :formation-gibbs-unit "kJ/kmol"
        :specific-heat (component-thermo-specific-heat entry)
        :specific-heat-unit "kJ/tonne/K"
        :note (component-thermo-note entry)))

(defstruct (thermo-catalog (:constructor %make-thermo-catalog))
  (name "" :type string)
  (entries nil :type list)
  note)

(defun make-thermo-catalog (name entries &key note)
  "Construct a thermochemistry catalog from COMPONENT-THERMO records."
  (check-type name string)
  (assert (every #'component-thermo-p entries) (entries)
          "Thermo catalog entries must be component-thermo records.")
  (%make-thermo-catalog :name name
                        :entries (copy-list entries)
                        :note note))

(defun thermo-catalog-property (catalog key)
  "Return KEY's component thermochemistry record from CATALOG."
  (check-type catalog thermo-catalog)
  (or (find key (thermo-catalog-entries catalog)
            :key #'component-thermo-key
            :test #'equal)
      (error "No thermochemistry entry for component ~S in catalog ~S."
             key (thermo-catalog-name catalog))))

(defun thermo-catalog->plist (catalog)
  "Return a stable plist for CATALOG."
  (check-type catalog thermo-catalog)
  (list :name (thermo-catalog-name catalog)
        :entries (mapcar #'component-thermo->plist
                         (thermo-catalog-entries catalog))
        :note (thermo-catalog-note catalog)))

(defparameter *default-thermo-catalog*
  (make-thermo-catalog
   "small gas/vapor thermochemistry"
   (list
    (make-component-thermo :h2 :formation-enthalpy 0d0 :formation-gibbs 0d0
                           :specific-heat 14300d0)
    (make-component-thermo :co :formation-enthalpy -110530d0 :formation-gibbs -137160d0
                           :specific-heat 1040d0)
    (make-component-thermo :co2 :formation-enthalpy -393520d0 :formation-gibbs -394360d0
                           :specific-heat 844d0)
    (make-component-thermo :water :formation-enthalpy -241820d0 :formation-gibbs -228570d0
                           :specific-heat 1996d0)
    (make-component-thermo :methane :formation-enthalpy -74850d0 :formation-gibbs -50490d0
                           :specific-heat 2220d0)
    (make-component-thermo :methanol :formation-enthalpy -201000d0 :formation-gibbs -162000d0
                           :specific-heat 1560d0)
    (make-component-thermo :oxygen :formation-enthalpy 0d0 :formation-gibbs 0d0
                           :specific-heat 918d0)
    (make-component-thermo :nitrogen :formation-enthalpy 0d0 :formation-gibbs 0d0
                           :specific-heat 1040d0))
   :note "Approximate standard gas/vapor values near 298 K for audit evidence.")
  "Small thermochemistry catalog for process witnesses, not a reference DB.")

(defun %component-specific-heat (catalog key)
  (let ((cp (component-thermo-specific-heat
             (thermo-catalog-property catalog key))))
    (or cp (error "Component ~S has no specific heat." key))))

(defun %component-formation-enthalpy (catalog key)
  (let ((hf (component-thermo-formation-enthalpy
             (thermo-catalog-property catalog key))))
    (or hf (error "Component ~S has no formation enthalpy." key))))

(defun %component-formation-gibbs (catalog key)
  (let ((gf (component-thermo-formation-gibbs
             (thermo-catalog-property catalog key))))
    (or gf (error "Component ~S has no formation Gibbs energy." key))))

(defun %stream-temperature (stream)
  (let ((temperature (pe:stream-temperature stream)))
    (unless (realp temperature)
      (error "Stream ~S has no numeric temperature."
             (pe:stream-name stream)))
    (float temperature 1d0)))

(defun stream-sensible-enthalpy
    (stream &key
       (catalog *default-thermo-catalog*)
       (reference-temperature 298.15d0))
  "Return STREAM sensible enthalpy relative to REFERENCE-TEMPERATURE.

The return unit is kJ/day when stream flows are tonnes/day and component
specific heats are kJ/tonne/K."
  (check-type stream pe:material-stream)
  (check-type catalog thermo-catalog)
  (check-type reference-temperature real)
  (let ((delta-t (- (%stream-temperature stream)
                    (float reference-temperature 1d0))))
    (reduce #'+ (pe:stream-flows stream)
            :key (lambda (pair)
                   (* (cdr pair)
                      (%component-specific-heat catalog (car pair))
                      delta-t))
            :initial-value 0d0)))

(defun stream-heating-duty
    (stream target-temperature &key
       (catalog *default-thermo-catalog*)
       (unit "kJ/day")
       note)
  "Return a heat-duty record for heating or cooling STREAM to TARGET-TEMPERATURE.

Positive values mean heat demand; negative values mean heat removal."
  (check-type stream pe:material-stream)
  (check-type target-temperature real)
  (check-type unit string)
  (let ((delta-t (- (float target-temperature 1d0)
                    (%stream-temperature stream))))
    (pe:make-heat-duty
     (format nil "~A sensible heat" (pe:stream-name stream))
     (reduce #'+ (pe:stream-flows stream)
             :key (lambda (pair)
                    (* (cdr pair)
                       (%component-specific-heat catalog (car pair))
                       delta-t))
             :initial-value 0d0)
     :unit unit
     :note note)))

(defun reaction-standard-enthalpy
    (reaction &key (catalog *default-thermo-catalog*))
  "Return standard reaction enthalpy in kJ/kmol of stoichiometric extent."
  (check-type reaction pe:reaction)
  (check-type catalog thermo-catalog)
  (reduce #'+ (pe:reaction-stoich reaction)
          :key (lambda (pair)
                 (* (cdr pair)
                    (%component-formation-enthalpy catalog (car pair))))
          :initial-value 0d0))

(defun reaction-standard-gibbs
    (reaction &key (catalog *default-thermo-catalog*))
  "Return standard reaction Gibbs energy in kJ/kmol of stoichiometric extent."
  (check-type reaction pe:reaction)
  (check-type catalog thermo-catalog)
  (reduce #'+ (pe:reaction-stoich reaction)
          :key (lambda (pair)
                 (* (cdr pair)
                    (%component-formation-gibbs catalog (car pair))))
          :initial-value 0d0))

(defun reaction-step-actual-extent
    (inlet step &key (component-catalog pe:*default-component-catalog*))
  "Return actual stoichiometric extent for STEP over INLET in kmol/day."
  (check-type inlet pe:material-stream)
  (check-type step rn:reaction-step)
  (multiple-value-bind (limit limiting)
      (pe:reaction-extent-limit inlet
                                (rn:reaction-step-reaction step)
                                :catalog component-catalog)
    (declare (ignore limiting))
    (* limit (rn:reaction-step-conversion step))))

(defun reaction-step-heat-duty
    (inlet step &key
       (thermo-catalog *default-thermo-catalog*)
       (component-catalog pe:*default-component-catalog*)
       (unit "kJ/day"))
  "Return STEP reaction heat duty over INLET as a process heat-duty record.

The duty is standard reaction enthalpy times actual extent.  Negative values
are exothermic heat sources by the process-engineering heat-duty convention."
  (check-type unit string)
  (let* ((reaction (rn:reaction-step-reaction step))
         (enthalpy (reaction-standard-enthalpy reaction
                                              :catalog thermo-catalog))
         (extent (reaction-step-actual-extent
                  inlet step :component-catalog component-catalog)))
    (pe:make-heat-duty
     (format nil "~A reaction heat" (rn:reaction-step-name step))
     (* enthalpy extent)
     :unit unit
     :note (list :standard-reaction-enthalpy enthalpy
                 :standard-reaction-enthalpy-unit "kJ/kmol"
                 :extent extent
                 :extent-unit "kmol/day"))))

(defun reaction-network-heat-duties
    (inlet network &key
       (thermo-catalog *default-thermo-catalog*)
       (component-catalog pe:*default-component-catalog*)
       (unit "kJ/day"))
  "Return per-step reaction heat duties for NETWORK over INLET."
  (check-type inlet pe:material-stream)
  (check-type network rn:reaction-network)
  (let ((stream inlet)
        (duties nil))
    (dolist (step (rn:reaction-network-steps network))
      (push (reaction-step-heat-duty
             stream step
             :thermo-catalog thermo-catalog
             :component-catalog component-catalog
             :unit unit)
            duties)
      (setf stream
            (pe:stoichiometric-reactor
             (rn:reaction-step-name step)
             stream
             (rn:reaction-step-reaction step)
             (rn:reaction-step-conversion step)
             :catalog component-catalog)))
    (nreverse duties)))

(defun reaction-network-net-heat-duty
    (inlet network &key
       (thermo-catalog *default-thermo-catalog*)
       (component-catalog pe:*default-component-catalog*))
  "Return NETWORK net reaction heat duty in kJ/day."
  (pe:heat-balance-net-duty
   (reaction-network-heat-balance
    "network reaction heat"
    inlet network
    :thermo-catalog thermo-catalog
    :component-catalog component-catalog)))

(defun reaction-network-heat-balance
    (name inlet network &key
       (thermo-catalog *default-thermo-catalog*)
       (component-catalog pe:*default-component-catalog*)
       (unit "kJ/day")
       (tolerance pe:+default-balance-tolerance+)
       note)
  "Return a heat-balance containing NETWORK's reaction heat duties.

This balance is usually not closed by itself; callers add utility/recovery
duties when they want an overall closed heat ledger."
  (check-type name string)
  (pe:make-heat-balance
   name
   (reaction-network-heat-duties
    inlet network
    :thermo-catalog thermo-catalog
    :component-catalog component-catalog
    :unit unit)
   :unit unit
   :tolerance tolerance
   :note note))
