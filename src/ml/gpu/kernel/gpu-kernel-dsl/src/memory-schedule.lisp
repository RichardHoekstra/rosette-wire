;;;; memory-schedule.lisp --- canonical hierarchical tensor traffic schedules.

(in-package #:rosette-gpu-kernel-dsl)

(defparameter +memory-schedule-tensor-keys+
  '(:name :role :axes :mode :element-bytes :shared
    :shared-reuse :register-reuse))

(defun %memory-schedule-error (form reason)
  (error 'dsl-syntax-error :form form :reason reason))

(defun %canonical-axis-map (entries name &optional logical-axes)
  (unless (and (consp entries)
               (every (lambda (entry)
                        (and (listp entry) (= (length entry) 2)
                             (keywordp (first entry))
                             (integerp (second entry))
                             (plusp (second entry))))
                      entries))
    (%memory-schedule-error
     entries (format nil "~A must contain (KEYWORD POSITIVE-INTEGER) pairs"
                     name)))
  (let ((axes (mapcar #'first entries)))
    (unless (= (length axes) (length (remove-duplicates axes)))
      (%memory-schedule-error entries
                              (format nil "~A axes must be unique" name)))
    (when (and logical-axes (not (equal axes logical-axes)))
      (%memory-schedule-error
       entries (format nil "~A must use the logical axis order ~S"
                       name logical-axes)))
    (mapcar #'copy-list entries)))

(defun %axis-value (axis entries)
  (second (assoc axis entries)))

(defun %canonical-axis-scopes (entries logical-axes)
  (unless (and (listp entries)
               (= (length entries) (length logical-axes))
               (every (lambda (entry)
                        (and (listp entry) (= (length entry) 2)
                             (keywordp (first entry))
                             (member (second entry) '(:grid :block :serial))))
                      entries)
               (equal (mapcar #'first entries) logical-axes))
    (%memory-schedule-error
     entries
     (format nil "AXIS-SCOPES must map logical axes ~S in order to :GRID, :BLOCK, or :SERIAL"
             logical-axes)))
  (mapcar #'copy-list entries))

(defun %canonical-axis-subset (axes logical-axes form name)
  (unless (and (listp axes) (every #'keywordp axes)
               (= (length axes) (length (remove-duplicates axes)))
               (every (lambda (axis) (member axis logical-axes)) axes))
    (%memory-schedule-error
     form (format nil "~A must be unique logical-axis keywords" name)))
  (remove-if-not (lambda (axis) (member axis axes)) logical-axes))

(defun %plist-has-exact-keys-p (plist keys)
  (and (listp plist) (evenp (length plist))
       (= (length plist) (* 2 (length keys)))
       (every (lambda (key)
                (= 1 (loop for tail on plist by #'cddr
                           count (eq key (first tail)))))
              keys)))

(defun %canonical-memory-tensor (tensor logical-axes axis-scopes)
  (unless (%plist-has-exact-keys-p tensor +memory-schedule-tensor-keys+)
    (%memory-schedule-error
     tensor (format nil "memory tensor must have exactly keys ~S"
                    +memory-schedule-tensor-keys+)))
  (let* ((name (getf tensor :name))
         (role (getf tensor :role))
         (axes (%canonical-axis-subset
                (getf tensor :axes) logical-axes tensor "tensor axes"))
         (mode (getf tensor :mode))
         (element-bytes (getf tensor :element-bytes))
         (shared (getf tensor :shared))
         (shared-reuse
           (%canonical-axis-subset
            (getf tensor :shared-reuse) logical-axes tensor
            "shared reuse axes"))
         (register-reuse
           (%canonical-axis-subset
            (getf tensor :register-reuse) logical-axes tensor
            "register reuse axes"))
         (complement (set-difference logical-axes axes)))
    (unless (keywordp name)
      (%memory-schedule-error tensor "tensor name must be a keyword"))
    (unless (member role '(:activation :weight :output :auxiliary))
      (%memory-schedule-error tensor "unsupported tensor role"))
    (unless (member mode '(:read :write :read-write))
      (%memory-schedule-error tensor "unsupported tensor access mode"))
    (unless (and (integerp element-bytes) (plusp element-bytes))
      (%memory-schedule-error tensor "element bytes must be positive"))
    (unless (member shared '(nil t))
      (%memory-schedule-error tensor "SHARED must be boolean"))
    (unless (and (every (lambda (axis) (member axis complement)) shared-reuse)
                 (every (lambda (axis) (member axis complement)) register-reuse))
      (%memory-schedule-error
       tensor "reuse axes must be logical axes absent from the tensor projection"))
    (when (and (not shared) shared-reuse)
      (%memory-schedule-error tensor
                              "shared reuse requires shared residency"))
    (unless (every (lambda (axis)
                     (not (eq :grid (%axis-value axis axis-scopes))))
                   shared-reuse)
      (%memory-schedule-error
       tensor "shared reuse cannot cross independently scheduled grid tiles"))
    (unless (every (lambda (axis)
                     (eq :serial (%axis-value axis axis-scopes)))
                   register-reuse)
      (%memory-schedule-error
       tensor "register reuse requires a serial axis owned by one worker"))
    (list :name name :role role :axes axes :mode mode
          :element-bytes element-bytes :shared shared
          :shared-reuse shared-reuse :register-reuse register-reuse)))

(defun %tensor-name< (left right)
  (string< (symbol-name (getf left :name))
           (symbol-name (getf right :name))))

(defun %stationary-role (dataflow)
  (ecase dataflow
    (:none nil)
    (:weight-stationary :weight)
    (:output-stationary :output)
    (:row-stationary :activation)))

(defun %check-stationarity-witness (dataflow tensors logical-axes)
  (let ((role (%stationary-role dataflow)))
    (when role
      (unless
          (some
           (lambda (tensor)
             (let* ((axes (getf tensor :axes))
                    (complement (set-difference logical-axes axes))
                    (reuse (union (getf tensor :shared-reuse)
                                  (getf tensor :register-reuse))))
               (and (eq role (getf tensor :role))
                    complement
                    (subsetp complement reuse))))
           tensors)
        (%memory-schedule-error
         tensors
         (format nil "~S requires a matching tensor resident across every projected-out axis"
                 dataflow))))))

(defun make-memory-schedule
    (&key axes tile-shape axis-scopes (dataflow :none) tensors
          (shared-capacity 49152) (register-capacity 65536))
  "Return a canonical hierarchical memory schedule identity.

TENSORS contain exact logical projections and declared shared/register reuse.
AXIS-SCOPES bind every logical axis to independent grid tiles, block-cooperative
work, or serial work owned by one worker. Register reuse is legal only over
serial axes; shared reuse cannot cross grid ownership. DATAFLOW is a checked
label, and capacities are byte budgets rather than ambient probes."
  (let* ((canonical-axes (%canonical-axis-map axes "AXES"))
         (logical-axes (mapcar #'first canonical-axes))
         (canonical-tile
           (%canonical-axis-map tile-shape "TILE-SHAPE" logical-axes))
         (canonical-scopes
           (%canonical-axis-scopes axis-scopes logical-axes)))
    (unless (member dataflow
                    '(:none :weight-stationary :output-stationary
                      :row-stationary))
      (%memory-schedule-error dataflow "unsupported memory dataflow"))
    (dolist (entry canonical-tile)
      (when (> (second entry) (%axis-value (first entry) canonical-axes))
        (%memory-schedule-error entry
                                "tile extent exceeds logical extent")))
    (unless (and (consp tensors) (listp tensors))
      (%memory-schedule-error tensors "TENSORS must be nonempty"))
    (unless (and (integerp shared-capacity) (plusp shared-capacity)
                 (integerp register-capacity) (plusp register-capacity))
      (%memory-schedule-error
       (list shared-capacity register-capacity)
       "memory capacities must be positive byte counts"))
    (let ((canonical-tensors
            (sort (mapcar (lambda (tensor)
                            (%canonical-memory-tensor
                             tensor logical-axes canonical-scopes))
                          tensors)
                  #'%tensor-name<)))
      (unless (= (length canonical-tensors)
                 (length (remove-duplicates
                          canonical-tensors :key (lambda (tensor)
                                                   (getf tensor :name)))))
        (%memory-schedule-error tensors "tensor names must be unique"))
      (%check-stationarity-witness dataflow canonical-tensors logical-axes)
      (list :schema :rosette-memory-schedule/v1
            :axes canonical-axes
            :tile-shape canonical-tile
            :axis-scopes canonical-scopes
            :dataflow dataflow
            :tensors canonical-tensors
            :levels (list (list :global :capacity :unbounded)
                          (list :shared :capacity shared-capacity)
                          (list :register :capacity register-capacity))))))

(defun %validated-memory-schedule (schedule)
  (unless (and (listp schedule)
               (eq (getf schedule :schema) :rosette-memory-schedule/v1))
    (%memory-schedule-error schedule
                            "not an :ROSETTE-MEMORY-SCHEDULE/V1 identity"))
  (let* ((levels (getf schedule :levels))
         (canonical
           (make-memory-schedule
            :axes (getf schedule :axes)
            :tile-shape (getf schedule :tile-shape)
            :axis-scopes (getf schedule :axis-scopes)
            :dataflow (getf schedule :dataflow)
            :tensors (getf schedule :tensors)
            :shared-capacity (getf (rest (second levels)) :capacity)
            :register-capacity (getf (rest (third levels)) :capacity))))
    (unless (equal schedule canonical)
      (%memory-schedule-error schedule
                              "memory schedule identity is non-canonical"))
    canonical))

(defun %integer-product (values)
  (reduce #'* values :initial-value 1))

(defun %access-multiplier (mode)
  (if (eq mode :read-write) 2 1))

(defun %replication-factor (complement reuse tile-counts)
  (%integer-product
   (loop for axis in complement
         unless (member axis reuse)
           collect (%axis-value axis tile-counts))))

(defun memory-schedule-traffic (schedule)
  "Derive exact logical per-level byte traffic and capacity residues.

The receipt counts declared logical transfers. It does not claim DRAM
transactions, cache-hit rates, bank-conflict freedom, or measured latency."
  (let* ((canonical (%validated-memory-schedule schedule))
         (axes (getf canonical :axes))
         (logical-axes (mapcar #'first axes))
         (tile-shape (getf canonical :tile-shape))
         (tile-counts
           (loop for (axis extent) in axes
                 collect (list axis
                               (ceiling extent (%axis-value axis tile-shape)))))
         (levels (getf canonical :levels))
         (shared-capacity (getf (rest (second levels)) :capacity))
         (register-capacity (getf (rest (third levels)) :capacity))
         (rows '())
         (global-bytes 0)
         (shared-bytes 0)
         (register-bytes 0)
         (shared-footprint 0)
         (register-footprint 0))
    (dolist (tensor (getf canonical :tensors))
      (let* ((tensor-axes (getf tensor :axes))
             (complement (set-difference logical-axes tensor-axes))
             (element-bytes (getf tensor :element-bytes))
             (multiplier (%access-multiplier (getf tensor :mode)))
             (elements (%integer-product
                        (mapcar (lambda (axis) (%axis-value axis axes))
                                tensor-axes)))
             (tile-elements (%integer-product
                             (mapcar (lambda (axis)
                                       (%axis-value axis tile-shape))
                                     tensor-axes)))
             (global-reuse (union (getf tensor :shared-reuse)
                                  (getf tensor :register-reuse)))
             (global-replication
               (%replication-factor complement global-reuse tile-counts))
             (shared-replication
               (if (getf tensor :shared)
                   (%replication-factor
                    complement (getf tensor :register-reuse) tile-counts)
                   0))
             (register-replication
               (%replication-factor complement nil tile-counts))
             (tensor-global
               (* elements global-replication element-bytes multiplier))
             (tensor-shared
               (* elements shared-replication element-bytes multiplier))
             (tensor-register
               (* elements register-replication element-bytes multiplier))
             (tensor-shared-footprint
               (if (getf tensor :shared) (* tile-elements element-bytes) 0))
             (tensor-register-footprint (* tile-elements element-bytes)))
        (incf global-bytes tensor-global)
        (incf shared-bytes tensor-shared)
        (incf register-bytes tensor-register)
        (incf shared-footprint tensor-shared-footprint)
        (incf register-footprint tensor-register-footprint)
        (push (list :name (getf tensor :name)
                    :global-bytes tensor-global
                    :shared-bytes tensor-shared
                    :register-bytes tensor-register
                    :global-replication global-replication
                    :shared-replication shared-replication
                    :register-replication register-replication
                    :shared-footprint tensor-shared-footprint
                    :register-footprint tensor-register-footprint)
              rows)))
    (list :schema :rosette-memory-traffic-receipt/v1
          :schedule canonical
         :logical-work-elements
          (%integer-product (mapcar #'second axes))
          :launch-elements
          (%integer-product
           (loop for (axis tiles) in tile-counts
                 when (eq :grid
                          (%axis-value axis (getf canonical :axis-scopes)))
                   collect tiles))
          :tile-counts tile-counts
          :tensor-traffic (nreverse rows)
          :global-bytes global-bytes
          :shared-bytes shared-bytes
          :register-bytes register-bytes
          :shared-footprint shared-footprint
          :register-footprint register-footprint
          :shared-capacity-residue (- shared-footprint shared-capacity)
          :register-capacity-residue (- register-footprint register-capacity))))

(defun verify-memory-schedule-traffic (schedule receipt)
  "Recompute RECEIPT from SCHEDULE; return a boolean and located reasons."
  (handler-case
      (let ((expected (memory-schedule-traffic schedule)))
        (if (equal receipt expected)
            (values t nil)
            (values nil (list (list :kind :derived-traffic-drift)))))
    (error (condition)
      (values nil (list (list :kind :invalid-memory-schedule
                              :detail (princ-to-string condition)))))))
