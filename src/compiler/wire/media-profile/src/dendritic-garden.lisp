;;;; dendritic-garden.lisp --- public branch/state snapshots as semantic scenes.

(in-package #:rosette-media)

(defun %bounded-unit-interval (value path)
  (unless (and (%finite-real-p value) (<= 0 value 1))
    (%media-error :invalid-dendritic-value path
                  "expected a finite value in [0,1]"))
  (coerce value 'double-float))

(defun %snapshot-field (record name path)
  (let ((pair (and (listp record) (assoc name record :test #'string=))))
    (unless pair (%media-error :missing-field path "missing ~A" name))
    (cdr pair)))

(defun %validate-synapses (synapses path)
  (unless (and (listp synapses) (<= (length synapses) 64))
    (%media-error :invalid-synapses path "expected at most 64 synapses"))
  (copy-tree synapses))

(defun %own-dendritic-branch (branch ui bi)
  (let ((path (format nil "$.units[~D].branches[~D]" ui bi)))
    `(("id" . ,(%snapshot-field branch "id" path))
      ("retention" . ,(%bounded-unit-interval
                         (%snapshot-field branch "retention" path) path))
      ("state" . ,(%bounded-unit-interval
                     (%snapshot-field branch "state" path) path))
      ("synapses" . ,(%validate-synapses
                        (%snapshot-field branch "synapses" path) path))
      ("timescale" . ,(%bounded-unit-interval
                         (%snapshot-field branch "timescale" path) path))
      ("writeGate" . ,(%bounded-unit-interval
                         (%snapshot-field branch "writeGate" path) path)))))

(defun %own-dendritic-unit (unit ui)
  (let* ((path (format nil "$.units[~D]" ui))
         (id (%snapshot-field unit "id" path))
         (x (%snapshot-field unit "x" path))
         (y (%snapshot-field unit "y" path))
         (branches (%snapshot-field unit "branches" path)))
    (unless (and (stringp id) (plusp (length id))
                 (%finite-real-p x) (%finite-real-p y)
                 (listp branches) (plusp (length branches))
                 (<= (length branches) 32))
      (%media-error :invalid-dendritic-unit path
                    "invalid id/position or branch count"))
    `(("branches" . ,(loop for branch in branches for bi from 0
                           collect (%own-dendritic-branch branch ui bi)))
      ("id" . ,id)
      ("output" . ,(%bounded-unit-interval
                      (%snapshot-field unit "output" path) path))
      ("x" . ,(coerce x 'double-float))
      ("y" . ,(coerce y 'double-float)))))

(defun make-dendritic-snapshot (units &key (tick 0) (sequence 0)
                                       (source-contract
                                         "rosette.dendritic-state/1"))
  "Validate and own a portable dendritic state snapshot.

No training object, tensor, device pointer, or implementation crosses this
boundary. Private Rosette adapters can populate the same public data contract."
  (unless (and (listp units) (plusp (length units)) (<= (length units) 64))
    (%media-error :invalid-dendritic-snapshot "$.units" "expected 1..64 units"))
  (unless (and (integerp tick) (not (minusp tick))
               (integerp sequence) (not (minusp sequence)))
    (%media-error :invalid-clock "$.snapshot"
                  "tick/sequence must be nonnegative"))
  `(("schema" . "rosette.dendritic-snapshot/1")
    ("sequence" . ,sequence)
    ("sourceContract" . ,source-contract)
    ("tick" . ,tick)
    ("units" . ,(loop for unit in units for ui from 0
                      collect (%own-dendritic-unit unit ui)))))

(defparameter +garden-directions+
  '((-42 -48) (-24 -68) (0 -78) (24 -68) (42 -48)
    (-52 -28) (52 -28) (-12 -54) (12 -54)))

(defun %garden-color (state write)
  (list (round (+ 40 (* 200 write)))
        (round (+ 90 (* 140 state)))
        (round (+ 120 (* 115 (- 1 state))))))

(defun %typed-dendritic-identity-value (value)
  "Encode binary floats explicitly before entering Wire content identity."
  (typecase value
    (float
     (multiple-value-bind (significand exponent sign)
         (integer-decode-float value)
       `(("$float" . (("exponent" . ,exponent)
                       ("format" . ,(string-downcase
                                      (princ-to-string (type-of value))))
                       ("sign" . ,sign)
                       ("significand" . ,(format nil "~D" significand)))))))
    (cons (cons (%typed-dendritic-identity-value (car value))
                (%typed-dendritic-identity-value (cdr value))))
    (string (copy-seq value))
    (vector (map 'vector #'%typed-dendritic-identity-value value))
    (t value)))

(defun make-dendritic-garden-scene (snapshot &key (width 900) (height 520))
  "Project a validated portable snapshot into a deterministic branch garden."
  (unless (and (listp snapshot)
               (string= "rosette.dendritic-snapshot/1"
                        (or (cdr (assoc "schema" snapshot :test #'string=)) "")))
    (%media-error :invalid-dendritic-snapshot "$.snapshot" "unknown schema"))
  (unless (and (integerp width) (>= width 480)
               (integerp height) (>= height 300))
    (%media-error :invalid-extent "$.garden.extent"
                  "expected width >= 480 and height >= 300"))
  (let ((units (%snapshot-field snapshot "units" "$.snapshot.units"))
        (nodes (list (%draw (list :text 18 22 1000 204 238 222 1
                                  "DENDRITIC GARDEN"))
                     (%draw '(:clear 6 12 18)))))
    (loop for unit in units
          for x = (round (%snapshot-field unit "x" "$unit.x"))
          for y = (round (%snapshot-field unit "y" "$unit.y"))
          for output = (%snapshot-field unit "output" "$unit.output")
          for branches = (%snapshot-field unit "branches" "$unit.branches")
          do (push (%draw (list :fill-rect (- x 8) (- y 8) 16 16
                                70 (round (+ 100 (* 150 output))) 190 950)) nodes)
             (push (%draw (list :text (+ x 12) (+ y 4) 650 154 178 196 1
                                (%snapshot-field unit "id" "$unit.id"))) nodes)
             (loop for branch in branches for bi from 0
                   for direction = (nth (mod bi (length +garden-directions+))
                                        +garden-directions+)
                   for timescale = (%snapshot-field branch "timescale"
                                                     "$branch.timescale")
                   for state = (%snapshot-field branch "state" "$branch.state")
                   for write = (%snapshot-field branch "writeGate"
                                                 "$branch.writeGate")
                   for retention = (%snapshot-field branch "retention"
                                                     "$branch.retention")
                   for bx = (+ x (first direction))
                   for by = (+ y (round (* (second direction)
                                            (+ 0.45d0 timescale))))
                   for color = (%garden-color state write)
                   do (push (%draw (list :line x y bx by
                                         (first color) (second color) (third color)
                                         (max 1 (round (+ 1 (* 4 retention)))) 900))
                            nodes)
                      (push (%draw (list :fill-rect (- bx 3) (- by 3) 6 6
                                        (first color) (second color) (third color)
                                        1000)) nodes)
                      (loop for synapse in (%snapshot-field
                                            branch "synapses" "$branch.synapses")
                            for si from 0 below 6
                            for sx = (+ bx (- (* si 5) 12))
                            for sy = (- by 10 (mod si 2))
                            do (when synapse
                                 (push (%draw (list :line bx by sx sy
                                                   90 128 144 1 700)) nodes)))))
    (push (%draw (list :text 18 (- height 16) 650 126 160 170 1
                       "HEIGHT = TIMESCALE | COLOR = STATE/WRITE | WIDTH = RETENTION"))
          nodes)
    (scene:parse-scene
     (list :scene (list :size width height)
           (cons :group (nreverse nodes))))))

(defun make-dendritic-garden-frame (snapshot &key (rate 30))
  "Wrap the garden in the snapshot's explicit model clock."
  (unless (and (integerp rate) (plusp rate))
    (%media-error :invalid-clock "$.garden.rate" "expected a positive rate"))
  (let* ((semantic (make-dendritic-garden-scene snapshot))
         (tick (%snapshot-field snapshot "tick" "$.snapshot.tick"))
         (sequence (%snapshot-field snapshot "sequence" "$.snapshot.sequence")))
    `(("clock" . (("domain" . (("model" . t)))
                    ("epoch" . "sequence")
                    ("rateDenominator" . 1)
                    ("rateNumerator" . ,rate)))
      ("durationTicks" . 1)
      ("format" . "application/vnd.rosette-wire.semantic-scene+json")
      ("height" . 520)
      ("payload" . (("scene" . ,(wire:canonical-json
                                    (scene:unparse-scene semantic)))
                     ("sceneId" . ,(semantic-scene-id semantic))
                     ("snapshotId" . ,(wire:canonical-id
                                        (%typed-dendritic-identity-value
                                         snapshot)))))
      ("presentationTick" . ,tick)
      ("sequence" . ,sequence)
      ("width" . 900))))
