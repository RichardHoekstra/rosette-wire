;;;; debug-scenes.lisp --- semantic debugger projection over public evidence.

(in-package #:rosette-media)

(defparameter +eshkol-debug-floors+
  '((:direct . "DIRECT") (:kernel-vm . "KERNEL VM")
    (:eshkol-jit . "ESHKOL JIT") (:eshkol-aot . "ESHKOL AOT")))

(defun %plist-required (plist key path)
  (let* ((marker (list nil)) (value (getf plist key marker)))
    (when (eq value marker)
      (%media-error :missing-field path "missing ~S" key))
    value))

(defun %diagnostic-content (bundle-form)
  (unless (and (listp bundle-form) (evenp (length bundle-form)))
    (%media-error :invalid-diagnostic "$.diagnostic" "expected a property list"))
  (let ((id (%plist-required bundle-form :id "$.diagnostic.id"))
        (content (%plist-required bundle-form :content "$.diagnostic.content")))
    (unless (and (stringp id) (plusp (length id))
                 (listp content) (evenp (length content)))
      (%media-error :invalid-diagnostic "$.diagnostic"
                    "expected a non-empty id and property-list content"))
    content))

(defun %debug-floor-state (floor observations disagreements boundary verdict)
  (cond ((eq verdict :unavailable) :unavailable)
        ((some (lambda (entry) (eq floor (getf entry :right))) disagreements)
         :mismatch)
        ((or (getf observations floor)
             (and (eq verdict :pass) (eq boundary :complete))) :pass)
        (t :pending)))

(defun %debug-color (state)
  (ecase state
    (:pass '(52 211 153)) (:mismatch '(255 92 112))
    (:unavailable '(255 190 74)) (:pending '(92 112 158))))

(defun make-eshkol-debug-scene (bundle-form &key (width 720) (height 300))
  "Render a path-free Eshkol diagnostic form as a semantic floor timeline.

The diagnostic producer remains authoritative. This projection cannot run
Eshkol or upgrade a stored verdict."
  (unless (and (integerp width) (>= width 480)
               (integerp height) (>= height 220))
    (%media-error :invalid-extent "$.debug.extent"
                  "expected width >= 480 and height >= 220"))
  (let* ((content (%diagnostic-content bundle-form))
         (verdict (%plist-required content :verdict "$.diagnostic.verdict"))
         (outcome (%plist-required content :outcome "$.diagnostic.outcome"))
         (boundary (%plist-required content :earliest-boundary
                                    "$.diagnostic.earliestBoundary"))
         (observations (or (getf content :observations) nil))
         (disagreements (or (getf content :disagreements) nil))
         (left 28) (top 78) (gap 12)
         (card-width (floor (- width (* 2 left) (* 3 gap)) 4))
         (nodes (list (%draw (list :text 18 44 750 150 167 205 1
                                   (format nil "~A / ~A / boundary ~A"
                                           verdict outcome boundary)))
                      (%draw (list :text 18 22 1000 210 224 255 1
                                   "ESHKOL EXECUTION FLOORS"))
                      (%draw '(:clear 7 10 22)))))
    (loop for (floor . label) in +eshkol-debug-floors+
          for index from 0
          for x = (+ left (* index (+ card-width gap)))
          for state = (%debug-floor-state floor observations disagreements
                                          boundary verdict)
          for color = (%debug-color state)
          for value = (getf observations floor)
          do (push (%draw (list* :fill-rect x top card-width 112
                                 (append color '(260)))) nodes)
             (push (%draw (list :line x top (+ x card-width) top
                                (first color) (second color) (third color)
                                3 1000)) nodes)
             (push (%draw (list :text (+ x 10) (+ top 24) 750
                                226 234 255 1 label)) nodes)
             (push (%draw (list :text (+ x 10) (+ top 50) 700
                                (first color) (second color) (third color) 1
                                (string-upcase (symbol-name state)))) nodes)
             (push (%draw (list :text (+ x 10) (+ top 78) 650
                                150 167 205 1
                                (if value (format nil "value ~A" value) "no value")))
                   nodes))
    (push (%draw (list :text 18 (- height 18) 650 126 145 184 1
                       "VIEW ONLY - THE DIAGNOSTIC RECEIPT REMAINS AUTHORITY"))
          nodes)
    (scene:parse-scene
     (list :scene (list :size width height)
           (cons :group (nreverse nodes))))))

(defun make-eshkol-debug-frame (bundle-form sequence
                                &key (tick sequence) (rate 10))
  "Wrap one debugger projection in an explicit diagnostic-clock frame."
  (unless (and (integerp sequence) (not (minusp sequence))
               (integerp tick) (not (minusp tick))
               (integerp rate) (plusp rate))
    (%media-error :invalid-clock "$.debug.frame"
                  "sequence/tick must be nonnegative and rate positive"))
  (let* ((semantic (make-eshkol-debug-scene bundle-form))
         (value (scene:unparse-scene semantic)))
    `(("clock" . (("domain" . (("diagnostic" . t)))
                    ("epoch" . "campaign")
                    ("rateDenominator" . 1)
                    ("rateNumerator" . ,rate)))
      ("durationTicks" . 1)
      ("format" . "application/vnd.rosette-wire.semantic-scene+json")
      ("height" . 300)
      ("payload" . (("scene" . ,(wire:canonical-json value))
                     ("sceneId" . ,(semantic-scene-id semantic))))
      ("presentationTick" . ,tick)
      ("sequence" . ,sequence)
      ("width" . 720))))
