;;;; media-profile.lisp --- typed temporal media and semantic visual objects.

(in-package #:rosette-media)

(defparameter +media-profile-version+ "rosette.media/1")

(define-condition media-profile-error (error)
  ((code :initarg :code :reader media-profile-error-code)
   (path :initarg :path :reader media-profile-error-path :initform "$")
   (detail :initarg :detail :reader media-profile-error-detail :initform ""))
  (:report (lambda (condition stream)
             (format stream "Rosette media ~A at ~A: ~A"
                     (media-profile-error-code condition)
                     (media-profile-error-path condition)
                     (media-profile-error-detail condition)))))

(defun %media-error (code path control &rest arguments)
  (error 'media-profile-error :code code :path path
                              :detail (apply #'format nil control arguments)))

(defun %field (name type)
  (wire:make-component-field name type))

(defun %tag-type (name)
  "A one-case structural tag.  Unlike a free string, NAME affects type identity."
  (wire:variant-type (%field name (wire:scalar-type :bool))))

(defun media-clock-type (&optional (domain "monotonic"))
  "The explicit clock carried by every temporal media value.

DOMAIN is part of the structural type identity.  RATE is a rational number of
ticks per second and EPOCH identifies the origin chosen by the host."
  (unless (and (stringp domain) (plusp (length domain)))
    (%media-error :invalid-clock "$.clock.domain" "expected a non-empty domain"))
  (wire:record-type
   (%field "domain" (%tag-type domain))
   (%field "epoch" (wire:scalar-type :string))
   (%field "rateDenominator" (wire:scalar-type :u64))
   (%field "rateNumerator" (wire:scalar-type :u64))))

(defun %require-wire-type (value path)
  (unless (wire:wire-type-p value)
    (%media-error :invalid-payload-type path "expected a Wire type"))
  value)

(defun media-signal-type (value-type &key (clock-domain "monotonic"))
  "A sampled value at an explicit clock tick; sequence makes ties total."
  (%require-wire-type value-type "$.signal.value")
  (wire:record-type
   (%field "clock" (media-clock-type clock-domain))
   (%field "sequence" (wire:scalar-type :u64))
   (%field "tick" (wire:scalar-type :u64))
   (%field "value" value-type)))

(defun media-event-type (value-type &key (clock-domain "monotonic"))
  "A discrete ordered occurrence on an explicit clock."
  (%require-wire-type value-type "$.event.value")
  (wire:record-type
   (%field "clock" (media-clock-type clock-domain))
   (%field "kind" (wire:scalar-type :string))
   (%field "sequence" (wire:scalar-type :u64))
   (%field "tick" (wire:scalar-type :u64))
   (%field "value" value-type)))

(defun media-frame-type (payload-type &key (clock-domain "monotonic"))
  "A bounded presentation unit with shape, duration, format and explicit PTS."
  (%require-wire-type payload-type "$.frame.payload")
  (wire:record-type
   (%field "clock" (media-clock-type clock-domain))
   (%field "durationTicks" (wire:scalar-type :u64))
   (%field "format" (wire:scalar-type :string))
   (%field "height" (wire:scalar-type :u64))
   (%field "payload" payload-type)
   (%field "presentationTick" (wire:scalar-type :u64))
   (%field "sequence" (wire:scalar-type :u64))
   (%field "width" (wire:scalar-type :u64))))

(defun media-stream-type (element-type &key (clock-domain "monotonic"))
  "An ordered, explicitly terminated temporal sequence."
  (%require-wire-type element-type "$.stream.element")
  (wire:record-type
   (%field "clock" (media-clock-type clock-domain))
   (%field "end" (wire:scalar-type :bool))
   (%field "sequence" (wire:scalar-type :u64))
   (%field "value" (wire:option-type element-type))))

(defparameter +resource-lifetimes+
  '("call" "frame" "stream" "composition" "host"))

(defun media-resource-type (name owner ownership lifetime)
  "A Wire resource whose lifetime is part of its structural identity."
  (unless (member lifetime +resource-lifetimes+ :test #'string=)
    (%media-error :invalid-lifetime "$.resource.lifetime"
                  "expected one of ~{~A~^, ~}, got ~S"
                  +resource-lifetimes+ lifetime))
  (wire:record-type
   (%field "handle" (wire:resource-type name owner ownership))
   (%field "lifetime" (%tag-type lifetime))))

(defun %media-kind-type (kind payload-type clock-domain)
  (ecase kind
    (:signal (media-signal-type payload-type :clock-domain clock-domain))
    (:event (media-event-type payload-type :clock-domain clock-domain))
    (:frame (media-frame-type payload-type :clock-domain clock-domain))
    (:stream (media-stream-type payload-type :clock-domain clock-domain))))

(defun make-media-port (name kind payload-type
                        &key (direction :export) (operation "emit")
                             (clock-domain "monotonic"))
  "Construct one canonical temporal Port.

DIRECTION determines whether the operation consumes or produces the media
value.  Resource Ports use MEDIA-RESOURCE-TYPE directly because resources are
not clocks or streams by implication."
  (unless (member direction '(:import :export))
    (%media-error :invalid-direction "$.port.direction" "got ~S" direction))
  (let ((type (%media-kind-type kind payload-type clock-domain)))
    (wire:make-port
     name
     (list (if (eq direction :export)
               (wire:make-component-operation operation nil type)
               (wire:make-component-operation
                operation (list (%field "value" type))
                (wire:scalar-type :bool)))))))

(defun make-media-component-descriptor
    (&key name version imports exports (effects '(:pure)) capabilities
          adapter verifiers)
  "Build a Component descriptor stamped with the Rosette media profile."
  (wire:make-component-descriptor
   :name name :version version :imports imports :exports exports
   :effects effects :capabilities capabilities
   :adapter (append `(("mediaProfile" . ,+media-profile-version+)) adapter)
   :verifiers verifiers))

(defun %quantum-state-type ()
  (wire:tensor-type :f64 '(:dynamic 2)))

(defun %quantum-evidence-type ()
  (wire:record-type
   (%field "cpuFingerprint" (wire:scalar-type :string))
   (%field "scene" (wire:scalar-type :string))
   (%field "sceneId" (wire:scalar-type :string))
   (%field "svg" (wire:scalar-type :string))))

(defun %quantum-frame-type ()
  (media-frame-type (%quantum-evidence-type) :clock-domain "simulation"))

(defun quantum-microscope-descriptor ()
  "Pure public-data adapter contract for Moonlab-compatible state vectors.

It names no executable path and grants no native authority.  A host adapter
may source the tensor from Moonlab's documented public ABI or any equivalent
producer; the semantic renderer consumes only the copied state data."
  (let ((state (%quantum-state-type))
        (frame (%quantum-frame-type)))
    (make-media-component-descriptor
     :name "org.rosette.media/quantum-microscope"
     :version "0.1.0"
     :imports nil
     :exports
     (list (wire:make-port
            "visual"
            (list (wire:make-component-operation
                   "render"
                   (list (%field "state" state)) frame))))
     :effects '(:pure)
     :capabilities nil
     :adapter '(("inputContract" . "moonlab.public.state-vector/1")
                ("renderer" . "rosette.semantic-scene/1"))
     :verifiers '("rosette.media/scene-replay-v1"))))

(defun make-quantum-microscope-composition ()
  "Build the one-step, capability-free quantum microscope Composition."
  (let* ((descriptor (quantum-microscope-descriptor))
         (implementation-id
           (wire:canonical-id
            '(("implementation" . "rosette.media/quantum-microscope-cl/1"))))
         (dependency-set-id
           (wire:canonical-id
            '(("profile" . "rosette.media/1")
              ("scene" . "scene-ir/1")
              ("wire" . "composition/1"))))
         (node (wire:make-component-node
                "microscope" descriptor implementation-id
                :dependency-set-id dependency-set-id))
         (step (wire:make-wire-step
                :id "render" :node-id "microscope" :port "visual"
                :operation "render"
                :bindings (list (wire:make-data-binding
                                 "state" (wire:input-source "state"))))))
    (wire:make-composition
     :name "org.rosette.media/quantum-microscope-demo"
     :nodes (list node) :services nil :steps (list step)
     :inputs (list (%field "state" (%quantum-state-type)))
     :outputs (list (wire:make-wire-output "visual" "render"))
     :capability-grants nil
     :required-evidence '("rosette.media/scene-replay-v1")
     :limits '(("maxOutputBytes" . 1048576) ("maxSteps" . 1)))))

(defun %finite-real-p (value)
  (and (realp value)
       (handler-case
           (let ((number (coerce value 'double-float)))
             (and (= number number)
                  (<= (abs number) most-positive-double-float)))
         (error () nil))))

(defun %amplitude-pair (value index)
  (unless (and (listp value) (= (length value) 2)
               (every #'%finite-real-p value))
    (%media-error :invalid-amplitude
                  (format nil "$.state[~D]" index)
                  "expected finite (real imaginary), got ~S" value))
  (mapcar (lambda (part) (coerce part 'double-float)) value))

(defun %phase-color (real imaginary)
  ;; Four exact, renderer-independent phase sectors.  This intentionally
  ;; avoids platform-sensitive hue arithmetic in the semantic object.
  (cond ((>= real (abs imaginary)) '(52 211 255))
        ((>= imaginary (abs real)) '(213 92 255))
        ((<= real (- (abs imaginary))) '(255 166 64))
        (t '(88 232 150))))

(defun %draw (operation) (list :draw operation))

(defun make-quantum-field-scene (amplitudes &key (width 640) (height 360))
  "Map copied public state-vector data to one canonical semantic scene.

Bar area is Born probability; the phase-sector colour and radial marker retain
sign/phase information.  The result is ordinary SCENE-IR and therefore has a
CPU oracle independent of SVG or browser presentation."
  (unless (and (integerp width) (>= width 160)
               (integerp height) (>= height 120))
    (%media-error :invalid-extent "$.scene.extent"
                  "expected integer width >=160 and height >=120"))
  (unless (and (listp amplitudes) (plusp (length amplitudes))
               (<= (length amplitudes) 256))
    (%media-error :invalid-state "$.state"
                  "expected 1..256 amplitude pairs"))
  (let* ((state (loop for amplitude in amplitudes for index from 0
                      collect (%amplitude-pair amplitude index)))
         (weights (mapcar (lambda (pair)
                            (+ (* (first pair) (first pair))
                               (* (second pair) (second pair))))
                          state))
         (total (reduce #'+ weights))
         (count (length state)))
    (unless (plusp total)
      (%media-error :zero-state "$.state" "state norm is zero"))
    (let* ((left 22) (right 18) (top 34) (bottom 30)
           (chart-width (- width left right))
           (chart-height (- height top bottom))
           (cell-width (max 1 (floor chart-width count)))
           (bar-width (max 1 (- cell-width 3)))
           (base (+ top chart-height))
           ;; NODES is accumulated with PUSH and reversed once at the end.
           (nodes (list (%draw (list :line left base (- width right) base
                                     92 112 158 1 1000))
                        (%draw (list :text 14 18 1000 210 224 255 1
                                     "ROSETTE QUANTUM MICROSCOPE"))
                        (%draw '(:clear 7 10 22)))))
      (loop for pair in state
            for probability in weights
            for index from 0
            for x = (+ left (* index cell-width))
            for bar-height = (max 1 (round (* chart-height (/ probability total))))
            for y = (- base bar-height)
            for color = (%phase-color (first pair) (second pair))
            do (push (%draw (list* :fill-rect x y bar-width bar-height
                                   (append color '(920)))) nodes)
               (push (%draw (list :line (+ x (floor bar-width 2)) base
                                  (+ x (floor bar-width 2)) y
                                  236 244 255 1 850)) nodes)
               (when (<= count 16)
                 (push (%draw (list :text x (+ base 15) 700
                                    150 167 205 1 (format nil "~D" index)))
                       nodes)))
      (scene:parse-scene
       (list :scene (list :size width height)
             (cons :group (nreverse nodes)))))))

(defun semantic-scene-id (semantic-scene)
  "Content identity of the canonical renderer-independent scene value."
  (wire:canonical-id (scene:unparse-scene
                      (scene:parse-scene semantic-scene))))

(defun render-semantic-scene (semantic-scene)
  "Render through the deterministic CPU scene oracle."
  (scene:render-scene (scene:parse-scene semantic-scene)))

(defun semantic-scene-fingerprint (semantic-scene)
  (scene:frame-fingerprint (render-semantic-scene semantic-scene)))

(defun %xml-escape (value)
  (with-output-to-string (out)
    (loop for character across value do
      (case character
        (#\& (write-string "&amp;" out))
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\" (write-string "&quot;" out))
        (#\' (write-string "&apos;" out))
        (t (write-char character out))))))

(defun %svg-color (r g b)
  (format nil "rgb(~D,~D,~D)" r g b))

(defun %svg-opacity (alpha)
  ;; Exact decimal with no implementation-dependent float printer.
  (format nil "~D.~3,'0D" (floor alpha 1000) (mod alpha 1000)))

(defun scene->svg (semantic-scene)
  "Project the immediate-mode semantic scene to deterministic standalone SVG."
  (let ((parsed (scene:parse-scene semantic-scene)))
    (multiple-value-bind (width height) (scene:scene-size parsed)
      (with-output-to-string (out)
        (format out "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ~D ~D\" width=\"~D\" height=\"~D\">" width height width height)
        (dolist (operation (scene:flatten-scene parsed))
          (case (first operation)
            (:clear
             (destructuring-bind (_ r g b) operation
               (declare (ignore _))
               (format out "<rect x=\"0\" y=\"0\" width=\"~D\" height=\"~D\" fill=\"~A\"/>"
                       width height (%svg-color r g b))))
            (:fill-rect
             (destructuring-bind (_ x y w h r g b alpha) operation
               (declare (ignore _))
               (format out "<rect x=\"~D\" y=\"~D\" width=\"~D\" height=\"~D\" fill=\"~A\" fill-opacity=\"~A\"/>"
                       x y w h (%svg-color r g b) (%svg-opacity alpha))))
            (:line
             (destructuring-bind (_ x0 y0 x1 y1 r g b thickness alpha) operation
               (declare (ignore _))
               (format out "<line x1=\"~D\" y1=\"~D\" x2=\"~D\" y2=\"~D\" stroke=\"~A\" stroke-opacity=\"~A\" stroke-width=\"~D\"/>"
                       x0 y0 x1 y1 (%svg-color r g b)
                       (%svg-opacity alpha) thickness)))
            (:text
             (destructuring-bind (_ x y scale r g b thickness string) operation
               (declare (ignore _ thickness))
               (format out "<text x=\"~D\" y=\"~D\" fill=\"~A\" font-family=\"monospace\" font-size=\"~D\">~A</text>"
                       x y (%svg-color r g b) (max 1 (round (* 8 scale) 1000))
                       (%xml-escape string))))))
        (write-string "</svg>" out)))))

(defun scene->canvas-commands (semantic-scene)
  "Lower a scene to provider-free Canvas-like commands, not browser calls.

The result remains inert data.  A browser adapter may execute these commands
only after Rosette has validated the containing Composition."
  (let ((parsed (scene:parse-scene semantic-scene)))
    (multiple-value-bind (width height) (scene:scene-size parsed)
      `(("schema" . "rosette.canvas-commands/1")
        ("width" . ,width)
        ("height" . ,height)
        ("operations" . ,(mapcar (lambda (operation)
                                    (coerce (copy-list operation) 'vector))
                                  (scene:flatten-scene parsed)))))))

(defun scene->webgl-batches (semantic-scene)
  "Lower the 2D immediate scene subset to inert WebGL-friendly batch data.

Filled rectangles become two triangles; lines remain explicit line segments.
Text stays in a separate overlay list because glyph expansion belongs to the
browser adapter.  This function performs no GPU or DOM action."
  (let ((parsed (scene:parse-scene semantic-scene))
        (clear '(0 0 0)) (triangles nil) (lines nil) (text nil))
    (dolist (operation (scene:flatten-scene parsed))
      (case (first operation)
        (:clear (setf clear (copy-list (rest operation))))
        (:fill-rect
         (destructuring-bind (_ x y width height r g b alpha) operation
           (declare (ignore _))
           (let ((x2 (+ x width)) (y2 (+ y height))
                 (color (list r g b alpha)))
             (push `(("color" . ,color)
                     ("vertices" . (,x ,y ,x2 ,y ,x2 ,y2
                                     ,x ,y ,x2 ,y2 ,x ,y2)))
                   triangles))))
        (:line
         (destructuring-bind (_ x0 y0 x1 y1 r g b thickness alpha) operation
           (declare (ignore _))
           (push `(("color" . (,r ,g ,b ,alpha))
                   ("thickness" . ,thickness)
                   ("vertices" . (,x0 ,y0 ,x1 ,y1)))
                 lines)))
        (:text (push (copy-list operation) text))))
    (multiple-value-bind (width height) (scene:scene-size parsed)
      `(("clear" . ,clear)
        ("height" . ,height)
        ("lines" . ,(nreverse lines))
        ("schema" . "rosette.webgl-batches/1")
        ("textOverlay" . ,(nreverse text))
        ("triangles" . ,(nreverse triangles))
        ("width" . ,width)))))

(defun %octets (&rest values)
  (make-array (length values) :element-type '(unsigned-byte 8)
              :initial-contents values))

(defun %append-octets (&rest vectors)
  (let* ((length (reduce #'+ vectors :key #'length :initial-value 0))
         (result (make-array length :element-type '(unsigned-byte 8)))
         (offset 0))
    (dolist (vector vectors result)
      (replace result vector :start1 offset)
      (incf offset (length vector)))))

(defun %u32be (integer)
  (%octets (ldb (byte 8 24) integer) (ldb (byte 8 16) integer)
           (ldb (byte 8 8) integer) (ldb (byte 8 0) integer)))

(defun %ascii-octets (string)
  (let ((result (make-array (length string) :element-type '(unsigned-byte 8))))
    (loop for character across string for index from 0
          for code = (char-code character)
          do (unless (< code 128)
               (%media-error :non-ascii-png-chunk "$.png.chunk"
                             "chunk type must be ASCII"))
             (setf (aref result index) code))
    result))

(defun %png-chunk (type payload)
  (unless (= 4 (length type))
    (%media-error :invalid-png-chunk "$.png.chunk" "type must have 4 bytes"))
  (let* ((type-bytes (%ascii-octets type))
         (body (%append-octets type-bytes payload)))
    (%append-octets (%u32be (length payload)) body
                    (%u32be (net:crc32 body)))))

(defun %png-scanlines (frame)
  (let* ((width (raster:frame-width frame))
         (height (raster:frame-height frame))
         (pixels (raster:frame-pixels frame))
         (stride (* 3 width))
         (result (make-array (* height (1+ stride))
                             :element-type '(unsigned-byte 8))))
    (dotimes (row height result)
      (let ((destination (* row (1+ stride)))
            (source (* row stride)))
        (setf (aref result destination) 0)
        (replace result pixels :start1 (1+ destination)
                               :start2 source :end2 (+ source stride))))))

(defun frame->png-bytes (frame)
  "Encode an RGB frame as deterministic PNG using verified substrate codecs."
  (let* ((width (raster:frame-width frame))
         (height (raster:frame-height frame))
         (ihdr (%append-octets (%u32be width) (%u32be height)
                               ;; 8-bit, truecolor, deflate, filter, no interlace
                               (%octets 8 2 0 0 0)))
         (scanlines (%png-scanlines frame))
         (compressed (codec:zlib-compress scanlines :strategy :fixed)))
    (%append-octets
     (%octets 137 80 78 71 13 10 26 10)
     (%png-chunk "IHDR" ihdr)
     (%png-chunk "IDAT" compressed)
     (%png-chunk "IEND" (%octets)))))

(defun scene->png-bytes (semantic-scene)
  "Render through the CPU oracle and encode the exact frame as PNG bytes."
  (frame->png-bytes (render-semantic-scene semantic-scene)))

(defun %quantum-frame-value (state-value)
  (let* ((shape (cdr (assoc "shape" state-value :test #'string=)))
         (amplitudes (cdr (assoc "data" state-value :test #'string=))))
    (unless (and (listp shape) (= (length shape) 2)
                 (= (second shape) 2) (= (first shape) (length amplitudes)))
      (%media-error :invalid-state-shape "$.state.shape"
                    "expected [N,2] matching the data length"))
    (let* ((semantic-scene (make-quantum-field-scene amplitudes))
           (scene-value (scene:unparse-scene semantic-scene))
           (scene-json (wire:canonical-json scene-value))
           (svg (scene->svg semantic-scene))
           (fingerprint (format nil "~16,'0x"
                                (semantic-scene-fingerprint semantic-scene))))
      `(("clock" . (("domain" . (("simulation" . t)))
                     ("epoch" . "composition")
                     ("rateDenominator" . 1)
                     ("rateNumerator" . 1)))
        ("durationTicks" . 1)
        ("format" . "application/vnd.rosette-wire.semantic-scene+json")
        ("height" . 360)
        ("payload" . (("cpuFingerprint" . ,fingerprint)
                       ("scene" . ,scene-json)
                       ("sceneId" . ,(semantic-scene-id semantic-scene))
                       ("svg" . ,svg)))
        ("presentationTick" . 0)
        ("sequence" . 0)
        ("width" . 640)))))

(defparameter +scene-keywords+
  '("scene" "size" "group" "transform" "translate" "scale" "rotate"
    "mat" "clip" "rect" "opacity" "scroll" "offset" "layer" "draw"
    "clear" "line" "fill-rect" "text" "blit"))

(defun %json-scene-form (value)
  (if (listp value)
      (let ((items (mapcar #'%json-scene-form value)))
        (if (and items (stringp (first items))
                 (member (first items) +scene-keywords+ :test #'string=))
            (cons (intern (string-upcase (first items)) :keyword) (rest items))
            items))
      value))

(defun %quantum-replay-verifier (composition receipt)
  (declare (ignore composition))
  (let* ((outputs (wire:wire-receipt-outputs receipt))
         (frame (cdr (assoc "visual" outputs :test #'string=)))
         (payload (and frame (cdr (assoc "payload" frame :test #'string=))))
         (scene-json (and payload (cdr (assoc "scene" payload :test #'string=))))
         (claimed-id (and payload (cdr (assoc "sceneId" payload :test #'string=))))
         (claimed-svg (and payload (cdr (assoc "svg" payload :test #'string=))))
         (claimed-fingerprint
           (and payload (cdr (assoc "cpuFingerprint" payload :test #'string=)))))
    (if (not (every #'identity
                    (list frame payload scene-json claimed-id claimed-svg
                          claimed-fingerprint)))
        (values nil '(("reason" . "missing-scene-evidence")))
        (let* ((semantic-scene
                 (scene:parse-scene
                  (%json-scene-form (json:json-parse scene-json))))
               (actual-id (semantic-scene-id semantic-scene))
               (actual-svg (scene->svg semantic-scene))
               (actual-fingerprint
                 (format nil "~16,'0x"
                         (semantic-scene-fingerprint semantic-scene)))
               (pass (and (string= claimed-id actual-id)
                          (string= claimed-svg actual-svg)
                          (string= claimed-fingerprint actual-fingerprint))))
          (values pass
                  `(("cpuFingerprint" . ,actual-fingerprint)
                    ("sceneId" . ,actual-id)
                    ("svgId" . ,(wire:canonical-id actual-svg))))))))

(defun make-quantum-microscope-runner ()
  "Create an explicitly stateful host runner with the pure adapter and verifier."
  (let ((runner (wire:make-composition-runner)))
    (wire:register-component-handler
     runner "microscope" "visual" "render"
     (lambda (arguments services context)
       (declare (ignore services context))
       (%quantum-frame-value
        (cdr (assoc "state" arguments :test #'string=)))))
    (wire:register-component-verifier
     runner "rosette.media/scene-replay-v1" #'%quantum-replay-verifier)
    runner))

(defun run-quantum-microscope (state-value)
  "Execute and independently verify the flagship Composition.

Returns the execution receipt and verification receipt as two values."
  (let ((composition (make-quantum-microscope-composition))
        (runner (make-quantum-microscope-runner)))
    (let ((execution
            (wire:run-composition composition runner
                                  (list (cons "state" state-value)))))
      (values execution
              (wire:verify-composition-receipt composition execution runner)))))
