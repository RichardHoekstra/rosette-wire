;;;; media-profile/src/package.lisp --- Public API.
;;;;
;;;; Rosette's typed temporal media profile, renderer-independent semantic scene contract, deterministic SVG/CPU projections, and first public-data quantum microscope vertical.

(defpackage #:rosette-media
  (:nicknames #:media-profile)
  (:use #:cl)
  (:local-nicknames (#:wire #:rosette-wire)
                    (#:scene #:rosette-scene-ir)
                    (#:json #:rosette-json)
                    (#:raster #:rosette-raster-core)
                    (#:codec #:rosette-compression-codec)
                    (#:net #:rosette-net-frame))
  (:export
   #:+media-profile-version+
   #:media-profile-error #:media-profile-error-code
   #:media-profile-error-path #:media-profile-error-detail
   #:media-clock-type #:media-signal-type #:media-event-type
   #:media-frame-type #:media-stream-type #:media-resource-type
   #:make-media-port #:make-media-component-descriptor
   #:quantum-microscope-descriptor
   #:make-quantum-microscope-composition
   #:make-quantum-microscope-runner
   #:run-quantum-microscope
   #:make-quantum-field-scene #:semantic-scene-id
   #:render-semantic-scene #:semantic-scene-fingerprint
   #:scene->svg #:scene->canvas-commands #:scene->webgl-batches
   #:frame->png-bytes #:scene->png-bytes
   #:make-eshkol-debug-scene #:make-eshkol-debug-frame
   #:make-dendritic-snapshot #:make-dendritic-garden-scene
   #:make-dendritic-garden-frame
   #:make-eshkol-dsp-request #:verify-eshkol-dsp-output
   #:make-browser-audio-buffer
   #:make-eshkol-diagnostic-sonification-request))
