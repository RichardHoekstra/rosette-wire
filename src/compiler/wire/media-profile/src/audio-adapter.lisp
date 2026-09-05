;;;; audio-adapter.lisp --- Eshkol DSP request + explicit browser playback data.

(in-package #:rosette-media)

(defun %finite-samples (values path &key (limit 262144))
  (unless (and (listp values) (plusp (length values)) (<= (length values) limit)
               (every #'%finite-real-p values))
    (%media-error :invalid-audio path "expected 1..~D finite samples" limit))
  (mapcar (lambda (value) (coerce value 'double-float)) values))

(defun %fir-oracle (coefficients samples)
  (loop for index below (length samples) collect
    (loop for coefficient in coefficients for tap from 0
          when (>= index tap)
            sum (* coefficient (nth (- index tap) samples)) into value
          finally (return (coerce value 'double-float)))))

(defun %eshkol-number (value)
  (let ((text (format nil "~,17E" (coerce value 'double-float))))
    (substitute #\e #\D text :test #'char-equal)))

(defun %eshkol-vector-source (values)
  (format nil "(vector~{ ~A~})" (mapcar #'%eshkol-number values)))

(defun make-eshkol-dsp-request (samples coefficients &key (sample-rate 48000))
  "Build a public, replayable Eshkol `signal.filters` FIR request.

The request contains source and an independent Rosette oracle. A private host
may run SOURCE with any Eshkol installation and return copied samples; no
command, path, FFI handle, or upstream dependency enters the artifact."
  (let ((input (%finite-samples samples "$.audio.samples"))
        (kernel (%finite-samples coefficients "$.audio.coefficients"
                                 :limit 4096)))
    (unless (and (integerp sample-rate) (<= 8000 sample-rate 384000))
      (%media-error :invalid-audio "$.audio.sampleRate"
                    "expected 8000..384000 Hz"))
    (let* ((expected (%fir-oracle kernel input))
           (source (format nil
                           "(require signal.filters)~%(define input ~A)~%(define coefficients ~A)~%(display (fir-filter coefficients input))~%(newline)~%"
                           (%eshkol-vector-source input)
                           (%eshkol-vector-source kernel))))
      `(("authority" . "none")
        ("expectedSamples" . ,expected)
        ("inputSamples" . ,input)
        ("kernel" . ,kernel)
        ("module" . "signal.filters")
        ("sampleRate" . ,sample-rate)
        ("schema" . "rosette.eshkol-dsp-request/1")
        ("source" . ,source)
        ("sourceId" . ,(wire:canonical-id source))))))

(defun verify-eshkol-dsp-output (request observed-samples &key (tolerance 1d-12))
  "Check copied Eshkol samples against the independent FIR oracle."
  (unless (and (listp request)
               (string= "rosette.eshkol-dsp-request/1"
                        (or (cdr (assoc "schema" request :test #'string=)) "")))
    (%media-error :invalid-audio "$.audio.request" "unknown request schema"))
  (unless (and (%finite-real-p tolerance) (not (minusp tolerance)))
    (%media-error :invalid-audio "$.audio.tolerance"
                  "expected a finite nonnegative tolerance"))
  (let ((observed (%finite-samples observed-samples "$.audio.observed"))
        (expected (cdr (assoc "expectedSamples" request :test #'string=))))
    (and (= (length observed) (length expected))
         (loop for actual in observed for oracle in expected
               always (<= (abs (- actual oracle)) tolerance)))))

(defun make-browser-audio-buffer (samples sample-rate &key (channels 1))
  "Create inert Web Audio adapter data; playback always requires host action."
  (let ((pcm (%finite-samples samples "$.browserAudio.samples")))
    (unless (and (integerp sample-rate) (<= 8000 sample-rate 384000)
                 (integerp channels) (= channels 1))
      (%media-error :invalid-audio "$.browserAudio"
                    "v1 requires mono audio at 8000..384000 Hz"))
    (unless (every (lambda (sample) (<= -1d0 sample 1d0)) pcm)
      (%media-error :invalid-audio "$.browserAudio.samples"
                    "browser PCM must be normalized to [-1,1]"))
    (let ((pcm16 (mapcar (lambda (sample)
                           (round (* 32767d0 (max -1d0 (min 1d0 sample)))))
                         pcm)))
      `(("activation" . "user-gesture-required")
      ("authority" . "none")
      ("channels" . ,channels)
      ("format" . "s16-planar")
      ("frames" . ,(length pcm))
      ("sampleRate" . ,sample-rate)
      ("samples" . (,pcm16))
      ("schema" . "rosette.web-audio-buffer/1")))))

(defun make-eshkol-diagnostic-sonification-request (bundle-form
                                                     &key (sample-rate 48000))
  "Map the four public execution floors to an impulse train for Eshkol FIR DSP."
  (let* ((content (%diagnostic-content bundle-form))
         (observations (or (getf content :observations) nil))
         (direct (getf observations :direct))
         (floors '(:direct :kernel-vm :eshkol-jit :eshkol-aot))
         (samples
           (loop for floor in floors append
             (let* ((value (getf observations floor))
                    (amplitude (cond ((null value) 0d0)
                                     ((or (eq floor :direct) (eql value direct))
                                      0.65d0)
                                     (t -0.9d0))))
               (cons amplitude (make-list 63 :initial-element 0d0))))))
    (make-eshkol-dsp-request samples '(0.25d0 0.5d0 0.25d0)
                             :sample-rate sample-rate)))
