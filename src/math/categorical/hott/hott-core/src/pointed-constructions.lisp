;;;; pointed-constructions.lisp --- pointed-constructions vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defun %unit-type ()
  (make-hott-type :unit :predicate (lambda (x) (eq x :star))
                  :truncation +contractible+))

(defstruct (homotopy-cofiber
            (:constructor %make-homotopy-cofiber
                (source target function pushout)))
  "The cofiber of FUNCTION : SOURCE -> TARGET as a homotopy pushout."
  (source nil :type hott-type :read-only t)
  (target nil :type hott-type :read-only t)
  (function nil :type function :read-only t)
  (pushout nil :type homotopy-pushout :read-only t))

(defun make-homotopy-cofiber (source target function &key name)
  "Construct the homotopy cofiber of FUNCTION : SOURCE -> TARGET.

This is the pushout of TARGET <- SOURCE -> UNIT, so TARGET is included and
the image of SOURCE is glued to the distinguished cofiber basepoint."
  (let ((unit (%unit-type)))
    (%make-homotopy-cofiber
     source target function
     (make-homotopy-pushout
      target unit source
      function
      (lambda (value)
        (declare (ignore value))
        :star)
      :name (or name (list :cofiber (hott-type-name source)
                           (hott-type-name target)))))))

(defun homotopy-cofiber-type (cofiber)
  "Return COFIBER's underlying HIT carrier."
  (homotopy-pushout-type (homotopy-cofiber-pushout cofiber)))

(defun cofiber-include (cofiber value)
  "Include VALUE : TARGET into COFIBER."
  (pushout-left (homotopy-cofiber-pushout cofiber) value))

(defun cofiber-basepoint (cofiber)
  "Return the collapsed UNIT point of COFIBER."
  (pushout-right (homotopy-cofiber-pushout cofiber) :star))

(defun cofiber-glue (cofiber source-value)
  "Return the cofiber glue path include(f x)=basepoint."
  (pushout-glue (homotopy-cofiber-pushout cofiber) source-value))

(defstruct (pointed-cofiber
            (:constructor %make-pointed-cofiber
                (map cofiber pointed-type include-map collapse-path)))
  "The pointed cofiber of a pointed map."
  (map nil :type pointed-map :read-only t)
  (cofiber nil :type homotopy-cofiber :read-only t)
  (pointed-type nil :type pointed-type :read-only t)
  (include-map nil :type pointed-map :read-only t)
  (collapse-path nil :type hott-path :read-only t))

(defun make-pointed-cofiber (map &key name)
  "Construct the pointed cofiber of MAP.

The cofiber basepoint is the collapsed point.  The inclusion of MAP's target
is made pointed by composing the inverse image of MAP's basepoint path with
the cofiber glue at the source basepoint."
  (let* ((source (pointed-map-source map))
         (target (pointed-map-target map))
         (source-type (pointed-type-type source))
         (target-type (pointed-type-type target))
         (source-base (pointed-type-basepoint source))
         (cofiber (make-homotopy-cofiber
                   source-type target-type (pointed-map-function map)
                   :name (or name (list :pointed-cofiber
                                        (hott-type-name source-type)
                                        (hott-type-name target-type)))))
         (cofiber-type (homotopy-cofiber-type cofiber))
         (pointed (make-pointed-type cofiber-type
                                     (cofiber-basepoint cofiber)))
         (included-base-path
           (path-ap (lambda (value)
                      (cofiber-include cofiber value))
                    cofiber-type
                    (pointed-map-basepoint-path map)))
         (collapse
           (path-compose
            (path-inverse included-base-path)
            (cofiber-glue cofiber source-base)))
         (include-map
           (make-pointed-map
            target
            pointed
            (lambda (value)
              (cofiber-include cofiber value))
            :basepoint-path collapse)))
    (%make-pointed-cofiber map cofiber pointed include-map collapse)))

(defun pointed-cofiber-type (pointed-cofiber)
  "Return POINTED-COFIBER's underlying type."
  (pointed-type-type (pointed-cofiber-pointed-type pointed-cofiber)))

(defun pointed-cofiber-basepoint (pointed-cofiber)
  "Return POINTED-COFIBER's basepoint."
  (pointed-type-basepoint (pointed-cofiber-pointed-type pointed-cofiber)))

(defun pointed-cofiber-include (pointed-cofiber value)
  "Include a target value into POINTED-COFIBER."
  (cofiber-include (pointed-cofiber-cofiber pointed-cofiber) value))

(defun pointed-cofiber-glue (pointed-cofiber source-value)
  "Return the cofiber glue path for SOURCE-VALUE."
  (cofiber-glue (pointed-cofiber-cofiber pointed-cofiber) source-value))

(defstruct (pointed-fiber
            (:constructor %make-pointed-fiber
                (map fiber-type pointed-type inclusion-map basepoint)))
  "The pointed homotopy fiber of a pointed map over the target basepoint."
  (map nil :type pointed-map :read-only t)
  (fiber-type nil :type hott-type :read-only t)
  (pointed-type nil :type pointed-type :read-only t)
  (inclusion-map nil :type pointed-map :read-only t)
  (basepoint nil :type hott-fiber-value :read-only t))

(defun make-pointed-fiber (map &key name)
  "Construct the pointed homotopy fiber of MAP over the target basepoint."
  (let* ((source (pointed-map-source map))
         (target (pointed-map-target map))
         (source-type (pointed-type-type source))
         (target-type (pointed-type-type target))
         (source-base (pointed-type-basepoint source))
         (target-base (pointed-type-basepoint target))
         (fiber-type (homotopy-fiber-type
                      source-type target-type
                      (pointed-map-function map)
                      target-base
                      :name (or name (list :pointed-fiber
                                           (hott-type-name source-type)
                                           (hott-type-name target-type)))))
         (fiber-base (make-hott-fiber-value
                      source-type target-type
                      (pointed-map-function map)
                      target-base
                      source-base
                      :image-path (pointed-map-basepoint-path map)))
         (pointed (make-pointed-type fiber-type fiber-base))
         (inclusion
           (make-pointed-map
            pointed
            source
            #'hott-fiber-value-point
            :basepoint-path (refl source-type source-base))))
    (%make-pointed-fiber map fiber-type pointed inclusion fiber-base)))

(defun pointed-fiber-type (pointed-fiber)
  "Return POINTED-FIBER's underlying homotopy fiber type."
  (pointed-type-type (pointed-fiber-pointed-type pointed-fiber)))

(defun pointed-fiber-basepoint-value (pointed-fiber)
  "Return POINTED-FIBER's basepoint fiber value."
  (pointed-type-basepoint (pointed-fiber-pointed-type pointed-fiber)))

(defun pointed-fiber-include (pointed-fiber value)
  "Project a pointed fiber VALUE back into the source type."
  (unless (in-type-p (pointed-fiber-type pointed-fiber) value)
    (error "Pointed fiber include expected a fiber value, got ~S." value))
  (hott-fiber-value-point value))

(defstruct (pointed-nullhomotopy
            (:constructor %make-pointed-nullhomotopy (map homotopy-fn)))
  "A pointed homotopy from MAP to the constant target-basepoint map."
  (map nil :type pointed-map :read-only t)
  (homotopy-fn nil :type function :read-only t))

(defun make-pointed-nullhomotopy (map homotopy-fn)
  "Construct a checked pointed nullhomotopy witness for MAP."
  (%make-pointed-nullhomotopy map homotopy-fn))

(defun pointed-nullhomotopy-path (nullhomotopy value)
  "Return the path MAP(value)=target-basepoint supplied by NULLHOMOTOPY."
  (let* ((map (pointed-nullhomotopy-map nullhomotopy))
         (source-type (pointed-type-type (pointed-map-source map)))
         (target (pointed-map-target map))
         (target-type (pointed-type-type target))
         (image (funcall (pointed-map-function map) value))
         (path (funcall (pointed-nullhomotopy-homotopy-fn nullhomotopy)
                        value)))
    (unless (in-type-p source-type value)
      (error "Nullhomotopy value ~S is not in source type ~S."
             value (hott-type-name source-type)))
    (unless (and (typep path 'hott-path)
                 (eq (hott-path-type path) target-type)
                 (equalp (hott-path-from path) image)
                 (equalp (hott-path-to path)
                         (pointed-type-basepoint target)))
      (error "Nullhomotopy path is not coherent at ~S." value))
    path))

(defstruct (pointed-fiber-sequence
            (:constructor %make-pointed-fiber-sequence
                (map fiber inclusion-map composite-map nullhomotopy)))
  "The canonical pointed fiber sequence Fib(f) -> X -> Y."
  (map nil :type pointed-map :read-only t)
  (fiber nil :type pointed-fiber :read-only t)
  (inclusion-map nil :type pointed-map :read-only t)
  (composite-map nil :type pointed-map :read-only t)
  (nullhomotopy nil :type pointed-nullhomotopy :read-only t))

(defun make-pointed-fiber-sequence (map &key name)
  "Construct the canonical pointed fiber sequence of MAP."
  (declare (ignore name))
  (let* ((fiber (make-pointed-fiber map))
         (fiber-pointed (pointed-fiber-pointed-type fiber))
         (target (pointed-map-target map))
         (map-fn (pointed-map-function map))
         (fiber-base (pointed-fiber-basepoint-value fiber))
         (composite
           (make-pointed-map
            fiber-pointed
            target
            (lambda (fiber-value)
              (funcall map-fn (hott-fiber-value-point fiber-value)))
            :basepoint-path (hott-fiber-value-image-path fiber-base)))
         (nullhomotopy
           (make-pointed-nullhomotopy
            composite
            #'hott-fiber-value-image-path)))
    (%make-pointed-fiber-sequence
     map fiber (pointed-fiber-inclusion-map fiber) composite nullhomotopy)))

