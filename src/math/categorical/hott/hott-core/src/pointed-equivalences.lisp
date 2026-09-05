;;;; pointed-equivalences.lisp --- pointed equivalence vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defstruct (pointed-equivalence
            (:constructor %make-pointed-equivalence
                (source target equivalence forward-map backward-map)))
  "An equivalence between pointed types with basepoint-preserving maps."
  (source nil :type pointed-type :read-only t)
  (target nil :type pointed-type :read-only t)
  (equivalence nil :type hott-equivalence :read-only t)
  (forward-map nil :type pointed-map :read-only t)
  (backward-map nil :type pointed-map :read-only t))

(defun make-pointed-equivalence (source target equivalence
                                 &key forward-basepoint-path)
  "Construct a checked pointed equivalence SOURCE ~= TARGET.

FORWARD-BASEPOINT-PATH witnesses f(source.basepoint)=target.basepoint.
The backward basepoint path is derived by applying the inverse map to that
path and composing with the equivalence's left homotopy."
  (unless (and (eq (pointed-type-type source)
                   (hott-equivalence-source equivalence))
               (eq (pointed-type-type target)
                   (hott-equivalence-target equivalence)))
    (error "Pointed equivalence type endpoints do not match equivalence."))
  (let* ((source-base (pointed-type-basepoint source))
         (target-base (pointed-type-basepoint target))
         (forward-map
           (make-pointed-map
            source target
            (lambda (value) (equiv-forward equivalence value))
            :basepoint-path forward-basepoint-path))
         (forward-path (pointed-map-basepoint-path forward-map))
         (backward-image-path
           (path-ap (lambda (value) (equiv-backward equivalence value))
                    (pointed-type-type source)
                    forward-path))
         (backward-path
           (path-compose (path-inverse backward-image-path)
                         (equiv-path equivalence :left source-base)))
         (backward-map
           (make-pointed-map
            target source
            (lambda (value) (equiv-backward equivalence value))
            :basepoint-path backward-path)))
    (declare (ignore target-base))
    (%make-pointed-equivalence source target equivalence
                               forward-map backward-map)))

(defun inverse-pointed-equivalence (pointed-equivalence)
  "Return POINTED-EQUIVALENCE with source and target swapped."
  (%make-pointed-equivalence
   (pointed-equivalence-target pointed-equivalence)
   (pointed-equivalence-source pointed-equivalence)
   (inverse-equivalence
    (pointed-equivalence-equivalence pointed-equivalence))
   (pointed-equivalence-backward-map pointed-equivalence)
   (pointed-equivalence-forward-map pointed-equivalence)))

(defun compose-pointed-equivalences (left right)
  "Compose LEFT : A ~=* B with RIGHT : B ~=* C."
  (unless (eq (pointed-equivalence-target left)
              (pointed-equivalence-source right))
    (error "Pointed equivalence endpoints do not meet."))
  (let* ((left-forward (pointed-equivalence-forward-map left))
         (right-forward (pointed-equivalence-forward-map right))
         (composed-equivalence
           (compose-equivalences
            (pointed-equivalence-equivalence left)
            (pointed-equivalence-equivalence right)))
         (mapped-left-path
           (path-ap (pointed-map-function right-forward)
                    (pointed-type-type
                     (pointed-equivalence-target right))
                    (pointed-map-basepoint-path left-forward)))
         (basepoint-path
           (path-compose mapped-left-path
                         (pointed-map-basepoint-path right-forward))))
    (make-pointed-equivalence
     (pointed-equivalence-source left)
     (pointed-equivalence-target right)
     composed-equivalence
     :forward-basepoint-path basepoint-path)))

(defun pointed-equivalence-loop-map (pointed-equivalence loop)
  "Map a based LOOP across POINTED-EQUIVALENCE's forward pointed map."
  (pointed-map-loop (pointed-equivalence-forward-map pointed-equivalence)
                    loop))
