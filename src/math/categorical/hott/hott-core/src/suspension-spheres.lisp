;;;; suspension-spheres.lisp --- suspension-spheres vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defstruct (suspension-type
            (:constructor %make-suspension-type (base hit)))
  "The suspension HIT of BASE with NORTH, SOUTH, and MERIDIAN constructors."
  (base nil :type hott-type :read-only t)
  (hit nil :type higher-inductive-type :read-only t))

(defun suspension-type (base &key name)
  "Return the suspension HIT Susp(BASE).

The point constructors are NORTH and SOUTH.  For each x : BASE, MERIDIAN(x)
is a path NORTH = SOUTH."
  (let ((hit nil))
    (setf hit
          (make-higher-inductive-type
           (or name (list :suspension (hott-type-name base)))
           '(:north :south)
           `((:meridian . (:from ,(lambda (x)
                                    (declare (ignore x))
                                    (make-hit-value hit :north))
                          :to ,(lambda (x)
                                 (declare (ignore x))
                                 (make-hit-value hit :south)))))))
    (%make-suspension-type base hit)))

(defun suspension-hit (suspension)
  "Return the underlying HIT of SUSPENSION."
  (suspension-type-hit suspension))

(defun suspension-base (suspension)
  "Return the suspended base type of SUSPENSION."
  (suspension-type-base suspension))

(defun suspension-north (suspension)
  "Return the north point of SUSPENSION."
  (make-hit-value (suspension-hit suspension) :north))

(defun suspension-south (suspension)
  "Return the south point of SUSPENSION."
  (make-hit-value (suspension-hit suspension) :south))

(defun suspension-meridian (suspension value)
  "Return the meridian path NORTH = SOUTH indexed by VALUE : BASE."
  (ensure-in-type
   (suspension-base suspension)
   value
   "Suspension meridian index ~S is not in base type ~S."
   value
   (hott-type-name (suspension-base suspension)))
  (hit-path (suspension-hit suspension) :meridian value))

(defun suspension-recursion
    (suspension target north-image south-image meridian-action)
  "Return the non-dependent suspension recursor into TARGET.

MERIDIAN-ACTION is called on each base value and must return a path from
NORTH-IMAGE to SOUTH-IMAGE in TARGET."
  (unless (and (in-type-p target north-image)
               (in-type-p target south-image))
    (error "Suspension point images must inhabit target ~S."
           (hott-type-name target)))
  (make-hit-eliminator
   (suspension-hit suspension)
   target
   `((:north . ,(lambda () north-image))
     (:south . ,(lambda () south-image)))
   `((:meridian . ,(lambda (value)
                     (ensure-in-type
                      (suspension-base suspension)
                      value
                      "Suspension recursor index ~S is not in base ~S."
                      value
                      (hott-type-name (suspension-base suspension)))
                     (let ((path (funcall meridian-action value)))
                       (unless (and (typep path 'hott-path)
                                    (eq (hott-path-type path) target)
                                    (equal (hott-path-from path) north-image)
                                     (equal (hott-path-to path) south-image))
                         (error "Suspension meridian image is not coherent."))
                       path))))))

(defstruct (pointed-suspension
            (:constructor %make-pointed-suspension
                (pointed suspension pointed-type)))
  "The reduced pointed suspension of a pointed type, based at NORTH."
  (pointed nil :type pointed-type :read-only t)
  (suspension nil :type suspension-type :read-only t)
  (pointed-type nil :type pointed-type :read-only t))

(defun make-pointed-suspension (pointed &key name)
  "Construct the pointed suspension Susp(POINTED), based at NORTH."
  (let* ((suspension
           (suspension-type
            (pointed-type-type pointed)
            :name (or name (list :pointed-suspension
                                 (hott-type-name
                                  (pointed-type-type pointed))))))
         (susp-pointed
           (make-pointed-type
            (higher-inductive-type-base-type
             (suspension-hit suspension))
            (suspension-north suspension))))
    (%make-pointed-suspension pointed suspension susp-pointed)))

(defun pointed-suspension-type (pointed-suspension)
  "Return POINTED-SUSPENSION's underlying carrier type."
  (pointed-type-type (pointed-suspension-pointed-type pointed-suspension)))

(defun pointed-suspension-basepoint (pointed-suspension)
  "Return POINTED-SUSPENSION's basepoint, the north pole."
  (pointed-type-basepoint
   (pointed-suspension-pointed-type pointed-suspension)))

(defun pointed-suspension-north (pointed-suspension)
  "Return POINTED-SUSPENSION's north point."
  (suspension-north (pointed-suspension-suspension pointed-suspension)))

(defun pointed-suspension-south (pointed-suspension)
  "Return POINTED-SUSPENSION's south point."
  (suspension-south (pointed-suspension-suspension pointed-suspension)))

(defun pointed-suspension-meridian (pointed-suspension value)
  "Return POINTED-SUSPENSION's meridian indexed by VALUE."
  (suspension-meridian
   (pointed-suspension-suspension pointed-suspension)
   value))

(defstruct (pointed-suspension-map
            (:constructor %make-pointed-suspension-map
                (base-map source target pointed-map)))
  "The suspension functor applied to a pointed map."
  (base-map nil :type pointed-map :read-only t)
  (source nil :type pointed-suspension :read-only t)
  (target nil :type pointed-suspension :read-only t)
  (pointed-map nil :type pointed-map :read-only t))

(defun make-pointed-suspension-map (base-map &key source target)
  "Lift BASE-MAP : X -> Y to Susp(X) -> Susp(Y)."
  (let* ((source-susp (or source
                          (make-pointed-suspension
                           (pointed-map-source base-map))))
         (target-susp (or target
                          (make-pointed-suspension
                           (pointed-map-target base-map))))
         (source-type (pointed-suspension-type source-susp))
         (target-type (pointed-suspension-type target-susp))
         (map-fn
           (lambda (value)
             (unless (and (typep value 'hit-value)
                          (eq (hit-value-type value)
                              (suspension-hit
                               (pointed-suspension-suspension source-susp))))
               (error "Suspension map expected a source suspension value."))
             (ecase (hit-value-constructor value)
               (:north (pointed-suspension-north target-susp))
               (:south (pointed-suspension-south target-susp)))))
         (pointed
           (make-pointed-map
            (pointed-suspension-pointed-type source-susp)
            (pointed-suspension-pointed-type target-susp)
            map-fn
            :basepoint-path
            (refl target-type (pointed-suspension-basepoint target-susp)))))
    (declare (ignore source-type))
    (%make-pointed-suspension-map base-map source-susp target-susp pointed)))

(defun pointed-suspension-map-meridian (suspension-map value)
  "Return the target meridian image of VALUE under SUSPENSION-MAP."
  (let* ((base-map (pointed-suspension-map-base-map suspension-map))
         (source-base (pointed-type-type (pointed-map-source base-map)))
         (target-susp (pointed-suspension-map-target suspension-map)))
    (ensure-in-type
     source-base
     value
     "Suspension map meridian index ~S is not in source base ~S."
     value
     (hott-type-name source-base))
    (pointed-suspension-meridian
     target-susp
     (funcall (pointed-map-function base-map) value))))

(defstruct (pointed-suspension-recursion
            (:constructor %make-pointed-suspension-recursion
                (source target suspension loop-action recursor pointed-map
                 transpose-map)))
  "A pointed map Susp(X) -> Y together with its transpose X -> Omega(Y)."
  (source nil :type pointed-type :read-only t)
  (target nil :type pointed-type :read-only t)
  (suspension nil :type pointed-suspension :read-only t)
  (loop-action nil :type function :read-only t)
  (recursor nil :type hit-eliminator :read-only t)
  (pointed-map nil :type pointed-map :read-only t)
  (transpose-map nil :type pointed-map :read-only t))

(defun make-pointed-suspension-recursion
    (source target loop-action &key suspension loop-basepoint-path)
  "Build a pointed recursor Susp(SOURCE) -> TARGET from loops in TARGET.

LOOP-ACTION is called on each source value and must return a based loop at the
target basepoint.  The returned object records both directions of the executable
suspension/loop transpose: a pointed map out of the suspension and a pointed
map SOURCE -> Omega(TARGET)."
  (let* ((source-susp (or suspension
                          (make-pointed-suspension source)))
         (target-type (pointed-type-type target))
         (target-base (pointed-type-basepoint target))
         (loop-type (loop-space target))
         (loop-base (loop-refl target))
         (source-base (pointed-type-basepoint source))
         (base-loop (funcall loop-action source-base)))
    (unless (and (typep base-loop 'hott-path)
                 (eq (hott-path-type base-loop) target-type)
                 (equalp (hott-path-from base-loop) target-base)
                 (equalp (hott-path-to base-loop) target-base))
      (error "Pointed suspension loop action must return based target loops."))
    (let* ((loop-pointed (make-pointed-type loop-type loop-base))
           (transpose
             (make-pointed-map
              source
              loop-pointed
              (lambda (value)
                (let ((loop (funcall loop-action value)))
                  (ensure-in-type
                   loop-type
                   loop
                   "Loop action for ~S is not in Omega(target)."
                   value)
                  loop))
              :basepoint-path
              (or loop-basepoint-path
                  (make-hott-path loop-type base-loop loop-base
                                  :witness :suspension-loop-basepoint))))
           (recursor
             (suspension-recursion
              (pointed-suspension-suspension source-susp)
              target-type
              target-base
              target-base
              (lambda (value)
                (funcall (pointed-map-function transpose) value))))
           (pointed
             (make-pointed-map
              (pointed-suspension-pointed-type source-susp)
              target
              (lambda (value)
                (hit-eliminate-value recursor value))
              :basepoint-path (refl target-type target-base))))
      (%make-pointed-suspension-recursion
       source target source-susp loop-action recursor pointed transpose))))

(defun pointed-suspension-recursion-meridian (recursion value)
  "Return the target loop assigned to VALUE by pointed suspension recursion."
  (let ((source-type (pointed-type-type
                      (pointed-suspension-recursion-source recursion))))
    (unless (in-type-p source-type value)
      (error "Suspension recursion meridian index ~S is not in source ~S."
             value (hott-type-name source-type)))
    (hit-eliminate-path
     (pointed-suspension-recursion-recursor recursion)
     :meridian
     value)))

(defun sphere-type (dimension &key name)
  "Return a finite HIT presentation of the DIMENSION-sphere.

S^0 is represented as a two-point HIT.  Higher spheres are iterated
suspensions."
  (unless (and (integerp dimension) (>= dimension 0))
    (error "Sphere dimension must be a non-negative integer: ~S." dimension))
  (labels ((carrier (object)
             (etypecase object
               (higher-inductive-type
                (higher-inductive-type-base-type object))
               (suspension-type
                (higher-inductive-type-base-type
                 (suspension-hit object))))))
    (if (zerop dimension)
        (make-higher-inductive-type (or name :sphere-0) '(:minus :plus) nil)
        (loop with current = (make-higher-inductive-type :sphere-0
                                                         '(:minus :plus)
                                                         nil)
            for i from 1 to dimension
            do (setf current
                     (suspension-type
                      (carrier current)
                      :name (if (= i dimension)
                                (or name (list :sphere dimension))
                                (list :sphere i))))
              finally (return current)))))
