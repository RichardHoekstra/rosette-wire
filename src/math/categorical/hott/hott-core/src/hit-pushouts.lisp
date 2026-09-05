;;;; hit-pushouts.lisp --- hit-pushouts vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defstruct (higher-inductive-type
            (:constructor %make-higher-inductive-type
                (name base-type point-constructors path-constructors)))
  "A small executable descriptor for higher inductive types.

POINT-CONSTRUCTORS names value constructors.  PATH-CONSTRUCTORS is an alist
from constructor name to a plist with :FROM and :TO endpoint functions.  The
endpoint functions receive the path-constructor arguments and must return
inhabitants of BASE-TYPE."
  (name nil :read-only t)
  (base-type nil :type hott-type :read-only t)
  (point-constructors nil :type list :read-only t)
  (path-constructors nil :type list :read-only t))

(defstruct (hit-eliminator
            (:constructor %make-hit-eliminator
                (hit target point-actions path-actions)))
  "A non-dependent eliminator out of a higher inductive type.

POINT-ACTIONS maps point constructors to value functions.  PATH-ACTIONS maps
path constructors to target-path functions.  HIT-ELIMINATE-PATH checks that a
path action's endpoints match the eliminated source endpoints."
  (hit nil :type higher-inductive-type :read-only t)
  (target nil :type hott-type :read-only t)
  (point-actions nil :type list :read-only t)
  (path-actions nil :type list :read-only t))

(defstruct (hit-value
            (:constructor %make-hit-value
                (type constructor arguments)))
  "A value introduced by a higher inductive point constructor."
  (type nil :type higher-inductive-type :read-only t)
  (constructor nil :read-only t)
  (arguments nil :type list :read-only t))

(defun %hit-value-in-type-p (hit value)
  (and (typep value 'hit-value)
       (eq (hit-value-type value) hit)
       (member (hit-value-constructor value)
               (higher-inductive-type-point-constructors hit)
               :test #'equal)))

(defun make-higher-inductive-type (name point-constructors path-constructors
                                   &key (truncation +groupoid+))
  "Construct a higher inductive type descriptor.

This records constructors and checks path endpoints at runtime.  It is a
semantic witness layer for Rosette analyzers, not a full eliminator engine."
  (let* ((hit nil)
         (base (make-hott-type
                name
                :predicate (lambda (value)
                             (%hit-value-in-type-p hit value))
                :truncation truncation)))
    (setf hit (%make-higher-inductive-type
               name base
               (copy-list point-constructors)
               (copy-list path-constructors)))
    hit))

(defun make-hit-value (hit constructor &rest arguments)
  "Construct a value from HIT's point constructor named CONSTRUCTOR."
  (unless (member constructor (higher-inductive-type-point-constructors hit)
                  :test #'equal)
    (error "Unknown HIT point constructor ~S for ~S."
           constructor (higher-inductive-type-name hit)))
  (%make-hit-value hit constructor arguments))

(defun hit-path (hit constructor &rest arguments)
  "Construct a path from HIT's path constructor named CONSTRUCTOR."
  (let ((entry (assoc constructor
                      (higher-inductive-type-path-constructors hit)
                      :test #'equal)))
    (unless entry
      (error "Unknown HIT path constructor ~S for ~S."
             constructor (higher-inductive-type-name hit)))
    (let* ((spec (cdr entry))
           (from-fn (getf spec :from))
           (to-fn (getf spec :to)))
      (unless (and (functionp from-fn) (functionp to-fn))
        (error "HIT path constructor ~S lacks endpoint functions."
               constructor))
      (let ((from (apply from-fn arguments))
            (to (apply to-fn arguments)))
        (make-hott-path (higher-inductive-type-base-type hit)
                        from to
                        :witness (list :hit-path
                                       (higher-inductive-type-name hit)
                                       constructor
                                       arguments))))))

(defun make-hit-eliminator (hit target point-actions path-actions)
  "Construct a checked non-dependent eliminator from HIT into TARGET."
  (loop for constructor in (higher-inductive-type-point-constructors hit)
        do (unless (functionp (cdr (assoc constructor point-actions
                                          :test #'equal)))
             (error "Missing HIT point action for ~S." constructor)))
  (loop for entry in (higher-inductive-type-path-constructors hit)
        for constructor = (car entry)
        do (unless (functionp (cdr (assoc constructor path-actions
                                          :test #'equal)))
             (error "Missing HIT path action for ~S." constructor)))
  (%make-hit-eliminator hit target
                        (copy-list point-actions)
                        (copy-list path-actions)))

(defun hit-eliminate-value (eliminator value)
  "Eliminate a HIT point VALUE through ELIMINATOR."
  (let ((hit (hit-eliminator-hit eliminator)))
    (unless (%hit-value-in-type-p hit value)
      (error "Value ~S is not an inhabitant of HIT ~S."
             value (higher-inductive-type-name hit)))
    (let ((action (cdr (assoc (hit-value-constructor value)
                              (hit-eliminator-point-actions eliminator)
                              :test #'equal))))
      (unless action
        (error "No point action for constructor ~S."
               (hit-value-constructor value)))
      (let ((out (apply action (hit-value-arguments value))))
        (ensure-in-type
         (hit-eliminator-target eliminator)
         out
         "HIT eliminator produced ~S outside target ~S."
         out
         (hott-type-name (hit-eliminator-target eliminator)))
        out))))

(defun hit-eliminate-path (eliminator constructor &rest arguments)
  "Eliminate a HIT path constructor and check endpoint coherence."
  (let* ((hit (hit-eliminator-hit eliminator))
         (source-path (apply #'hit-path hit constructor arguments))
         (action (cdr (assoc constructor
                             (hit-eliminator-path-actions eliminator)
                             :test #'equal))))
    (unless action
      (error "No path action for constructor ~S." constructor))
    (let* ((target (hit-eliminator-target eliminator))
           (path (apply action arguments))
           (from (hit-eliminate-value eliminator
                                      (hott-path-from source-path)))
           (to (hit-eliminate-value eliminator
                                    (hott-path-to source-path))))
      (unless (and (typep path 'hott-path)
                   (eq (hott-path-type path) target)
                   (equal (hott-path-from path) from)
                   (equal (hott-path-to path) to))
        (error "HIT path action ~S is not coherent with endpoints ~S -> ~S."
               constructor from to))
      path)))

(defstruct (homotopy-pushout
            (:constructor %make-homotopy-pushout
                (hit left-type right-type base-type left-map right-map)))
  "A homotopy pushout HIT for maps f : C -> A and g : C -> B."
  (hit nil :type higher-inductive-type :read-only t)
  (left-type nil :type hott-type :read-only t)
  (right-type nil :type hott-type :read-only t)
  (base-type nil :type hott-type :read-only t)
  (left-map nil :type function :read-only t)
  (right-map nil :type function :read-only t))

(defun make-homotopy-pushout
    (left-type right-type base-type left-map right-map &key
       (name :homotopy-pushout)
       (truncation +groupoid+))
  "Construct the homotopy pushout HIT of LEFT-MAP and RIGHT-MAP.

The point constructors are LEFT and RIGHT injections.  The GLUE path
constructor identifies (left (f c)) with (right (g c)) for c in BASE-TYPE."
  (let ((pushout nil)
        (hit nil))
    (setf hit
          (make-higher-inductive-type
           name
           '(:left :right)
           `((:glue . (:from ,(lambda (base-value)
                                (pushout-left
                                 pushout
                                 (funcall left-map base-value)))
                       :to ,(lambda (base-value)
                              (pushout-right
                               pushout
                               (funcall right-map base-value))))))
           :truncation truncation))
    (setf pushout (%make-homotopy-pushout
                   hit left-type right-type base-type left-map right-map))
    pushout))

(defun homotopy-pushout-type (pushout)
  "Return PUSHOUT's underlying HIT carrier type."
  (higher-inductive-type-base-type (homotopy-pushout-hit pushout)))

(defun pushout-left (pushout value)
  "Inject VALUE from the left type into PUSHOUT."
  (ensure-in-type
   (homotopy-pushout-left-type pushout)
   value
   "Pushout left value ~S is not in type ~S."
   value
   (hott-type-name (homotopy-pushout-left-type pushout)))
  (make-hit-value (homotopy-pushout-hit pushout) :left value))

(defun pushout-right (pushout value)
  "Inject VALUE from the right type into PUSHOUT."
  (ensure-in-type
   (homotopy-pushout-right-type pushout)
   value
   "Pushout right value ~S is not in type ~S."
   value
   (hott-type-name (homotopy-pushout-right-type pushout)))
  (make-hit-value (homotopy-pushout-hit pushout) :right value))

(defun pushout-glue (pushout base-value)
  "Return the glue path left(f c)=right(g c) for BASE-VALUE c."
  (ensure-in-type
   (homotopy-pushout-base-type pushout)
   base-value
   "Pushout glue value ~S is not in base type ~S."
   base-value
   (hott-type-name (homotopy-pushout-base-type pushout)))
  (let ((left-image (funcall (homotopy-pushout-left-map pushout) base-value))
        (right-image (funcall (homotopy-pushout-right-map pushout) base-value)))
    (ensure-in-type
     (homotopy-pushout-left-type pushout)
     left-image
     "Pushout left map produced ~S outside left type ~S."
     left-image
     (hott-type-name (homotopy-pushout-left-type pushout)))
    (ensure-in-type
     (homotopy-pushout-right-type pushout)
     right-image
     "Pushout right map produced ~S outside right type ~S."
     right-image
     (hott-type-name (homotopy-pushout-right-type pushout)))
    (hit-path (homotopy-pushout-hit pushout) :glue base-value)))

(defun pushout-recursion
    (pushout target left-action right-action glue-action)
  "Return the non-dependent pushout recursor into TARGET.

LEFT-ACTION maps left-side values to TARGET, RIGHT-ACTION maps right-side
values to TARGET, and GLUE-ACTION maps each base value to a target path from
left-action(f c) to right-action(g c)."
  (make-hit-eliminator
   (homotopy-pushout-hit pushout)
   target
   `((:left . ,(lambda (value)
                 (let ((out (funcall left-action value)))
                   (unless (in-type-p target out)
                     (error "Pushout left action produced ~S outside target ~S."
                            out (hott-type-name target)))
                   out)))
     (:right . ,(lambda (value)
                  (let ((out (funcall right-action value)))
                    (unless (in-type-p target out)
                      (error "Pushout right action produced ~S outside target ~S."
                             out (hott-type-name target)))
                    out))))
   `((:glue . ,(lambda (base-value)
                 (unless (in-type-p (homotopy-pushout-base-type pushout)
                                    base-value)
                   (error "Pushout glue action value ~S is not in base type ~S."
                          base-value
                          (hott-type-name
                           (homotopy-pushout-base-type pushout))))
                 (let* ((left-image
                          (funcall (homotopy-pushout-left-map pushout)
                                   base-value))
                        (right-image
                          (funcall (homotopy-pushout-right-map pushout)
                                   base-value))
                        (left-target (funcall left-action left-image))
                        (right-target (funcall right-action right-image))
                        (path (funcall glue-action base-value)))
                   (unless (and (typep path 'hott-path)
                                (eq (hott-path-type path) target)
                                (equal (hott-path-from path) left-target)
                                (equal (hott-path-to path) right-target))
                     (error "Pushout glue action is not coherent at ~S."
                            base-value))
                   path))))))

