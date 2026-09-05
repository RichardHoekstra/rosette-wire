;;;; equivalences.lisp --- equivalences vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defstruct (hott-equivalence
            (:constructor %make-hott-equivalence
                (source target forward backward left-homotopy right-homotopy)))
  "A runtime equivalence A ~= B with forward/backward maps and homotopies."
  (source nil :type hott-type :read-only t)
  (target nil :type hott-type :read-only t)
  (forward nil :type function :read-only t)
  (backward nil :type function :read-only t)
  (left-homotopy nil :type function :read-only t)
  (right-homotopy nil :type function :read-only t))

(defstruct (hott-homotopy
            (:constructor %make-hott-homotopy
                (source target left right witness-fn)))
  "A pointwise path witness between two functions SOURCE -> TARGET."
  (source nil :type hott-type :read-only t)
  (target nil :type hott-type :read-only t)
  (left nil :type function :read-only t)
  (right nil :type function :read-only t)
  (witness-fn nil :type function :read-only t))

(defun make-hott-homotopy (source target left right witness-fn)
  "Construct a homotopy LEFT ~ RIGHT between functions SOURCE -> TARGET.

WITNESS-FN is called on a source value and must return a path in TARGET from
(LEFT x) to (RIGHT x).  POINT-HOMOTOPY-PATH performs the endpoint checks."
  (%make-hott-homotopy source target left right witness-fn))

(defun point-homotopy-path (homotopy value)
  "Return HOMOTOPY's pointwise path at VALUE, checking function endpoints."
  (unless (in-type-p (hott-homotopy-source homotopy) value)
    (error "Homotopy value ~S is not in source type ~S."
           value
           (hott-type-name (hott-homotopy-source homotopy))))
  (let* ((target (hott-homotopy-target homotopy))
         (left-value (funcall (hott-homotopy-left homotopy) value))
         (right-value (funcall (hott-homotopy-right homotopy) value))
         (path (funcall (hott-homotopy-witness-fn homotopy) value)))
    (unless (and (in-type-p target left-value)
                 (in-type-p target right-value))
      (error "Homotopy functions produced values outside target ~S."
             (hott-type-name target)))
    (unless (typep path 'hott-path)
      (error "Homotopy witness for ~S is not a path: ~S." value path))
    (unless (eq (hott-path-type path) target)
      (error "Homotopy path for ~S is over ~S, expected ~S."
             value
             (hott-type-name (hott-path-type path))
             (hott-type-name target)))
    (unless (and (equal (hott-path-from path) left-value)
                 (equal (hott-path-to path) right-value))
      (error "Homotopy path endpoints ~S -> ~S do not match values ~S -> ~S."
             (hott-path-from path)
             (hott-path-to path)
             left-value
             right-value))
    path))

(defun homotopic-on-p (homotopy samples)
  "Return true when HOMOTOPY has valid pointwise paths for all SAMPLES."
  (every (lambda (sample)
           (typep (point-homotopy-path homotopy sample) 'hott-path))
         samples))

(defun make-hott-equivalence (source target forward backward
                              &key left-homotopy right-homotopy)
  "Construct an equivalence SOURCE ~= TARGET.

LEFT-HOMOTOPY proves backward(forward x) = x.  RIGHT-HOMOTOPY proves
forward(backward y) = y.  When omitted, reflexivity witnesses are used,
which is appropriate for identity-like equivalences."
  (let ((left (or left-homotopy
                  (lambda (x)
                    (refl source x))))
        (right (or right-homotopy
                   (lambda (y)
                     (refl target y)))))
    (%make-hott-equivalence source target forward backward left right)))

(defun identity-equivalence (type)
  "Return the identity equivalence TYPE ~= TYPE."
  (make-hott-equivalence type type #'identity #'identity
                         :left-homotopy (lambda (x) (refl type x))
                         :right-homotopy (lambda (x) (refl type x))))

(defun inverse-equivalence (equivalence)
  "Return the inverse equivalence by swapping forward and backward maps."
  (make-hott-equivalence
   (hott-equivalence-target equivalence)
   (hott-equivalence-source equivalence)
   (hott-equivalence-backward equivalence)
   (hott-equivalence-forward equivalence)
   :left-homotopy (lambda (y)
                    (equiv-path equivalence :right y))
   :right-homotopy (lambda (x)
                     (equiv-path equivalence :left x))))

(defun compose-equivalences (left right)
  "Compose LEFT : A ~= B with RIGHT : B ~= C, returning A ~= C."
  (unless (eq (hott-equivalence-target left)
              (hott-equivalence-source right))
    (error "Equivalence endpoints do not meet: ~S and ~S."
           (hott-type-name (hott-equivalence-target left))
           (hott-type-name (hott-equivalence-source right))))
  (let ((source (hott-equivalence-source left))
        (middle (hott-equivalence-target left))
        (target (hott-equivalence-target right)))
    (declare (ignore middle))
    (make-hott-equivalence
     source
     target
     (lambda (x)
       (equiv-forward right (equiv-forward left x)))
     (lambda (z)
       (equiv-backward left (equiv-backward right z)))
     :left-homotopy
     (lambda (x)
       (let* ((fx (equiv-forward left x))
              (inner (equiv-path right :left fx))
              (mapped (path-ap (hott-equivalence-backward left)
                               source
                               inner))
              (outer (equiv-path left :left x)))
         (path-compose mapped outer)))
     :right-homotopy
     (lambda (z)
       (let* ((gz (equiv-backward right z))
              (inner (equiv-path left :right gz))
              (mapped (path-ap (hott-equivalence-forward right)
                               target
                               inner))
              (outer (equiv-path right :right z)))
         (path-compose mapped outer))))))

(defun equiv-forward (equivalence value)
  "Apply EQUIVALENCE's forward map."
  (unless (in-type-p (hott-equivalence-source equivalence) value)
    (error "Value ~S is not in equivalence source ~S."
           value
           (hott-type-name (hott-equivalence-source equivalence))))
  (let ((out (funcall (hott-equivalence-forward equivalence) value)))
    (unless (in-type-p (hott-equivalence-target equivalence) out)
      (error "Forward map produced ~S outside target ~S."
             out
             (hott-type-name (hott-equivalence-target equivalence))))
    out))

(defun equiv-backward (equivalence value)
  "Apply EQUIVALENCE's backward map."
  (unless (in-type-p (hott-equivalence-target equivalence) value)
    (error "Value ~S is not in equivalence target ~S."
           value
           (hott-type-name (hott-equivalence-target equivalence))))
  (let ((out (funcall (hott-equivalence-backward equivalence) value)))
    (unless (in-type-p (hott-equivalence-source equivalence) out)
      (error "Backward map produced ~S outside source ~S."
             out
             (hott-type-name (hott-equivalence-source equivalence))))
    out))

(defun equiv-path (equivalence side value)
  "Return a homotopy path for EQUIVALENCE.

SIDE is :LEFT for backward(forward x) = x, or :RIGHT for
forward(backward y) = y."
  (ecase side
    (:left (funcall (hott-equivalence-left-homotopy equivalence) value))
    (:right (funcall (hott-equivalence-right-homotopy equivalence) value))))

(defun type-universe (&key (truncation +groupoid+))
  "Return a runtime universe whose inhabitants are HOTT-TYPE descriptors."
  (make-hott-type :type-universe
                  :predicate (lambda (x) (typep x 'hott-type))
                  :truncation truncation))

(defun equiv->path (equivalence &key universe)
  "Return the univalence-shaped path induced by EQUIVALENCE.

This is a runtime witness, not a proof assistant axiom: it records that the
source and target type descriptors may be treated as path-related because an
explicit equivalence object supplies the forward/backward maps."
  (let ((u (or universe (type-universe))))
    (make-hott-path u
                    (hott-equivalence-source equivalence)
                    (hott-equivalence-target equivalence)
                    :witness (list :univalence equivalence))))

(defun path->equiv (path)
  "Recover the equivalence stored in an EQUIV->PATH witness."
  (let ((witness (hott-path-witness path)))
    (unless (and (consp witness)
                 (eq (first witness) :univalence)
                 (typep (second witness) 'hott-equivalence))
      (error "Path does not carry a univalence equivalence witness: ~S."
             witness))
    (second witness)))

(defun equiv-transport (equivalence value &key (direction :forward))
  "Transport VALUE across EQUIVALENCE in DIRECTION.

DIRECTION is :FORWARD for source to target, or :BACKWARD for target to
source.  The normal EQUIV-FORWARD/EQUIV-BACKWARD checks are used."
  (ecase direction
    (:forward (equiv-forward equivalence value))
    (:backward (equiv-backward equivalence value))))

(defun univalence-transport (path value &key (direction :forward))
  "Transport VALUE along a path produced by EQUIV->PATH."
  (equiv-transport (path->equiv path) value :direction direction))
