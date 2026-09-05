;;;; equivalence-fibers.lisp --- fiberwise equivalence witnesses.

(in-package #:rosette-hott-core)

(defstruct (hott-fiber-value
            (:constructor %make-hott-fiber-value (point image-path)))
  "An inhabitant of the homotopy fiber of f : A -> B over y.

POINT is an A-value and IMAGE-PATH witnesses f(POINT) = y in B."
  (point nil :read-only t)
  (image-path nil :type hott-path :read-only t))

(defun make-hott-fiber-value (source target function target-value point
                              &key image-path)
  "Construct a checked homotopy-fiber value for FUNCTION over TARGET-VALUE."
  (unless (in-type-p source point)
    (error "Fiber point ~S is not in source type ~S."
           point (hott-type-name source)))
  (unless (in-type-p target target-value)
    (error "Fiber target ~S is not in target type ~S."
           target-value (hott-type-name target)))
  (let* ((image (funcall function point))
         (path (or image-path
                   (make-hott-path target image target-value
                                   :witness :fiber-image))))
    (unless (in-type-p target image)
      (error "Fiber function produced ~S outside target ~S."
             image (hott-type-name target)))
    (unless (and (eq (hott-path-type path) target)
                 (equal (hott-path-from path) image)
                 (equal (hott-path-to path) target-value))
      (error "Fiber image path must witness f(x)=y, got ~S -> ~S."
             (hott-path-from path)
             (hott-path-to path)))
    (%make-hott-fiber-value point path)))

(defun homotopy-fiber-type (source target function target-value
                            &key (name :homotopy-fiber))
  "Return the homotopy fiber type of FUNCTION over TARGET-VALUE."
  (make-hott-type
   name
   :predicate (lambda (value)
                (and (typep value 'hott-fiber-value)
                     (in-type-p source (hott-fiber-value-point value))
                     (let* ((image (funcall function
                                            (hott-fiber-value-point value)))
                            (path (hott-fiber-value-image-path value)))
                       (and (in-type-p target image)
                            (eq (hott-path-type path) target)
                            (equal (hott-path-from path) image)
                            (equal (hott-path-to path) target-value)))))
   :truncation +groupoid+))

(defstruct (homotopy-square
            (:constructor %make-homotopy-square
                (top-source top-target bottom-source bottom-target
                 top-map bottom-map left-map right-map homotopy-fn)))
  "A commuting square up to homotopy.

The square has TOP-MAP : A -> B, BOTTOM-MAP : C -> D, LEFT-MAP : A -> C,
and RIGHT-MAP : B -> D. HOMOTOPY-FN returns paths
right(top x)=bottom(left x) in D."
  (top-source nil :type hott-type :read-only t)
  (top-target nil :type hott-type :read-only t)
  (bottom-source nil :type hott-type :read-only t)
  (bottom-target nil :type hott-type :read-only t)
  (top-map nil :type function :read-only t)
  (bottom-map nil :type function :read-only t)
  (left-map nil :type function :read-only t)
  (right-map nil :type function :read-only t)
  (homotopy-fn nil :type function :read-only t))

(defun make-homotopy-square
    (top-source top-target bottom-source bottom-target
     top-map bottom-map left-map right-map homotopy-fn)
  "Construct a homotopy-commuting square witness."
  (%make-homotopy-square top-source top-target bottom-source bottom-target
                         top-map bottom-map left-map right-map homotopy-fn))

(defun homotopy-square-path (square value)
  "Return the square homotopy path at VALUE.

The path must witness right(top VALUE)=bottom(left VALUE) in the square's
bottom target."
  (unless (in-type-p (homotopy-square-top-source square) value)
    (error "Square value ~S is not in top source ~S."
           value
           (hott-type-name (homotopy-square-top-source square))))
  (let* ((top-value (funcall (homotopy-square-top-map square) value))
         (left-value (funcall (homotopy-square-left-map square) value))
         (right-top (funcall (homotopy-square-right-map square) top-value))
         (bottom-left
           (funcall (homotopy-square-bottom-map square) left-value))
         (path (funcall (homotopy-square-homotopy-fn square) value)))
    (unless (in-type-p (homotopy-square-top-target square) top-value)
      (error "Square top map produced ~S outside top target ~S."
             top-value
             (hott-type-name (homotopy-square-top-target square))))
    (unless (in-type-p (homotopy-square-bottom-source square) left-value)
      (error "Square left map produced ~S outside bottom source ~S."
             left-value
             (hott-type-name (homotopy-square-bottom-source square))))
    (unless (and (in-type-p (homotopy-square-bottom-target square) right-top)
                 (in-type-p (homotopy-square-bottom-target square)
                            bottom-left))
      (error "Square right/top or bottom/left image is outside bottom target."))
    (unless (and (typep path 'hott-path)
                 (eq (hott-path-type path)
                     (homotopy-square-bottom-target square))
                 (equal (hott-path-from path) right-top)
                 (equal (hott-path-to path) bottom-left))
      (error "Square homotopy path must witness right(top x)=bottom(left x)."))
    path))

(defun homotopy-square-fiber-map (square fiber-value target-value)
  "Map a homotopy fiber of TOP-MAP over TARGET-VALUE through SQUARE.

Given (x, p : top(x)=y), this returns
(left(x), bottom(left(x))=right(y)) in the bottom homotopy fiber."
  (let* ((x (hott-fiber-value-point fiber-value))
         (top-image (funcall (homotopy-square-top-map square) x))
         (fiber-path (hott-fiber-value-image-path fiber-value)))
    (unless (and (in-type-p (homotopy-square-top-source square) x)
                 (in-type-p (homotopy-square-top-target square) target-value)
                 (in-type-p (homotopy-square-top-target square) top-image)
                 (eq (hott-path-type fiber-path)
                     (homotopy-square-top-target square))
                 (equal (hott-path-from fiber-path) top-image)
                 (equal (hott-path-to fiber-path) target-value))
      (error "Fiber value is not a top-map fiber over ~S." target-value))
    (let* ((left-value (funcall (homotopy-square-left-map square) x))
           (right-target
             (funcall (homotopy-square-right-map square) target-value))
           (square-path (homotopy-square-path square x))
           (mapped-fiber-path
             (path-ap (homotopy-square-right-map square)
                      (homotopy-square-bottom-target square)
                      fiber-path))
           (bottom-path
             (path-compose (path-inverse square-path) mapped-fiber-path)))
      (make-hott-fiber-value
       (homotopy-square-bottom-source square)
       (homotopy-square-bottom-target square)
       (homotopy-square-bottom-map square)
       right-target
       left-value
       :image-path bottom-path))))

(defstruct (contractible-witness
            (:constructor %make-contractible-witness
                (type center contraction-fn)))
  "A runtime witness that TYPE is contractible."
  (type nil :type hott-type :read-only t)
  (center nil :read-only t)
  (contraction-fn nil :type function :read-only t))

(defun make-contractible-witness (type center contraction-fn)
  "Construct a contractibility witness for TYPE with CENTER.

CONTRACTION-FN must return paths CENTER = x for values x of TYPE."
  (unless (in-type-p type center)
    (error "Contractible center ~S is not in type ~S."
           center (hott-type-name type)))
  (%make-contractible-witness type center contraction-fn))

(defun contraction-path (witness value)
  "Return the contraction path CENTER = VALUE from WITNESS."
  (let ((type (contractible-witness-type witness)))
    (unless (in-type-p type value)
      (error "Contraction value ~S is not in type ~S."
             value (hott-type-name type)))
    (let ((path (funcall (contractible-witness-contraction-fn witness)
                         value)))
      (unless (and (typep path 'hott-path)
                   (eq (hott-path-type path) type)
                   (equal (hott-path-from path)
                          (contractible-witness-center witness))
                   (equal (hott-path-to path) value))
        (error "Invalid contraction path for ~S." value))
      path)))

(defun contractible-on-p (witness samples)
  "Return true when WITNESS contracts all sampled inhabitants."
  (every (lambda (sample)
           (typep (contraction-path witness sample) 'hott-path))
         samples))

(defstruct (fiberwise-equivalence-witness
            (:constructor %make-fiberwise-equivalence-witness
                (source target forward fiber-witness-fn)))
  "Equivalence evidence via contractible homotopy fibers."
  (source nil :type hott-type :read-only t)
  (target nil :type hott-type :read-only t)
  (forward nil :type function :read-only t)
  (fiber-witness-fn nil :type function :read-only t))

(defun make-fiberwise-equivalence-witness
    (source target forward fiber-witness-fn)
  "Construct evidence that FORWARD is an equivalence by contractible fibers.

FIBER-WITNESS-FN is called on each target value and must return a
CONTRACTIBLE-WITNESS for the homotopy fiber over that value."
  (%make-fiberwise-equivalence-witness source target forward fiber-witness-fn))

(defun fiberwise-witness-for (witness target-value)
  "Return the contractible-fiber witness over TARGET-VALUE."
  (unless (in-type-p (fiberwise-equivalence-witness-target witness)
                     target-value)
    (error "Target value ~S is not in fiberwise target type ~S."
           target-value
           (hott-type-name
            (fiberwise-equivalence-witness-target witness))))
  (let ((fiber-witness
          (funcall (fiberwise-equivalence-witness-fiber-witness-fn witness)
                   target-value)))
    (unless (typep fiber-witness 'contractible-witness)
      (error "Fiber witness for ~S is not contractible evidence: ~S."
             target-value fiber-witness))
    fiber-witness))

(defun equiv-from-fiberwise-witness (witness)
  "Build a HOTT-EQUIVALENCE from contractible homotopy fibers.

The backward map chooses the center point of each target fiber. The right
homotopy is the fiber center's image path."
  (let ((source (fiberwise-equivalence-witness-source witness))
        (target (fiberwise-equivalence-witness-target witness))
        (forward (fiberwise-equivalence-witness-forward witness)))
    (make-hott-equivalence
     source
     target
     forward
     (lambda (y)
       (hott-fiber-value-point
        (contractible-witness-center
         (fiberwise-witness-for witness y))))
     :left-homotopy
     (lambda (x)
       (let* ((y (funcall forward x))
              (fiber-witness (fiberwise-witness-for witness y))
              (fiber-value (make-hott-fiber-value
                            source target forward y x
                            :image-path (refl target y)))
              (fiber-path (contraction-path fiber-witness fiber-value)))
         (path-ap #'hott-fiber-value-point source fiber-path)))
     :right-homotopy
     (lambda (y)
       (hott-fiber-value-image-path
        (contractible-witness-center
         (fiberwise-witness-for witness y)))))))
