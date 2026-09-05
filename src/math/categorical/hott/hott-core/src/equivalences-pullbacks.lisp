;;;; equivalences-pullbacks.lisp --- homotopy pullback vocabulary.

(in-package #:rosette-hott-core)

(defstruct (homotopy-pullback-value
            (:constructor %make-homotopy-pullback-value
                (left right path)))
  "An inhabitant of the homotopy pullback of f : A -> C and g : B -> C.

LEFT is an A-value, RIGHT is a B-value, and PATH witnesses f(LEFT)=g(RIGHT)
in the common target type."
  (left nil :read-only t)
  (right nil :read-only t)
  (path nil :type hott-path :read-only t))

(defun make-homotopy-pullback-value
    (left-type right-type target-type left-map right-map left-value right-value
     &key path)
  "Construct a checked homotopy-pullback value for LEFT-MAP and RIGHT-MAP."
  (unless (in-type-p left-type left-value)
    (error "Pullback left value ~S is not in type ~S."
           left-value (hott-type-name left-type)))
  (unless (in-type-p right-type right-value)
    (error "Pullback right value ~S is not in type ~S."
           right-value (hott-type-name right-type)))
  (let* ((left-image (funcall left-map left-value))
         (right-image (funcall right-map right-value))
         (path-witness (or path
                           (make-hott-path target-type
                                           left-image
                                           right-image
                                           :witness :homotopy-pullback))))
    (unless (and (in-type-p target-type left-image)
                 (in-type-p target-type right-image))
      (error "Pullback maps produced values outside target ~S."
             (hott-type-name target-type)))
    (unless (and (eq (hott-path-type path-witness) target-type)
                 (equal (hott-path-from path-witness) left-image)
                 (equal (hott-path-to path-witness) right-image))
      (error "Pullback path must witness f(left)=g(right), got ~S -> ~S."
             (hott-path-from path-witness)
             (hott-path-to path-witness)))
    (%make-homotopy-pullback-value left-value right-value path-witness)))

(defun homotopy-pullback-type
    (left-type right-type target-type left-map right-map &key
       (name :homotopy-pullback)
       (truncation +groupoid+))
  "Return the homotopy pullback type of two maps into TARGET-TYPE."
  (make-hott-type
   name
   :predicate (lambda (value)
                (and (typep value 'homotopy-pullback-value)
                     (in-type-p left-type
                                (homotopy-pullback-value-left value))
                     (in-type-p right-type
                                (homotopy-pullback-value-right value))
                     (let* ((left-image
                              (funcall left-map
                                       (homotopy-pullback-value-left value)))
                            (right-image
                              (funcall right-map
                                       (homotopy-pullback-value-right value)))
                            (path (homotopy-pullback-value-path value)))
                       (and (in-type-p target-type left-image)
                            (in-type-p target-type right-image)
                            (eq (hott-path-type path) target-type)
                            (equal (hott-path-from path) left-image)
                            (equal (hott-path-to path) right-image)))))
   :truncation truncation))

(defun homotopy-pullback-left (value)
  "Return the left projection of a homotopy-pullback VALUE."
  (homotopy-pullback-value-left value))

(defun homotopy-pullback-right (value)
  "Return the right projection of a homotopy-pullback VALUE."
  (homotopy-pullback-value-right value))

(defun homotopy-pullback-path (value)
  "Return the comparison path f(left)=g(right) of VALUE."
  (homotopy-pullback-value-path value))
