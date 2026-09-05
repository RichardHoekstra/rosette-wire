;;;; circle-interval.lisp --- circle-interval vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defun circle-recursion (circle target base-image loop-image)
  "Return the circle recursor into TARGET.

BASE-IMAGE must inhabit TARGET.  LOOP-IMAGE must be a target path from
BASE-IMAGE to itself."
  (ensure-in-type
   target
   base-image
   "Circle base image ~S is not in target ~S."
   base-image
   (hott-type-name target))
  (unless (and (typep loop-image 'hott-path)
               (eq (hott-path-type loop-image) target)
               (equal (hott-path-from loop-image) base-image)
               (equal (hott-path-to loop-image) base-image))
    (error "Circle loop image must be a loop at the base image."))
  (make-hit-eliminator
   circle
   target
   `((:base . ,(lambda () base-image)))
   `((:loop . ,(lambda () loop-image)))))

(defun interval-recursion (interval target left-image right-image segment-image)
  "Return the interval recursor into TARGET."
  (unless (and (in-type-p target left-image)
               (in-type-p target right-image))
    (error "Interval endpoint images must inhabit target ~S."
           (hott-type-name target)))
  (unless (and (typep segment-image 'hott-path)
               (eq (hott-path-type segment-image) target)
               (equal (hott-path-from segment-image) left-image)
               (equal (hott-path-to segment-image) right-image))
    (error "Interval segment image must connect left image to right image."))
  (make-hit-eliminator
   interval
   target
   `((:left . ,(lambda () left-image))
     (:right . ,(lambda () right-image)))
   `((:segment . ,(lambda () segment-image)))))

(defun circle-type (&key (name :circle))
  "Return the standard circle HIT with BASE and LOOP constructors."
  (let ((hit nil))
    (setf hit
          (make-higher-inductive-type
           name
           '(:base)
           `((:loop . (:from ,(lambda ()
                                (make-hit-value hit :base))
                       :to ,(lambda ()
                              (make-hit-value hit :base)))))))
    hit))

(defun circle-base (circle)
  "Return the base point of CIRCLE."
  (make-hit-value circle :base))

(defun circle-loop (circle)
  "Return the generating loop path of CIRCLE."
  (hit-path circle :loop))

(defun interval-type (&key (name :interval))
  "Return the interval HIT with LEFT, RIGHT, and SEGMENT constructors."
  (let ((hit nil))
    (setf hit
          (make-higher-inductive-type
           name
           '(:left :right)
           `((:segment . (:from ,(lambda ()
                                   (make-hit-value hit :left))
                         :to ,(lambda ()
                                (make-hit-value hit :right)))))))
    hit))

(defun interval-left (interval)
  "Return the left endpoint of INTERVAL."
  (make-hit-value interval :left))

(defun interval-right (interval)
  "Return the right endpoint of INTERVAL."
  (make-hit-value interval :right))

(defun interval-segment (interval)
  "Return the generating segment path of INTERVAL."
  (hit-path interval :segment))

