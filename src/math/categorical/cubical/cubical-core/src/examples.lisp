;;;; examples.lisp --- concrete equivalences + the book-HoTT contrast.
;;;;
;;;; Two computable equivalences to fire ua-beta on:
;;;;   * not : Bool ~= Bool     (transp (ua not) T  ==>  NIL)
;;;;   * succ : Z ~= Z          (transp (ua succ) 3 ==>  4)
;;;; plus a helper that LOCATES the book-HoTT wall: rosette-hott-core's
;;;; genuine dependent-family transport is stuck across these, because a
;;;; family knows no map between distinct fibers.

(in-package #:rosette-cubical-core)

(defun bool-type ()
  "The two-element type {T, NIL} (a HoTT set)."
  (make-hott-type :bool
                  :predicate (lambda (x) (or (eq x t) (eq x nil)))
                  :truncation 0))

(defun bool-not-equivalence ()
  "The negation auto-equivalence  not : Bool ~= Bool."
  (let ((b (bool-type)))
    (make-hott-equivalence b b #'not #'not
                           :left-homotopy (lambda (x) (crefl-hott b x))
                           :right-homotopy (lambda (x) (crefl-hott b x)))))

(defun int-type ()
  "The integers Z (a HoTT set)."
  (make-hott-type :int :predicate #'integerp :truncation 0))

(defun int-succ-equivalence ()
  "The successor auto-equivalence  succ : Z ~= Z, inverse pred."
  (let ((z (int-type)))
    (make-hott-equivalence z z #'1+ #'1-
                           :left-homotopy (lambda (x) (crefl-hott z x))
                           :right-homotopy (lambda (x) (crefl-hott z x)))))

;; refl in the book-HoTT path representation (for the equivalence
;; homotopy witnesses, which live in rosette-hott-core's world).
(defun crefl-hott (type value)
  (rosette-hott-core:refl type value))

;;; ---- Locating the book-HoTT wall -------------------------------------

(defun book-transport-stuck-p (equivalence value)
  "Return T when book-HoTT's GENUINE dependent-family transport
(rosette-hott-core:transport over a type family) cannot move VALUE across the
equivalence -- i.e. it is STUCK.  This is the wall cubical transp crosses.

We build the family whose fiber is the source type at i0 and the target
type at i1 and ask the book transporter to move the value.  With distinct
fibers and no hand-supplied transport-fn, rosette-hott-core errors -- that
error IS the stuckness, so we catch it and report T."
  (let* ((src (hott-equivalence-source equivalence))
         (dst (hott-equivalence-target equivalence))
         ;; index type with two points :a, :b
         (idx (make-hott-type :idx
                              :predicate (lambda (x) (member x '(:a :b)))
                              :truncation 0))
         (fam (rosette-hott-core:make-hott-family
               idx
               (lambda (i) (ecase i (:a src) (:b dst)))))
         (p (rosette-hott-core:make-hott-path idx :a :b)))
    (handler-case
        (progn
          (rosette-hott-core:transport fam p value)
          ;; If src and dst are the *same* descriptor (auto-equivalence
          ;; like not/succ they ARE), identity transport succeeds but
          ;; returns the value UNCHANGED -- it never applies the map.
          ;; So "stuck" = it does not equal the equivalence's action.
          (not (eql (rosette-hott-core:transport fam p value)
                    (equiv-forward equivalence value))))
      (error () t))))
