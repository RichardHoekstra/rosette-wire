;;;; paths.lisp --- paths vocabulary for rosette-hott-core.

(in-package #:rosette-hott-core)

(defun path-inverse (path)
  "Invert a path, turning A = B into B = A."
  (%make-hott-path (hott-path-type path)
                   (hott-path-to path)
                   (hott-path-from path)
                   (list :inverse (hott-path-witness path))))

(defun path-compose (left right)
  "Compose LEFT : A = B with RIGHT : B = C, returning A = C."
  (unless (eq (hott-path-type left) (hott-path-type right))
    (error "Cannot compose paths over different HoTT types."))
  (unless (equalp (hott-path-to left) (hott-path-from right))
    (error "Path endpoints do not meet: ~S and ~S."
           (hott-path-to left)
           (hott-path-from right)))
  (%make-hott-path (hott-path-type left)
                   (hott-path-from left)
                   (hott-path-to right)
                   (list :compose (hott-path-witness left)
                         (hott-path-witness right))))

(defun path-ap (function target-type path)
  "Apply FUNCTION to both endpoints of PATH, producing a path in TARGET-TYPE."
  (let ((from (funcall function (hott-path-from path)))
        (to (funcall function (hott-path-to path))))
    (unless (and (in-type-p target-type from)
                 (in-type-p target-type to))
      (error "PATH-AP result is outside target HoTT type ~S."
             (hott-type-name target-type)))
    (%make-hott-path target-type from to
                     (list :ap function (hott-path-witness path)))))

(defun path-induction (path refl-case &key checker)
  "Eliminate PATH by reducing to the reflexivity case at PATH's source.

REFL-CASE is called as (source refl-path original-path).  CHECKER, when
supplied, is called as (original-path result) and must return true.  This is
the runtime J rule: it records the same control shape as path induction while
leaving motive-specific validation to the caller."
  (let* ((source (hott-path-from path))
         (refl-path (refl (hott-path-type path) source))
         (result (funcall refl-case source refl-path path)))
    (when (and checker (not (funcall checker path result)))
      (error "PATH-INDUCTION checker rejected result ~S for path ~S."
             result path))
    result))

(defun path-recursion (path refl-case &key checker)
  "Non-dependent path recursion over PATH.

REFL-CASE is called as (source refl-path)."
  (path-induction
   path
   (lambda (source refl-path original-path)
     (declare (ignore original-path))
     (funcall refl-case source refl-path))
   :checker checker))

(defun based-path-induction (base path refl-case
                             &key (test #'equalp) checker)
  "Path induction for paths whose source is fixed to BASE."
  (unless (funcall test (hott-path-from path) base)
    (error "BASED-PATH-INDUCTION expected source ~S, got ~S."
           base (hott-path-from path)))
  (path-induction
   path
   (lambda (source refl-path original-path)
     (declare (ignore source))
     (funcall refl-case base refl-path original-path))
   :checker checker))

(defun path-loop-p (path &key (test #'equalp))
  "Return true when PATH starts and ends at the same point."
  (funcall test (hott-path-from path) (hott-path-to path)))

(defun loop-space (pointed &key name)
  "Return the loop-space type Omega(POINTED).

Its inhabitants are paths in POINTED's carrier whose endpoints are both the
basepoint.  This is represented one truncation level lower when possible."
  (let* ((type (pointed-type-type pointed))
         (basepoint (pointed-type-basepoint pointed))
         (level (hott-type-truncation type)))
    (make-hott-type
     (or name (list :loop-space (hott-type-name type)))
     :predicate (lambda (value)
                  (and (typep value 'hott-path)
                       (eq (hott-path-type value) type)
                       (equalp (hott-path-from value) basepoint)
                       (equalp (hott-path-to value) basepoint)))
     :truncation (1- level))))

(defun loop-refl (pointed)
  "Return the identity loop at POINTED's basepoint."
  (refl (pointed-type-type pointed)
        (pointed-type-basepoint pointed)))

(defun loop-compose (left right)
  "Compose two based loops."
  (unless (and (path-loop-p left) (path-loop-p right))
    (error "LOOP-COMPOSE requires two loops."))
  (path-compose left right))

(defun loop-power (loop n)
  "Return LOOP^N for integer N using path composition and inverse paths."
  (unless (integerp n)
    (error "Loop power exponent must be an integer: ~S." n))
  (unless (path-loop-p loop)
    (error "LOOP-POWER requires a loop path."))
  (cond
    ((zerop n) (refl (hott-path-type loop) (hott-path-from loop)))
    ((plusp n)
     (let ((out loop))
       (dotimes (i (1- n) out)
         (setf out (loop-compose out loop)))))
    (t (loop-power (path-inverse loop) (- n)))))

(defun pointed-map-loop (pointed-map loop)
  "Apply POINTED-MAP to a based LOOP, producing a based loop in the target.

The image path f(base)=target-base is used to conjugate the raw AP image back
to the target basepoint."
  (let* ((source (pointed-map-source pointed-map))
         (target (pointed-map-target pointed-map))
         (source-type (pointed-type-type source))
         (target-type (pointed-type-type target))
         (base-path (pointed-map-basepoint-path pointed-map)))
    (unless (and (eq (hott-path-type loop) source-type)
                 (equalp (hott-path-from loop)
                         (pointed-type-basepoint source))
                 (equalp (hott-path-to loop)
                         (pointed-type-basepoint source)))
      (error "POINTED-MAP-LOOP requires a loop in the source pointed type."))
    (let ((mapped (path-ap (pointed-map-function pointed-map)
                           target-type
                           loop)))
      (path-compose (path-compose (path-inverse base-path) mapped)
                    base-path))))

(defstruct (loop-group
            (:constructor %make-loop-group
                (pointed carrier equality-test)))
  "A runtime group-shaped structure on the based loops of POINTED."
  (pointed nil :type pointed-type :read-only t)
  (carrier nil :type hott-type :read-only t)
  (equality-test nil :type function :read-only t))

(defun loop-endpoint-equal-p (left right &key (test #'equalp))
  "Return true when LEFT and RIGHT are paths over the same type and endpoints."
  (and (typep left 'hott-path)
       (typep right 'hott-path)
       (eq (hott-path-type left) (hott-path-type right))
       (funcall test (hott-path-from left) (hott-path-from right))
       (funcall test (hott-path-to left) (hott-path-to right))))

(defun make-loop-group (pointed &key name (equality-test #'loop-endpoint-equal-p))
  "Return the group-shaped loop object Omega(POINTED)."
  (%make-loop-group pointed
                    (loop-space pointed :name (or name
                                                  (list :fundamental-group
                                                        (hott-type-name
                                                         (pointed-type-type
                                                          pointed)))))
                    equality-test))

(defun fundamental-group (pointed &key name equality-test)
  "Return the sampled fundamental group object for POINTED."
  (make-loop-group pointed :name name
                   :equality-test (or equality-test
                                      #'loop-endpoint-equal-p)))

(defun loop-group-identity (group)
  "Return GROUP's identity loop."
  (loop-refl (loop-group-pointed group)))

(defun loop-group-compose (group left right)
  "Compose LEFT and RIGHT inside GROUP."
  (unless (and (in-type-p (loop-group-carrier group) left)
               (in-type-p (loop-group-carrier group) right))
    (error "Loop-group compose requires loops in the same carrier."))
  (loop-compose left right))

(defun loop-group-inverse (group loop)
  "Return LOOP's inverse inside GROUP."
  (unless (in-type-p (loop-group-carrier group) loop)
    (error "Loop-group inverse requires a loop in the carrier."))
  (path-inverse loop))

(defun loop-group-power (group loop n)
  "Return LOOP^N inside GROUP."
  (unless (in-type-p (loop-group-carrier group) loop)
    (error "Loop-group power requires a loop in the carrier."))
  (loop-power loop n))

(defun loop-group-equal-p (group left right)
  "Compare two loops using GROUP's equality approximation."
  (funcall (loop-group-equality-test group) left right))

(defun loop-group-law-report (group samples)
  "Return sampled group-law checks for GROUP over SAMPLES.

The equality is GROUP's runtime equality approximation, so this is a compact
executable witness rather than a quotient proof over all loops."
  (let ((unit (loop-group-identity group)))
    (labels ((eq-loop (left right)
               (loop-group-equal-p group left right))
             (sample-loop-p (loop)
               (in-type-p (loop-group-carrier group) loop))
             (all-pairs-p (predicate)
               (every (lambda (left)
                        (every (lambda (right)
                                 (funcall predicate left right))
                               samples))
                      samples))
             (all-triples-p (predicate)
               (every (lambda (left)
                        (every (lambda (middle)
                                 (every (lambda (right)
                                          (funcall predicate left middle right))
                                        samples))
                               samples))
                      samples)))
      (unless (every #'sample-loop-p samples)
        (error "Loop-group law samples must all inhabit the carrier."))
      (list
       :left-unit
       (every (lambda (loop)
                (eq-loop (loop-group-compose group unit loop) loop))
              samples)
       :right-unit
       (every (lambda (loop)
                (eq-loop (loop-group-compose group loop unit) loop))
              samples)
       :inverse-left
       (every (lambda (loop)
                (eq-loop (loop-group-compose group
                                             (loop-group-inverse group loop)
                                             loop)
                         unit))
              samples)
       :inverse-right
       (every (lambda (loop)
                (eq-loop (loop-group-compose group loop
                                             (loop-group-inverse group loop))
                         unit))
              samples)
       :associative
       (all-triples-p
        (lambda (left middle right)
          (eq-loop
           (loop-group-compose group
                               (loop-group-compose group left middle)
                               right)
           (loop-group-compose group
                               left
                               (loop-group-compose group middle right)))))
       :closed
       (all-pairs-p
        (lambda (left right)
          (sample-loop-p (loop-group-compose group left right))))))))

