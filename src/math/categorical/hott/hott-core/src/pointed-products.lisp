;;;; pointed-products.lisp --- product, join, wedge, and smash constructions.

(in-package #:rosette-hott-core)

(defstruct (hott-product-value
            (:constructor %make-hott-product-value (left right)))
  "A checked binary product inhabitant."
  (left nil :read-only t)
  (right nil :read-only t))

(defun make-hott-product-value (left right)
  "Construct a raw product value."
  (%make-hott-product-value left right))

(defun hott-product-type (left-type right-type &key name)
  "Return the binary product type LEFT-TYPE x RIGHT-TYPE."
  (make-hott-type
   (or name (list :product (hott-type-name left-type)
                  (hott-type-name right-type)))
   :predicate (lambda (value)
                (and (typep value 'hott-product-value)
                     (in-type-p left-type
                                (hott-product-value-left value))
                     (in-type-p right-type
                                (hott-product-value-right value))))
   :truncation (max (hott-type-truncation left-type)
                    (hott-type-truncation right-type))))

(defun hott-product-left (value)
  "Return VALUE's left product component."
  (hott-product-value-left value))

(defun hott-product-right (value)
  "Return VALUE's right product component."
  (hott-product-value-right value))

(defstruct (homotopy-join
            (:constructor %make-homotopy-join
                (left-type right-type product-type pushout)))
  "The homotopy join X * Y as the pushout of X <- X x Y -> Y."
  (left-type nil :type hott-type :read-only t)
  (right-type nil :type hott-type :read-only t)
  (product-type nil :type hott-type :read-only t)
  (pushout nil :type homotopy-pushout :read-only t))

(defun make-homotopy-join (left-type right-type &key name)
  "Construct the homotopy join LEFT-TYPE * RIGHT-TYPE."
  (let ((product (hott-product-type left-type right-type)))
    (%make-homotopy-join
     left-type right-type product
     (make-homotopy-pushout
      left-type right-type product
      #'hott-product-left
      #'hott-product-right
      :name (or name (list :join (hott-type-name left-type)
                           (hott-type-name right-type)))))))

(defun homotopy-join-type (join)
  "Return JOIN's underlying HIT carrier."
  (homotopy-pushout-type (homotopy-join-pushout join)))

(defun join-left (join value)
  "Inject VALUE from JOIN's left type."
  (pushout-left (homotopy-join-pushout join) value))

(defun join-right (join value)
  "Inject VALUE from JOIN's right type."
  (pushout-right (homotopy-join-pushout join) value))

(defun join-glue (join left-value right-value)
  "Return the join segment path between LEFT-VALUE and RIGHT-VALUE."
  (let ((pair (make-hott-product-value left-value right-value)))
    (unless (in-type-p (homotopy-join-product-type join) pair)
      (error "Join pair ~S, ~S is not in product type."
             left-value right-value))
    (pushout-glue (homotopy-join-pushout join) pair)))

(defstruct (pointed-wedge
            (:constructor %make-pointed-wedge (left right pushout)))
  "The wedge sum of two pointed types as a homotopy pushout over UNIT."
  (left nil :type pointed-type :read-only t)
  (right nil :type pointed-type :read-only t)
  (pushout nil :type homotopy-pushout :read-only t))

(defun make-pointed-wedge (left right &key name)
  "Construct the wedge sum LEFT v RIGHT by identifying basepoints."
  (let ((unit (%unit-type)))
    (%make-pointed-wedge
     left right
     (make-homotopy-pushout
      (pointed-type-type left)
      (pointed-type-type right)
      unit
      (lambda (value)
        (declare (ignore value))
        (pointed-type-basepoint left))
      (lambda (value)
        (declare (ignore value))
        (pointed-type-basepoint right))
      :name (or name (list :wedge
                           (hott-type-name (pointed-type-type left))
                           (hott-type-name
                            (pointed-type-type right))))))))

(defun pointed-wedge-type (wedge)
  "Return WEDGE's underlying HIT carrier."
  (homotopy-pushout-type (pointed-wedge-pushout wedge)))

(defun wedge-left (wedge value)
  "Inject VALUE from the left pointed type into WEDGE."
  (pushout-left (pointed-wedge-pushout wedge) value))

(defun wedge-right (wedge value)
  "Inject VALUE from the right pointed type into WEDGE."
  (pushout-right (pointed-wedge-pushout wedge) value))

(defun wedge-basepoint (wedge)
  "Return the left basepoint image used as WEDGE's basepoint."
  (wedge-left wedge
              (pointed-type-basepoint
               (pointed-wedge-left wedge))))

(defun wedge-glue (wedge)
  "Return the path identifying the two wedge basepoint images."
  (pushout-glue (pointed-wedge-pushout wedge) :star))

(defstruct (pointed-smash
            (:constructor %make-pointed-smash
                (left right product-type wedge inclusion cofiber)))
  "The smash product X smash Y as cofiber(X wedge Y -> X times Y)."
  (left nil :type pointed-type :read-only t)
  (right nil :type pointed-type :read-only t)
  (product-type nil :type hott-type :read-only t)
  (wedge nil :type pointed-wedge :read-only t)
  (inclusion nil :type function :read-only t)
  (cofiber nil :type homotopy-cofiber :read-only t))

(defun %wedge-product-inclusion (wedge product-type left-base right-base value)
  (unless (in-type-p (pointed-wedge-type wedge) value)
    (error "Wedge-product inclusion expected a wedge value, got ~S." value))
  (unless (typep value 'hit-value)
    (error "Wedge-product inclusion expected a HIT value, got ~S." value))
  (let ((pair
          (ecase (hit-value-constructor value)
            (:left
             (make-hott-product-value
              (first (hit-value-arguments value))
              right-base))
            (:right
             (make-hott-product-value
              left-base
              (first (hit-value-arguments value)))))))
    (unless (in-type-p product-type pair)
      (error "Wedge-product inclusion produced value outside product type."))
    pair))

(defun make-pointed-smash (left right &key name)
  "Construct the smash product LEFT smash RIGHT.

This is the cofiber of the canonical inclusion
LEFT wedge RIGHT -> LEFT x RIGHT."
  (let* ((left-type (pointed-type-type left))
         (right-type (pointed-type-type right))
         (left-base (pointed-type-basepoint left))
         (right-base (pointed-type-basepoint right))
         (product (hott-product-type
                   left-type right-type
                   :name (list :product (hott-type-name left-type)
                               (hott-type-name right-type))))
         (wedge (make-pointed-wedge
                 left right
                 :name (list :smash-wedge (hott-type-name left-type)
                             (hott-type-name right-type))))
         (inclusion (lambda (value)
                      (%wedge-product-inclusion
                       wedge product left-base right-base value)))
         (cofiber (make-homotopy-cofiber
                   (pointed-wedge-type wedge)
                   product
                   inclusion
                   :name (or name (list :smash (hott-type-name left-type)
                                         (hott-type-name right-type))))))
    (%make-pointed-smash left right product wedge inclusion cofiber)))

(defun pointed-smash-type (smash)
  "Return SMASH's underlying HIT carrier."
  (homotopy-cofiber-type (pointed-smash-cofiber smash)))

(defun smash-pair (smash left-value right-value)
  "Include a product pair into SMASH."
  (let ((pair (make-hott-product-value left-value right-value)))
    (ensure-in-type
     (pointed-smash-product-type smash)
     pair
     "Smash pair ~S, ~S is not in the product type."
     left-value
     right-value)
    (cofiber-include (pointed-smash-cofiber smash) pair)))

(defun smash-basepoint (smash)
  "Return SMASH's collapsed wedge basepoint."
  (cofiber-basepoint (pointed-smash-cofiber smash)))

(defun smash-collapse-left (smash left-value)
  "Return the path collapsing (left-value, base-right) in SMASH."
  (cofiber-glue (pointed-smash-cofiber smash)
                (wedge-left (pointed-smash-wedge smash) left-value)))

(defun smash-collapse-right (smash right-value)
  "Return the path collapsing (base-left, right-value) in SMASH."
  (cofiber-glue (pointed-smash-cofiber smash)
                (wedge-right (pointed-smash-wedge smash) right-value)))
