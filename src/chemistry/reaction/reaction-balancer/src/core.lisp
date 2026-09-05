;;;; core.lisp --- Exact formula-based reaction balancing.

(in-package #:rosette-reaction-balancer)

(defstruct (reaction-balance (:constructor %make-reaction-balance))
  (name "" :type string)
  reactants
  products
  coefficients
  stoich
  reaction
  elements
  residuals
  balanced-p
  note)

(defun %component-key (key)
  (etypecase key
    (symbol key)
    (string (intern (string-upcase key) "KEYWORD"))))

(defun %component-keys (keys)
  (let ((result (mapcar #'%component-key keys)))
    (unless result
      (error "Reaction side must contain at least one component."))
    (when (/= (length result) (length (remove-duplicates result)))
      (error "Duplicate components in reaction side: ~S" keys))
    result))

(defun %component-formula (catalog key)
  (let ((formula (pe:component-formula
                  (pe:catalog-component catalog key))))
    (unless formula
      (error "Component ~S has no formula and cannot be balanced." key))
    formula))

(defun %formula-elements (formula)
  (mapcar #'car formula))

(defun %element-count (element formula)
  (let ((entry (assoc element formula :test #'string=)))
    (if entry
        (rationalize (cdr entry))
        0)))

(defun %reaction-elements (catalog components)
  (sort
   (remove-duplicates
    (loop for component in components
          append (%formula-elements (%component-formula catalog component)))
    :test #'string=)
   #'string<))

(defun %stoich-matrix (catalog reactants products elements)
  (let ((reactant-formulas
          (mapcar (lambda (key) (%component-formula catalog key)) reactants))
        (product-formulas
          (mapcar (lambda (key) (%component-formula catalog key)) products)))
    (loop for element in elements
          collect
          (append
           (loop for formula in reactant-formulas
                 collect (%element-count element formula))
           (loop for formula in product-formulas
                 collect (- (%element-count element formula)))))))

(defun %matrix-dimensions (matrix)
  (values (length matrix)
          (if matrix (length (first matrix)) 0)))

(defun %copy-matrix (matrix)
  (map 'vector
       (lambda (row)
         (map 'vector #'rationalize row))
       matrix))

(defun %swap-rows (matrix a b)
  (rotatef (aref matrix a) (aref matrix b)))

(defun %rref (matrix)
  (multiple-value-bind (row-count column-count)
      (%matrix-dimensions matrix)
    (let ((mat (%copy-matrix matrix))
          (row 0)
          (pivots nil))
      (loop for column from 0 below column-count
            while (< row row-count)
            do
               (let ((pivot-row
                       (loop for r from row below row-count
                             when (not (zerop (aref (aref mat r) column)))
                               return r)))
                 (when pivot-row
                   (%swap-rows mat row pivot-row)
                   (let ((pivot (aref (aref mat row) column)))
                     (loop for c from column below column-count
                           do (setf (aref (aref mat row) c)
                                    (/ (aref (aref mat row) c) pivot))))
                   (loop for r from 0 below row-count
                         unless (= r row)
                           do
                              (let ((factor (aref (aref mat r) column)))
                                (unless (zerop factor)
                                  (loop for c from column below column-count
                                        do (setf
                                            (aref (aref mat r) c)
                                            (- (aref (aref mat r) c)
                                               (* factor
                                                  (aref (aref mat row)
                                                        c))))))))
                   (push column pivots)
                   (incf row))))
      (values mat (nreverse pivots)))))

(defun %enumerate-positive-tuples (width max-value)
  (if (zerop width)
      (list nil)
      (loop for value from 1 to max-value
            append
            (mapcar (lambda (tail) (cons value tail))
                    (%enumerate-positive-tuples (1- width) max-value)))))

(defun %free-assignments (free-count)
  (cond
    ((= free-count 1) '((1)))
    ((<= free-count 3) (%enumerate-positive-tuples free-count 6))
    (t (cons (loop repeat free-count collect 1)
             (loop for index from 0 below free-count
                   collect
                   (loop for j from 0 below free-count
                         collect (if (= index j) 1 0)))))))

(defun %solution-from-free-values (rref pivots free-columns free-values)
  (let ((solution (make-array (+ (length pivots) (length free-columns))
                              :initial-element 0)))
    (loop for column in free-columns
          for value in free-values
          do (setf (aref solution column) value))
    (loop for row from 0
          for pivot in pivots
          do (setf (aref solution pivot)
                   (- (loop for column in free-columns
                            sum (* (aref (aref rref row) column)
                                   (aref solution column))))))
    (coerce solution 'list)))

(defun %same-sign-positive (values)
  (cond
    ((every #'plusp values) values)
    ((every #'minusp values) (mapcar #'- values))
    (t nil)))

(defun %primitive-integers (values)
  (let* ((scale (reduce #'lcm values
                        :key #'denominator
                        :initial-value 1))
         (integers (mapcar (lambda (value)
                             (truncate (* value scale)))
                           values))
         (divisor (reduce #'gcd integers
                          :key #'abs
                          :initial-value 0))
         (primitive (mapcar (lambda (value) (/ value divisor))
                            integers)))
    (if (minusp (first primitive))
        (mapcar #'- primitive)
        primitive)))

(defun %positive-null-vector (matrix)
  (multiple-value-bind (rref pivots)
      (%rref matrix)
    (multiple-value-bind (row-count column-count)
        (%matrix-dimensions matrix)
      (declare (ignore row-count))
      (let ((free-columns
              (loop for column from 0 below column-count
                    unless (member column pivots)
                      collect column)))
        (unless free-columns
          (error "Reaction formula matrix has no nontrivial nullspace."))
        (dolist (assignment (%free-assignments (length free-columns)))
          (let ((positive
                  (%same-sign-positive
                   (%solution-from-free-values
                    rref pivots free-columns assignment))))
            (when (and positive (every (lambda (value)
                                         (not (zerop value)))
                                       positive))
              (return-from %positive-null-vector
                (%primitive-integers positive)))))
        (error "Could not find positive stoichiometric coefficients.")))))

(defun %stoich-from-coefficients (reactants products coefficients)
  (let ((reactant-count (length reactants)))
    (append
     (loop for key in reactants
           for coefficient in coefficients
           collect (cons key (- coefficient)))
     (loop for key in products
           for coefficient in (subseq coefficients reactant-count)
           collect (cons key coefficient)))))

(defun balance-reaction
    (name reactants products &key
       (catalog pe:*default-component-catalog*)
       note)
  "Balance REACTANTS -> PRODUCTS using component formulas from CATALOG.

The result carries both the exact integer coefficients and the native
`rosette-process-engineering` reaction object used by reaction networks and unit
operations."
  (check-type name string)
  (let* ((reactants (%component-keys reactants))
         (products (%component-keys products))
         (components (append reactants products)))
    (when (intersection reactants products)
      (error "Components cannot appear on both sides of a balanced reaction: ~S"
             (intersection reactants products)))
    (let* ((elements (%reaction-elements catalog components))
           (matrix (%stoich-matrix catalog reactants products elements))
           (coefficients (%positive-null-vector matrix))
           (stoich (%stoich-from-coefficients
                    reactants products coefficients))
           (reaction (pe:make-reaction name stoich :note note))
           (residuals (pe:reaction-element-residuals reaction
                                                     :catalog catalog))
           (balanced-p (pe:reaction-balanced-p reaction
                                               :catalog catalog)))
      (%make-reaction-balance
       :name name
       :reactants (copy-list reactants)
       :products (copy-list products)
       :coefficients (copy-list coefficients)
       :stoich (copy-tree stoich)
       :reaction reaction
       :elements (copy-list elements)
       :residuals residuals
       :balanced-p balanced-p
       :note note))))

(defun reaction-balance->plist (balance)
  "Return a stable plist representation of BALANCE."
  (check-type balance reaction-balance)
  (list :name (reaction-balance-name balance)
        :reactants (copy-list (reaction-balance-reactants balance))
        :products (copy-list (reaction-balance-products balance))
        :coefficients (copy-list (reaction-balance-coefficients balance))
        :stoich (copy-tree (reaction-balance-stoich balance))
        :elements (copy-list (reaction-balance-elements balance))
        :residuals (copy-tree (reaction-balance-residuals balance))
        :balanced-p (reaction-balance-balanced-p balance)
        :reaction (pe:reaction->plist
                   (reaction-balance-reaction balance))
        :note (reaction-balance-note balance)))
