;;;; core.lisp --- exact finite-dimensional ker/im quotients over Q.
;;;;
;;;; Row-list matrices act on column vectors.  For A:t-by-m and B:m-by-s,
;;;; this file constructs H=ker(A)/im(B), never a quotient inferred from
;;;; dimensions alone.  Every matrix operation uses CL rational arithmetic.

(in-package #:rosette-exact-linear-subquotient)

;;; Private exact matrices -------------------------------------------------

(defstruct (exact-matrix
             (:constructor %make-exact-matrix (rows columns data))
             (:copier nil))
  (rows 0 :type (integer 0 *) :read-only t)
  (columns 0 :type (integer 0 *) :read-only t)
  (data #() :type simple-vector :read-only t))

(defun %snapshot-proper-list (object name)
  (let ((seen (make-hash-table :test #'eq))
        (cursor object)
        (copy '()))
    (loop while (consp cursor) do
      (when (gethash cursor seen)
        (error "~A must be acyclic." name))
      (setf (gethash cursor seen) t)
      (push (car cursor) copy)
      (setf cursor (cdr cursor)))
    (unless (null cursor)
      (error "~A must be a proper list." name))
    (nreverse copy)))

(defun %snapshot-matrix-rows (object name)
  (let ((outer (%snapshot-proper-list object name))
        (width nil)
        (result '()))
    (loop for row in outer
          for index from 0
          for copy = (%snapshot-proper-list
                      row (format nil "row ~D of ~A" index name)) do
            (if width
                (unless (= width (length copy))
                  (error "~A has inconsistent row lengths ~D and ~D."
                         name width (length copy)))
                (setf width (length copy)))
            (loop for value in copy do
              (unless (rationalp value)
                (error "~A contains non-rational entry ~S." name value)))
            (push copy result))
    (values (nreverse result) width)))

(defun %require-dimension (value name)
  (unless (and (integerp value) (not (minusp value)))
    (error "~A must be a nonnegative integer, got ~S." name value))
  value)

(defun %matrix-from-snapshot (row-lists rows columns name)
  (unless (= rows (length row-lists))
    (error "~A has ~D rows, expected ~D." name (length row-lists) rows))
  (let ((data (make-array (* rows columns))))
    (loop for row in row-lists
          for i from 0 do
            (unless (= columns (length row))
              (error "~A row ~D has length ~D, expected ~D."
                     name i (length row) columns))
            (loop for value in row
                  for j from 0 do
                    (setf (svref data (+ (* i columns) j)) value)))
    (%make-exact-matrix rows columns data)))

(defun %matrix-from-input (object rows columns name)
  (multiple-value-bind (snapshot width)
      (%snapshot-matrix-rows object name)
    (when (and snapshot (/= width columns))
      (error "~A has ~D columns, expected ~D." name width columns))
    (%matrix-from-snapshot
     (or snapshot
         (loop repeat rows collect (make-list columns :initial-element 0)))
     rows columns name)))

(defun %zero-matrix (rows columns)
  (%make-exact-matrix rows columns
                      (make-array (* rows columns) :initial-element 0)))

(defun %entry (matrix row column)
  (svref (exact-matrix-data matrix)
         (+ (* row (exact-matrix-columns matrix)) column)))

(defun (setf %entry) (value matrix row column)
  (setf (svref (exact-matrix-data matrix)
               (+ (* row (exact-matrix-columns matrix)) column))
        value))

(defun %matrix->rows (matrix)
  (loop for i below (exact-matrix-rows matrix)
        collect (loop for j below (exact-matrix-columns matrix)
                      collect (%entry matrix i j))))

(defun %matrix-receipt (matrix)
  (list :shape (list (exact-matrix-rows matrix)
                     (exact-matrix-columns matrix))
        :rows (%matrix->rows matrix)))

(defun %identity-matrix (dimension)
  (let ((result (%zero-matrix dimension dimension)))
    (dotimes (i dimension result)
      (setf (%entry result i i) 1))))

(defun %matrix= (left right)
  (and (= (exact-matrix-rows left) (exact-matrix-rows right))
       (= (exact-matrix-columns left) (exact-matrix-columns right))
       (loop for index below (length (exact-matrix-data left))
             always (= (svref (exact-matrix-data left) index)
                       (svref (exact-matrix-data right) index)))))

(defun %zero-matrix-p (matrix)
  (loop for value across (exact-matrix-data matrix)
        always (zerop value)))

(defun %rational-matrix-p (matrix)
  (and (= (length (exact-matrix-data matrix))
          (* (exact-matrix-rows matrix) (exact-matrix-columns matrix)))
       (loop for value across (exact-matrix-data matrix)
             always (rationalp value))))

(defun %multiply (left right)
  (unless (= (exact-matrix-columns left) (exact-matrix-rows right))
    (error "Cannot multiply shapes (~D ~D) and (~D ~D)."
           (exact-matrix-rows left) (exact-matrix-columns left)
           (exact-matrix-rows right) (exact-matrix-columns right)))
  (let* ((rows (exact-matrix-rows left))
         (middle (exact-matrix-columns left))
         (columns (exact-matrix-columns right))
         (result (%zero-matrix rows columns)))
    (dotimes (i rows result)
      (dotimes (j columns)
        (let ((sum 0))
          (dotimes (k middle)
            (incf sum (* (%entry left i k) (%entry right k j))))
          (setf (%entry result i j) sum))))))

(defun %subtract (left right)
  (unless (and (= (exact-matrix-rows left) (exact-matrix-rows right))
               (= (exact-matrix-columns left)
                  (exact-matrix-columns right)))
    (error "Cannot subtract matrices of unequal shape."))
  (let ((result (%zero-matrix (exact-matrix-rows left)
                              (exact-matrix-columns left))))
    (dotimes (index (length (exact-matrix-data left)) result)
      (setf (svref (exact-matrix-data result) index)
            (- (svref (exact-matrix-data left) index)
               (svref (exact-matrix-data right) index))))))

(defun %transpose (matrix)
  (let ((result (%zero-matrix (exact-matrix-columns matrix)
                              (exact-matrix-rows matrix))))
    (dotimes (i (exact-matrix-rows matrix) result)
      (dotimes (j (exact-matrix-columns matrix))
        (setf (%entry result j i) (%entry matrix i j))))))

(defun %append-columns (left right)
  (unless (= (exact-matrix-rows left) (exact-matrix-rows right))
    (error "Cannot append matrices with different row counts."))
  (let* ((rows (exact-matrix-rows left))
         (left-columns (exact-matrix-columns left))
         (right-columns (exact-matrix-columns right))
         (result (%zero-matrix rows (+ left-columns right-columns))))
    (dotimes (i rows result)
      (dotimes (j left-columns)
        (setf (%entry result i j) (%entry left i j)))
      (dotimes (j right-columns)
        (setf (%entry result i (+ left-columns j)) (%entry right i j))))))

(defun %select-columns (matrix indices)
  (let ((result (%zero-matrix (exact-matrix-rows matrix)
                              (length indices))))
    (loop for source in indices
          for target from 0 do
            (unless (< -1 source (exact-matrix-columns matrix))
              (error "Column index ~D is outside the matrix." source))
            (dotimes (i (exact-matrix-rows matrix))
              (setf (%entry result i target) (%entry matrix i source))))
    result))

(defun %select-rows (matrix indices)
  (let ((result (%zero-matrix (length indices)
                              (exact-matrix-columns matrix))))
    (loop for source in indices
          for target from 0 do
            (unless (< -1 source (exact-matrix-rows matrix))
              (error "Row index ~D is outside the matrix." source))
            (dotimes (j (exact-matrix-columns matrix))
              (setf (%entry result target j) (%entry matrix source j))))
    result))

(defun %row-range (matrix start end)
  (%select-rows matrix (loop for i from start below end collect i)))

;;; Exact RREF and bases ----------------------------------------------------

(defun %copy-matrix (matrix)
  (%make-exact-matrix
   (exact-matrix-rows matrix)
   (exact-matrix-columns matrix)
   (copy-seq (exact-matrix-data matrix))))

(defun %rref (matrix)
  "Return exact RREF, pivot-column indices, and rank."
  (let* ((result (%copy-matrix matrix))
         (rows (exact-matrix-rows result))
         (columns (exact-matrix-columns result))
         (lead-row 0)
         (pivots '()))
    (dotimes (column columns)
      (when (< lead-row rows)
        (let ((pivot (loop for row from lead-row below rows
                           when (not (zerop (%entry result row column)))
                             return row)))
          (when pivot
            (unless (= pivot lead-row)
              (dotimes (j columns)
                (rotatef (%entry result lead-row j)
                         (%entry result pivot j))))
            (let ((value (%entry result lead-row column)))
              (dotimes (j columns)
                (setf (%entry result lead-row j)
                      (/ (%entry result lead-row j) value))))
            (dotimes (row rows)
              (unless (= row lead-row)
                (let ((factor (%entry result row column)))
                  (unless (zerop factor)
                    (dotimes (j columns)
                      (decf (%entry result row j)
                            (* factor (%entry result lead-row j))))))))
            (push column pivots)
            (incf lead-row)))))
    (values result (nreverse pivots) lead-row)))

(defun %rank (matrix)
  (nth-value 2 (%rref matrix)))

(defun %null-space-basis (matrix)
  (multiple-value-bind (reduced pivots rank)
      (%rref matrix)
    (declare (ignore rank))
    (let* ((ambient (exact-matrix-columns matrix))
           (pivot-p (make-array ambient :initial-element nil))
           (free-columns '()))
      (dolist (pivot pivots)
        (setf (aref pivot-p pivot) t))
      (dotimes (column ambient)
        (unless (aref pivot-p column)
          (push column free-columns)))
      (setf free-columns (nreverse free-columns))
      (let ((basis (%zero-matrix ambient (length free-columns))))
        (loop for free in free-columns
              for basis-column from 0 do
                (setf (%entry basis free basis-column) 1)
                (loop for pivot-row from 0
                      for pivot-column in pivots do
                        (setf (%entry basis pivot-column basis-column)
                              (- (%entry reduced pivot-row free)))))
        basis))))

(defun %column-space-basis (matrix)
  (multiple-value-bind (reduced pivots rank)
      (%rref matrix)
    (declare (ignore reduced rank))
    (%select-columns matrix pivots)))

(defun %basis-complement (subspace full-space)
  "Choose columns of FULL-SPACE extending SUBSPACE to the same span."
  (let ((current subspace)
        (representatives (%zero-matrix (exact-matrix-rows full-space) 0))
        (current-rank (%rank subspace))
        (full-rank (%rank full-space)))
    (unless (= full-rank (%rank (%append-columns full-space subspace)))
      (error "The proposed subspace is not contained in the full space."))
    (dotimes (column (exact-matrix-columns full-space))
      (when (< current-rank full-rank)
        (let* ((candidate-column (%select-columns full-space (list column)))
               (candidate (%append-columns current candidate-column))
               (candidate-rank (%rank candidate)))
          (when (> candidate-rank current-rank)
            (setf current candidate
                  representatives
                  (%append-columns representatives candidate-column)
                  current-rank candidate-rank)))))
    (unless (= current-rank full-rank)
      (error "Could not extend the subspace to the requested full space."))
    representatives))

(defun %inverse (matrix)
  (let ((dimension (exact-matrix-rows matrix)))
    (unless (= dimension (exact-matrix-columns matrix))
      (error "Only square matrices can be inverted."))
    (let ((augmented (%append-columns matrix (%identity-matrix dimension))))
      (multiple-value-bind (reduced pivots rank)
          (%rref augmented)
        (declare (ignore pivots))
        (unless (= rank dimension)
          (error "Matrix is singular."))
        (%select-columns reduced
                         (loop for j from dimension below (* 2 dimension)
                               collect j))))))

(defun %left-inverse (matrix)
  "Return L with L*M=I for a full-column-rank M."
  (let ((rows (exact-matrix-rows matrix))
        (columns (exact-matrix-columns matrix)))
    (multiple-value-bind (reduced pivot-rows rank)
        (%rref (%transpose matrix))
      (declare (ignore reduced))
      (unless (= rank columns)
        (error "Matrix does not have full column rank."))
      (let* ((square (%select-rows matrix pivot-rows))
             (selector (%zero-matrix columns rows)))
        (loop for source-row in pivot-rows
              for target-row from 0 do
                (setf (%entry selector target-row source-row) 1))
        (%multiply (%inverse square) selector)))))

(defun %column-lists (matrix)
  (loop for j below (exact-matrix-columns matrix)
        collect (loop for i below (exact-matrix-rows matrix)
                      collect (%entry matrix i j))))

(defun %vector-column (object dimension name)
  (let ((values (%snapshot-proper-list object name)))
    (unless (= dimension (length values))
      (error "~A has length ~D, expected ~D."
             name (length values) dimension))
    (dolist (value values)
      (unless (rationalp value)
        (error "~A contains non-rational entry ~S." name value)))
    (%matrix-from-snapshot (mapcar #'list values) dimension 1 name)))

(defun %only-column (matrix)
  (unless (= 1 (exact-matrix-columns matrix))
    (error "Internal vector result is not a single column."))
  (loop for i below (exact-matrix-rows matrix)
        collect (%entry matrix i 0)))

;;; Subquotient carrier -----------------------------------------------------

(defstruct (linear-subquotient
             (:constructor %make-linear-subquotient
                 (target-dimension ambient-dimension source-dimension
                  boundary-out boundary-in kernel-basis image-basis
                  representatives coordinate-map projection retraction))
             (:conc-name %linear-subquotient-)
             (:copier nil))
  (target-dimension 0 :type (integer 0 *) :read-only t)
  (ambient-dimension 0 :type (integer 0 *) :read-only t)
  (source-dimension 0 :type (integer 0 *) :read-only t)
  (boundary-out (error "BOUNDARY-OUT required") :type exact-matrix
                :read-only t)
  (boundary-in (error "BOUNDARY-IN required") :type exact-matrix
               :read-only t)
  (kernel-basis (error "KERNEL-BASIS required") :type exact-matrix
                :read-only t)
  (image-basis (error "IMAGE-BASIS required") :type exact-matrix
               :read-only t)
  (representatives (error "REPRESENTATIVES required") :type exact-matrix
                   :read-only t)
  (coordinate-map (error "COORDINATE-MAP required") :type exact-matrix
                  :read-only t)
  (projection (error "PROJECTION required") :type exact-matrix
              :read-only t)
  (retraction (error "RETRACTION required") :type exact-matrix
              :read-only t))

(defun make-linear-subquotient
    (boundary-out boundary-in
     &key middle-dimension source-dimension target-dimension)
  "Construct the exact space ker(BOUNDARY-OUT)/im(BOUNDARY-IN).

Matrices are proper row lists over Q and act on column vectors:
BOUNDARY-OUT is target-by-middle; BOUNDARY-IN is middle-by-source.  Explicit
dimension keywords disambiguate zero-row matrices and otherwise must agree
with the supplied shape.  A nonzero A*B residual is rejected."
  (multiple-value-bind (out-rows out-width)
      (%snapshot-matrix-rows boundary-out "BOUNDARY-OUT")
    (multiple-value-bind (in-rows in-width)
        (%snapshot-matrix-rows boundary-in "BOUNDARY-IN")
      (when middle-dimension
        (%require-dimension middle-dimension "MIDDLE-DIMENSION"))
      (when source-dimension
        (%require-dimension source-dimension "SOURCE-DIMENSION"))
      (when target-dimension
        (%require-dimension target-dimension "TARGET-DIMENSION"))
      (let* ((ambient (or middle-dimension
                          (and in-rows (length in-rows))
                          out-width
                          0))
             (target (or target-dimension (length out-rows)))
             (source (or source-dimension in-width 0)))
        (when (and in-rows (/= (length in-rows) ambient))
          (error "BOUNDARY-IN has ~D rows but the middle dimension is ~D."
                 (length in-rows) ambient))
        (when (and out-width (/= out-width ambient))
          (error "BOUNDARY-OUT has ~D columns but the middle dimension is ~D."
                 out-width ambient))
        (when out-rows
          (unless (= target (length out-rows))
            (error "TARGET-DIMENSION is ~D but BOUNDARY-OUT has ~D rows."
                   target (length out-rows))))
        (when (and in-width (/= source in-width))
          (error "SOURCE-DIMENSION is ~D but BOUNDARY-IN has ~D columns."
                 source in-width))
        ;; NIL carries no row count. Explicit or surrounding dimensions make
        ;; it an unambiguous zero matrix of the required shape.
        (let* ((normalized-out
                 (or out-rows
                     (loop repeat target
                           collect (make-list ambient :initial-element 0))))
               (normalized-in
                 (or in-rows
                     (loop repeat ambient
                           collect (make-list source :initial-element 0))))
               (a (%matrix-from-snapshot normalized-out target ambient
                                         "BOUNDARY-OUT"))
               (b (%matrix-from-snapshot normalized-in ambient source
                                         "BOUNDARY-IN"))
               (chain-residual (%multiply a b)))
          (unless (%zero-matrix-p chain-residual)
            (error "BOUNDARY-OUT*BOUNDARY-IN is nonzero; residual ~S."
                   (%matrix->rows chain-residual)))
          (let* ((kernel (%null-space-basis a))
                 (image (%column-space-basis b))
                 (representatives (%basis-complement image kernel))
                 (kernel-dimension (exact-matrix-columns kernel))
                 (image-dimension (exact-matrix-columns image))
                 (quotient-dimension
                   (exact-matrix-columns representatives))
                 (decomposition (%append-columns image representatives))
                 (coordinates (%left-inverse decomposition))
                 (projection (%row-range coordinates image-dimension
                                         kernel-dimension))
                 (retraction (%multiply representatives projection))
                 (result
                   (%make-linear-subquotient
                    target ambient source a b kernel image representatives
                    coordinates projection retraction)))
            (unless (= quotient-dimension
                       (- kernel-dimension image-dimension))
              (error "Internal quotient-dimension invariant failed."))
            (unless (linear-subquotient-valid-p result)
              (error "Internal subquotient certificate failed."))
            result))))))

(defun linear-subquotient-dimension (subquotient)
  (unless (linear-subquotient-p subquotient)
    (error "Expected a LINEAR-SUBQUOTIENT, got ~S." subquotient))
  (exact-matrix-columns
   (%linear-subquotient-representatives subquotient)))

(defun linear-subquotient-ambient-dimension (subquotient)
  (unless (linear-subquotient-p subquotient)
    (error "Expected a LINEAR-SUBQUOTIENT, got ~S." subquotient))
  (%linear-subquotient-ambient-dimension subquotient))

(defun linear-subquotient-representatives (subquotient)
  "Return fresh ambient column vectors representing a quotient basis."
  (unless (linear-subquotient-p subquotient)
    (error "Expected a LINEAR-SUBQUOTIENT, got ~S." subquotient))
  (%column-lists (%linear-subquotient-representatives subquotient)))

(defun linear-subquotient-class-coordinates (subquotient vector)
  "Return the exact quotient coordinates of an ambient cycle VECTOR."
  (unless (linear-subquotient-p subquotient)
    (error "Expected a LINEAR-SUBQUOTIENT, got ~S." subquotient))
  (let* ((column (%vector-column
                  vector (%linear-subquotient-ambient-dimension subquotient)
                  "VECTOR"))
         (cycle-residual
           (%multiply (%linear-subquotient-boundary-out subquotient) column)))
    (unless (%zero-matrix-p cycle-residual)
      (error "VECTOR is not in ker(A); residual ~S."
             (%matrix->rows cycle-residual)))
    (%only-column
     (%multiply (%linear-subquotient-projection subquotient) column))))

(defun %subquotient-residuals (subquotient)
  (let* ((a (%linear-subquotient-boundary-out subquotient))
         (b (%linear-subquotient-boundary-in subquotient))
         (kernel (%linear-subquotient-kernel-basis subquotient))
         (image (%linear-subquotient-image-basis subquotient))
         (representatives
           (%linear-subquotient-representatives subquotient))
         (coordinates (%linear-subquotient-coordinate-map subquotient))
         (projection (%linear-subquotient-projection subquotient))
         (retraction (%linear-subquotient-retraction subquotient))
         (image-dimension (exact-matrix-columns image))
         (kernel-dimension (exact-matrix-columns kernel))
         (quotient-dimension (exact-matrix-columns representatives))
         (image-coordinates (%row-range coordinates 0 image-dimension)))
    (list
     :chain (%multiply a b)
     :kernel (%multiply a kernel)
     :image-containment (%multiply a image)
     :projection-image (%multiply projection image)
     :projection-lift
     (%subtract (%multiply projection representatives)
                (%identity-matrix quotient-dimension))
     :coordinate-decomposition
     (%subtract (%multiply coordinates
                          (%append-columns image representatives))
                (%identity-matrix kernel-dimension))
     :retraction-idempotence
     (%subtract (%multiply retraction retraction) retraction)
     :retraction-lift
     (%subtract (%multiply retraction representatives) representatives)
     :kernel-retraction
     (%subtract
      (%subtract kernel (%multiply retraction kernel))
      (%multiply image (%multiply image-coordinates kernel))))))

(defun linear-subquotient-valid-p (subquotient)
  "Recompute every exact law carried by SUBQUOTIENT."
  (and
   (linear-subquotient-p subquotient)
   (handler-case
       (let* ((a (%linear-subquotient-boundary-out subquotient))
              (b (%linear-subquotient-boundary-in subquotient))
              (ambient (%linear-subquotient-ambient-dimension subquotient))
              (target (%linear-subquotient-target-dimension subquotient))
              (source (%linear-subquotient-source-dimension subquotient))
              (kernel (%linear-subquotient-kernel-basis subquotient))
              (image (%linear-subquotient-image-basis subquotient))
              (representatives
                (%linear-subquotient-representatives subquotient))
              (coordinates (%linear-subquotient-coordinate-map subquotient))
              (projection (%linear-subquotient-projection subquotient))
              (retraction (%linear-subquotient-retraction subquotient))
              (kernel-dimension (exact-matrix-columns kernel))
              (image-dimension (exact-matrix-columns image))
              (quotient-dimension
                (exact-matrix-columns representatives))
              (residuals (%subquotient-residuals subquotient)))
         (and
          (every #'%rational-matrix-p
                 (list a b kernel image representatives coordinates
                       projection retraction))
          (= target (exact-matrix-rows a))
          (= ambient (exact-matrix-columns a))
          (= ambient (exact-matrix-rows b))
          (= source (exact-matrix-columns b))
          (= ambient (exact-matrix-rows kernel)
                     (exact-matrix-rows image)
                     (exact-matrix-rows representatives))
          (= kernel-dimension (- ambient (%rank a)))
          (= image-dimension (%rank b) (%rank image))
          (= quotient-dimension (- kernel-dimension image-dimension))
          (= kernel-dimension (%rank kernel)
             (%rank (%append-columns image representatives)))
          (= kernel-dimension (exact-matrix-rows coordinates))
          (= ambient (exact-matrix-columns coordinates))
          (= quotient-dimension (exact-matrix-rows projection))
          (= ambient (exact-matrix-columns projection))
          (%matrix= projection
                    (%row-range coordinates image-dimension kernel-dimension))
          (= ambient (exact-matrix-rows retraction)
             (exact-matrix-columns retraction))
          (%matrix= retraction (%multiply representatives projection))
          (loop for (name residual) on residuals by #'cddr
                always (and name (%zero-matrix-p residual)))))
     (error () nil))))

(defun linear-subquotient-receipt (subquotient)
  "Return a fresh, executable-shape receipt for the quotient laws."
  (unless (linear-subquotient-p subquotient)
    (error "Expected a LINEAR-SUBQUOTIENT, got ~S." subquotient))
  (let* ((kernel (%linear-subquotient-kernel-basis subquotient))
         (image (%linear-subquotient-image-basis subquotient))
         (representatives
           (%linear-subquotient-representatives subquotient))
         (residuals (%subquotient-residuals subquotient)))
    (list
     :kind :exact-linear-subquotient
     :target-dimension (%linear-subquotient-target-dimension subquotient)
     :ambient-dimension (%linear-subquotient-ambient-dimension subquotient)
     :source-dimension (%linear-subquotient-source-dimension subquotient)
     :kernel-dimension (exact-matrix-columns kernel)
     :image-dimension (exact-matrix-columns image)
     :dimension (exact-matrix-columns representatives)
     :boundary-out (%matrix-receipt
                    (%linear-subquotient-boundary-out subquotient))
     :boundary-in (%matrix-receipt
                   (%linear-subquotient-boundary-in subquotient))
     :kernel-basis (%matrix-receipt kernel)
     :image-basis (%matrix-receipt image)
     :projection (%matrix-receipt
                  (%linear-subquotient-projection subquotient))
     :lift (%matrix-receipt representatives)
     :retraction (%matrix-receipt
                  (%linear-subquotient-retraction subquotient))
     :residuals
     (loop for (name residual) on residuals by #'cddr
           append (list name (%matrix-receipt residual)))
     :valid-p (linear-subquotient-valid-p subquotient))))

;;; Descent ---------------------------------------------------------------

(define-condition linear-descent-error (error)
  ((kind :initarg :kind :reader %linear-descent-error-kind)
   (residual :initarg :residual :reader %linear-descent-error-residual))
  (:report
   (lambda (condition stream)
     (format stream "Linear map fails ~A; residual ~S."
             (%linear-descent-error-kind condition)
             (%matrix->rows (%linear-descent-error-residual condition))))))

(defun linear-descent-error-kind (condition)
  (%linear-descent-error-kind condition))

(defun linear-descent-error-residual (condition)
  (%matrix->rows (%linear-descent-error-residual condition)))

(defstruct (linear-map-descent
             (:constructor %make-linear-map-descent
                 (domain codomain ambient-map induced-matrix
                  cycle-residual relation-residual))
             (:copier nil))
  (domain (error "DOMAIN required") :type linear-subquotient :read-only t)
  (codomain (error "CODOMAIN required") :type linear-subquotient :read-only t)
  (ambient-map (error "AMBIENT-MAP required") :type exact-matrix :read-only t)
  (induced-matrix (error "INDUCED-MATRIX required") :type exact-matrix
                  :read-only t)
  (cycle-residual (error "CYCLE-RESIDUAL required") :type exact-matrix
                  :read-only t)
  (relation-residual (error "RELATION-RESIDUAL required") :type exact-matrix
                     :read-only t))

(defun descend-linear-map (matrix domain codomain)
  "Descend MATRIX from DOMAIN to CODOMAIN, or signal LINEAR-DESCENT-ERROR.

MATRIX is codomain-ambient by domain-ambient.  The first residual checks
F(ker A_domain) is in ker A_codomain.  Only after it vanishes does the second
check that F(im B_domain) lies in im B_codomain."
  (unless (and (linear-subquotient-p domain)
               (linear-subquotient-valid-p domain))
    (error "DOMAIN must be a valid LINEAR-SUBQUOTIENT."))
  (unless (and (linear-subquotient-p codomain)
               (linear-subquotient-valid-p codomain))
    (error "CODOMAIN must be a valid LINEAR-SUBQUOTIENT."))
  (let* ((ambient-map
           (%matrix-from-input
            matrix
            (%linear-subquotient-ambient-dimension codomain)
            (%linear-subquotient-ambient-dimension domain)
            "MATRIX"))
         (cycle-residual
           (%multiply
            (%linear-subquotient-boundary-out codomain)
            (%multiply ambient-map
                       (%linear-subquotient-kernel-basis domain)))))
    (unless (%zero-matrix-p cycle-residual)
      (error 'linear-descent-error
             :kind :cycle-preservation
             :residual cycle-residual))
    (let ((relation-residual
            (%multiply
             (%linear-subquotient-projection codomain)
             (%multiply ambient-map
                        (%linear-subquotient-image-basis domain)))))
      (unless (%zero-matrix-p relation-residual)
        (error 'linear-descent-error
               :kind :relation-preservation
               :residual relation-residual))
      (let* ((induced
               (%multiply
                (%linear-subquotient-projection codomain)
                (%multiply ambient-map
                           (%linear-subquotient-representatives domain))))
             (descent
               (%make-linear-map-descent
                domain codomain ambient-map induced
                cycle-residual relation-residual)))
        (unless (linear-map-descent-valid-p descent)
          (error "Internal map-descent certificate failed."))
        descent))))

(defun linear-map-descent-matrix (descent)
  "Return a fresh row-list matrix for the induced quotient map."
  (unless (linear-map-descent-p descent)
    (error "Expected a LINEAR-MAP-DESCENT, got ~S." descent))
  (%matrix->rows (linear-map-descent-induced-matrix descent)))

(defun linear-map-descent-valid-p (descent)
  (and
   (linear-map-descent-p descent)
   (handler-case
       (let* ((domain (linear-map-descent-domain descent))
              (codomain (linear-map-descent-codomain descent))
              (ambient-map (linear-map-descent-ambient-map descent))
              (cycle
                (%multiply
                 (%linear-subquotient-boundary-out codomain)
                 (%multiply ambient-map
                            (%linear-subquotient-kernel-basis domain))))
              (relation
                (%multiply
                 (%linear-subquotient-projection codomain)
                 (%multiply ambient-map
                            (%linear-subquotient-image-basis domain))))
              (induced
                (%multiply
                 (%linear-subquotient-projection codomain)
                 (%multiply ambient-map
                            (%linear-subquotient-representatives domain)))))
         (and (linear-subquotient-valid-p domain)
              (linear-subquotient-valid-p codomain)
              (%rational-matrix-p ambient-map)
              (= (exact-matrix-rows ambient-map)
                 (%linear-subquotient-ambient-dimension codomain))
              (= (exact-matrix-columns ambient-map)
                 (%linear-subquotient-ambient-dimension domain))
              (%zero-matrix-p cycle)
              (%zero-matrix-p relation)
              (%matrix= cycle (linear-map-descent-cycle-residual descent))
              (%matrix= relation
                        (linear-map-descent-relation-residual descent))
              (%matrix= induced
                        (linear-map-descent-induced-matrix descent))))
     (error () nil))))

(defun linear-map-descent-receipt (descent)
  "Return a fresh receipt for ambient and induced maps and both walls."
  (unless (linear-map-descent-p descent)
    (error "Expected a LINEAR-MAP-DESCENT, got ~S." descent))
  (list
   :kind :exact-linear-map-descent
   :domain-dimension
   (linear-subquotient-dimension (linear-map-descent-domain descent))
   :codomain-dimension
   (linear-subquotient-dimension (linear-map-descent-codomain descent))
   :ambient-map (%matrix-receipt (linear-map-descent-ambient-map descent))
   :matrix (%matrix-receipt (linear-map-descent-induced-matrix descent))
   :cycle-residual
   (%matrix-receipt (linear-map-descent-cycle-residual descent))
   :relation-residual
   (%matrix-receipt (linear-map-descent-relation-residual descent))
   :valid-p (linear-map-descent-valid-p descent)))
