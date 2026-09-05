;;;; chain-map.lisp --- exact chain maps and contravariant H^n pullback.

(in-package #:rosette-chain-complex)

(defun %proper-list-p (object)
  (handler-case (progn (length object) t)
    (type-error () nil)))

(defun %canonical-exact-matrix (matrix rows columns label)
  "Validate MATRIX as ROWS x COLUMNS over Q; NIL denotes any shaped zero map."
  (when (null matrix) (return-from %canonical-exact-matrix nil))
  (unless (and (%proper-list-p matrix) (= (length matrix) rows))
    (error "~A must have ~D rows (or be NIL for zero)." label rows))
  (loop for row in matrix for index from 0 do
    (unless (and (%proper-list-p row) (= (length row) columns))
      (error "Row ~D of ~A must have ~D entries." index label columns))
    (unless (every #'rationalp row)
      (error "~A must contain exact rational entries." label)))
  (copy-tree matrix))

(defun %matrix-residual (left right rows columns)
  (when (zerop rows) (return-from %matrix-residual nil))
  (loop for row below rows
        collect (loop for column below columns
                      collect (- (if left (nth column (nth row left)) 0)
                                 (if right (nth column (nth row right)) 0)))))

(defstruct (chain-map
            (:constructor %make-chain-map)
            (:conc-name %chain-map-)
            (:copier nil))
  (source nil :read-only t)
  (target nil :read-only t)
  (matrices nil :type list :read-only t)
  (receipt nil :type list :read-only t))

(defun %chain-map-law-residuals (source target matrices)
  "Residuals d^target_n f_n - f_(n-1) d^source_n for n >= 1."
  (loop for degree from 1 to (chain-complex-top source)
        for rows = (nth (1- degree) (chain-complex-dims target))
        for columns = (nth degree (chain-complex-dims source))
        for left = (%matmul (boundary-matrix target degree)
                            (nth degree matrices))
        for right = (%matmul (nth (1- degree) matrices)
                             (boundary-matrix source degree))
        collect (list :degree degree
                      :residual (%matrix-residual left right rows columns))))

(defun make-chain-map (source target matrices)
  "Construct an exact degree-preserving chain map SOURCE -> TARGET.

MATRICES contains f_n : C_n(SOURCE) -> C_n(TARGET), one row-list matrix per
degree; NIL denotes a shaped zero map.  Construction proves
d^TARGET_n f_n = f_(n-1) d^SOURCE_n in every positive degree."
  (unless (and (chain-complex-p source) (chain-complex-p target))
    (error "SOURCE and TARGET must both be chain complexes."))
  (unless (and (d-squared-zero-p source) (d-squared-zero-p target))
    (error "A chain map requires source and target with d^2 = 0."))
  (unless (and (%exact-rational-chain-complex-p source)
               (%exact-rational-chain-complex-p target))
    (error "An exact chain map requires rational source and target boundaries."))
  (unless (= (chain-complex-top source) (chain-complex-top target))
    (error "This finite chain-map carrier requires equal top degrees."))
  (let ((degree-count (length (chain-complex-dims source))))
    (unless (= (length matrices) degree-count)
      (error "Expected ~D per-degree matrices, received ~D."
             degree-count (length matrices)))
    (let* ((source-copy (%copy-chain-complex source))
           (target-copy (%copy-chain-complex target))
           (canonical
             (loop for matrix in matrices
                   for degree from 0
                   collect
                   (%canonical-exact-matrix
                    matrix
                    (nth degree (chain-complex-dims target-copy))
                    (nth degree (chain-complex-dims source-copy))
                    (format nil "f_~D" degree))))
           (residuals (%chain-map-law-residuals
                       source-copy target-copy canonical)))
      (dolist (entry residuals)
        (unless (%zero-matrix-p (getf entry :residual))
          (error "Chain-map law fails in degree ~D with residual ~S."
                 (getf entry :degree) (getf entry :residual))))
      (%make-chain-map
       :source source-copy :target target-copy :matrices canonical
       :receipt
       (list :version 1
             :construction :exact-chain-map
             :source-dimensions (copy-list (chain-complex-dims source-copy))
             :target-dimensions (copy-list (chain-complex-dims target-copy))
             :source-boundaries
             (copy-tree (chain-complex-boundaries source-copy))
             :target-boundaries
             (copy-tree (chain-complex-boundaries target-copy))
             :matrices (copy-tree canonical)
             :matrix-shapes
             (loop for degree below degree-count
                   collect (list (nth degree (chain-complex-dims target-copy))
                                 (nth degree (chain-complex-dims source-copy))))
             :chain-law-residuals (copy-tree residuals))))))

(defun chain-map-source (map) (%copy-chain-complex (%chain-map-source map)))
(defun chain-map-target (map) (%copy-chain-complex (%chain-map-target map)))
(defun chain-map-matrices (map) (copy-tree (%chain-map-matrices map)))
(defun chain-map-matrix (map degree)
  (when (and (integerp degree) (<= 0 degree (chain-complex-top (%chain-map-source map))))
    (copy-tree (nth degree (%chain-map-matrices map)))))
(defun chain-map-receipt (map) (copy-tree (%chain-map-receipt map)))

(defun chain-map-valid-p (map)
  "Replay every shape and chain-law check represented by MAP."
  (and (chain-map-p map)
       (handler-case
           (let ((replayed (make-chain-map (%chain-map-source map)
                                           (%chain-map-target map)
                                           (%chain-map-matrices map))))
             (equal (%chain-map-receipt map) (%chain-map-receipt replayed)))
         (error () nil))))

(defun %identity-matrix (dimension)
  (loop for row below dimension
        collect (loop for column below dimension
                      collect (if (= row column) 1 0))))

(defun identity-chain-map (complex)
  "The exact identity chain map on COMPLEX."
  (make-chain-map complex complex
                  (mapcar #'%identity-matrix (chain-complex-dims complex))))

(defun compose-chain-maps (after before)
  "Compose BEFORE : A -> B and AFTER : B -> C, returning AFTER o BEFORE."
  (unless (and (chain-map-p before) (chain-map-p after)
               (chain-map-valid-p before) (chain-map-valid-p after))
    (error "Both operands must be valid chain maps."))
  (let ((middle-left (%chain-map-target before))
        (middle-right (%chain-map-source after)))
    (unless (and (equal (chain-complex-dims middle-left)
                        (chain-complex-dims middle-right))
                 (equal (chain-complex-boundaries middle-left)
                        (chain-complex-boundaries middle-right)))
      (error "Chain-map composition has unequal intermediate complexes."))
    (make-chain-map
     (%chain-map-source before) (%chain-map-target after)
     (mapcar #'%matmul (%chain-map-matrices after)
             (%chain-map-matrices before)))))

(defstruct (induced-cohomology-map
            (:constructor %make-induced-cohomology-map)
            (:conc-name %induced-cohomology-map-)
            (:copier nil))
  (chain-map nil :read-only t)
  (degree 0 :type (integer 0 *) :read-only t)
  (domain nil :read-only t)
  (codomain nil :read-only t)
  (descent nil :read-only t)
  (receipt nil :type list :read-only t))

(defun induced-cohomology-map (map degree)
  "The contravariant pullback H^n(TARGET;Q) -> H^n(SOURCE;Q) induced by MAP."
  (unless (and (chain-map-p map) (chain-map-valid-p map))
    (error "MAP must be a valid chain map."))
  (let* ((domain (make-rational-cohomology-space
                  (%chain-map-target map) degree))
         (codomain (make-rational-cohomology-space
                    (%chain-map-source map) degree))
         (descent
           (descend-linear-map
            (%matrix-or-shaped-zero
             (%transpose (nth degree (%chain-map-matrices map)))
             (nth degree (chain-complex-dims (%chain-map-source map)))
             (nth degree (chain-complex-dims (%chain-map-target map))))
            (%rational-cohomology-space-subquotient domain)
            (%rational-cohomology-space-subquotient codomain)))
         (receipt
           (list :version 1
                 :construction :contravariant-rational-cohomology-map
                 :degree degree
                 :domain :target-cohomology
                 :codomain :source-cohomology
                 :matrix-shape
                 (list (rational-cohomology-space-dimension codomain)
                       (rational-cohomology-space-dimension domain))
                 :domain-receipt
                 (rational-cohomology-space-receipt domain)
                 :codomain-receipt
                 (rational-cohomology-space-receipt codomain)
                 :descent-receipt (linear-map-descent-receipt descent))))
    (%make-induced-cohomology-map
     :chain-map map :degree degree :domain domain :codomain codomain
     :descent descent :receipt receipt)))

(defun induced-cohomology-map-domain (map)
  (%induced-cohomology-map-domain map))
(defun induced-cohomology-map-codomain (map)
  (%induced-cohomology-map-codomain map))
(defun induced-cohomology-map-degree (map)
  (%induced-cohomology-map-degree map))
(defun induced-cohomology-map-matrix (map)
  (copy-tree (linear-map-descent-matrix
              (%induced-cohomology-map-descent map))))
(defun induced-cohomology-map-receipt (map)
  (copy-tree (%induced-cohomology-map-receipt map)))

(defun induced-cohomology-map-valid-p (map)
  "Replay the chain map, both cohomology quotients, and the descent receipt."
  (and (induced-cohomology-map-p map)
       (chain-map-valid-p (%induced-cohomology-map-chain-map map))
       (rational-cohomology-space-valid-p (%induced-cohomology-map-domain map))
       (rational-cohomology-space-valid-p (%induced-cohomology-map-codomain map))
       (linear-map-descent-valid-p (%induced-cohomology-map-descent map))
       (handler-case
           (let ((replayed
                   (induced-cohomology-map
                    (%induced-cohomology-map-chain-map map)
                    (%induced-cohomology-map-degree map))))
             (and (equal (induced-cohomology-map-matrix map)
                         (induced-cohomology-map-matrix replayed))
                  (equal (%induced-cohomology-map-receipt map)
                         (%induced-cohomology-map-receipt replayed))))
         (error () nil))))
