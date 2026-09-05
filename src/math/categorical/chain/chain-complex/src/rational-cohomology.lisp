;;;; rational-cohomology.lisp --- exact H^n representatives over Q.

(in-package #:rosette-chain-complex)

(defun %copy-chain-complex (complex)
  (make-chain-complex (copy-tree (chain-complex-boundaries complex))
                      (copy-list (chain-complex-dims complex))))

(defun %matrix-or-shaped-zero (matrix rows columns)
  (or matrix
      (loop repeat rows collect (make-list columns :initial-element 0))))

(defun %exact-rational-chain-complex-p (complex)
  (every (lambda (matrix)
           (or (null matrix)
               (every (lambda (row) (every #'rationalp row)) matrix)))
         (chain-complex-boundaries complex)))

(defstruct (rational-cohomology-space
            (:constructor %make-rational-cohomology-space)
            (:conc-name %rational-cohomology-space-)
            (:copier nil))
  (complex nil :read-only t)
  (degree 0 :type (integer 0 *) :read-only t)
  (subquotient nil :read-only t)
  (receipt nil :type list :read-only t))

(defun make-rational-cohomology-space (complex degree)
  "Construct the exact rational space

  H^DEGREE(COMPLEX;Q) = ker(d_(n+1)^T) / im(d_n^T).

The returned representatives are ambient cochain column vectors.  Construction
refuses a boundary sequence that does not satisfy d^2=0."
  (unless (chain-complex-p complex)
    (error 'type-error :datum complex :expected-type 'chain-complex))
  (unless (and (integerp degree) (<= 0 degree (chain-complex-top complex)))
    (error "Cohomology degree ~S is outside [0,~D]."
           degree (chain-complex-top complex)))
  (unless (%exact-rational-chain-complex-p complex)
    (error "Rational cohomology requires exact rational boundary entries."))
  (unless (d-squared-zero-p complex)
    (error "Rational cohomology is undefined: the supplied boundary has d^2 /= 0."))
  (let* ((copy (%copy-chain-complex complex))
         (dimensions (chain-complex-dims copy))
         (middle-dimension (nth degree dimensions))
         (source-dimension (if (plusp degree)
                               (nth (1- degree) dimensions)
                               0))
         (target-dimension (if (< degree (chain-complex-top copy))
                               (nth (1+ degree) dimensions)
                               0))
         (cocycle-equations
           (%matrix-or-shaped-zero
            (%transpose (boundary-matrix copy (1+ degree)))
            target-dimension middle-dimension))
         (coboundary-generators
           (%matrix-or-shaped-zero
            (%transpose (boundary-matrix copy degree))
            middle-dimension source-dimension))
         (subquotient
           (make-linear-subquotient
            cocycle-equations coboundary-generators
            :middle-dimension middle-dimension
            :source-dimension source-dimension
            :target-dimension target-dimension))
         (receipt
           (list :version 1
                 :construction :rational-cohomology
                 :degree degree
                 :chain-dimensions (copy-list dimensions)
                 :formula :ker-d-next-transpose/mod-im-d-transpose
                 :dimension (linear-subquotient-dimension subquotient)
                 :subquotient-receipt
                 (linear-subquotient-receipt subquotient))))
    (%make-rational-cohomology-space
     :complex copy :degree degree :subquotient subquotient :receipt receipt)))

(defun rational-cohomology-space-complex (space)
  "A defensive copy of the chain complex underlying SPACE."
  (%copy-chain-complex (%rational-cohomology-space-complex space)))

(defun rational-cohomology-space-degree (space)
  (%rational-cohomology-space-degree space))

(defun rational-cohomology-space-dimension (space)
  (linear-subquotient-dimension
   (%rational-cohomology-space-subquotient space)))

(defun rational-cohomology-space-representatives (space)
  "Fresh ambient cochain representatives forming a basis of SPACE."
  (linear-subquotient-representatives
   (%rational-cohomology-space-subquotient space)))

(defun rational-cohomology-class-coordinates (space cochain)
  "Coordinates of the cocycle COCHAIN in the certified quotient basis."
  (linear-subquotient-class-coordinates
   (%rational-cohomology-space-subquotient space) cochain))

(defun rational-cohomology-space-receipt (space)
  (copy-tree (%rational-cohomology-space-receipt space)))

(defun rational-cohomology-space-valid-p (space)
  "Replay the quotient construction and its exact residual checks."
  (and (rational-cohomology-space-p space)
       (linear-subquotient-valid-p
        (%rational-cohomology-space-subquotient space))
       (handler-case
           (let ((replayed
                   (make-rational-cohomology-space
                    (%rational-cohomology-space-complex space)
                    (%rational-cohomology-space-degree space))))
             (and (equal (rational-cohomology-space-representatives space)
                         (rational-cohomology-space-representatives replayed))
                  (equal (%rational-cohomology-space-receipt space)
                         (%rational-cohomology-space-receipt replayed))))
         (error () nil))))
