;;;; tests.lisp --- laws and located walls for exact linear subquotients.

(defpackage #:rosette-exact-linear-subquotient/tests
  (:use #:cl #:rosette-exact-linear-subquotient)
  (:export #:run-all-tests))

(in-package #:rosette-exact-linear-subquotient/tests)

(defvar *checks* 0)
(defvar *failures* 0)

(defun check (condition control &rest arguments)
  (incf *checks*)
  (unless condition
    (incf *failures*)
    (format t "~&FAIL: ~?~%" control arguments)))

(defun signals-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun relation-error-receipt (thunk)
  (handler-case (progn (funcall thunk) nil)
    (linear-descent-error (condition)
      (list (linear-descent-error-kind condition)
            (linear-descent-error-residual condition)))))

(defun run-all-tests ()
  (let ((*checks* 0)
        (*failures* 0))
    ;; ker(1 1 0) / span(1 -1 0) is represented by e_3.
    (let* ((a (list (list 1 1 0)))
           (b (list (list 1) (list -1) (list 0)))
           (space (make-linear-subquotient a b))
           (receipt (linear-subquotient-receipt space)))
      (check (linear-subquotient-p space)
             "constructor returns the typed carrier")
      (check (= 3 (linear-subquotient-ambient-dimension space))
             "ambient dimension is retained")
      (check (= 1 (linear-subquotient-dimension space))
             "dim ker(A)/im(B) is one")
      (check (equal '((0 0 1))
                    (linear-subquotient-representatives space))
             "the complement selects an exact representative")
      (check (equal '(5)
                    (linear-subquotient-class-coordinates
                     space '(-2 2 5)))
             "class coordinates discard an exact boundary")
      (check (equal '(0)
                    (linear-subquotient-class-coordinates
                     space '(7 -7 0)))
             "an incoming boundary has the zero class")
      (check (getf receipt :valid-p)
             "the public receipt records a valid certificate")
      (check (linear-subquotient-valid-p space)
             "valid-p recomputes every quotient law")

      ;; Inputs and aggregate outputs are snapshots.
      (setf (caar a) 99
            (caar b) 99)
      (let ((representatives (linear-subquotient-representatives space)))
        (setf (caar representatives) 99))
      (let* ((copy (linear-subquotient-receipt space))
             (rows (getf (getf copy :boundary-out) :rows)))
        (setf (caar rows) 99))
      (check (equal '((0 0 1))
                    (linear-subquotient-representatives space))
             "caller mutation cannot alter stored representatives")
      (check (linear-subquotient-valid-p space)
             "caller mutation cannot alter the stored certificate")

      ;; A diagonal ambient map descends and acts by 3 on the quotient.
      (let* ((ambient-map (list (list 2 0 0)
                                (list 0 2 0)
                                (list 0 0 3)))
             (descent (descend-linear-map ambient-map space space))
             (descent-receipt (linear-map-descent-receipt descent)))
        (check (linear-map-descent-p descent)
               "descent returns a typed immutable carrier")
        (check (equal '((3)) (linear-map-descent-matrix descent))
               "the induced matrix is computed by projection F lift")
        (check (linear-map-descent-valid-p descent)
               "the descent certificate recomputes")
        (check (and (equal '(1 1)
                           (getf (getf descent-receipt :matrix) :shape))
                    (getf descent-receipt :valid-p))
               "descent receipt exposes shape and verdict")
        (setf (caar ambient-map) 999)
        (let ((matrix-copy (linear-map-descent-matrix descent)))
          (setf (caar matrix-copy) 999))
        (check (equal '((3)) (linear-map-descent-matrix descent))
               "ambient input and induced output are defensively copied"))

      ;; Near miss: cycles go to cycles, but the relation (1,-1,0) goes
      ;; to the nonzero class e_3. The residual locates that exact defect.
      (let ((wall (relation-error-receipt
                   (lambda ()
                     (descend-linear-map
                      '((0 0 0) (0 0 0) (1 0 1)) space space)))))
        (check (equal '(:relation-preservation ((1))) wall)
               "a relation-descent near miss returns residual ((1))"))

      ;; A different near miss leaves the target kernel.
      (let ((wall (relation-error-receipt
                   (lambda ()
                     (descend-linear-map
                      '((1 0 0) (0 0 0) (0 0 0)) space space)))))
        (check (and wall (eq :cycle-preservation (first wall))
                    (not (equal '((0 0)) (second wall))))
               "cycle preservation is checked before relation descent"))

      (check (signals-error-p
              (lambda ()
                (linear-subquotient-class-coordinates space '(1 0 0))))
             "class coordinates reject a non-cycle")
      (check (signals-error-p
              (lambda () (descend-linear-map '((1 0) (0 1)) space space)))
             "descent rejects a malformed ambient-map shape"))

    ;; Zero-dimensional and rational controls.
    (let ((zero (make-linear-subquotient nil nil
                                         :middle-dimension 0
                                         :source-dimension 4
                                         :target-dimension 0)))
      (check (= 0 (linear-subquotient-dimension zero))
             "explicit dimensions support a 0-by-4 incoming matrix")
      (check (equal nil (linear-subquotient-representatives zero))
             "the zero quotient has no representatives")
      (check (equal nil (linear-subquotient-class-coordinates zero nil))
             "the unique zero vector has empty class coordinates")
      (check (equal nil (linear-map-descent-matrix
                         (descend-linear-map nil zero zero)))
             "the unique zero map descends"))
    (let ((free (make-linear-subquotient nil nil
                                         :middle-dimension 2
                                         :source-dimension 0
                                         :target-dimension 0)))
      (check (= 2 (linear-subquotient-dimension free))
             "explicit middle dimension turns NIL boundaries into shaped zeros")
      (check (equal '((1 0) (0 1))
                    (linear-subquotient-representatives free))
             "the shaped zero complex has the full ambient quotient")
      (check (equal '((0 0) (0 0))
                    (linear-map-descent-matrix
                     (descend-linear-map nil free free)))
             "NIL is the unambiguous shaped zero ambient map"))
    (let ((zero-out (make-linear-subquotient nil nil
                                             :middle-dimension 2
                                             :source-dimension 0
                                             :target-dimension 3)))
      (check (= 2 (linear-subquotient-dimension zero-out))
             "explicit target dimension shapes a NIL zero boundary"))
    (let ((rational-space
            (make-linear-subquotient '((1/2 -1/3))
                                     nil)))
      (check (= 1 (linear-subquotient-dimension rational-space))
             "CL ratios are reduced exactly without tolerances")
      (check (linear-subquotient-valid-p rational-space)
             "the rational quotient receipt is exact"))

    ;; Exhaust the standard-coordinate rank lattice through ambient rank six.
    ;; This covers every combination dim(A), rank(B), and quotient dimension
    ;; in that range, including all zero-row and zero-column boundaries.
    (loop for ambient from 0 to 6 do
      (loop for out-rank from 0 to ambient do
        (loop for image-rank from 0 to (- ambient out-rank) do
          (let* ((a (loop for row below out-rank
                          collect (loop for column below ambient
                                        collect (if (= row column) 1 0))))
                 (b (loop for row below ambient
                          collect (loop for column below image-rank
                                        collect (if (= row
                                                       (+ out-rank column))
                                                    1 0))))
                 (space (make-linear-subquotient
                         a b
                         :middle-dimension ambient
                         :source-dimension image-rank
                         :target-dimension out-rank)))
            (check (= (- ambient out-rank image-rank)
                      (linear-subquotient-dimension space))
                   "coordinate rank lattice has the expected quotient dimension")
            (check (linear-subquotient-valid-p space)
                   "coordinate rank lattice retains every exact receipt law")))))

    ;; Shape, aggregate, scalar, dimension, and chain-complex walls.
    (check (signals-error-p
            (lambda ()
              (make-linear-subquotient '((1 0) (0)) '((0) (0)))))
           "ragged matrices are rejected")
    (check (signals-error-p
            (lambda ()
              (make-linear-subquotient '((1 0d0)) '((0) (0)))))
           "floating-point entries are rejected")
    (check (signals-error-p
            (lambda ()
              (make-linear-subquotient '((1 0 0)) '((0) (0)))))
           "A columns must equal B rows")
    (check (signals-error-p
            (lambda ()
              (make-linear-subquotient '((1 0)) '((0) (0))
                                       :middle-dimension 3)))
           "an explicit middle dimension must agree")
    (check (signals-error-p
            (lambda ()
              (make-linear-subquotient '((1 0)) '((0) (0))
                                       :target-dimension 2)))
           "an explicit target dimension must agree")
    (check (signals-error-p
            (lambda ()
              (make-linear-subquotient '((1 0)) '((0) (0))
                                       :source-dimension 2)))
           "an explicit source dimension must agree")
    (check (signals-error-p
            (lambda ()
              (make-linear-subquotient '((1 0)) '((1) (0)))))
           "a nonzero A*B residual is rejected")
    (let ((dotted (cons (list 1 0) :tail)))
      (check (signals-error-p
              (lambda ()
                (make-linear-subquotient dotted '((0) (0)))))
             "dotted outer lists are rejected"))
    (let ((cyclic (list 1 0)))
      (setf (cdr (last cyclic)) cyclic)
      (check (signals-error-p
              (lambda ()
                (make-linear-subquotient (list cyclic) '((0) (0)))))
             "cyclic matrix rows are rejected"))

    (format t "~&rosette-exact-linear-subquotient: ~D checks, ~D failure(s).~%"
            *checks* *failures*)
    (when (plusp *failures*)
      (error "rosette-exact-linear-subquotient: ~D test failure(s)." *failures*))
    t))
