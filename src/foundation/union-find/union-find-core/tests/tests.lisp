;;;; tests.lisp --- tests for rosette-union-find-core.
;;;;
;;;; Laws verified:
;;;;   (a) equivalence-relation laws (reflexive/symmetric/transitive) on a
;;;;       deterministic pseudo-random union sequence, checked against a
;;;;       brute-force partition-refinement reference;
;;;;   (b) component count = n - (number of effective merges);
;;;;   (c) path-compression idempotence: uf-find twice gives the same rep;
;;;;   (d) a 1000-element chain union completes and yields one component.

(in-package #:rosette-union-find-core/tests)

;;; Deterministic linear congruential generator (no CL random state).
(defun make-lcg (seed)
  (let ((state seed))
    (lambda (n)
      (setf state (mod (+ (* 6364136223846793005 state)
                          1442695040888963407)
                       (expt 2 64)))
      (mod (ash state -33) n))))

;;; Brute-force reference: a partition as a vector of component labels,
;;; merged by relabeling (no forest, no compression -- obviously correct).
(defun make-reference (n)
  (let ((labels* (make-array n)))
    (dotimes (i n) (setf (aref labels* i) i))
    labels*))

(defun reference-union (labels* i j)
  "Merge components of I and J; return T if a merge happened."
  (let ((li (aref labels* i))
        (lj (aref labels* j)))
    (if (= li lj)
        nil
        (progn
          (dotimes (k (length labels*))
            (when (= (aref labels* k) lj)
              (setf (aref labels* k) li)))
          t))))

(defun reference-connected-p (labels* i j)
  (= (aref labels* i) (aref labels* j)))

(defun reference-component-count (labels*)
  (let ((seen (make-hash-table :test #'eql)))
    (loop for l across labels* do (setf (gethash l seen) t))
    (hash-table-count seen)))

(defun frozen-original-uf-component-sizes (uf)
  (let ((counts (make-hash-table :test #'eql)))
    (dotimes (i (uf-size uf))
      (incf (gethash (uf-find uf i) counts 0)))
    (sort (loop for root being the hash-keys of counts
                  using (hash-value n)
                collect (cons root n))
          #'< :key #'car)))

(defun component-sizes-oracle-p ()
  (let ((lcg (make-lcg 20260612)))
    (loop repeat 1000
          for n = (+ 1 (funcall lcg 200))
          for steps = (funcall lcg (* 4 n))
          always (let ((old (make-union-find n))
                       (new (make-union-find n)))
                   (dotimes (_ steps)
                     (let ((i (funcall lcg n))
                           (j (funcall lcg n)))
                       (uf-union old i j)
                       (uf-union new i j)))
                   (equal (frozen-original-uf-component-sizes old)
                          (uf-component-sizes new))))))

(defun run-all-tests ()
  (with-test-run (run "rosette-union-find-core")
    ;; --- construction
    (let ((uf (make-union-find 5)))
      (check run (union-find-p uf) "make-union-find returns a union-find")
      (check run (= (uf-size uf) 5) "size is 5")
      (check run (= (uf-component-count uf) 5) "5 singletons initially")
      (check run (not (uf-connected-p uf 0 4)) "fresh elements disconnected")
      (check run (uf-connected-p uf 3 3) "reflexive on fresh structure"))

    ;; --- basic union semantics
    (let ((uf (make-union-find 4)))
      (check run (uf-union uf 0 1) "first union of 0,1 is effective (T)")
      (check run (not (uf-union uf 0 1)) "repeat union of 0,1 is a no-op (NIL)")
      (check run (uf-connected-p uf 0 1) "0 and 1 connected after union")
      (check run (= (uf-component-count uf) 3) "4 elements, 1 merge -> 3 components"))

    ;; --- out-of-range rejection
    (check run
           (handler-case (progn (uf-find (make-union-find 3) 7) nil)
             (error () t))
           "uf-find rejects out-of-range element")

    ;; --- (a) equivalence laws vs brute-force reference on a deterministic
    ;;     pseudo-random union sequence
    (let* ((n 60)
           (uf (make-union-find n))
           (ref (make-reference n))
           (lcg (make-lcg 42))
           (agree t)
           (effective-merges 0))
      (dotimes (_ 90)
        (let ((i (funcall lcg n))
              (j (funcall lcg n)))
          (let ((merged-uf (uf-union uf i j))
                (merged-ref (reference-union ref i j)))
            (when merged-uf (incf effective-merges))
            (unless (eq (and merged-uf t) (and merged-ref t))
              (setf agree nil)))))
      (check run agree "uf-union effectiveness agrees with reference at every step")
      ;; reflexive
      (check run
             (loop for i below n always (uf-connected-p uf i i))
             "reflexive: every element connected to itself")
      ;; symmetric
      (check run
             (loop for i below n always
                   (loop for j below n always
                         (eq (uf-connected-p uf i j) (uf-connected-p uf j i))))
             "symmetric: connected-p(i,j) = connected-p(j,i)")
      ;; transitive
      (check run
             (loop named outer
                   for i below n do
                     (loop for j below n do
                       (loop for k below n do
                         (when (and (uf-connected-p uf i j)
                                    (uf-connected-p uf j k)
                                    (not (uf-connected-p uf i k)))
                           (return-from outer nil))))
                   finally (return-from outer t))
             "transitive: i~j and j~k imply i~k")
      ;; full agreement with the reference partition
      (check run
             (loop for i below n always
                   (loop for j below n always
                         (eq (uf-connected-p uf i j)
                             (reference-connected-p ref i j))))
             "connectivity agrees pairwise with partition-refinement reference")
      ;; (b) component count law
      (check run (= (uf-component-count uf) (- n effective-merges))
             "component count = n - effective merges")
      (check run (= (uf-component-count uf) (reference-component-count ref))
             "component count agrees with reference")
      ;; component sizes sum to n
      (check run (= n (reduce #'+ (uf-component-sizes uf) :key #'cdr))
             "component sizes sum to n")
      (check run (= (length (uf-component-sizes uf)) (uf-component-count uf))
             "one size entry per component")
      (check run (component-sizes-oracle-p)
             "component sizes oracle: frozen original on 1000 random forests")
      ;; components partition 0..n-1
      (let ((comps (uf-components uf)))
        (check run (= (length comps) (uf-component-count uf))
               "uf-components returns one list per component")
        (check run
               ;; copy-list before sort: append shares its final argument,
               ;; and sort is destructive.
               (equal (sort (copy-list (reduce #'append comps :initial-value '())) #'<)
                      (loop for i below n collect i))
               "uf-components is a partition of 0..n-1")
        (check run
               (loop for comp in comps always
                     (let ((rep (uf-find uf (first comp))))
                       (loop for el in comp always (= (uf-find uf el) rep))))
               "every listed component is internally connected"))
      ;; (c) path-compression idempotence
      (check run
             (loop for i below n always (= (uf-find uf i) (uf-find uf i)))
             "uf-find twice gives the same representative")
      ;; representative is itself a fixed point
      (check run
             (loop for i below n always
                   (let ((r (uf-find uf i)))
                     (= r (uf-find uf r))))
             "representative is its own representative"))

    ;; --- (d) 1000-element chain
    (let ((uf (make-union-find 1000)))
      (dotimes (i 999)
        (uf-union uf i (1+ i)))
      (check run (= (uf-component-count uf) 1) "1000-chain yields one component")
      (check run (uf-connected-p uf 0 999) "chain ends connected")
      (check run (= (cdr (first (uf-component-sizes uf))) 1000)
             "single component has size 1000"))

    ;; --- growable element set
    (let ((uf (make-union-find 2)))
      (uf-union uf 0 1)
      (let ((new (uf-add-element uf)))
        (check run (= new 2) "uf-add-element returns the next index")
        (check run (= (uf-size uf) 3) "size grows to 3")
        (check run (= (uf-component-count uf) 2) "new element is a fresh singleton")
        (check run (not (uf-connected-p uf 0 new)) "new element disconnected")
        (uf-union uf new 0)
        (check run (uf-connected-p uf 1 new) "grown element merges normally")
        (check run (= (uf-component-count uf) 1) "all merged after growth")))))
