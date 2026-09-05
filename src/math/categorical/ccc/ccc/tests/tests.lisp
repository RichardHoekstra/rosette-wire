;;;; tests.lisp --- law-verifying tests for rosette-ccc.
;;;;
;;;; The certificates of a cartesian closed category, computed in FinSet:
;;;;   1. category laws       -- associativity + identity;
;;;;   2. terminal object     -- the arrow into 1 is unique;
;;;;   3. product UP          -- pi_i . <f,g> = f_i, and <f,g> is the unique mediator;
;;;;   4. exponential adjunction  Hom(A x B, C) ~ Hom(A, C^B)  -- curry/uncurry
;;;;      are mutually-inverse and NATURAL (the headline: this IS currying);
;;;;   5. beta/eta as categorical equations;
;;;;   6. a de-Bruijn STLC term round-trips through its FinSet morphism, and the
;;;;      denotation is invariant under beta/eta reduction (Lambek soundness).

(defpackage #:rosette-ccc/tests
  (:use #:cl #:rosette-ccc)
  (:export #:run-all-tests))

(in-package #:rosette-ccc/tests)

(defvar *pass* 0)
(defvar *fail* 0)

(defun check (name ok &optional got want)
  (if ok
      (incf *pass*)
      (progn (incf *fail*)
             (format t "  FAIL ~A~@[  got ~S~]~@[ want ~S~]~%" name got want))))

;;; ---- concrete small objects --------------------------------------------
(defparameter +a+ '(:a0 :a1))
(defparameter +b+ '(:b0 :b1))
(defparameter +c+ '(:c0 :c1))

;;; build a morphism from an explicit table (an alist), validated by MOR
(defun table-mor (dom cod tbl)
  (mor dom cod (lambda (x) (cdr (assoc x tbl :test #'equal)))))

;;; every morphism dom -> cod (used to quantify the laws over ALL of Hom)
(defun all-mors (dom cod)
  (mapcar (lambda (tbl) (table-mor dom cod tbl)) (all-functions dom cod)))

;;; ===========================================================================
(defun test-category-laws ()
  (let* ((f (table-mor +a+ +b+ '((:a0 . :b0) (:a1 . :b1))))
         (g (table-mor +b+ +c+ '((:b0 . :c1) (:b1 . :c0))))
         (h (table-mor +c+ +a+ '((:c0 . :a1) (:c1 . :a0)))))
    (check "CAT: identity is a morphism" (mor-p (mid +a+)))
    (check "CAT: id . f = f" (mor-equal (mcompose (mid +b+) f) f))
    (check "CAT: f . id = f" (mor-equal (mcompose f (mid +a+)) f))
    (check "CAT: (h.g).f = h.(g.f)"
           (mor-equal (mcompose (mcompose h g) f)
                      (mcompose h (mcompose g f))))
    (check "CAT: composite computes correctly"
           (eq (mapply (mcompose g f) :a0) :c1))))

;;; ===========================================================================
(defun test-terminal ()
  (let ((!a (terminal-arrow +a+)))
    (check "TERM: terminal has one point" (= 1 (length (terminal))))
    (check "TERM: A -> 1 lands in 1" (eq (mapply !a :a0) :*))
    ;; uniqueness: any arrow A -> 1 equals the canonical one
    (let ((other (mor +a+ (terminal) (constantly :*))))
      (check "TERM: arrow into 1 is unique" (mor-equal !a other)))))

;;; ===========================================================================
(defun test-product-up ()
  (let* ((x +c+)
         (f (table-mor x +a+ '((:c0 . :a1) (:c1 . :a0))))
         (g (table-mor x +b+ '((:c0 . :b0) (:c1 . :b1))))
         (pair (pairing f g)))
    (check "PROD: pi1 . <f,g> = f"
           (mor-equal (mcompose (proj1 +a+ +b+) pair) f))
    (check "PROD: pi2 . <f,g> = g"
           (mor-equal (mcompose (proj2 +a+ +b+) pair) g))
    (check "PROD: <f,g> computes the pair"
           (equal (mapply pair :c0) '(:a1 . :b0)))
    ;; uniqueness / product eta: any m : X -> AxB equals <pi1.m, pi2.m>
    (let ((ok t))
      (dolist (m (all-mors x (prod +a+ +b+)))
        (unless (mor-equal m (pairing (mcompose (proj1 +a+ +b+) m)
                                      (mcompose (proj2 +a+ +b+) m)))
          (setf ok nil)))
      (check "PROD: mediating morphism is UNIQUE (product eta)" ok))))

;;; ===========================================================================
;;; THE HEADLINE: Hom(A x B, C) ~ Hom(A, C^B) is a natural bijection = currying
(defun test-exponential-adjunction ()
  ;; (a) curry/uncurry are mutually inverse over ALL of Hom(A x B, C)
  (let ((ok-rt t))
    (dolist (f (all-mors (prod +a+ +b+) +c+))
      (unless (mor-equal (uncurry (curry f)) f) (setf ok-rt nil)))
    (check "EXP: uncurry . curry = id on Hom(A x B, C)" ok-rt))
  (let ((ok-rt t))
    (dolist (g (all-mors +a+ (exponential +b+ +c+)))
      (unless (mor-equal (curry (uncurry g)) g) (setf ok-rt nil)))
    (check "EXP: curry . uncurry = id on Hom(A, C^B)" ok-rt))
  ;; (b) the two Hom-sets have the same cardinality (a bijection exists)
  (check "EXP: |Hom(AxB,C)| = |Hom(A,C^B)|"
         (= (length (all-functions (prod +a+ +b+) +c+))
            (length (all-functions +a+ (exponential +b+ +c+)))))
  ;; (c) naturality in A: curry(f . (h x id_B)) = curry(f) . h
  (let* ((aa '(:p :q))
         (h (table-mor aa +a+ '((:p . :a0) (:q . :a1))))
         (ok t))
    (dolist (f (all-mors (prod +a+ +b+) +c+))
      (unless (mor-equal (curry (mcompose f (prod-mor h (mid +b+))))
                         (mcompose (curry f) h))
        (setf ok nil)))
    (check "EXP: adjunction is NATURAL in A" ok))
  ;; (d) naturality in C: curry(k . f) = (k^B) . curry(f)
  (let* ((cc '(:c0 :c1 :c2))
         (k (table-mor +c+ cc '((:c0 . :c2) (:c1 . :c0))))
         (ok t))
    (dolist (f (all-mors (prod +a+ +b+) +c+))
      (unless (mor-equal (curry (mcompose k f))
                         (mcompose (exp-mor k +b+) (curry f)))
        (setf ok nil)))
    (check "EXP: adjunction is NATURAL in C" ok)))

;;; ===========================================================================
;;; beta and eta as categorical equations
(defun test-beta-eta ()
  ;; beta: eval . (curry f x id_B) = f
  (let ((ok t))
    (dolist (f (all-mors (prod +a+ +b+) +c+))
      (unless (mor-equal (mcompose (ev +b+ +c+)
                                   (prod-mor (curry f) (mid +b+)))
                         f)
        (setf ok nil)))
    (check "BETA: eval . (curry f x id) = f" ok))
  ;; eta: curry(eval) = id on the exponential C^B
  (check "ETA: curry(eval) = id on C^B"
         (mor-equal (curry (ev +b+ +c+)) (mid (exponential +b+ +c+)))))

;;; ===========================================================================
;;; Lambek: STLC type formers correspond to CCC structure
(defun test-lambek-types ()
  (check "LAMBEK: unit type <-> terminal object"
         (set-equal (denote-type :unit) (terminal)))
  (check "LAMBEK: product type <-> product object"
         (set-equal (denote-type '(:prod :bool :bool))
                    (prod '(:true :false) '(:true :false))))
  (check "LAMBEK: arrow type <-> exponential object"
         (set-equal (denote-type '(:arr :bool :bool))
                    (exponential '(:true :false) '(:true :false)))))

;;; ===========================================================================
;;; STLC terms round-trip through their morphisms; denotation = beta/eta invariant
(defun test-stlc-roundtrip ()
  (let* ((bt :bool)
         (tt (list :const :true bt))
         (ft (list :const :false bt))
         (idb (list :lam bt (list :var 0)))
         ;; (lambda x. x) true
         (t1 (list :app idb tt))
         ;; fst <true,false>
         (t2 (list :fst (list :pair tt ft)))
         ;; higher-order: ((lambda f. lambda x. f x) id) true
         (t3 (list :app
                   (list :app
                         (list :lam (list :arr bt bt)
                               (list :lam bt (list :app (list :var 1) (list :var 0))))
                         idb)
                   tt)))
    (dolist (spec (list (cons t1 :true) (cons t2 :true) (cons t3 :true)))
      (destructuring-bind (term . want) spec
        (let* ((m (denote '() term))
               (v (mapply m :*)))
          ;; term -> morphism -> value: the morphism computes the answer
          (check "RT: term denotes the expected value" (eq v want))
          ;; value -> term: read-back equals the beta-normal form (round trip)
          (check "RT: read-back = normal form"
                 (equal (nf term) (list :const v :bool)))
          ;; denotation is invariant under reduction (Lambek soundness)
          (check "RT: [t] = [nf t] (denotation beta/eta-invariant)"
                 (mor-equal m (denote '() (nf term)))))))
    ;; a function-valued reduction: (lambda x. (lambda y.y) x) = (lambda x.x)
    (let ((red (list :lam bt (list :app idb (list :var 0)))))
      (check "RT: function-valued [t] = [nf t]"
             (mor-equal (denote '() red) (denote '() (nf red))))
      (check "RT: function-valued nf = identity term"
             (equal (nf red) idb)))
    ;; open term invariance in a non-empty context
    (let* ((ctx (list bt))
           (open (list :app idb (list :var 0))))
      (check "RT: open-term [t] = [nf t] in context"
             (mor-equal (denote ctx open) (denote ctx (nf open)))))
    ;; swap, applied: swap <true,false> = <false,true>
    (let* ((swap (list :lam (list :prod bt bt)
                       (list :pair (list :snd (list :var 0))
                             (list :fst (list :var 0)))))
           (app (list :app swap (list :pair tt ft)))
           (m (denote '() app)))
      (check "RT: swap <true,false> = (false . true)"
             (equal (mapply m :*) '(:false . :true))))))

;;; ===========================================================================
(defun run-all-tests ()
  (setf *pass* 0 *fail* 0)
  (format t "~&rosette-ccc tests~%")
  (test-category-laws)
  (test-terminal)
  (test-product-up)
  (test-exponential-adjunction)
  (test-beta-eta)
  (test-lambek-types)
  (test-stlc-roundtrip)
  (format t "~&  ~D passed, ~D failed~%" *pass* *fail*)
  (when (plusp *fail*)
    (error "rosette-ccc: ~D test(s) failed" *fail*))
  t)
