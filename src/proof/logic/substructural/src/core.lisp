;;;; core.lisp --- A propositional sequent calculus parameterized by which
;;;; structural rules are admissible.
;;;;
;;;; ONE engine, five points.  A sequent  Gamma |- A  (single conclusion,
;;;; intuitionistic) is derived under a `logic' that names which of the three
;;;; structural rules -- exchange, weakening, contraction -- it may spend.
;;;; Drop weakening and contraction and the hypotheses in Gamma become
;;;; RESOURCES that must be used exactly once: that is linear logic (Girard
;;;; 1987), the substructural turn on Gentzen's 1935 calculus.
;;;;
;;;;   unrestricted (intuitionistic/classical) = exchange + weakening + contraction
;;;;   linear                                  = exchange only      (use exactly once)
;;;;   affine                                   = exchange + weakening (at most once)
;;;;   relevant                                 = exchange + contraction (at least once)
;;;;   ordered   (Lambek)                       = none
;;;;
;;;; FORMULAE are s-expressions:
;;;;   an atom is any non-cons (a symbol);
;;;;   (:tensor a b)  multiplicative conjunction  (x), splits the context;
;;;;   (:lolli  a b)  linear implication (-o);
;;;;   (:with   a b)  additive conjunction  (&), shares the context;
;;;;   (:plus   a b)  additive disjunction  ((+)).
;;;;
;;;; The search is cut-free.  Every logical rule strictly decreases the total
;;;; connective count of (goal + context), so the search is well-founded; a
;;;; depth bound is kept as a defensive backstop only.
;;;;
;;;; Where the structural rules become physical: rosette-gate-effect-types and
;;;; rosette-modal-types realize this linear discipline over hardware (resources =
;;;; pumps / effects, free=linear vs pumped=effectful); rosette-proof-kernel is the
;;;; full-structural unrestricted point as a dependent type theory.

(in-package #:rosette-substructural)

;;; ---------------------------------------------------------------------------
;;; The structural-rule lattice.

(defparameter *logics*
  '((:unrestricted . (:exchange :weakening :contraction))
    (:linear       . (:exchange))
    (:affine   . (:exchange :weakening))
    (:relevant . (:exchange :contraction))
    (:ordered  . ()))
  "Alist mapping each canonical logic to the structural rules it admits.")

(defstruct (structural-profile
            (:constructor make-structural-profile
                (&key exchange-p weakening-p contraction-p)))
  "A first-class structural usage profile.

The three booleans are capabilities, not observations: they state whether a
derivation may exchange, discard, or duplicate hypotheses."
  (exchange-p nil :type boolean)
  (weakening-p nil :type boolean)
  (contraction-p nil :type boolean))

(defun logic-structural-profile (logic)
  "Return the canonical STRUCTURAL-PROFILE named by LOGIC."
  (let ((rules (cdr (or (assoc logic *logics*)
                        (error "Unknown logic: ~S" logic)))))
    (make-structural-profile
     :exchange-p (not (null (member :exchange rules)))
     :weakening-p (not (null (member :weakening rules)))
     :contraction-p (not (null (member :contraction rules))))))

(defun structural-profile-rules (profile)
  "Return the structural rules admitted by PROFILE in canonical order."
  (check-type profile structural-profile)
  (loop for (rule accessor) in
        '((:exchange structural-profile-exchange-p)
          (:weakening structural-profile-weakening-p)
          (:contraction structural-profile-contraction-p))
        when (funcall accessor profile)
          collect rule))

(defun structural-profile-name (profile)
  "Return the canonical logic name for PROFILE, or NIL for an unnamed point."
  (check-type profile structural-profile)
  (car (find (structural-profile-rules profile) *logics*
             :key #'cdr :test #'equal)))

(defun admissible-structural-rules (logic-or-profile)
  "The structural rules admitted by a named logic or STRUCTURAL-PROFILE."
  (if (structural-profile-p logic-or-profile)
      (structural-profile-rules logic-or-profile)
      (let ((cell (assoc logic-or-profile *logics*)))
        (unless cell (error "Unknown logic: ~S" logic-or-profile))
        (cdr cell))))

(defun logic<= (a b)
  "True when every structural rule of logic A is admissible in logic B, i.e.
A sits below B in the structural-rule lattice.  Provability is monotone along
this order: anything provable in A is provable in B."
  (subsetp (admissible-structural-rules a)
           (admissible-structural-rules b)))

(defun logic-provability-order ()
  "The lattice inclusions as (LOWER . UPPER) pairs over the named logics."
  '((:ordered  . :linear)
    (:ordered  . :affine)
    (:ordered  . :relevant)
    (:linear   . :affine)
    (:linear   . :relevant)
    (:affine   . :unrestricted)
    (:relevant . :unrestricted)))

;;; ---------------------------------------------------------------------------
;;; Formulae.

(defun atomic-p (f)
  "True when F is an atomic proposition (a non-cons)."
  (not (consp f)))

(defun connective (f)
  (and (consp f) (car f)))

(defun formula-size (f)
  "Number of connectives in F (atoms = 0)."
  (if (atomic-p f)
      0
      (+ 1 (formula-size (second f)) (formula-size (third f)))))

(defun %context-size (gamma)
  (reduce #'+ gamma :key #'formula-size :initial-value 0))

;;; ---------------------------------------------------------------------------
;;; Derivations.

(defstruct (deriv (:constructor %make-deriv (rule gamma goal premises tags)))
  "A node of a derivation tree.  RULE is a keyword (e.g. :id, :tensor-r,
:lolli-l, :cut); GAMMA |- GOAL is the sequent it concludes; PREMISES are child
derivs; TAGS are the structural rules SPENT at this node."
  rule gamma goal premises tags)

(defun derivation-end-sequent (d)
  "The sequent (GAMMA . GOAL) concluded by derivation D."
  (cons (deriv-gamma d) (deriv-goal d)))

(defun derivation-size (d)
  "Number of inference nodes in derivation D."
  (if (null d)
      0
      (+ 1 (reduce #'+ (deriv-premises d) :key #'derivation-size
                                          :initial-value 0))))

(defun derivation-struct-tags (d)
  "The set of structural rules SPENT anywhere in derivation D (the union of
per-node tags).  This is the structural fingerprint that types the proof."
  (let ((acc '()))
    (labels ((walk (n)
               (when n
                 (dolist (tg (deriv-tags n)) (pushnew tg acc))
                 (mapc #'walk (deriv-premises n)))))
      (walk d))
    acc))

(defun cut-free-p (d)
  "True when derivation D contains no :cut node."
  (and d
       (not (eq (deriv-rule d) :cut))
       (every #'cut-free-p (deriv-premises d))))

(defun legal-in-p (d logic)
  "True when every structural rule spent by derivation D is admissible in
LOGIC -- i.e. D, exactly as built, is a legal derivation of its end-sequent in
LOGIC."
  (subsetp (derivation-struct-tags d)
           (admissible-structural-rules logic)))

;;; ---------------------------------------------------------------------------
;;; Context surgery.

(defun %remove-nth (n list)
  (append (subseq list 0 n) (subseq list (1+ n))))

(defun %insert-at (n items list)
  "Splice ITEMS into LIST at position N (preserving order)."
  (append (subseq list 0 n) items (subseq list n)))

;;; Context splits for the multiplicative rules (tensor-r, lolli-l).
;;;
;;; Each element of the context is routed to the LEFT branch, the RIGHT branch,
;;; BOTH (a copy -- spends contraction) or NEITHER (dropped -- spends
;;; weakening).  In ordered logic only pure, contiguous prefix/suffix splits
;;; are allowed; a pure split that interleaves spends exchange.

(defun %choice-options (ac aw)
  (append '(:l :r) (when ac '(:b)) (when aw '(:n))))

(defun %choice-vectors (n ac aw)
  "All length-N routing vectors over {:l :r :b :n} permitted by AC/AW."
  (if (zerop n)
      (list '())
      (let ((opts (%choice-options ac aw))
            (rest (%choice-vectors (1- n) ac aw))
            (out '()))
        (dolist (o opts)
          (dolist (r rest) (push (cons o r) out)))
        (nreverse out))))

(defun %contiguous-pure-p (cv)
  "True when CV is a pure (:l/:r only) split of shape :l* :r* -- needs no
exchange."
  (and (notany (lambda (c) (member c '(:b :n))) cv)
       (let ((seen-r nil) (ok t))
         (dolist (c cv ok)
           (cond ((eq c :r) (setf seen-r t))
                 ((and (eq c :l) seen-r) (setf ok nil)))))))

(defun %split-cost (cv)
  (+ (count :b cv) (count :n cv) (if (%contiguous-pure-p cv) 0 1)))

(defun %apply-split (ctx cv)
  "Return (LEFT RIGHT TAGS) for routing CTX by CV."
  (let ((left '()) (right '()) (tags '()))
    (loop for x in ctx for c in cv do
      (case c
        (:l (push x left))
        (:r (push x right))
        (:b (push x left) (push x right) (pushnew :contraction tags))
        (:n (pushnew :weakening tags))))
    (unless (%contiguous-pure-p cv) (pushnew :exchange tags))
    (list (nreverse left) (nreverse right) tags)))

(defun %splits (ctx ac aw ordered)
  "All (LEFT RIGHT TAGS) splits of CTX, cheapest (fewest structural rules)
first.  ORDERED restricts to pure contiguous splits."
  (let* ((cvs (%choice-vectors (length ctx) ac aw))
         (cvs (if ordered (remove-if-not #'%contiguous-pure-p cvs) cvs))
         (cvs (stable-sort cvs #'< :key #'%split-cost)))
    (mapcar (lambda (cv) (%apply-split ctx cv)) cvs)))

;;; ---------------------------------------------------------------------------
;;; The cut-free proof search.

(defparameter *max-depth* 400
  "Defensive recursion bound for the proof search (the connective measure
already guarantees termination).")

(defun %first-some (fn list)
  (dolist (x list nil)
    (let ((r (funcall fn x))) (when r (return r)))))

(defun %derive (gamma goal ac aw ordered depth)
  "Search for a cut-free derivation of GAMMA |- GOAL spending only the
structural rules enabled by AC (contraction), AW (weakening) and ORDERED
(no exchange).  Returns a DERIV or NIL.  Cheapest proofs are tried first."
  (when (< depth 0) (return-from %derive nil))
  (let ((d (1- depth)))
    (or
     ;; --- identity / axiom ------------------------------------------------
     (when (member goal gamma :test #'equal)
       (cond ((and (= (length gamma) 1))
              (%make-deriv :id gamma goal nil nil))
             (aw                         ; drop the unused hypotheses
              (%make-deriv :id-weak gamma goal nil '(:weakening)))
             (t nil)))
     ;; --- right rules on the goal ----------------------------------------
     (case (connective goal)
       (:tensor
        (destructuring-bind (a b) (cdr goal)
          (%first-some
           (lambda (s)
             (destructuring-bind (l r tags) s
               (let ((d1 (%derive l a ac aw ordered d)))
                 (when d1
                   (let ((d2 (%derive r b ac aw ordered d)))
                     (when d2
                       (%make-deriv :tensor-r gamma goal (list d1 d2) tags)))))))
           (%splits gamma ac aw ordered))))
       (:lolli
        (destructuring-bind (a b) (cdr goal)
          (let ((d1 (%derive (append gamma (list a)) b ac aw ordered d)))
            (when d1 (%make-deriv :lolli-r gamma goal (list d1) nil)))))
       (:with
        (destructuring-bind (a b) (cdr goal)
          (let ((d1 (%derive gamma a ac aw ordered d)))
            (when d1
              (let ((d2 (%derive gamma b ac aw ordered d)))
                (when d2 (%make-deriv :with-r gamma goal (list d1 d2) nil)))))))
       (:plus
        (destructuring-bind (a b) (cdr goal)
          (let ((d1 (%derive gamma a ac aw ordered d)))
            (if d1
                (%make-deriv :plus-r1 gamma goal (list d1) nil)
                (let ((d2 (%derive gamma b ac aw ordered d)))
                  (when d2 (%make-deriv :plus-r2 gamma goal (list d2) nil)))))))
       (t nil))
     ;; --- left rules on a principal hypothesis ---------------------------
     (%first-some
      (lambda (i)
        (let* ((principal (nth i gamma))
               (rest (%remove-nth i gamma)))
          (case (connective principal)
            (:tensor
             (destructuring-bind (a b) (cdr principal)
               (let ((d1 (%derive (%insert-at i (list a b) rest)
                                  goal ac aw ordered d)))
                 (when d1
                   (%make-deriv :tensor-l gamma goal (list d1) nil)))))
            (:lolli
             (destructuring-bind (a b) (cdr principal)
               (%first-some
                (lambda (s)
                  (destructuring-bind (l r tags) s
                    (let ((d1 (%derive l a ac aw ordered d)))
                      (when d1
                        (let ((d2 (%derive (append r (list b))
                                           goal ac aw ordered d)))
                          (when d2
                            (%make-deriv :lolli-l gamma goal
                                         (list d1 d2) tags)))))))
                (%splits rest ac aw ordered))))
            (:with
             (destructuring-bind (a b) (cdr principal)
               (or (let ((d1 (%derive (%insert-at i (list a) rest)
                                      goal ac aw ordered d)))
                     (when d1 (%make-deriv :with-l1 gamma goal (list d1) nil)))
                   (let ((d2 (%derive (%insert-at i (list b) rest)
                                      goal ac aw ordered d)))
                     (when d2 (%make-deriv :with-l2 gamma goal (list d2) nil))))))
            (:plus
             (destructuring-bind (a b) (cdr principal)
               (let ((d1 (%derive (%insert-at i (list a) rest)
                                  goal ac aw ordered d)))
                 (when d1
                   (let ((d2 (%derive (%insert-at i (list b) rest)
                                      goal ac aw ordered d)))
                     (when d2
                       (%make-deriv :plus-l gamma goal (list d1 d2) nil)))))))
            (t nil))))
      (loop for i below (length gamma) collect i)))))

;;; ---------------------------------------------------------------------------
;;; Public engine.

(defun %flags (logic)
  (let ((rules (admissible-structural-rules logic)))
    (values (and (member :contraction rules) t)   ; ac
            (and (member :weakening rules) t)      ; aw
            (not (member :exchange rules)))))      ; ordered

(defun prove (gamma goal &key (logic :linear) (max-depth *max-depth*))
  "Return a cut-free DERIV of GAMMA |- GOAL under LOGIC, or NIL.  GAMMA is a
list of formulae; GOAL a single formula."
  (multiple-value-bind (ac aw ordered) (%flags logic)
    (%derive gamma goal ac aw ordered max-depth)))

(defun provable-p (gamma goal &key (logic :linear) (max-depth *max-depth*))
  "True when GAMMA |- GOAL is derivable under LOGIC."
  (and (prove gamma goal :logic logic :max-depth max-depth) t))

(defun provable-logics (gamma goal &key (max-depth *max-depth*))
  "The canonical named logics in which GAMMA |- GOAL is provable."
  (loop for logic in '(:ordered :linear :affine :relevant :unrestricted)
        when (provable-p gamma goal :logic logic :max-depth max-depth)
          collect logic))

;;; ---------------------------------------------------------------------------
;;; Cut, and its admissibility.

(defun prove-with-cut (gamma goal lemma &key (logic :linear) (max-depth *max-depth*))
  "Build a derivation of GAMMA |- GOAL that USES LEMMA as a cut formula:
split GAMMA into G1,G2 with  G1 |- LEMMA  and  G2,LEMMA |- GOAL, joined by cut.
Returns a DERIV (containing a :cut node) or NIL."
  (multiple-value-bind (ac aw ordered) (%flags logic)
    (%first-some
     (lambda (s)
       (destructuring-bind (g1 g2 tags) s
         (let ((d1 (%derive g1 lemma ac aw ordered max-depth)))
           (when d1
             (let ((d2 (%derive (append g2 (list lemma)) goal
                                ac aw ordered max-depth)))
               (when d2
                 (%make-deriv :cut gamma goal (list d1 d2) tags)))))))
     (%splits gamma ac aw ordered))))

(defun eliminate-cut (cut-deriv &key (logic :linear) (max-depth *max-depth*))
  "Cut-admissibility, constructively: given CUT-DERIV (a proof that may spend
cut), return a CUT-FREE derivation of the SAME end-sequent under LOGIC, or NIL
if none exists.  Anything provable with cut is provable without it."
  (let ((gamma (deriv-gamma cut-deriv))
        (goal (deriv-goal cut-deriv)))
    (prove gamma goal :logic logic :max-depth max-depth)))
