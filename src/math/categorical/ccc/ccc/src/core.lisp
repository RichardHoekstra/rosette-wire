;;;; core.lisp --- a concrete cartesian closed category (FinSet) and the
;;;; Lambek correspondence to the simply-typed lambda-calculus.
;;;;
;;;; FinSet: objects are finite sets (plain Lisp lists of distinct elements
;;;; compared by EQUAL); morphisms are functions carrying their domain and
;;;; codomain so that equality is EXTENSIONAL and the universal properties are
;;;; literally checkable.  Product elements are conses (a . b); exponential
;;;; elements are CANONICALISED association lists (a function A -> B as a table),
;;;; so EQUAL is a faithful equality on every object -- including function
;;;; spaces, which is what makes the higher-order (exponential) laws decidable.

(in-package #:rosette-ccc)

;;; ---------------------------------------------------------------------------
;;; Canonicalisation -- a total order so that function tables have a unique
;;; representation and EQUAL is a sound equality on exponential objects.
;;; ---------------------------------------------------------------------------

(defun canon-key (x) (prin1-to-string x))

(defun canon-alist (al)
  "Sort an association list (a function table) by key into canonical form."
  (sort (copy-list al) #'string< :key (lambda (kv) (canon-key (car kv)))))

(defun set-equal (a b)
  "Do lists A and B denote the same finite set (same members under EQUAL)?"
  (and (= (length (remove-duplicates a :test #'equal))
          (length (remove-duplicates b :test #'equal)))
       (every (lambda (x) (member x b :test #'equal)) a)
       (every (lambda (x) (member x a :test #'equal)) b)))

;;; ---------------------------------------------------------------------------
;;; Morphisms of FinSet
;;; ---------------------------------------------------------------------------

(defstruct (mor (:constructor %make-mor))
  "A FinSet morphism: a function FN from the finite set DOM to the finite set
COD.  DOM/COD are carried explicitly so that composition is type-checked and
morphism equality is the extensional one."
  dom cod fn)

(defun mor (dom cod fn)
  "Construct a morphism DOM -> COD computed by FN, checking FN lands in COD."
  (let ((m (%make-mor :dom dom :cod cod :fn fn)))
    (dolist (x dom)
      (unless (member (funcall fn x) cod :test #'equal)
        (error "rosette-ccc: morphism image ~S of ~S is not in the codomain"
               (funcall fn x) x)))
    m))

(defun mapply (m x) (funcall (mor-fn m) x))

(defun mid (a)
  "The identity morphism on the object A."
  (mor a a #'identity))

(defun mcompose (g f)
  "Composition g . f : dom(f) -> cod(g).  Requires cod(f) = dom(g)."
  (unless (set-equal (mor-cod f) (mor-dom g))
    (error "rosette-ccc: cannot compose -- cod(f) /= dom(g)"))
  (mor (mor-dom f) (mor-cod g) (lambda (x) (mapply g (mapply f x)))))

(defun mor-equal (f g)
  "Extensional equality of morphisms: same dom, same cod, agree pointwise."
  (and (set-equal (mor-dom f) (mor-dom g))
       (set-equal (mor-cod f) (mor-cod g))
       (every (lambda (x) (equal (mapply f x) (mapply g x))) (mor-dom f))))

;;; ---------------------------------------------------------------------------
;;; Terminal object (the unit type) -- one point, a unique arrow into it
;;; ---------------------------------------------------------------------------

(defun terminal () (list :*))

(defun terminal-arrow (a)
  "The unique morphism A -> 1 (the universal property of the terminal object)."
  (mor a (terminal) (constantly :*)))

;;; ---------------------------------------------------------------------------
;;; Binary products
;;; ---------------------------------------------------------------------------

(defun prod (a b)
  "The product object A x B = the cartesian product, elements (a . b)."
  (loop for x in a nconc (loop for y in b collect (cons x y))))

(defun proj1 (a b) (mor (prod a b) a #'car))
(defun proj2 (a b) (mor (prod a b) b #'cdr))

(defun pairing (f g)
  "<f,g> : X -> A x B, the unique mediating morphism with pi1.<f,g>=f,
pi2.<f,g>=g.  Requires f and g to share a domain."
  (unless (set-equal (mor-dom f) (mor-dom g))
    (error "rosette-ccc: <f,g> requires dom(f)=dom(g)"))
  (mor (mor-dom f) (prod (mor-cod f) (mor-cod g))
       (lambda (x) (cons (mapply f x) (mapply g x)))))

(defun prod-mor (f g)
  "The functorial action f x g : A x B -> A' x B' of the product."
  (mor (prod (mor-dom f) (mor-dom g))
       (prod (mor-cod f) (mor-cod g))
       (lambda (p) (cons (mapply f (car p)) (mapply g (cdr p))))))

;;; ---------------------------------------------------------------------------
;;; Exponentials  B^A  -- the function space, the closed structure
;;; ---------------------------------------------------------------------------

(defun all-functions (a b)
  "Every function A -> B as a canonical association-list table."
  (if (null a)
      (list nil)
      (let ((rest (all-functions (cdr a) b)))
        (loop for y in b
              nconc (loop for r in rest
                          collect (canon-alist (cons (cons (car a) y) r)))))))

(defun exponential (a b)
  "The exponential object B^A = the set of all morphisms A -> B."
  (all-functions a b))

(defun ev (a b)
  "eval : B^A x A -> B, the counit of the adjunction (apply a function)."
  (mor (prod (exponential a b) a) b
       (lambda (p) (cdr (assoc (cdr p) (car p) :test #'equal)))))

(defun %dom-factors (f)
  "Recover (A . B) from a morphism whose domain is a product A x B."
  (let ((dom (mor-dom f)))
    (cons (remove-duplicates (mapcar #'car dom) :test #'equal)
          (remove-duplicates (mapcar #'cdr dom) :test #'equal))))

(defun curry (f)
  "Phi : Hom(A x B, C) -> Hom(A, C^B).  curry(f)(a) is the table b |-> f(a,b)."
  (destructuring-bind (a . b) (%dom-factors f)
    (let ((c (mor-cod f)))
      (mor a (exponential b c)
           (lambda (av)
             (canon-alist
              (loop for bv in b collect (cons bv (mapply f (cons av bv))))))))))

(defun %exp-factors (expset)
  "Recover (B . C) from an exponential object C^B (a set of function tables).
B = the shared key set; C = the set of all values taken (constant functions are
present, so every element of C appears)."
  (let ((b (mapcar #'car (first expset)))
        (c '()))
    (dolist (tbl expset)
      (dolist (kv tbl) (pushnew (cdr kv) c :test #'equal)))
    (cons b c)))

(defun uncurry (g)
  "Phi^{-1} : Hom(A, C^B) -> Hom(A x B, C), inverse to CURRY."
  (let ((a (mor-dom g)))
    (destructuring-bind (b . c) (%exp-factors (mor-cod g))
      (mor (prod a b) c
           (lambda (p) (cdr (assoc (cdr p) (mapply g (car p)) :test #'equal)))))))

(defun exp-mor (k b)
  "The functorial action k^B : C^B -> C'^B of the exponential (post-compose by
k : C -> C'), needed to state naturality of the adjunction in C."
  (mor (exponential b (mor-dom k))
       (exponential b (mor-cod k))
       (lambda (tbl)
         (canon-alist
          (mapcar (lambda (kv) (cons (car kv) (mapply k (cdr kv)))) tbl)))))

;;; ===========================================================================
;;; The simply-typed lambda-calculus (de Bruijn) and the Lambek correspondence
;;; ===========================================================================
;;;
;;; Types:  :unit | :bool | (:base name (elems...)) | (:prod A B) | (:arr A B)
;;; Terms:  (:var n) | (:lam A body) | (:app s t)
;;;         | (:pair s t) | (:fst t) | (:snd t)
;;;         | (:unit) | (:const value type)
;;;
;;; Lambek: types <-> objects (:prod<->product, :arr<->exponential,
;;; :unit<->terminal); well-typed terms-in-context <-> morphisms.

(defun denote-type (ty)
  "STLC type -> FinSet object."
  (cond
    ((eq ty :unit) (terminal))
    ((eq ty :bool) (list :true :false))
    ((and (consp ty) (eq (car ty) :base)) (copy-list (third ty)))
    ((and (consp ty) (eq (car ty) :prod))
     (prod (denote-type (second ty)) (denote-type (third ty))))
    ((and (consp ty) (eq (car ty) :arr))
     (exponential (denote-type (second ty)) (denote-type (third ty))))
    (t (error "rosette-ccc: bad type ~S" ty))))

(defun ctx-obj (ctx)
  "Interpret a context (list of types, index 0 = innermost binder) as the
left-nested product object  ( ... (1 x t_{n-1}) ... ) x t_0 ."
  (if (null ctx)
      (terminal)
      (prod (ctx-obj (cdr ctx)) (denote-type (car ctx)))))

(defun term-type (ctx tm)
  "Infer the STLC type of TM in context CTX."
  (cond
    ((and (consp tm) (eq (car tm) :var)) (nth (second tm) ctx))
    ((and (consp tm) (eq (car tm) :unit)) :unit)
    ((and (consp tm) (eq (car tm) :const)) (third tm))
    ((and (consp tm) (eq (car tm) :lam))
     (list :arr (second tm) (term-type (cons (second tm) ctx) (third tm))))
    ((and (consp tm) (eq (car tm) :app))
     (third (term-type ctx (second tm))))      ; (:arr A B) -> B
    ((and (consp tm) (eq (car tm) :pair))
     (list :prod (term-type ctx (second tm)) (term-type ctx (third tm))))
    ((and (consp tm) (eq (car tm) :fst)) (second (term-type ctx (second tm))))
    ((and (consp tm) (eq (car tm) :snd)) (third (term-type ctx (second tm))))
    (t (error "rosette-ccc: ill-formed term ~S" tm))))

(defun denote (ctx tm)
  "The interpretation functor: a well-typed term Gamma |- t : T denotes a
morphism [Gamma] -> [T].  Application denotes via EVAL, abstraction via CURRY --
the adjunction at work."
  (cond
    ((and (consp tm) (eq (car tm) :var))
     (let ((n (second tm)))
       (if (= n 0)
           (proj2 (ctx-obj (cdr ctx)) (denote-type (car ctx)))
           (mcompose (denote (cdr ctx) (list :var (1- n)))
                     (proj1 (ctx-obj (cdr ctx)) (denote-type (car ctx)))))))
    ((and (consp tm) (eq (car tm) :unit))
     (terminal-arrow (ctx-obj ctx)))
    ((and (consp tm) (eq (car tm) :const))
     (mor (ctx-obj ctx) (denote-type (third tm)) (constantly (second tm))))
    ((and (consp tm) (eq (car tm) :lam))
     (curry (denote (cons (second tm) ctx) (third tm))))
    ((and (consp tm) (eq (car tm) :app))
     (let* ((sty (term-type ctx (second tm)))         ; (:arr A B)
            (a (denote-type (second sty)))
            (b (denote-type (third sty))))
       (mcompose (ev a b)
                 (pairing (denote ctx (second tm)) (denote ctx (third tm))))))
    ((and (consp tm) (eq (car tm) :pair))
     (pairing (denote ctx (second tm)) (denote ctx (third tm))))
    ((and (consp tm) (eq (car tm) :fst))
     (let ((pty (term-type ctx (second tm))))
       (mcompose (proj1 (denote-type (second pty)) (denote-type (third pty)))
                 (denote ctx (second tm)))))
    ((and (consp tm) (eq (car tm) :snd))
     (let ((pty (term-type ctx (second tm))))
       (mcompose (proj2 (denote-type (second pty)) (denote-type (third pty)))
                 (denote ctx (second tm)))))
    (t (error "rosette-ccc: cannot denote ~S" tm))))

;;; ---------------------------------------------------------------------------
;;; beta/eta normalisation of de-Bruijn STLC terms (Pierce TAPL conventions)
;;; ---------------------------------------------------------------------------

(defun tshift (d c tm)
  "Shift the free de-Bruijn indices of TM (>= cutoff C) by D."
  (cond
    ((eq (car tm) :var)
     (list :var (if (>= (second tm) c) (+ (second tm) d) (second tm))))
    ((eq (car tm) :lam) (list :lam (second tm) (tshift d (1+ c) (third tm))))
    ((eq (car tm) :app) (list :app (tshift d c (second tm)) (tshift d c (third tm))))
    ((eq (car tm) :pair) (list :pair (tshift d c (second tm)) (tshift d c (third tm))))
    ((eq (car tm) :fst) (list :fst (tshift d c (second tm))))
    ((eq (car tm) :snd) (list :snd (tshift d c (second tm))))
    (t tm)))                             ; :unit, :const

(defun tsubst (j s tm)
  "Substitute S for the de-Bruijn variable J in TM."
  (cond
    ((eq (car tm) :var) (if (= (second tm) j) s tm))
    ((eq (car tm) :lam) (list :lam (second tm)
                              (tsubst (1+ j) (tshift 1 0 s) (third tm))))
    ((eq (car tm) :app) (list :app (tsubst j s (second tm)) (tsubst j s (third tm))))
    ((eq (car tm) :pair) (list :pair (tsubst j s (second tm)) (tsubst j s (third tm))))
    ((eq (car tm) :fst) (list :fst (tsubst j s (second tm))))
    ((eq (car tm) :snd) (list :snd (tsubst j s (second tm))))
    (t tm)))

(defun subst-top (arg body)
  "The beta substitution: replace the bound variable 0 of BODY by ARG."
  (tshift -1 0 (tsubst 0 (tshift 1 0 arg) body)))

(defun nf (tm)
  "Normalise TM to beta/eta-normal form (normal-order; reduces under lambda).
STLC is strongly normalising, so this terminates."
  (cond
    ((and (consp tm) (eq (car tm) :app))
     (let ((s (nf (second tm)))
           (a (nf (third tm))))
       (if (and (consp s) (eq (car s) :lam))
           (nf (subst-top a (third s)))            ; beta
           (list :app s a))))
    ((and (consp tm) (eq (car tm) :fst))
     (let ((p (nf (second tm))))
       (if (and (consp p) (eq (car p) :pair)) (second p) (list :fst p))))
    ((and (consp tm) (eq (car tm) :snd))
     (let ((p (nf (second tm))))
       (if (and (consp p) (eq (car p) :pair)) (third p) (list :snd p))))
    ((and (consp tm) (eq (car tm) :pair))
     (list :pair (nf (second tm)) (nf (third tm))))
    ((and (consp tm) (eq (car tm) :lam))
     (list :lam (second tm) (nf (third tm))))
    (t tm)))                             ; :var, :unit, :const
