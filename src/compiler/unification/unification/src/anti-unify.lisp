;;;; anti-unify.lisp --- anti-unification (Plotkin-Reynolds least general
;;;; generalization) and one-sided matching: the DUAL of Robinson unification.
;;;;
;;;; Robinson UNIFY takes two terms and returns one substitution making them
;;;; EQUAL -- a most general common INSTANCE (a shared descendant in the
;;;; subsumption order).  ANTI-UNIFY is the mirror image: it returns the most
;;;; specific term that GENERALIZES both -- a least general common ANCESTOR --
;;;; together with the two substitutions that specialize it back to each input.
;;;; Where the inputs disagree, the generalization carries a variable; identical
;;;; disagreements reuse the SAME variable, e.g.  f(a,a) ^ f(b,b) = f(?g,?g).
;;;;
;;;; MATCH is the ordering that ties the two together: G subsumes T iff there is
;;;; a one-sided substitution of G's variables giving T.  UNIFY (meet) and
;;;; ANTI-UNIFY (join) are then the two operations of the subsumption lattice.

(in-package #:rosette-unification)

;;; ----------------------------------------------------------------------
;;; Structural term equality (logic variables by EQ identity)
;;; ----------------------------------------------------------------------
(defun term-equal (a b)
  "Structural equality of two terms; logic variables compared by identity."
  (cond ((and (var-p a) (var-p b)) (eq a b))
        ((or (var-p a) (var-p b)) nil)
        ((and (compound-p a) (compound-p b))
         (and (eql (functor a) (functor b))
              (= (length (term-args a)) (length (term-args b)))
              (every #'term-equal (term-args a) (term-args b))))
        (t (eql a b))))

;;; ----------------------------------------------------------------------
;;; Anti-unification (least general generalization)
;;; ----------------------------------------------------------------------
(defun anti-unify (a b)
  "Least general generalization (LGG) of terms A and B -- the dual of UNIFY.
Return three values: the generalization G, and two substitutions TH1, TH2
(alists lvar->term) with (RESOLVE G TH1) term-equal A and (RESOLVE G TH2)
term-equal B.  A disagreement contributes a fresh variable, and equal
disagreement pairs reuse the SAME variable, so G is the MOST SPECIFIC common
generalization (Plotkin 1970, Reynolds 1970)."
  (let ((store '()) (counter 0))            ; store: list of (a b . var)
    (labels ((fresh-for (x y)
               (dolist (entry store)
                 (when (and (term-equal (car entry) x) (term-equal (cadr entry) y))
                   (return-from fresh-for (cddr entry))))
               (let ((v (make-var (format nil "g~D" (incf counter)))))
                 (setf store (append store (list (list* x y v))))
                 v))
             (au (x y)
               (cond
                 ((term-equal x y) x)       ; agree -> keep constant/var/subterm
                 ((and (compound-p x) (compound-p y)
                       (eql (functor x) (functor y))
                       (= (length (term-args x)) (length (term-args y))))
                  (cons (functor x) (mapcar #'au (term-args x) (term-args y))))
                 (t (fresh-for x y)))))      ; disagree -> a (shared) variable
      (let ((g (au a b)))
        (values g
                (mapcar (lambda (e) (cons (cddr e) (car e)))  store)   ; TH1: G -> A
                (mapcar (lambda (e) (cons (cddr e) (cadr e))) store)))))) ; TH2: G -> B

(defun lgg (a b)
  "The least general generalization term of A and B (first value of ANTI-UNIFY)."
  (values (anti-unify a b)))

(defun anti-unify-terms (terms)
  "N-ary LGG: fold ANTI-UNIFY across a non-empty list of TERMS (the generalization
that covers them all).  Returns the generalization term."
  (unless terms (error "anti-unify-terms: need at least one term"))
  (reduce #'lgg terms))

;;; ----------------------------------------------------------------------
;;; One-sided matching (the subsumption order)
;;; ----------------------------------------------------------------------
(defun match (pattern term &optional subst)
  "One-sided match: extend SUBST so (RESOLVE PATTERN result) term-equals TERM,
binding ONLY PATTERN's variables (TERM's own variables are treated as opaque
constants).  Returns the substitution (NIL is empty success) or :FAIL.  This is
the asymmetric restriction of UNIFY that decides subsumption."
  (if (unify-failure-p subst)
      :fail
      (let ((p (walk pattern subst)))
        (cond
          ((var-p p)
           (let ((seen (assoc p subst :test #'eq)))
             (if seen
                 (if (term-equal (cdr seen) term) subst :fail)
                 (acons p term subst))))
          ((and (compound-p p) (compound-p term)
                (eql (functor p) (functor term))
                (= (length (term-args p)) (length (term-args term))))
           (loop with s = subst
                 for x in (term-args p) for y in (term-args term)
                 do (setf s (match x y s))
                    (when (unify-failure-p s) (return :fail))
                 finally (return s)))
          ((and (not (compound-p p)) (eql p term)) subst)   ; equal constants
          (t :fail)))))

(defun subsumes-p (general specific)
  "True iff GENERAL is more general than (or equal to) SPECIFIC -- i.e. some
substitution of GENERAL's variables yields SPECIFIC.  The partial order under
which UNIFY is the meet and ANTI-UNIFY the join."
  (not (unify-failure-p (match general specific))))
