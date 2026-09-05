;;;; tests.lisp --- law-verified tests for rosette-unification.

(defpackage #:rosette-unification/tests
  (:use #:cl #:rosette-unification))

(in-package #:rosette-unification/tests)

(defmacro is (c msg) `(unless ,c (error "FAIL: ~A" ,msg)))

(defun set= (a b &key (test #'eql))
  (and (= (length a) (length b))
       (every (lambda (x) (member x b :test test)) a)
       (every (lambda (x) (member x a :test test)) b)))

(defun who-values (answers)
  "Pull the ?who binding out of each QUERY answer."
  (mapcar (lambda (ans) (cdr (assoc "who" ans :test #'string-equal))) answers))

(defun run-all-tests ()
  (let ((n 0))
    (flet ((ok (c msg) (incf n) (is c msg)))

      ;; --- unification ----------------------------------------------------
      (let* ((vm (make-hash-table :test 'equal))
             (t1 (parse-term '(f ?x b) vm))
             (t2 (parse-term '(f a ?y) vm))
             (s  (unify t1 t2)))
        (ok (not (unify-failure-p s)) "f(X,b) unifies with f(a,Y)")
        (ok (eql 'a (term->sexp (resolve (parse-term '?x vm) s))) "X := a")
        (ok (eql 'b (term->sexp (resolve (parse-term '?y vm) s))) "Y := b"))

      ;; nested unification, shared variable
      (let* ((vm (make-hash-table :test 'equal))
             (s (unify (parse-term '(p ?x (g ?x)) vm)
                       (parse-term '(p a ?z) vm))))
        (ok (not (unify-failure-p s)) "p(X,g(X)) unifies with p(a,Z)")
        (ok (equal '(g a) (term->sexp (resolve (parse-term '?z vm) s)))
            "Z := g(a) by propagation through the shared X"))

      ;; occurs-check
      (let* ((vm (make-hash-table :test 'equal)))
        (ok (unify-failure-p (unify (parse-term '?x vm) (parse-term '(f ?x) vm)))
            "occurs-check rejects X = f(X)"))

      ;; constants and arity
      (ok (not (unify-failure-p (unify 'a 'a))) "constant a unifies with a")
      (ok (unify-failure-p (unify 'a 'b)) "constant a does not unify with b")
      (let ((vm (make-hash-table :test 'equal)))
        (ok (unify-failure-p (unify (parse-term '(f ?x) vm) (parse-term '(f a b) vm)))
            "arity mismatch f/1 vs f/2 fails"))

      ;; mgu returns success flag
      (multiple-value-bind (sub ok?) (mgu (parse-term '(f ?x)) (parse-term '(f a)))
        (declare (ignore sub))
        (ok ok? "mgu reports success for unifiable terms"))
      (multiple-value-bind (sub ok?) (mgu 'a 'b)
        (declare (ignore sub))
        (ok (not ok?) "mgu reports failure for distinct constants"))

      ;; --- SLD resolution -------------------------------------------------
      (let ((kb (list (make-fact '(parent tom bob))
                      (make-fact '(parent bob ann))
                      (make-fact '(parent bob pat))
                      (make-fact '(parent pat jim))
                      (make-rule '(grandparent ?x ?z) '(parent ?x ?y) '(parent ?y ?z))
                      (make-rule '(ancestor ?x ?y) '(parent ?x ?y))
                      (make-rule '(ancestor ?x ?z) '(parent ?x ?y) '(ancestor ?y ?z)))))

        ;; grandparent(tom, Who) -> {ann, pat}
        (ok (set= '(ann pat) (who-values (query '(grandparent tom ?who) kb)))
            "grandparent(tom,Who) = {ann, pat}")

        ;; recursive ancestor(tom, Who) -> {bob, ann, pat, jim}
        (ok (set= '(bob ann pat jim) (who-values (query '(ancestor tom ?who) kb)))
            "ancestor(tom,Who) = {bob, ann, pat, jim}")

        ;; ground provability, positive and negative
        (ok (provable-p '(grandparent tom ann) kb) "grandparent(tom,ann) holds")
        (ok (not (provable-p '(grandparent tom jim) kb))
            "grandparent(tom,jim) is false (jim is a great-grandchild)")
        (ok (provable-p '(ancestor tom jim) kb) "ancestor(tom,jim) holds")

        ;; a query with no solutions yields no answers
        (ok (null (query '(parent jim ?who) kb)) "parent(jim,Who) has no answers"))

      ;; --- anti-unification (LGG) -----------------------------------------
      ;; shared disagreement -> shared variable: f(a,a) ^ f(b,b) = f(?g,?g)
      (let* ((a (parse-term '(f a a) (make-hash-table :test 'equal)))
             (b (parse-term '(f b b) (make-hash-table :test 'equal))))
        (multiple-value-bind (g th1 th2) (anti-unify a b)
          (ok (compound-p g) "LGG of f(a,a),f(b,b) is compound")
          (ok (eql 'f (car g)) "LGG keeps the shared functor f")
          (ok (var-p (second g)) "LGG position 1 is a variable")
          (ok (eq (second g) (third g)) "identical disagreement reuses ONE variable")
          ;; the defining LAW: the generalization specializes back to each input
          (ok (term-equal (resolve g th1) a) "(resolve LGG th1) = A")
          (ok (term-equal (resolve g th2) b) "(resolve LGG th2) = B")
          ;; and it subsumes both
          (ok (subsumes-p g a) "LGG subsumes A")
          (ok (subsumes-p g b) "LGG subsumes B")))

      ;; distinct positions -> distinct variables: g(a,X,c) ^ g(a,Y,d)=g(a,?u,?v)
      (let* ((vm1 (make-hash-table :test 'equal)) (vm2 (make-hash-table :test 'equal))
             (a (parse-term '(g a ?x c) vm1))
             (b (parse-term '(g a ?y d) vm2)))
        (multiple-value-bind (gg th1 th2) (anti-unify a b)
          (ok (eql 'a (second gg)) "shared constant a preserved in LGG")
          (ok (var-p (third gg)) "distinct vars generalize to a variable")
          (ok (var-p (fourth gg)) "c vs d generalize to a variable")
          (ok (not (eq (third gg) (fourth gg))) "distinct disagreements -> distinct vars")
          (ok (term-equal (resolve gg th1) a) "LGG specializes to A (mixed)")
          (ok (term-equal (resolve gg th2) b) "LGG specializes to B (mixed)")))

      ;; least-GENERAL: LGG must be more specific than a bare variable when a
      ;; common functor exists (i.e. LGG is not just ?g).
      (let* ((a (parse-term '(h a) (make-hash-table :test 'equal)))
             (b (parse-term '(h b) (make-hash-table :test 'equal)))
             (g (lgg a b)))
        (ok (compound-p g) "LGG of h(a),h(b) is h(?g), not a bare variable")
        (ok (subsumes-p (make-var) g) "a bare variable is strictly more general than the LGG"))

      ;; no common structure -> a single variable generalizes both
      (let ((g (lgg 'apple 'orange)))
        (ok (var-p g) "LGG of two distinct constants is a variable"))

      ;; n-ary LGG folds
      (let* ((terms (mapcar (lambda (s) (parse-term s (make-hash-table :test 'equal)))
                            '((p 1 z) (p 2 z) (p 3 z))))
             (g (anti-unify-terms terms)))
        (ok (and (eql 'p (car g)) (var-p (second g)) (eql 'z (third g)))
            "n-ary LGG of p(1,z),p(2,z),p(3,z) = p(?g,z)")
        (ok (every (lambda (tm) (subsumes-p g tm)) terms) "n-ary LGG subsumes every input"))

      ;; --- lattice duality: unify = meet (instance), anti-unify = join ------
      ;; anti-unify(a,b) subsumes a and b; if a,b unify their instance is
      ;; subsumed by both -- the two operations bracket the pair.
      (let* ((vm (make-hash-table :test 'equal))
             (a (parse-term '(k ?x b) vm))
             (b (parse-term '(k a ?y) vm)))
        (let ((join (lgg a b)))
          (ok (and (subsumes-p join a) (subsumes-p join b))
              "join (anti-unify) is above both terms"))
        (multiple-value-bind (s okp) (mgu a b)
          (ok okp "k(X,b), k(a,Y) unify")
          (let ((meet (resolve a s)))
            (ok (and (subsumes-p a meet) (subsumes-p b meet))
                "meet (unifier instance) is below both terms"))))

    (format t "~&rosette-unification: ~D assertions passed.~%" n)
    n)))
