;;;; church.lisp --- Church numerals + arithmetic as TLC terms.
;;;;
;;;; These are the test computations: Church-encoded naturals and the
;;;; classic combinators add / mul / exp.  They are built as TLC-TERMs
;;;; (rosette-foundation-rewrite's AST) so the same term can be (a) compiled to
;;;; a net and reduced here, and (b) normalised by the reference evaluator
;;;; -- the correctness oracle.

(in-package #:rosette-sharing-reduction)

(defun church-numeral (n &key (f :f) (x :x))
  "Church numeral N = (lambda f. lambda x. f (f ... (f x))) with N f's."
  (check-type n (integer 0))
  (let ((body (tlc-var x)))
    (dotimes (i n)
      (setf body (tlc-app (tlc-var f) body)))
    (tlc-lam f (tlc-lam x body))))

(defun church-add ()
  "ADD = lambda m. lambda n. lambda f. lambda x. m f (n f x)."
  (let ((m :m) (n :n) (f :f) (x :x))
    (tlc-lam m (tlc-lam n (tlc-lam f (tlc-lam x
      (tlc-app (tlc-app (tlc-var m) (tlc-var f))
               (tlc-app (tlc-app (tlc-var n) (tlc-var f)) (tlc-var x)))))))))

(defun church-mul ()
  "MUL = lambda m. lambda n. lambda f. m (n f)."
  (let ((m :m) (n :n) (f :f))
    (tlc-lam m (tlc-lam n (tlc-lam f
      (tlc-app (tlc-var m) (tlc-app (tlc-var n) (tlc-var f))))))))

(defun church-exp ()
  "EXP = lambda m. lambda n. n m   (n applied to m yields m^n)."
  (let ((m :m) (n :n))
    (tlc-lam m (tlc-lam n (tlc-app (tlc-var n) (tlc-var m))))))

(defun apply* (&rest terms)
  "Left-fold application: (apply* F A B) = ((F A) B)."
  (reduce #'tlc-app terms))

;;; --- count a Church numeral from a normal-form TLC term -------------

(defun church-count (term)
  "If TERM is (alpha-equivalent to) a Church numeral, return its value N;
else NIL.  We match the shape lambda f. lambda x. f (f ... x)."
  (when (and (eq (tlc-term-kind term) :lam)
             (eq (tlc-term-kind (tlc-lam-body term)) :lam))
    (let* ((f (tlc-lam-param term))
           (inner (tlc-lam-body term))
           (x (tlc-lam-param inner))
           (body (tlc-lam-body inner))
           (count 0))
      (loop
        (cond
          ((and (eq (tlc-term-kind body) :var)
                (eq (tlc-var-name body) x))
           (return count))
          ((and (eq (tlc-term-kind body) :app)
                (eq (tlc-term-kind (tlc-app-fn body)) :var)
                (eq (tlc-var-name (tlc-app-fn body)) f))
           (incf count)
           (setf body (tlc-app-arg body)))
          (t (return nil)))))))

;;; --- run a term through the net -------------------------------------

(defun net-of-term (term)
  "Compile TERM to a net (alias of LAM->NET)."
  (lam->net term))

(defun run-term (term &key (parallel nil))
  "Compile TERM to a net, reduce to normal form, read back.
Returns (values normal-form-term interaction-count)."
  (let ((net (lam->net term)))
    (reduce-all net :parallel parallel)
    (values (net->term net) (net-interactions net))))
