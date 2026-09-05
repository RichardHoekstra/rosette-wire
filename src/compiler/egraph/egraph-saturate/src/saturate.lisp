;;;; saturate.lisp --- certificate-gated equality saturation.
;;;;
;;;; Equality saturation over rosette-egraph (content-addressed Forms) with a
;;;; MERGE-GATED-BY-CERTIFICATE rule: no e-class collapse happens without a
;;;; re-runnable rosette-proof-witness equality witness. The gate is a SYMBOLIC
;;;; polynomial normal form over {+ - * sq} (exact ring equality) -- NOT a
;;;; finite battery of samples, which a high-degree adversary can fool
;;;; (Schwartz-Zippel: e.g. a vs a + prod_{k=-7..7}(a-k) agree on every env in
;;;; [-7,7] yet differ at a=8). A near-miss / false equivalence never certifies
;;;; and so the classes stay apart. The battery survives only as a demo refuter.
;;;;
;;;; Three things this module provides, cleanly:
;;;;   - SATURATE: grow an e-graph to fixpoint, applying rewrite rules at the
;;;;     root and every subterm (congruence closure), merging only when the
;;;;     equivalence certifies. Returns the e-graph and the minted certificates.
;;;;   - CERTIFY-MERGE: the gate in isolation -- mint a certificate for two
;;;;     forms; the e-class merge is the caller's, and only fires on a pass.
;;;;   - OP-COUNT / SHARING-RATIO: the DAG-op-count sharing meter -- how many
;;;;     primitive ops a content-addressed (memoizing) evaluator performs vs a
;;;;     naive tree walk.

(in-package #:rosette-egraph-saturate)

;;; ------------------------------------------------------------------
;;; The expression language.
;;;
;;; Expressions are plain s-expressions over {+ - * sq} on variables (symbols)
;;; and integer literals, e.g. (sq (+ a b)) or (+ (* a a) (* 2 (* a b)) (* b b)).
;;; rosette-form-core interns structurally identical SUBexpressions to the SAME
;;; Form handle (hash-consing) -- that is the content-addressing this module
;;; rests on.
;;; ------------------------------------------------------------------

(declaim (inline %sym= %op=))
(defun %sym= (a b)
  "Symbols equal by NAME (package-agnostic), else EQL. Lets expressions read in
the caller's package match operators/variables interned here."
  (if (and (symbolp a) (symbolp b))
      (string= (symbol-name a) (symbol-name b))
      (eql a b)))
(defun %op= (op name)
  "TRUE iff OP is the operator symbol NAME (compared by name)."
  (and (symbolp op) (string= (symbol-name op) (symbol-name name))))

(defun expr-eval (expr env &optional counter)
  "Evaluate EXPR (an s-expression over {+ - * sq}, variables, integers) under
ENV (an alist VAR->INTEGER). When COUNTER is a cons cell, increment its CAR
once per primitive op executed (the naive cost model: one op per interior
node)."
  (flet ((tick () (when counter (incf (car counter)))))
    (cond
      ((integerp expr) expr)
      ;; Variables are looked up by symbol NAME, so an expression read in one
      ;; package and a battery built in another still bind (the operators below
      ;; are matched the same way) -- the substrate is symbol-package-agnostic.
      ((symbolp expr) (let ((cell (assoc expr env :test #'%sym=)))
                        (if cell (cdr cell) (error "unbound variable ~A" expr))))
      ((consp expr)
       (let ((op (first expr)) (args (rest expr)))
         (cond
           ((%op= op '+) (tick) (reduce #'+ (mapcar (lambda (e) (expr-eval e env counter)) args)))
           ((%op= op '-) (tick) (let ((vs (mapcar (lambda (e) (expr-eval e env counter)) args)))
                                  (if (= (length vs) 1) (- (first vs)) (reduce #'- vs))))
           ((%op= op '*) (tick) (reduce #'* (mapcar (lambda (e) (expr-eval e env counter)) args)))
           ((%op= op 'sq) (tick) (let ((v (expr-eval (first args) env counter))) (* v v)))
           (t (error "unknown operator ~S" op)))))
      (t (error "not an expression: ~S" expr)))))

(defun %free-vars (expr &optional acc)
  "Symbols appearing in EXPR (variables)."
  (cond
    ((symbolp expr) (if (member expr acc) acc (cons expr acc)))
    ((consp expr) (dolist (a (rest expr) acc) (setf acc (%free-vars a acc))))
    (t acc)))

(defun make-battery (vars &key (size 60) (lo -7) (hi 7) (seed 0))
  "Deterministic battery of integer environments over VARS: the all-ones and
all-zeros environments followed by SIZE pseudo-random ones in [LO,HI]. SEED
makes the random envs reproducible."
  (let ((state (sb-ext:seed-random-state seed)))
    (list* (mapcar (lambda (v) (cons v 1)) vars)
           (mapcar (lambda (v) (cons v 0)) vars)
           (loop repeat size
                 collect (mapcar (lambda (v)
                                   (cons v (+ lo (random (- (1+ hi) lo) state))))
                                 vars)))))

(defvar *default-battery*
  (make-battery '(a b c))
  "Battery used by FORMS-EQUAL-ON-BATTERY-P and CERTIFY-MERGE when none is
given. Covers variables A B C.")

(defun forms-equal-on-battery-p (e1 e2 &optional (battery *default-battery*))
  "TRUE iff E1 and E2 evaluate equal on every environment in BATTERY. An
environment missing a variable that the form references signals an error from
EXPR-EVAL; ensure the battery covers both forms' free variables."
  (every (lambda (env) (= (expr-eval e1 env) (expr-eval e2 env))) battery))

(defun runtime-verify (certificate)
  "Compatibility wrapper for the proof-witness verifier exported by this API."
  (rosette-proof-witness:runtime-verify certificate))

;;; ------------------------------------------------------------------
;;; The SOUND equality carrier: polynomial normal form over {+ - * sq}.
;;;
;;; The operator set {+ - * sq} on integer literals and symbolic variables is
;;; exactly a commutative ring, so every expression has a canonical multivariate
;;; polynomial normal form (monomial -> integer coefficient). Two expressions
;;; are equal as functions IFF their normal forms are identical -- this is
;;; decidable, complete for this operator set, and immune to the sampling hole
;;; that a finite battery has. A monomial is a sorted alist (VARNAME-STRING .
;;; POSITIVE-EXPONENT); a polynomial is a canonical alist (MONOMIAL . NONZERO-
;;; INTEGER-COEFF), sorted by monomial so EQUAL decides polynomial equality.
;;; ------------------------------------------------------------------

(defun %mono-canon (mono)
  "Canonical monomial: sum duplicate variables' exponents, drop zeros, sort by
variable name. Does not mutate MONO."
  (let ((tbl '()))
    (dolist (pair mono)
      (let ((cell (assoc (car pair) tbl :test #'string=)))
        (if cell (incf (cdr cell) (cdr pair))
            (push (cons (car pair) (cdr pair)) tbl))))
    (sort (remove-if (lambda (p) (zerop (cdr p))) tbl) #'string< :key #'car)))

(defun %mono-mul (m1 m2)
  "Product of two monomials: concatenate and re-canonicalize (exponents add)."
  (%mono-canon (append m1 m2)))

(defun %mono-string (mono)
  "A deterministic string key for a canonical MONOMIAL (for total ordering)."
  (if (null mono) ""
      (format nil "~{~A^~D~^*~}"
              (loop for pair in mono append (list (car pair) (cdr pair))))))

(defun %poly-canon (terms)
  "Canonicalize a list of (MONOMIAL . COEFF) TERMS into a polynomial: combine
like monomials, drop zero coefficients, sort by monomial. The result compares
by EQUAL, so it is a true canonical form."
  (let ((tbl (make-hash-table :test #'equal)))
    (dolist (term terms)
      (let ((m (%mono-canon (car term))))
        (incf (gethash m tbl 0) (cdr term))))
    (let ((out '()))
      (maphash (lambda (m c) (unless (zerop c) (push (cons m c) out))) tbl)
      (sort out #'string< :key (lambda (mc) (%mono-string (car mc)))))))

(defun %poly-const (n) (if (zerop n) '() (list (cons '() n))))
(defun %poly-var   (name) (list (cons (list (cons name 1)) 1)))
(defun %poly-add   (p q) (%poly-canon (append p q)))
(defun %poly-neg   (p) (mapcar (lambda (mc) (cons (car mc) (- (cdr mc)))) p))
(defun %poly-sub   (p q) (%poly-add p (%poly-neg q)))
(defun %poly-mul   (p q)
  (%poly-canon
   (loop for tp in p append
         (loop for tq in q
               collect (cons (%mono-mul (car tp) (car tq))
                             (* (cdr tp) (cdr tq)))))))

(defun %poly-normalize (expr)
  "Multivariate polynomial normal form of EXPR over {+ - * sq}. Signals an error
on any operator outside the ring (caller degrades to 'not proven equal')."
  (cond
    ((integerp expr) (%poly-const expr))
    ((symbolp expr)  (%poly-var (symbol-name expr)))
    ((consp expr)
     (let ((op (first expr)) (args (rest expr)))
       (cond
         ((%op= op '+) (reduce #'%poly-add (mapcar #'%poly-normalize args)
                               :initial-value (%poly-const 0)))
         ((%op= op '-) (let ((ps (mapcar #'%poly-normalize args)))
                         (cond ((null ps) (%poly-const 0))
                               ((null (rest ps)) (%poly-neg (first ps)))
                               (t (reduce #'%poly-sub (rest ps)
                                          :initial-value (first ps))))))
         ((%op= op '*) (reduce #'%poly-mul (mapcar #'%poly-normalize args)
                               :initial-value (%poly-const 1)))
         ((%op= op 'sq) (let ((p (%poly-normalize (first args)))) (%poly-mul p p)))
         (t (error "poly-normalize: non-ring operator ~S" op)))))
    (t (error "poly-normalize: not an expression ~S" expr))))

(defun symbolically-equal-p (e1 e2)
  "TRUE iff E1 and E2 are the SAME polynomial over {+ - * sq} -- exact ring
equality (not sampling). Conservatively NIL if either form uses an operator
outside the ring. This is the SOUND replacement for a battery agreement check:
a high-degree form that merely agrees on a finite battery is refused."
  (handler-case (equal (%poly-normalize e1) (%poly-normalize e2))
    (error () nil)))

;;; ------------------------------------------------------------------
;;; The merge gate: equivalence as a re-runnable theorem.
;;;
;;; CERTIFY-MERGE mints an rosette-proof-witness certificate whose embedded
;;; lean-witness decides equality by SYMBOLIC polynomial normal form
;;; (SYMBOLICALLY-EQUAL-P) -- exact ring equality over {+ - * sq}, decidable and
;;; complete for this operator set. PASSED is whether the two forms are the same
;;; polynomial; RUNTIME-VERIFY re-decides it on demand. A false equivalence --
;;; including a high-degree form that merely AGREES on a finite battery -- is
;;; REFUSED, so a gated caller never merges the classes.
;;; ------------------------------------------------------------------

(defun certify-merge (e1 e2 &optional (battery *default-battery*))
  "Return a proof-witness certificate that E1 == E2. The certificate PASSES iff
E1 and E2 are the SAME polynomial over {+ - * sq} (SYMBOLICALLY-EQUAL-P -- exact
ring equality, NOT battery sampling); its embedded lean-witness re-decides it
under RUNTIME-VERIFY. BATTERY is accepted for backward compatibility but is
never the source of a PASS (a finite sample cannot certify a ring identity).
Returns the certificate either way -- the caller merges e-classes only when
CERTIFICATE-PASSED is true (the gate)."
  (declare (ignore battery))
  (let* ((ok (symbolically-equal-p e1 e2))
         (witness (make-lean-witness
                   (intern (format nil "EQUIV-~X-~X"
                                   (form-address (intern-form e1))
                                   (form-address (intern-form e2)))
                           :keyword)
                   (lambda () (symbolically-equal-p e1 e2)))))
    (make-certificate
     :name :equiv-theorem :kind :proof :claim :forms-equal
     :payload (list :lhs e1 :rhs e2)
     :passed ok
     :witness witness)))

;;; ------------------------------------------------------------------
;;; Rewrite rules. Each maps a matched expression to an equivalent one, or
;;; NIL if it does not apply at this position.
;;; ------------------------------------------------------------------

(defun rw-expand-square (e)
  "(sq (+ x y)) => (+ (sq x) (* 2 (* x y)) (sq y))   [binomial]"
  (when (and (consp e) (%op= (first e) 'sq)
             (consp (second e)) (%op= (first (second e)) '+)
             (= 3 (length (second e))))
    (destructuring-bind (x y) (rest (second e))
      `(+ (sq ,x) (* 2 (* ,x ,y)) (sq ,y)))))

(defun rw-sq-to-mul (e)
  "(sq x) => (* x x)"
  (when (and (consp e) (%op= (first e) 'sq))
    `(* ,(second e) ,(second e))))

(defun rw-distribute (e)
  "(* x (+ y z)) => (+ (* x y) (* x z))   [left distribution]"
  (when (and (consp e) (%op= (first e) '*) (= 3 (length e))
             (consp (third e)) (%op= (first (third e)) '+)
             (= 3 (length (third e))))
    (destructuring-bind (y z) (rest (third e))
      `(+ (* ,(second e) ,y) (* ,(second e) ,z)))))

(defun rw-factor-common (e)
  "(+ (* x y) (* x z)) => (* x (+ y z))   [collect common left factor]"
  (when (and (consp e) (%op= (first e) '+) (= 3 (length e))
             (consp (second e)) (%op= (first (second e)) '*) (= 3 (length (second e)))
             (consp (third e))  (%op= (first (third e)) '*)  (= 3 (length (third e)))
             (equal (second (second e)) (second (third e))))
    `(* ,(second (second e)) (+ ,(third (second e)) ,(third (third e))))))

(defun rw-mul-comm (e)
  "(* x y) => (* y x)   [commutativity, one orientation]"
  (when (and (consp e) (%op= (first e) '*) (= 3 (length e))
             (not (equal (second e) (third e))))
    `(* ,(third e) ,(second e))))

(defun rw-double (e)
  "(* 2 x) => (+ x x)"
  (when (and (consp e) (%op= (first e) '*) (= 3 (length e))
             (eql (second e) 2))
    `(+ ,(third e) ,(third e))))

(defun rw-mul-assoc-r (e)
  "(* x (* y z)) => (* (* x y) z)   [right-to-left re-association]"
  (when (and (consp e) (%op= (first e) '*) (= 3 (length e))
             (consp (third e)) (%op= (first (third e)) '*) (= 3 (length (third e))))
    `(* (* ,(second e) ,(second (third e))) ,(third (third e)))))

(defun rw-add-flatten3 (e)
  "(+ x (+ y z)) => (+ x y z)   [flatten a nested binary sum into ternary]"
  (when (and (consp e) (%op= (first e) '+) (= 3 (length e))
             (consp (third e)) (%op= (first (third e)) '+) (= 3 (length (third e))))
    `(+ ,(second e) ,(second (third e)) ,(third (third e)))))

(defparameter *default-rules*
  (list #'rw-expand-square #'rw-sq-to-mul #'rw-distribute
        #'rw-factor-common #'rw-mul-comm #'rw-double
        #'rw-mul-assoc-r #'rw-add-flatten3)
  "Default arithmetic rewrite rule set for SATURATE.")

(defun %esize (x)
  "Interior-node count of the syntax tree X (used to bound rewrite blowup)."
  (if (consp x) (1+ (reduce #'+ (mapcar #'%esize (rest x)))) 0))

(defun rewrites-at-subterms (e &key (rules *default-rules*) (max-size 14))
  "Every distinct expression obtained by applying ONE rule from RULES at ONE
position (the root or any subterm) of E, dropping results larger than MAX-SIZE
interior nodes. This is congruence closure: a rewrite valid for a subterm is
valid for the whole term because the operators are functions of their
arguments -- if x == x' then f(...x...) == f(...x'...)."
  (let ((out '()))
    (flet ((push! (new)
             (when (and new (not (equal new e)) (<= (%esize new) max-size))
               (pushnew new out :test #'equal))))
      ;; root rewrites
      (dolist (rule rules) (push! (funcall rule e)))
      ;; subterm rewrites: rewrite each argument and rebuild
      (when (consp e)
        (loop for i from 1 below (length e)
              for sub = (nth i e)
              do (dolist (variant (rewrites-at-subterms sub :rules rules
                                                            :max-size max-size))
                   (let ((copy (copy-list e)))
                     (setf (nth i copy) variant)
                     (push! copy))))))
    out))

;;; ------------------------------------------------------------------
;;; Saturation.
;;;
;;; From a seed set of expressions, repeatedly apply rules to every known form,
;;; intern the rewritten form, and egraph-merge the two -- but ONLY after the
;;; equivalence certifies on the battery (gate ON). With the gate OFF, merges
;;; are blind (used only to demonstrate that the gate refuses a bad rule).
;;; Iterate to fixpoint.
;;; ------------------------------------------------------------------

(defstruct (saturation (:constructor %make-saturation)
                       (:predicate saturationp))
  "Result of SATURATE: the grown EGRAPH and the list of minted CERTIFICATES.
Use EQUIVALENT-P / CLASS-SPELLINGS / SATURATION-CERTIFICATES to read it."
  egraph
  certificates)

(defun saturate (seeds &key (rules *default-rules*) (battery *default-battery*)
                            (max-rounds 6) (gate t) (max-size 14))
  "Equality-saturate SEEDS (a list of expressions) in a fresh e-graph and
return a SATURATION. Each round applies RULES at every subterm of every known
form; a discovered pair is merged only when CERTIFY-MERGE passes on BATTERY
(when GATE is true -- the honest path). With GATE NIL, merges are blind. Every
attempted equivalence mints a certificate, collected in the result. Stops at
fixpoint or after MAX-ROUNDS."
  (let ((g (make-egraph))
        (known (make-hash-table :test #'equal))
        (certs '()))
    (dolist (s seeds) (egraph-add g s) (setf (gethash s known) t))
    (loop repeat max-rounds
          for changed = nil
          do (let ((frontier (loop for k being the hash-key of known collect k)))
               (dolist (e frontier)
                 (dolist (e2 (rewrites-at-subterms e :rules rules :max-size max-size))
                   (when (and e2 (not (equal e e2)))
                     (egraph-add g e2)
                     (unless (egraph-equivalent-p g e e2)
                       (let ((cert (certify-merge e e2 battery)))
                         (push cert certs)
                         (when (or (not gate) (certificate-passed cert))
                           (egraph-merge g e e2)
                           (setf changed t))))
                     (unless (gethash e2 known)
                       (setf (gethash e2 known) t changed t)))))
               (unless changed (return))))
    (%make-saturation :egraph g :certificates (nreverse certs))))

(defun equivalent-p (saturation e1 e2)
  "TRUE iff E1 and E2 landed in the same e-class of SATURATION's e-graph."
  (egraph-equivalent-p (saturation-egraph saturation) e1 e2))

(defun class-spellings (saturation e)
  "The distinct expression spellings sharing E's e-class in SATURATION (Form
handles unwrapped to their values)."
  (remove-duplicates
   (mapcar (lambda (m) (if (form-p m) (form-value m) m))
           (egraph-class-members (saturation-egraph saturation) e))
   :test #'equal))

;;; ------------------------------------------------------------------
;;; The sharing meter.
;;;
;;; A naive tree walk re-evaluates every interior node of the syntax TREE.
;;; Content-addressing collapses identical subtrees to one Form, so the number
;;; of DISTINCT interior subexpressions = the number of primitive ops a
;;; memoizing (content-addressed) evaluator must perform. The ratio is the
;;; sharing.
;;; ------------------------------------------------------------------

(defun %naive-op-count (expr)
  "Interior-node count of the syntax TREE (naive op count)."
  (if (consp expr)
      (+ 1 (reduce #'+ (mapcar #'%naive-op-count (rest expr))))
      0))

(defun %shared-op-count (expr)
  "Number of DISTINCT interior subexpressions (content-addressed op count)."
  (let ((seen (make-hash-table :test #'equal)))
    (labels ((walk (e)
               (when (and (consp e) (not (gethash e seen)))
                 (setf (gethash e seen) t)
                 (dolist (sub (rest e)) (walk sub)))))
      (walk expr))
    (hash-table-count seen)))

(defun op-count (expr &key shared)
  "Number of primitive ops to evaluate EXPR. With :SHARED NIL (default) this is
the naive syntax-tree interior-node count; with :SHARED T it is the count of
DISTINCT interior subexpressions a content-addressed (memoizing) evaluator
performs. For an expression that reuses a subexpression, the shared count is
strictly smaller."
  (if shared (%shared-op-count expr) (%naive-op-count expr)))

(defun sharing-ratio (expr)
  "Shared op-count of EXPR as a fraction of its naive op-count (1.0 = no
sharing, 0.5 = half the ops). Returns (values RATIO SHARED NAIVE)."
  (let ((naive (%naive-op-count expr))
        (shared (%shared-op-count expr)))
    (values (if (zerop naive) 1.0 (/ shared naive)) shared naive)))
