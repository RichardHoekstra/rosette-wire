;;;; tests.lisp --- rosette-core-term acceptance gate.
;;;;
;;;; ONE term, THREE coincidences, in ONE suite:
;;;;   (i)   EVAL gives value V;
;;;;   (ii)  NORMALIZATION emits a certificate that RE-RUNS and confirms V, and
;;;;         two beta-equal terms share a normal form while two in-equal do not;
;;;;   (iii) the LOWERED kernel-spec on the CPU oracle equals V bit-exactly
;;;;         across an input battery; a corrupted lowering is CAUGHT.
;;;; Plus negative gates with teeth: forged certificate, ill-typed term.

(defpackage #:rosette-core-term/tests
  (:use #:cl #:rosette-core-term))
(in-package #:rosette-core-term/tests)

(defvar *fails* 0)
(defun expect (label got want)
  (if (equal got want)
      (format t "  ok   ~a = ~a~%" label got)
      (progn (incf *fails*) (format t "  FAIL ~a: got ~s want ~s~%" label got want))))
(defun expect-true (label got)
  (if got (format t "  ok   ~a~%" label)
      (progn (incf *fails*) (format t "  FAIL ~a: got NIL~%" label))))

;;; --- the non-trivial core program ------------------------------------------
;;; A Horner polynomial p(x) = 2x^3 + 3x^2 + 4x + 5, written with LET (exercises
;;; the let-beta reduction AND let-substitution lowering), plus a bounded-rec
;;; SUM(n) = n + (n-1) + ... + 1 (exercises delta-unfold and the CALL/RET VM).
(defparameter *funs*
  '((poly (x) (let a (+ (* 2 x) 3)
                (let b (+ (* a x) 4)
                  (+ (* b x) 5))))
    (sum  (n) (if (< n 1) 0 (+ n (sum (- n 1)))))))

(defun poly-ref (x) (+ (* 2 x x x) (* 3 x x) (* 4 x) 5))

(defun prog-with-main (main) (make-core-program :funs *funs* :main main))

;;; --- an EXTERNAL toy rewrite system for the reusable certified-reduce seam --
;;; Successor arithmetic: state grammar  z | (s A) | (add A B) ; small-step
;;; leftmost-outermost with two rules.  This lives OUTSIDE rosette-core-term -- it
;;; only hands CERTIFIED-REDUCE a STEP function that emits one proof-node per
;;; rewrite, exactly what an external rewrite engine would do.
(defun succ-redex (tm)
  "If TM is a top-level redex, return (values rewritten rule-keyword)."
  (when (and (consp tm) (eq (first tm) 'add))
    (let ((a (second tm)) (b (third tm)))
      (cond ((eq a 'z) (values b :add-z))                     ; (add z B) -> B
            ((and (consp a) (eq (first a) 's))                ; (add (s A) B) -> (s (add A B))
             (values (list 's (list 'add (second a) b)) :add-s))
            (t (values nil nil))))))

(defun succ-step (tm)
  "The small-step relation: STATE -> (values NEXT proof-node), (values TM NIL)
at a normal form.  Leftmost-outermost redex."
  (multiple-value-bind (r rule) (succ-redex tm)
    (if r
        (values r (rosette-proof-carrier-core:proof-leaf :rewrite rule :payload r))
        (if (consp tm)
            (loop for i from 1 below (length tm)
                  do (multiple-value-bind (child node) (succ-step (nth i tm))
                       (when node
                         (return (values (append (subseq tm 0 i) (list child)
                                                 (subseq tm (1+ i)))
                                         node))))
                  finally (return (values tm nil)))
            (values tm nil)))))

(defun run-all-tests ()
  (setf *fails* 0)

  ;; === (i) EVAL ============================================================
  (let ((p (prog-with-main '(poly 7))))
    (expect "eval poly(7)" (program-eval p) (poly-ref 7)))
  (expect "eval sum(5)" (program-eval (prog-with-main '(sum 5))) 15)
  (expect "eval sum(100)" (program-eval (prog-with-main '(sum 100))) 5050)

  ;; === (ii) PROVE : normalization EMITS the certificate ====================
  (let* ((p (prog-with-main '(poly 7))))
    (multiple-value-bind (v node) (program-normalize p)
      (expect "normalize poly(7) = eval" v (poly-ref 7))
      ;; the proof was emitted BY the reduction: it has real structure.
      (expect-true "proof-node present" (rosette-proof-carrier-core:proof-node-p node))
      (expect-true "proof has >1 step"
                   (> (rosette-proof-carrier-core:proof-node-size node) 5)))
    ;; the sealed certificate RE-RUNS and confirms.
    (let ((cert (normalization-certificate p)))
      (expect-true "normalization certificate passes" (certificate-passed cert))
      (expect-true "certificate RE-RUNS (runtime-verify)"
                   (verify-normalization-certificate cert))))

  ;; definitional equality by normal-form identity:
  ;; poly(7) via LET  ==def  the same polynomial fully spelled out (no let).
  (let ((a (prog-with-main '(poly 7)))
        (b (prog-with-main '(+ (* (+ (* (+ (* 2 7) 3) 7) 4) 7) 5))))
    (expect-true "beta-equal terms SHARE a normal form"
                 (definitionally-equal-p a b))
    (expect-true "def-equality certificate passes"
                 (certificate-passed (definitional-equality-certificate a b))))
  ;; two IN-equal terms do NOT share a normal form (negative gate has teeth).
  (let ((a (prog-with-main '(poly 7)))
        (c (prog-with-main '(poly 8))))
    (expect-true "in-equal terms do NOT share a normal form"
                 (not (definitionally-equal-p a c)))
    (expect-true "def-equality certificate REFUTES the false claim"
                 (not (certificate-passed (definitional-equality-certificate a c)))))

  ;; === (iii) COMPILE : lowered kernel on the CPU oracle == eval, bit-exact ==
  (let ((battery (battery-check *funs* 'poly '(0 1 2 3 5 7 10 -3 -8))))
    (dolist (row battery)
      (expect (format nil "oracle poly(~a) bit-exact" (getf row :input))
              (getf row :oracle) (getf row :eval))
      (expect-true (format nil "  match flag poly(~a)" (getf row :input))
                   (getf row :match))))
  (let ((battery (battery-check *funs* 'sum '(0 1 5 10 50 100))))
    (expect-true "oracle sum battery all bit-exact"
                 (every (lambda (r) (getf r :match)) battery)))

  ;; Capture-avoidance regression: LET elimination and beta reduction must not
  ;; let an inner binder capture a free variable carried by the substituted
  ;; value.  Both examples evaluated correctly before lowering; historically
  ;; the kernel path silently returned 0 for each one.
  (dolist (case
           (list
            (make-core-program
             :funs '((capture-let (y) (let z y (let y 0 z))))
             :main '(capture-let 7))
            (make-core-program
             :funs '((capture-beta (y)
                       (app (lam z :int (let y 0 z)) y)))
             :main '(capture-beta 11))
            ;; The alpha-renamed binder must also avoid the substitution
            ;; target, even when that target is absent from TERM and VAL.
            (make-core-program
             :funs '((capture-fresh-collision (y)
                       (let |ROSETTE$ALPHA$0| y (let y 0 y))))
             :main '(capture-fresh-collision 7))))
    (expect "capture-avoiding lowering stays bit-exact"
            (oracle-run (lower-to-program case))
            (program-eval case)))

  ;; The public invocation carrier is the one handoff to N-backend agreement.
  (let ((lowered (lower-to-program (prog-with-main '(poly 7)))))
    (multiple-value-bind (spec args)
        (make-core-vm-invocation lowered :ops-size 256 :call-size 128)
      (expect-true "public VM invocation returns the shared kernel spec"
                   (eq spec *core-vm-spec*))
      (expect-true "public VM invocation carries seven aligned arguments"
                   (= 7 (length args)))
      (rosette-gpu-kernel-dsl/cpu:interpret-kernel-spec spec args)
      (expect "public VM invocation result decodes the oracle value"
              (core-vm-invocation-result args) (poly-ref 7))))

  ;; NEGATIVE: a CORRUPTED lowering must be CAUGHT by the bit-check.
  (let ((corrupt (battery-check *funs* 'poly '(0 1 2 5 7 10) :corrupt t)))
    (expect-true "corrupted lowering is CAUGHT (some input diverges)"
                 (some (lambda (r) (not (getf r :match))) corrupt)))

  ;; === NEGATIVE GATES with teeth ==========================================
  ;; A FORGED certificate (tampered sealed value) must fail its re-run.
  (let* ((cert (normalization-certificate (prog-with-main '(sum 10))))
         (pl (copy-list (certificate-payload cert))))
    (setf (getf pl :value) 999999)               ; forge the answer
    (setf (rosette-proof-witness:certificate-payload cert) pl)
    (expect-true "FORGED certificate FAILS runtime-verify"
                 (not (verify-normalization-certificate cert))))

  ;; --- B2 regression: the emitted proof TRACE is CHECKED on re-verification.
  ;; "The reduction trace IS the certificate" -- so tampering the sealed PROOF
  ;; node (not just the value) must be caught, even when the VALUE stays correct.
  (let* ((p22 (prog-with-main '(+ 2 2)))
         (genuine (normalization-certificate p22)))
    (expect-true "B2: genuine normalization certificate RE-RUNS"
                 (verify-normalization-certificate genuine))
    ;; (i) a GARBAGE proof node with the right value must be REFUSED.
    (let* ((cert (normalization-certificate p22))
           (pl (copy-list (certificate-payload cert))))
      (setf (getf pl :proof)
            (list :kind :garbage :label "forged" :payload 4 :children nil :metadata nil))
      (setf (rosette-proof-witness:certificate-payload cert) pl)
      (expect-true "B2: GARBAGE proof node FAILS runtime-verify"
                   (not (verify-normalization-certificate cert))))
    ;; (ii) a proof GRAFTED from an unrelated reduction of the SAME value must be
    ;;      REFUSED -- (* 2 2) also = 4, but its trace describes a different
    ;;      reduction, so it cannot masquerade as the (+ 2 2) certificate.
    (let* ((cert (normalization-certificate p22))
           (other (normalization-certificate (prog-with-main '(* 2 2))))
           (pl (copy-list (certificate-payload cert))))
      (expect "B2: grafted-from cert has the SAME value"
              (getf (certificate-payload other) :value) (getf pl :value))
      (setf (getf pl :proof) (getf (certificate-payload other) :proof))
      (setf (rosette-proof-witness:certificate-payload cert) pl)
      (expect-true "B2: GRAFTED same-value proof FAILS runtime-verify"
                   (not (verify-normalization-certificate cert))))
    ;; (iii) two genuinely-different-but-same-value reductions have DISTINCT
    ;;       traces -- the trace, not just the value, distinguishes them.
    (multiple-value-bind (v1 n1) (program-normalize (prog-with-main '(+ 2 2)))
      (multiple-value-bind (v2 n2) (program-normalize (prog-with-main '(* 2 2)))
        (expect "B2: (+ 2 2) and (* 2 2) share a VALUE" v1 v2)
        (expect-true "B2: ...but their proof TRACES differ"
                     (not (equal (rosette-proof-carrier-core:proof-node->plist n1)
                                 (rosette-proof-carrier-core:proof-node->plist n2)))))))

  ;; --- B3 regression: the i32 tagged-fixnum oracle refuses out-of-range values
  ;; GRACEFULLY (a clean CORE-VM-I32-RANGE-ERROR), never a raw crash or a silent
  ;; wrap. Faithful regime is [-2^28, 2^28); overflow modes and endpoints are
  ;; gated.
  ;; (i) a LITERAL whose tagged word overflows i32 (would silently mask to junk).
  (expect-true "B3: out-of-range literal is REFUSED (not silently corrupted)"
               (handler-case
                   (progn (oracle-run
                           (lower-to-program (make-core-program :funs nil
                                               :main 2147483647)))
                          nil)
                 (core-vm-i32-range-error () t)))
  ;; (ii) an INTERMEDIATE product that overflows (would raw-crash TYPE-ERROR).
  (expect-true "B3: out-of-range intermediate is REFUSED (no raw TYPE-ERROR)"
               (handler-case
                   (progn (oracle-run
                           (lower-to-program (make-core-program :funs nil
                                               :main '(* 100000 100000))))
                          nil)
                 (core-vm-i32-range-error () t)))
  ;; (iii) both asymmetric endpoints are explicit: -2^28 is representable,
  ;; while +2^28 is not.
  (let ((p (make-core-program :funs nil :main (1- +core-vm-value-limit+))))
    (expect "B3: 2^28-1 runs bit-exact on the oracle"
            (oracle-run (lower-to-program p)) (program-eval p)))
  (let ((p (make-core-program :funs nil :main (- +core-vm-value-limit+))))
    (expect "B3: -2^28 runs bit-exact on the oracle"
            (oracle-run (lower-to-program p)) (program-eval p)))
  (expect-true "B3: +2^28 is REFUSED"
               (handler-case
                   (progn
                     (oracle-run
                      (lower-to-program
                       (make-core-program :funs nil
                                          :main +core-vm-value-limit+)))
                     nil)
                 (core-vm-i32-range-error () t)))

  ;; The TYPE discipline rejects ill-formed terms (unbound var / bad arity).
  (expect-true "unbound variable is REJECTED"
               (handler-case (progn (term-check (prog-with-main '(+ y 1))) nil)
                 (core-type-error () t)))
  (expect-true "call arity mismatch is REJECTED"
               (handler-case (progn (term-check (prog-with-main '(sum 1 2))) nil)
                 (core-type-error () t)))
  (expect-true "call to undefined function is REJECTED"
               (handler-case (progn (term-check (prog-with-main '(nope 1))) nil)
                 (core-type-error () t)))
  (expect-true "function-valued DEFUN body is REJECTED"
               (handler-case
                   (progn
                     (term-check
                      (make-core-program
                       :funs '((bad (x) (lam y :int y)))
                       :main '(bad 1)))
                     nil)
                 (core-type-error () t)))

  ;; === (d) TYPED-LAMBDA LAYER =============================================
  (format t "~%-- typed-lambda layer --~%")
  (expect "eval (app (lam y (+ y 1)) 41)"
          (program-eval (make-core-program :funs nil
                          :main '(app (lam y :int (+ y 1)) 41))) 42)
  (expect "closure captures the lexical env"
          (program-eval (make-core-program :funs nil
                          :main '(let a 10 (app (lam y :int (+ y a)) 5)))) 15)
  (multiple-value-bind (v node)
      (program-normalize (make-core-program :funs nil
                           :main '(app (lam y :int (* y y)) 6)))
    (expect "normalize (app (lam y (* y y)) 6)" v 36)
    (expect-true "beta-app reduction emitted a proof node"
                 (> (rosette-proof-carrier-core:proof-node-size node) 3)))
  ;; a beta-normalizable lambda term lowers to first-order Lisp and runs on the
  ;; CPU oracle bit-exactly -- the lambda layer reaches coincidence 3 too.
  (let ((p (make-core-program :funs nil :main '(app (lam y :int (+ (* y y) 1)) 7))))
    (expect "lambda lowers + runs on CPU oracle"
            (oracle-run (lower-to-program p)) (program-eval p)))
  ;; type discipline for the lambda layer (negative gates with teeth):
  (expect-true "applying a non-function is REJECTED"
               (handler-case (progn (term-check (make-core-program :funs nil
                                                   :main '(app 5 3))) nil)
                 (core-type-error () t)))
  (expect-true "APP domain-type mismatch is REJECTED"
               (handler-case (progn (term-check (make-core-program :funs nil
                                      :main '(app (lam y :int y) (lam z :int z)))) nil)
                 (core-type-error () t)))
  (expect-true "MAIN of function type is REJECTED (must be :int)"
               (handler-case (progn (term-check (make-core-program :funs nil
                                                   :main '(lam y :int y))) nil)
                 (core-type-error () t)))

  ;; === (d) METACIRCULAR SELF-EVAL : eval_core(#e) = eval_core(e) ===========
  (format t "~%-- metacircular self-eval (E fragment) --~%")
  (let ((e1 '(+ (* 2 x) 3))                        ; arithmetic
        (e2 '(if (< x 5) (* x x) (- x 100)))       ; if + both branches
        (e3 '(% (+ x 7) 3)))                       ; %, exercises mod
    (dolist (x '(0 1 5 -4))
      (expect (format nil "self-eval e1(~a) = direct" x)
              (self-eval e1 x) (direct-eval-e e1 x)))
    (dolist (x '(3 5 7))
      (expect (format nil "self-eval e2(~a) = direct" x)
              (self-eval e2 x) (direct-eval-e e2 x)))
    (dolist (x '(0 4 9))
      (expect (format nil "self-eval e3(~a) = direct" x)
              (self-eval e3 x) (direct-eval-e e3 x)))
    ;; sealed metacircular certificate re-runs.
    (let ((cert (self-eval-certificate e2 '(-4 0 3 5 7 12))))
      (expect-true "self-eval certificate passes" (certificate-passed cert))
      (expect-true "self-eval certificate RE-RUNS"
                   (rosette-proof-witness:runtime-verify cert)))
    ;; NEGATIVE: two IN-equal encoded terms do NOT collide (encoding injective,
    ;; interpreter faithful).
    (let ((e1b '(+ (* 3 x) 3)))
      (expect-true "in-equal E-terms get DISTINCT encodings"
                   (/= (nth-value 0 (encode-e e1)) (nth-value 0 (encode-e e1b))))
      (expect-true "in-equal E-terms DIVERGE under the self-interpreter"
                   (not (eql (self-eval e1 2) (self-eval e1b 2)))))
    ;; NEGATIVE: a 1-token tamper of the encoding (flip the root operator tag
    ;; + -> -) diverges from the direct evaluator -- the decode is faithful.
    (multiple-value-bind (code height) (encode-e e1)
      (let ((bad (program-eval (make-core-program
                                :funs (self-interp-funs)
                                :main (list 'ev (+ code 1) 0 5 height)))))
        (expect-true "tampered encoding DIVERGES from direct eval"
                     (not (eql bad (direct-eval-e e1 5)))))))

  ;; === REUSABLE SEAM : certified-reduce over an EXTERNAL toy relation =======
  (format t "~%-- certified-reduce (external step relation) --~%")
  (multiple-value-bind (nf cert trace)
      (certified-reduce #'succ-step '(add (s (s z)) (s z)))
    (expect "certified-reduce normal form" nf '(s (s (s z))))
    (expect-true "the reduction really stepped (trace non-empty)" (plusp (length trace)))
    (expect-true "reduction certificate RE-RUNS (trace IS the certificate)"
                 (verify-reduction-certificate cert))
    (expect "reduction-normal-form accessor" (reduction-normal-form cert) '(s (s (s z))))
    ;; teeth 1: forge the sealed normal form.
    (let ((c2 (nth-value 1 (certified-reduce #'succ-step '(add (s z) (s (s z))))))
          (pl nil))
      (setf pl (copy-list (rosette-proof-witness:certificate-payload c2)))
      (setf (getf pl :normal-form) '(s z))
      (setf (rosette-proof-witness:certificate-payload c2) pl)
      (expect-true "FORGED normal form FAILS re-run"
                   (not (verify-reduction-certificate c2))))
    ;; teeth 2: forge the step count.
    (let ((c3 (nth-value 1 (certified-reduce #'succ-step '(add (s z) (s (s z))))))
          (pl nil))
      (setf pl (copy-list (rosette-proof-witness:certificate-payload c3)))
      (setf (getf pl :steps) 0)
      (setf (rosette-proof-witness:certificate-payload c3) pl)
      (expect-true "FORGED step count FAILS re-run"
                   (not (verify-reduction-certificate c3))))
    ;; teeth 3: doctor the trace fingerprint.
    (let ((c4 (nth-value 1 (certified-reduce #'succ-step '(add (s z) (s (s z))))))
          (pl nil))
      (setf pl (copy-list (rosette-proof-witness:certificate-payload c4)))
      (setf (getf pl :fingerprint) (list :n 999 :nodes nil))
      (setf (rosette-proof-witness:certificate-payload c4) pl)
      (expect-true "DOCTORED trace fingerprint FAILS re-run"
                   (not (verify-reduction-certificate c4)))))

  ;; === (e) F-FRAGMENT SELF-EVAL : let + multivar + first-order CALLs ========
  ;; The self-interpreter now covers `let`, MULTIPLE variables, and first-order
  ;; BOUNDED-RECURSIVE calls -- exactly the constructs its OWN body uses.  The
  ;; identity eval_core(#p) = eval_core(p) is checked against the DIRECT core
  ;; evaluator (direct-eval-f) for programs that genuinely use those constructs.
  (format t "~%-- F-fragment self-eval (let + multivar + first-order calls) --~%")
  ;; a program using LET and MULTIPLE variables (no calls):
  (let ((g nil)
        (main '(let s (+ x y) (let d (- x y) (* s d))))   ; (x+y)(x-y) = x^2-y^2
        (mv '(x y)))
    (dolist (in '((7 3) (10 4) (-5 2)))
      (expect (format nil "self-eval-f let+multivar ~a" in)
              (self-eval-f g main mv in 200)
              (direct-eval-f g main mv in))))
  ;; a program using first-order RECURSIVE calls (sum) through the interpreter:
  (let ((g '(g (n) (if (< n 1) 0 (+ n (call (- n 1))))))
        (main '(call x)) (mv '(x)))
    (dolist (in '((0) (1) (5) (50) (100)))
      (expect (format nil "self-eval-f sum~a" in)
              (self-eval-f g main mv in 5000)
              (direct-eval-f g main mv in))))
  ;; recursion + let INSIDE the called function (power b^e):
  (let ((g '(g (b e) (if (< e 1) 1 (let r (call b (- e 1)) (* b r)))))
        (main '(call x y)) (mv '(x y)))
    (dolist (in '((2 0) (2 10) (3 5)))
      (expect (format nil "self-eval-f pow~a" in)
              (self-eval-f g main mv in 500)
              (direct-eval-f g main mv in))))
  ;; two-argument recursion (Euclid gcd) -- exercises %, =, nested if, recursion:
  (let ((g '(g (a b) (if (= b 0) a (call b (% a b)))))
        (main '(call x y)) (mv '(x y)))
    (dolist (in '((48 36) (17 5) (100 0)))
      (expect (format nil "self-eval-f gcd~a" in)
              (self-eval-f g main mv in 500)
              (direct-eval-f g main mv in))))
  ;; sealed F metacircular certificate re-runs (re-drives the whole tower).
  (let ((cert (self-eval-f-certificate
               '(g (n) (if (< n 1) 0 (+ n (call (- n 1)))))
               '(call x) '(x) '((0) (5) (50) (100)) 5000)))
    (expect-true "self-eval-f certificate passes" (certificate-passed cert))
    (expect-true "self-eval-f certificate RE-RUNS (whole tower re-drives)"
                 (rosette-proof-witness:runtime-verify cert)))
  ;; NEGATIVE: two IN-equal F-programs get DISTINCT encodings AND diverge.
  (let ((p1 '(let s (+ x y) (* s s)))
        (p2 '(let s (+ x y) (* s x))))
    (expect-true "in-equal F-programs get DISTINCT encodings"
                 (/= (nth-value 0 (encode-f p1 '(x y)))
                     (nth-value 0 (encode-f p2 '(x y)))))
    (expect-true "in-equal F-programs DIVERGE under the F self-interpreter"
                 (not (eql (self-eval-f nil p1 '(x y) '(3 4) 200)
                           (self-eval-f nil p2 '(x y) '(3 4) 200)))))
  ;; NEGATIVE (teeth): a 1-token tamper of the encoded main (bump code by 1,
  ;; flipping the root slot's tag/payload) diverges from the direct evaluator.
  (let* ((main '(+ (* 2 x) (* 3 y))) (mv '(x y)) (in '(5 7))
         (mcode (nth-value 0 (encode-f main mv)))
         (bad (self-eval-f-raw (1+ mcode) mv in 200)))
    (expect-true "tampered F encoding DIVERGES from direct eval"
                 (not (eql bad (direct-eval-f nil main mv in)))))

  ;; === (e) REFLECTIVE-FIXPOINT WALL : the obstruction, made falsifiable ======
  ;; The full reflective fixpoint (interpreter on #ITSELF) is STUB.  We do not
  ;; hand-wave it: we GATE the two structural walls that block it, so the
  ;; boundary is a demonstrated fact, not a claim.
  (format t "~%-- reflective-fixpoint walls (demonstrated obstruction) --~%")
  ;; (a) VALUE-CAPACITY wall: even a tiny program's code exceeds the object
  ;;     value capacity, so #p cannot be an in-language interpreter's argument.
  (expect-true "value-capacity wall: (+ x y) code does NOT fit as an object value"
               (not (f-code-fits-as-value-p '(+ x y) '(x y))))
  ;; (b) EXPONENTIAL-INDEX wall: arity-3 heap index grows ~3^depth, so a deep
  ;;     program's code has ~26*index bits -- intractable to even form.
  (let ((shallow (f-heap-max-index '(+ x y) '(x y)))
        (deep (f-heap-max-index
               '(let a 1 (let a 1 (let a 1 (let a 1 (let a 1 (let a 1 x))))))
               '(x))))
    (expect-true "exponential-index wall: index grows super-linearly with depth"
                 (> deep (* 50 shallow))))

  (if (zerop *fails*)
      (progn (format t "rosette-core-term: ALL TESTS PASS~%") t)
      (error "rosette-core-term: ~a test(s) failed" *fails*)))
