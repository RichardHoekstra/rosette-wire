;;;; tests.lisp --- executable claims for the named front door.

(defpackage #:rosette-front-door/tests
  (:use #:cl #:rosette-assert-core)
  (:shadowing-import-from #:rosette-front-door #:eval)
  (:import-from #:rosette-front-door
                #:parse #:lower #:gate #:certify #:run
                #:surface->program-ir #:program-ir->surface
                #:front-door-result-p #:front-door-result-program-ir
                #:front-door-result-eval-value #:front-door-result-oracle-value
                #:front-door-result-gate-passed #:front-door-result-certificate
                #:gauge-term #:term-gauge-report-p
                #:term-gauge-report-core-value #:term-gauge-report-ccc-value
                #:term-gauge-report-blc-value #:term-gauge-report-blc-length
                #:term-gauge-report-passed #:term-gauge-report-certificate
                #:ccc-gauge-cost #:term-gauge-report-ccc-cost
                #:term-gauge-report-ccc-skipped
                #:cf-expression->core-term #:run-cf-expression
                #:front-door-refusal)
  (:export #:run-all-tests))

(in-package #:rosette-front-door/tests)

(defparameter *surface*
  '((defun poly (x)
      (let a (+ (* 2 x) 3)
        (let b (+ (* a x) 4)
          (+ (* b x) 5))))
    (defun sum (n)
      (if (< n 1) 0 (+ n (sum (- n 1)))))
    (main (+ (poly 7) (sum 100)))))

(defun signals-refusal-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (front-door-refusal () t)))

(defun run-all-tests ()
  (with-test-run (test "rosette-front-door")
    ;; Surface -> typed core -> ProgramIR -> surface is one route.
    (let* ((program (parse *surface*))
           (program-ir (surface->program-ir *surface*)))
      (check test (rosette-core-term:core-program-p program)
             "PARSE returns the substrate core program")
      (check test (rosette-program-ir:programp program-ir)
             "the same surface has a ProgramIR view")
      (check test (equal *surface* (program-ir->surface program-ir))
             "ProgramIR round-trips to the canonical surface")
      (check test (eq program (parse program))
             "an already typed core program is admitted without replacement")
      (check test (rosette-core-term:core-program-p (parse program-ir))
             "the ProgramIR view enters through the same parser"))

    ;; Direct meaning and kernel-oracle meaning agree bit-exactly.
    (check test (= 5916 (eval *surface*))
           "direct evaluator computes the polynomial plus bounded sum")
    (let ((lowered (lower *surface*)))
      (check test (= 5916 (rosette-core-term:oracle-run lowered))
             "LOWER reaches the shared kernel VM CPU oracle"))
    (multiple-value-bind (passed direct oracle) (gate *surface*)
      (check test passed "GATE admits exact eval/oracle agreement")
      (check test (= direct oracle) "GATE exposes both equal readouts"))

    ;; RUN carries every readout, and CERTIFY's claim has its own falsifier.
    (let* ((result (run *surface*))
           (certificate (front-door-result-certificate result)))
      (check test (front-door-result-p result) "RUN returns a typed result")
      (check test (rosette-program-ir:programp
                   (front-door-result-program-ir result))
             "RUN retains the ProgramIR view")
      (check test (= 5916 (front-door-result-eval-value result))
             "RUN retains direct evaluation")
      (check test (= (front-door-result-eval-value result)
                     (front-door-result-oracle-value result))
             "RUN retains the independent oracle readout")
      (check test (front-door-result-gate-passed result)
             "RUN's gate passes")
      (check test (rosette-proof-witness:runtime-verify certificate)
             "RUN's certificate independently re-runs")
      (let ((saved (copy-tree (rosette-proof-witness:certificate-payload certificate))))
        (setf (getf (rosette-proof-witness:certificate-payload certificate) :eval-value)
              -1)
        (check test (not (rosette-proof-witness:runtime-verify certificate))
               "a tampered gate payload fails its re-run")
        (setf (rosette-proof-witness:certificate-payload certificate) saved)))
    (check test (rosette-proof-witness:runtime-verify (certify *surface*))
           "CERTIFY is independently useful")

    ;; Parsing and semantic typing fail closed.
    (check test (signals-refusal-p (lambda () (parse '((wat 1) (main 2)))))
           "unknown surface forms are refused")
    (check test (signals-refusal-p (lambda () (parse '((main 1) (main 2)))))
           "multiple MAIN forms are refused")
    (check test (signals-refusal-p (lambda () (parse '((defun f (x) x)))))
           "a missing MAIN is refused")
    (check test
           (signals-refusal-p
            (lambda () (parse '((defun f (x) x) (defun f (y) y) (main 0)))))
           "duplicate function names are refused")
    (check test (signals-refusal-p (lambda () (parse '((main missing)))))
           "an unbound core term is refused during parse")

    ;; One honest closed STLC term has core, CCC, and BLC meanings.
    (let* ((term '(app (lam x :int x) 2))
           (report (gauge-term term :integer-domain '(0 1 2 3))))
      (check test (term-gauge-report-p report) "GAUGE-TERM returns a report")
      (check test (= 2 (term-gauge-report-core-value report))
             "the core readout is correct")
      (check test (= 2 (term-gauge-report-ccc-value report))
             "the independent FinSet-CCC denotation agrees")
      (check test (= 2 (term-gauge-report-blc-value report))
             "the independent BLC Church reduction agrees")
      (check test (plusp (term-gauge-report-blc-length report))
             "the very same BLC term has a measured bit length")
      (check test (term-gauge-report-passed report)
             "the three-way gauge passes")
      (check test
             (rosette-proof-witness:runtime-verify
              (term-gauge-report-certificate report))
             "the three-way gauge certificate re-runs"))
    (let ((report
            (gauge-term '(let x 2 (app (lam y :int y) x))
                        :integer-domain '(0 1 2 3))))
      (check test (and (term-gauge-report-passed report)
                       (= 2 (term-gauge-report-blc-value report)))
             "pure LET desugars identically for CCC and BLC"))
    (check test
           (signals-refusal-p
            (lambda () (gauge-term '(+ 1 2) :integer-domain '(0 1 2 3))))
           "arithmetic outside the common STLC fragment is refused")
    (check test
           (signals-refusal-p
            (lambda () (gauge-term -1 :integer-domain '(-1 0 1))))
           "negative integers are refused by the Church-numeral regime")
    (check test
           (signals-refusal-p
            (lambda () (gauge-term 2 :integer-domain '(0 1))))
           "CCC literals outside the declared finite domain are refused")

    ;; The extensional (CCC/FinSet) gauge is enumerative -- it materializes the
    ;; whole exponential object B^A, so its cost is |B|^|A|.  CCC-GAUGE-COST
    ;; prices that a priori, and :BUDGET turns the blowup into a LOCATED WALL:
    ;; the operational and BLC gauges still witness the invariant.
    (check test (= (expt 4 4)
                   (ccc-gauge-cost '(app (lam x :int x) 3) :integer-domain '(0 1 2 3)))
           "CCC-GAUGE-COST prices the identity's exponential object as |B|^|A|")
    (let ((report (gauge-term '(app (lam x :int x) 3)
                              :integer-domain '(0 1 2 3 4 5) :budget 1000)))
      (check test (term-gauge-report-ccc-skipped report)
             "an over-budget extensional gauge is skipped, not run")
      (check test (eq :wall (term-gauge-report-ccc-value report))
             "the skipped CCC value is the :WALL sentinel")
      (check test (= (expt 6 6) (term-gauge-report-ccc-cost report))
             "the located wall's magnitude |B|^|A| is reported")
      (check test (term-gauge-report-passed report)
             "agreement still holds on the operational and BLC gauges")
      (check test (rosette-proof-witness:runtime-verify
                   (term-gauge-report-certificate report))
             "the CCC-walled certificate honestly re-verifies"))

    ;; Exact expression-core polynomials reach the same kernel gate.
    (let* ((x (rosette-expression-core:cf-var 'x))
           (polynomial
             (rosette-expression-core:cf-add
              (rosette-expression-core:cf-mul x x)
              (rosette-expression-core:cf-const 3)))
           (term (cf-expression->core-term polynomial :bindings '((x . 7))))
           (result (run-cf-expression polynomial :bindings '((x . 7)))))
      (check test (equal '(+ (* 7 7) 3) term)
             "the CF polynomial lowers to an exact integer core term")
      (check test (= 52 (front-door-result-eval-value result))
             "the admitted CF polynomial evaluates exactly")
      (check test (front-door-result-gate-passed result)
             "the admitted CF polynomial reaches and passes the kernel gate"))
    (check test
           (signals-refusal-p
            (lambda ()
              (cf-expression->core-term
               (rosette-expression-core:cf-div
                (rosette-expression-core:cf-const 1)
                (rosette-expression-core:cf-const 2)))))
           "floating division has no accidental integer admission")
    (check test
           (signals-refusal-p
            (lambda ()
              (cf-expression->core-term
               (rosette-expression-core:cf-const 3/2))))
           "a non-integral constant is refused")
    (check test
           (signals-refusal-p
            (lambda ()
              (cf-expression->core-term (rosette-expression-core:cf-var 'x))))
           "a CF variable without an exact binding is refused")))
