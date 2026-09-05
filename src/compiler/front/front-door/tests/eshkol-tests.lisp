;;;; eshkol-tests.lisp --- ProgramIR admission/emission and real backend gates.

(defpackage #:rosette-front-door/eshkol/tests
  (:use #:cl)
  (:local-nicknames (#:esh #:rosette-front-door/eshkol)
                    (#:front #:rosette-front-door)
                    (#:pir #:rosette-program-ir)
                    (#:worker #:rosette-isolated-worker))
  (:export #:run-all-tests))

(in-package #:rosette-front-door/eshkol/tests)

(defvar *passes* 0)
(defvar *fails* 0)

(defmacro check (name expression)
  `(if ,expression
       (progn (incf *passes*) (format t "~&  PASS  ~A~%" ',name))
       (progn (incf *fails*)
              (format t "~&  FAIL  ~A~%    expression: ~S~%"
                      ',name ',expression))))

(defun signals-backend-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (esh:eshkol-backend-error () t)))

(defun caught-backend-error (thunk)
  (handler-case (progn (funcall thunk) nil)
    (esh:eshkol-backend-error (condition) condition)))

(defun fake-runner-path ()
  (uiop:native-namestring
   (asdf:system-relative-pathname
    :front-door/eshkol/tests "tests/fixtures/fake-eshkol.sh")))

(defun fake-command (program-id value &key (mode "ok") output-log)
  (append
   (list "env"
         (format nil "ROSETTE_FAKE_ESHKOL_MODE=~A" mode)
         (format nil "ROSETTE_FAKE_ESHKOL_PROGRAM_ID=~A" program-id)
         (format nil "ROSETTE_FAKE_ESHKOL_VALUE=~D" value))
   (when output-log
     (list (format nil "ROSETTE_FAKE_ESHKOL_OUTPUT_LOG=~A"
                   (uiop:native-namestring output-log))))
   (list "sh" (fake-runner-path))))

(defparameter *integration-surface*
  '((defun fact (n)
      (if (= n 0) 1 (* n (fact (- n 1)))))
    (defun gcd (a b)
      (if (= b 0) a (gcd b (% a b))))
    (defun even-n (n)
      (if (= n 0) 1 (odd-n (- n 1))))
    (defun odd-n (n)
      (if (= n 0) 0 (even-n (- n 1))))
    (defun capture (y)
      (let z y (let y 0 z)))
    (defun divisions (x)
      (+ (* (/ x 3) 10) (% x 3)))
    (defun beta-capture (x)
      (app (lam y :int (+ y x)) 5))
    (defun |A-B| (|X-Y| |X_Y|)
      (+ |X-Y| |X_Y|))
    (main
     (+ (fact 6)
        (+ (gcd 48 18)
           (+ (even-n 8)
              (+ (capture 7)
                 (+ (divisions -8)
                    (+ (beta-capture 9) (|A-B| 2 3))))))))))

(defun external-configuration ()
  "Return (values compiler-argv aot-run-prefix description), or NILs."
  (let ((binary (uiop:getenv "ROSETTE_ESHKOL_BIN"))
        (container (uiop:getenv "ROSETTE_ESHKOL_CONTAINER")))
    (cond
      ((and binary (plusp (length binary)))
       (values binary nil binary))
      ((and container (plusp (length container)))
       (values
        (list "docker" "exec"
              "-e" "ESHKOL_JIT_CACHE=0"
              "-e" "ESHKOL_JIT_COMPILE_THREADS=1"
              "-e" "ESHKOL_TIMEOUT_MS=110000"
              container
              "timeout" "--signal=TERM" "--kill-after=2s" "115s"
              "/eshkol/build/eshkol-run")
        (list "docker" "exec" container
              "timeout" "--signal=TERM" "--kill-after=2s" "115s")
        (format nil "container ~A" container)))
      (t (values nil nil nil)))))

(defun container-client-policy ()
  "Ceilings for the outer Docker client.

The compiler container owns the tighter PID/CPU/memory ceilings. RLIMIT_NPROC
is per host user, so the client ceiling must exceed unrelated existing host
processes or the outer Docker client can be refused before launch."
  (worker:make-isolation-policy
   :timeout-ms 120000 :max-output-bytes 1048576
   :max-address-space-bytes (* 128 1024 1024 1024)
   :max-cpu-seconds 120 :max-processes 4096 :max-open-files 256 :poll-ms 10))

(defun run-all-tests ()
  (setf *passes* 0 *fails* 0)
  (format t "~&;; rosette-front-door/eshkol test run -------------------------~%")

  ;; ProgramIR is the public handoff, but only the front-door dialect is
  ;; admitted. Emission is deterministic and source-bound by a durable id.
  (let ((ir (front:surface->program-ir '((main (if 0 11 22))))))
    (multiple-value-bind (source-a id-a) (esh:emit-eshkol-source ir)
      (multiple-value-bind (source-b id-b) (esh:emit-eshkol-source ir)
        (check deterministic-source (string= source-a source-b))
        (check deterministic-program-id (string= id-a id-b))
        (check durable-program-id
               (rosette-content-identity:content-id-long-p id-a))
        (check public-emitter-returns-exactly-two-values
               (= 2 (length (multiple-value-list
                              (esh:emit-eshkol-source ir)))))
        (check rosette-zero-truth-is-explicit
               (search "(if (= 0 0) 22 11)" source-a))
        (check receipt-is-bound-to-program-id (search id-a source-a)))))

  (multiple-value-bind (source id)
      (esh:emit-eshkol-source
       (front:surface->program-ir
        '((main (+ (/ -8 3) (% -8 3))))))
    (declare (ignore id))
    (check truncating-division-maps-to-quotient
           (search "(quotient -8 3)" source))
    (check truncating-remainder-maps-to-remainder
           (search "(remainder -8 3)" source)))

  (multiple-value-bind (source id)
      (esh:emit-eshkol-source
       (front:surface->program-ir '((main (+ (< 1 2) (> 3 4))))))
    (declare (ignore id))
    (check comparison-results-are-boxed-integers
           (and (search "(if (< 1 2) 1 0)" source)
                (search "(if (> 3 4) 1 0)" source))))

  (multiple-value-bind (source id)
      (esh:emit-eshkol-source
       (front:surface->program-ir
        '((main (app (lam x :int (+ x 1)) 4)))))
    (declare (ignore id))
    (check beta-terms-emit-native-lambda-and-application
           (and (search "(lambda (" source)
                (search ")) 4)" source))))

  (check punctuation-mangling-is-injective
         (not (string=
               (esh::%mangle-identifier '|A-B| :value)
               (esh::%mangle-identifier '|A_B| :value))))
  (check function-and-value-namespaces-are-separated
         (not (string=
               (esh::%mangle-identifier 'same :function)
               (esh::%mangle-identifier 'same :value))))

  (check arbitrary-program-ir-is-refused
         (signals-backend-error-p
          (lambda ()
            (esh:emit-eshkol-source (pir:lisp->program '(+ 1 2))))))
  (check non-program-ir-is-refused
         (signals-backend-error-p
          (lambda () (esh:emit-eshkol-source '((main 1))))))

  ;; Availability is explicit and bounded; a missing optional tool is not
  ;; converted into a fake pass.
  (check missing-toolchain-is-unavailable
         (not (esh:eshkol-available-p
               :eshkol-command "rosette-definitely-missing-eshkol-run")))

  ;; The state machine is exercised on every host with a deterministic fake
  ;; runner. It validates receipt binding, stage/status propagation, missing
  ;; artifacts, and cleanup; only the opt-in rung below claims Eshkol semantics.
  (let* ((ir (front:surface->program-ir '((main 7))))
         (program-id (nth-value 1 (esh:emit-eshkol-source ir)))
         (policy (container-client-policy)))
    (uiop:with-temporary-file
        (:pathname output-log :stream output-log-stream
         :prefix "rosette-fake-eshkol-output-" :type "log"
         :direction :io)
      (let* ((report
               (esh:gate-eshkol
                ir :eshkol-command
                   (fake-command program-id 7 :output-log output-log)
                   :policy policy))
             (artifact-path
               (progn
                 (finish-output output-log-stream)
                 (file-position output-log-stream 0)
                 (read-line output-log-stream nil nil))))
        (check fake-runner-four-way-gate
               (and (esh:eshkol-gate-report-passed report)
                    (= 7 (esh:eshkol-gate-report-jit-value report))
                    (= 7 (esh:eshkol-gate-report-aot-value report))))
        (check fake-runner-artifact-is-cleaned
               (and artifact-path
                    (not (probe-file artifact-path))
                    (not (probe-file
                          (uiop:pathname-directory-pathname artifact-path)))))))

    (let ((condition
            (caught-backend-error
             (lambda ()
               (esh:gate-eshkol
                ir :eshkol-command
                   (fake-command program-id 7 :mode "malformed-jit")
                   :policy policy)))))
      (check malformed-jit-receipt-fails-at-jit
             (and condition
                  (eq :jit (esh:eshkol-backend-error-stage condition))
                  (eq :malformed-receipt
                      (esh:eshkol-backend-error-status condition)))))

    (let ((condition
            (caught-backend-error
             (lambda ()
               (esh:gate-eshkol
                ir :eshkol-command
                   (fake-command "wrong-program-id" 7)
                   :policy policy)))))
      (check jit-receipt-is-bound-to-program-id
             (and condition
                  (eq :jit (esh:eshkol-backend-error-stage condition))
                  (eq :malformed-receipt
                      (esh:eshkol-backend-error-status condition)))))

    (let ((condition
            (caught-backend-error
             (lambda ()
               (esh:gate-eshkol
                ir :eshkol-command
                   (fake-command program-id 7 :mode "compile-fail")
                   :policy policy)))))
      (check failed-compile-propagates-stage
             (and condition
                  (eq :aot-compile
                      (esh:eshkol-backend-error-stage condition))
                  (eq :worker-error
                      (esh:eshkol-backend-error-status condition)))))

    (let ((condition
            (caught-backend-error
             (lambda ()
               (esh:gate-eshkol
                ir :eshkol-command
                   (fake-command program-id 7 :mode "missing-artifact")
                   :policy policy)))))
      (check missing-aot-artifact-fails-closed
             (and condition
                  (eq :aot-compile
                      (esh:eshkol-backend-error-stage condition))
                  (eq :missing-artifact
                      (esh:eshkol-backend-error-status condition)))))

    (let ((condition
            (caught-backend-error
             (lambda ()
               (esh:gate-eshkol
                ir :eshkol-command
                   (fake-command program-id 7 :mode "malformed-aot")
                   :policy policy)))))
      (check malformed-aot-receipt-fails-at-aot-run
             (and condition
                  (eq :aot-run (esh:eshkol-backend-error-stage condition))
                  (eq :malformed-receipt
                      (esh:eshkol-backend-error-status condition)))))

    (let ((condition
            (caught-backend-error
             (lambda ()
               (esh:gate-eshkol
                'not-program-ir
                :eshkol-command "rosette-definitely-missing-eshkol-run"
                :policy policy)))))
      (check admission-precedes-toolchain-probe
             (and condition
                  (eq :admit (esh:eshkol-backend-error-stage condition))
                  (eq :not-program-ir
                      (esh:eshkol-backend-error-status condition)))))

    ;; The front-door type system can represent a function selected at run
    ;; time, but the canonical kernel floor only lowers literal beta redexes.
    ;; Refuse that located wall before even probing the configured command.
    (let* ((residual-application
             (front:surface->program-ir
              '((main
                 (app (if 1 (lam x :int x) (lam y :int y)) 3)))))
           (condition
             (caught-backend-error
              (lambda ()
                (esh:gate-eshkol
                 residual-application
                 :eshkol-command "rosette-definitely-missing-eshkol-run"
                 :policy policy)))))
      (check kernel-preflight-precedes-toolchain-probe
             (and condition
                  (eq :kernel-preflight
                      (esh:eshkol-backend-error-stage condition))
                  (eq :unsupported-shape
                      (esh:eshkol-backend-error-status condition))))))

  ;; Real JIT/AOT checks are opt-in so clean hosts keep a green base suite
  ;; without pretending Eshkol ran. The release witness sets this path.
  (multiple-value-bind (command aot-prefix description)
      (external-configuration)
    (if command
        (let* ((container-p (uiop:getenv "ROSETTE_ESHKOL_CONTAINER"))
               (policy (and container-p (container-client-policy)))
               (ir (front:surface->program-ir *integration-surface*))
               (report (esh:gate-eshkol
                        ir :eshkol-command command
                           :aot-run-prefix aot-prefix
                           :policy (or policy
                                       (worker:make-isolation-policy
                                        :timeout-ms 120000)))))
          (format t "~&  INFO  real Eshkol via ~A~%" description)
          (check direct-value (= 731 (esh:eshkol-gate-report-direct-value report)))
          (check kernel-value (= 731 (esh:eshkol-gate-report-kernel-value report)))
          (check jit-value (= 731 (esh:eshkol-gate-report-jit-value report)))
          (check aot-value (= 731 (esh:eshkol-gate-report-aot-value report)))
          (check four-way-gate-passes (esh:eshkol-gate-report-passed report))
          ;; Compile and execute a real + -> - emitter mutant. The external
          ;; backend succeeds, but semantic agreement must redden.
          (let ((mutant
                  (esh::%gate-eshkol
                   ir :eshkol-command command
                      :aot-run-prefix aot-prefix
                      :policy (or policy
                                  (worker:make-isolation-policy
                                   :timeout-ms 120000))
                      :operator-mutant :add-to-sub)))
            (check executed-emitter-mutant-is-caught
                   (not (esh:eshkol-gate-report-passed mutant)))))
        (format t "~&  SKIP  real Eshkol JIT/AOT ~
(ROSETTE_ESHKOL_BIN and ROSETTE_ESHKOL_CONTAINER unset)~%")))

  (format t "~&==== rosette-front-door/eshkol: ~D passes, ~D failures ====~%"
          *passes* *fails*)
  (assert (zerop *fails*))
  t)
