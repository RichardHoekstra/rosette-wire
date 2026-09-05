;;;; tests.lisp --- rosette-ship gate. Hermetic unit tests over the pure closure,
;;;; emit, floor, and parity logic, plus one real-closure integration.

(defpackage #:rosette-ship/tests
  (:use #:cl #:rosette-ship)
  (:export #:run-all-tests))

(in-package #:rosette-ship/tests)

(defvar *pass* 0)
(defvar *fail* 0)
(defvar *test-web-html* "<!doctype html><title>test</title><p>sealed</p>")

(defun %emit-test-web (&key path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string *test-web-html* out))
  path)

(defmacro check (form &optional msg)
  `(handler-case
       (if ,form
           (incf *pass*)
           (progn (incf *fail*)
                  (format t "~&FAIL: ~S~@[ -- ~A~]~%" ',form ,msg)))
     (error (e)
       (incf *fail*)
       (format t "~&ERROR in ~S: ~A~%" ',form e))))

;;; ---- a synthetic dependency graph (no libraries loaded) ---------------
;;;   a -> b, c ; b -> d ; c -> d ; d -> (leaf)
(defparameter *g*
  '(("a" . ("b" "c")) ("b" . ("d")) ("c" . ("d")) ("d" . ())))

(defun g-deps (name) (cdr (assoc name *g* :test #'string=)))

(defun test-dep-name ()
  (check (string= "foo" (dep-name "Foo")))
  (check (string= "rosette-x" (dep-name :rosette-x)))
  (check (string= "rosette-x" (dep-name '#:rosette-x)))
  (check (string= "rosette-y" (dep-name '(:version #:rosette-y "1.0"))))
  (check (string= "rosette-z" (dep-name '(:feature :sbcl #:rosette-z)))))

(defun test-closure-order ()
  (let ((order (library-closure "a" :deps-fn #'g-deps)))
    (check (= 4 (length order)) "closure covers all reachable")
    (check (string= "a" (car (last order))) "root loads last")
    (check (string= "d" (first order)) "leaf loads first")
    ;; dependencies strictly precede dependents
    (flet ((idx (n) (position n order :test #'string=)))
      (check (< (idx "d") (idx "b")) "d before b")
      (check (< (idx "d") (idx "c")) "d before c")
      (check (< (idx "b") (idx "a")) "b before a")
      (check (< (idx "c") (idx "a")) "c before a"))))

(defun test-closure-cycle-safe ()
  ;; a -> b -> a : must terminate, keep both once.
  (let ((cyc '(("a" . ("b")) ("b" . ("a")))))
    (flet ((d (n) (cdr (assoc n cyc :test #'string=))))
      (let ((order (library-closure "a" :deps-fn #'d)))
        (check (= 2 (length order)) "cycle terminates, dedup")
        (check (member "a" order :test #'string=))
        (check (member "b" order :test #'string=))))))

(defun test-compute-slice-missing ()
  ;; "c" is unresolvable; must be flagged, never dropped from the order.
  (let ((slice (compute-slice "a" :deps-fn #'g-deps
                                   :resolvable-fn (lambda (n) (not (string= n "c"))))))
    (check (= 4 (slice-lib-count slice)))
    (check (equal '("c") (slice-missing slice)) "unresolvable flagged")
    (check (string= "a" (slice-root slice)))))

(defun test-reduction-report ()
  (let ((r (make-reduction-report :whole 100 :kept 4 :files 6 :lines 900)))
    (check (= 1/25 (reduction-report-ratio r)))
    (check (eq :not-applied (reduction-report-form-level r)))
    (check (equal 900 (getf (reduction-report->plist r) :lines))))
  ;; unknown whole => ratio 1 (never divide by zero)
  (check (= 1 (reduction-report-ratio (make-reduction-report :whole 0 :kept 3)))))

(defun test-emit-reduced-file ()
  (let* ((tmp (ensure-directories-exist
               (merge-pathnames (format nil "rosette-ship-test-~A/" (get-universal-time))
                                (uiop:temporary-directory))))
         (fa (merge-pathnames "a.lisp" tmp))
         (fb (merge-pathnames "b.lisp" tmp))
         (out (merge-pathnames "reduced.lisp" tmp)))
    (with-open-file (s fa :direction :output :if-exists :supersede)
      (format s "(defun a () 1)~%(defun a2 () 2)~%"))
    (with-open-file (s fb :direction :output :if-exists :supersede)
      (format s "(defun b () 3)~%"))
    (let* ((slice (make-slice :root "b" :order '("a" "b")))
           (src (lambda (n) (cond ((string= n "a") (list fa))
                                  ((string= n "b") (list fb)))))
           (rep (emit-reduced-file slice out :source-fn src
                                              :whole-count-fn (lambda () 10))))
      (check (probe-file out) "reduced file written")
      (check (= 2 (reduction-report-files rep)) "two source files copied")
      (check (= 3 (reduction-report-lines rep)) "three source lines copied")
      (check (= 2 (reduction-report-kept rep)))
      (check (= 10 (reduction-report-whole rep)))
      (check (= 1/5 (reduction-report-ratio rep)))
      (check (null (reduction-report-missing rep)) "nothing missing")
      ;; the emitted file must be readable Lisp, deps-first
      (with-open-file (in out :direction :input)
        (let ((forms (loop for f = (read in nil :eof) until (eq f :eof) collect f)))
          (check (= 3 (length forms)) "three top-level forms survive"))))))

(defun test-floors-bin ()
  (let* ((slice (make-slice :root "demo" :order '("demo")))
         (art (lower-to-floor slice :bin :entrypoint "DEMO:MAIN"
                                          :out-dir "/tmp/rosette-ship-test-bin")))
    (check (eq :bin (floor-artifact-floor art)))
    (check (probe-file (floor-artifact-driver art)) "build driver emitted")
    (check (probe-file (floor-artifact-reduced-file art)) "reduced file emitted")
    (check (search "sbcl" (first (floor-artifact-build-command art))) "sbcl build cmd")
    (with-open-file (in (floor-artifact-driver art))
      (let ((txt (make-string (file-length in))))
        (read-sequence txt in)
        (check (search "save-lisp-and-die" txt) "driver dumps an executable")
        (check (search "DEMO:MAIN" txt) "driver calls the entrypoint")))))

(defun test-floors-jit ()
  ;; :jit is a self-contained binary with the compiler LIVE inside it.
  (let* ((slice (make-slice :root "demo" :order '("demo")))
         (art (lower-to-floor slice :jit :entrypoint "DEMO:MAIN"
                                          :expr "(demo:smoke)"
                                          :out-dir "/tmp/rosette-ship-test-jit")))
    (check (eq :jit (floor-artifact-floor art)))
    (check (probe-file (floor-artifact-driver art)) "jit build driver emitted")
    (check (probe-file (floor-artifact-reduced-file art)) "reduced file emitted")
    (check (search "sbcl" (first (floor-artifact-build-command art))) "sbcl build cmd")
    (with-open-file (in (floor-artifact-driver art))
      (let ((txt (make-string (file-length in))))
        (read-sequence txt in)
        (check (search "save-lisp-and-die" txt) "jit dumps a real executable")
        (check (search "save-runtime-options" txt) "binary owns its argv")
        (check (search "--repl" txt) "live compile-eval loop exposed")
        (check (search "--eval" txt) "runtime eval exposed")
        (check (search "(eval " txt) "compiler live: read->compile->eval in-binary")
        (check (search "(args (funcall (read-from-string \"DEMO:MAIN\")))" txt)
               "positional arguments dispatch to the real entrypoint")
        (check (search "(demo:smoke)" txt)
               "empty invocation retains the parity smoke expression")))))

(defun test-floors-supported-and-guard ()
  (check (equal '(:bin :jit :oci :mcp) (supported-floors)))
  (check (null (ignore-errors (ship "whatever" :floor :nonsense)))
         "unsupported floor is rejected"))

(defun test-deliverable-behavior-cases ()
  (let* ((artifact (make-floor-artifact :floor :jit :output #P"/usr/bin/printf"))
         (green (rosette-ship::%run-behavior-cases
                 artifact
                 '((:name "prints" :args ("%s" "hello sovereign")
                    :exit 0 :contains ("hello" "sovereign")))))
         (red (rosette-ship::%run-behavior-cases
               artifact
               '((:name "missing-fragment" :args ("%s" "hello")
                  :exit 0 :contains ("absent"))))))
    (check (rosette-ship::%behavior-green-p green)
           "matching executable behavior is green")
    (check (not (rosette-ship::%behavior-green-p red))
           "missing output fragment falsifies behavior")
    (check (rosette-content-identity:durable-content-id-p
            (getf (first green) :output-content-id))
           "behavior output carries a durable identity")))

(defun test-form-level-reduce ()
  ;; Tree-shake: keep entry (seeded) + its transitive refs + anything an
  ;; anchor (a top-level call) references; drop the unreachable helper.
  (let* ((tmp (ensure-directories-exist
               (merge-pathnames (format nil "rosette-ship-shake-~A/" (get-universal-time))
                                (uiop:temporary-directory))))
         (fa (merge-pathnames "a.lisp" tmp))
         (out (merge-pathnames "shaken.lisp" tmp)))
    (with-open-file (s fa :direction :output :if-exists :supersede)
      (format s "(in-package :cl-user)~%")
      (format s "(defun used-helper (x) (* x 2))~%")
      (format s "(defun dead-helper (x) (+ x 999))~%")
      (format s "(defun kept-via-anchor (x) (1+ x))~%")
      (format s "(defun entry () (used-helper 21))~%")
      (format s "(print (kept-via-anchor 5))~%"))
    (let* ((slice (make-slice :root "demo" :order '("demo")))
           (src (lambda (n) (declare (ignore n)) (list fa)))
           (rep (form-level-reduce slice out :seeds '("cl-user::entry")
                                              :source-fn src)))
      (check rep "analysis ran")
      (check (= 4 (getf rep :total)) "four prunable defuns")
      (check (= 3 (getf rep :kept)) "entry + used-helper + anchor-referenced kept")
      (check (= 1 (getf rep :dropped)) "one dead helper dropped")
      (check (= 1/4 (getf rep :pruned-ratio)))
      (let ((txt (uiop:read-file-string out)))
        (check (search "in-package" txt) "package context (anchor) kept")
        (check (search "(defun entry" txt) "seeded entry kept")
        (check (search "used-helper" txt) "transitively-reachable helper kept")
        (check (search "kept-via-anchor" txt) "anchor-referenced defun kept")
        (check (not (search "dead-helper" txt)) "unreachable defun pruned")))))

(defun test-parity ()
  (let ((ok (make-parity-certificate "checksum=42" "checksum=42" :floor :bin))
        (no (make-parity-certificate "checksum=42" "checksum=99" :floor :bin))
        (absent (make-parity-certificate "checksum=42" nil :floor :bin)))
    (check (parity-passed-p ok) "identical output => parity")
    (check (not (parity-passed-p no)) "different output => no parity")
    (check (not (parity-passed-p absent)) "missing floor output => no parity")))

(defun test-ddmin ()
  ;; Pure delta-min against a synthetic oracle: the minimal subset that still
  ;; "passes" is exactly the required elements. Uses eq identity on conses.
  (let* ((items (loop for i below 10 collect (list i)))   ; distinct objects
         (need (list (nth 3 items) (nth 7 items)))
         (oracle (lambda (subset) (every (lambda (x) (member x subset)) need)))
         (minimal (ddmin items oracle)))
    (check (funcall oracle minimal) "minimal still passes the oracle")
    (check (= 2 (length minimal)) "ddmin reaches the 2 required elements")
    (check (every (lambda (x) (member x minimal)) need) "keeps exactly the needed")
    ;; removing any element of a 1-minimal set breaks the property
    (check (notany (lambda (x)
                     (funcall oracle (remove x minimal)))
                   minimal)
           "1-minimal: no element is removable")))

(defun test-certified-simplify ()
  ;; Compose rosette-sound-self-rewrite: factor a common term, certificate-carrying.
  (multiple-value-bind (opt steps improvement)
      (certified-simplify '(+ (* 2 x) (* 2 y)))
    (check (plusp improvement) "arithmetic expr strictly shrinks")
    (check steps "certificate chain is non-empty")
    (check (< (rosette-sound-self-rewrite:program-size opt)
              (rosette-sound-self-rewrite:program-size '(+ (* 2 x) (* 2 y))))
           "result is smaller by op-count"))
  ;; a non-matching arithmetic form is a safe no-op (improvement 0)
  (multiple-value-bind (opt steps improvement)
      (certified-simplify '(+ x 1))
    (declare (ignore opt))
    (check (zerop improvement) "no applicable rule => no-op")
    (check (null steps) "no-op => empty certificate chain"))
  ;; the report form carries the soundness flag
  (let ((r (certified-simplify-report '(+ (* 2 x) (* 2 y)))))
    (check (getf r :certified) "report marks the rewrite certified")))

(defun test-ship-manifest ()
  ;; A shipment's manifest is content-addressed over the reduced source and
  ;; carries the parity cert; tampering with the source breaks verification.
  (let* ((slice (make-slice :root "demo" :order '("demo")))
         (art (lower-to-floor slice :bin :entrypoint "DEMO:MAIN"
                                          :out-dir "/tmp/rosette-ship-test-manifest"))
         (cert (make-parity-certificate "out" "out" :floor :bin))
         (mpath (merge-pathnames "demo.manifest.sexp"
                                 (uiop:pathname-directory-pathname
                                  (floor-artifact-reduced-file art))))
         (m (write-ship-manifest art mpath :parity-certificate cert)))
    (check (eq :bin (getf m :floor)))
    (check (stringp (getf m :content-id)) "content id computed over reduced source")
    (check (= 64 (length (getf m :content-id))) "canonical 64-hex id")
    (check (getf m :parity) "parity cert embedded")
    (check (probe-file mpath) "manifest written")
    (check (verify-ship-manifest mpath) "manifest verifies against intact source")
    ;; tamper with the reduced source -> verification must fail
    (with-open-file (s (floor-artifact-reduced-file art) :direction :output
                                                         :if-exists :append)
      (format s "~%;; tampered~%"))
    (check (not (verify-ship-manifest mpath)) "tamper detected: content id diverges")))

(defun test-parse-ship-args ()
  ;; the self-sufficient entrypoint's arg parser (the ouroboros CLI).
  (let ((p (parse-ship-args '("rosette-x" "--floor" "jit" "--expr" "(f 1)"
                              "--optimize" "--out" "/tmp/z"))))
    (check (string= "rosette-x" (getf p :system)))
    (check (eq :jit (getf p :floor)))
    (check (string= "(f 1)" (getf p :expr)))
    (check (getf p :optimize))
    (check (not (getf p :ddmin)))
    (check (string= "/tmp/z" (getf p :out))))
  (let ((p (parse-ship-args '("rosette-y" "--ddmin"))))
    (check (eq :bin (getf p :floor)) "floor defaults to :bin")
    (check (getf p :ddmin)))
  (check (null (ignore-errors (parse-ship-args '("rosette-z" "--bogus"))))
         "unknown flag signals"))

(defun test-real-closure-integration ()
  ;; rosette-proof-witness is a real, loaded dependency: exercise the ASDF-backed
  ;; deps + source introspection end to end.
  (let ((slice (compute-slice "proof-witness")))
    (check (plusp (slice-lib-count slice)) "real closure is non-empty")
    (check (member "proof-witness" (slice-order slice) :test #'string=)
           "root present in its own closure")
    (check (string= "proof-witness" (car (last (slice-order slice))))
           "root loads last")
    (check (null (slice-missing slice)) "all real deps resolvable")
    (let* ((out (merge-pathnames (format nil "rosette-ship-pw-~A.lisp" (get-universal-time))
                                 (uiop:temporary-directory)))
           (rep (emit-reduced-file slice out)))
      (check (probe-file out) "real reduced file written")
      (check (plusp (reduction-report-files rep)) "real source copied")
      (check (plusp (reduction-report-lines rep)) "real lines copied")
      ;; the reason rosette-ship exists: the kept slice is a sliver of the
      ;; substrate. Assert the closure is small (robust to the ambient
      ;; denominator, which is 0 when the repo root can't be located).
      (check (< (slice-lib-count slice) 100)
             "shipping keeps a small sliver, not the whole substrate"))))

;;; ======================================================================
;;; The ten-directions batch (#1-#7 core parts; #3/#9/#10 in subsystems).
;;; ======================================================================

(defun test-cache ()
  ;; #2: content-addressed build cache -- matching sidecar => hit; a changed
  ;; source byte => miss (a rebuild would produce different bytes).
  (let* ((dir (ensure-directories-exist
               (merge-pathnames (format nil "rosette-ship-cache-~A/" (get-universal-time))
                                (uiop:temporary-directory))))
         (reduced (merge-pathnames "x.reduced.lisp" dir))
         (out (merge-pathnames "x" dir)))
    (with-open-file (s reduced :direction :output :if-exists :supersede)
      (format s "(defun f () 1)~%"))
    (with-open-file (s out :direction :output :if-exists :supersede)
      (format s "binary~%"))
    (let ((art (make-floor-artifact :floor :bin :reduced-file reduced :output out)))
      (check (not (cache-hit-p art)) "no sidecar => cache miss")
      (check (cache-record art) "record returns a content id")
      (check (cache-hit-p art) "after record => cache hit")
      (with-open-file (s reduced :direction :output :if-exists :supersede)
        (format s "(defun f () 2)~%"))
      (check (not (cache-hit-p art)) "changed source => cache miss"))))

;; --- #7 bug-shrinker (from agent) ---
(defun test-minimal-reproducer ()
  (let* ((items (loop for i below 10 collect (list i)))
         (m2 (nth 2 items)) (m6 (nth 6 items))
         (buggy-p (lambda (subset) (and (member m2 subset) (member m6 subset))))
         (repro (minimal-reproducer items buggy-p)))
    (check (funcall buggy-p repro) "minimal reproducer still triggers the bug")
    (check (= 2 (length repro)) "shrinks to exactly the 2 culprit forms")
    (check (and (member m2 repro) (member m6 repro)) "keeps exactly {2,6}")
    (check (notany (lambda (x) (funcall buggy-p (remove x repro))) repro)
           "1-minimal: no remaining element is removable")))

(defun test-property-oracle-alias ()
  (let* ((items (loop for i below 8 collect (list i)))
         (need (nth 5 items))
         (test-fn (lambda (subset) (member need subset)))
         (oracle (make-property-oracle test-fn))
         (minimal (shrink-to-property items oracle)))
    (check (funcall oracle items) "property holds for the full input")
    (check (equal minimal (ddmin items oracle))
           "shrink-to-property returns exactly what ddmin returns")
    (check (= 1 (length minimal)) "single-culprit property shrinks to one form")
    (check (member need minimal) "keeps the culprit")))

(defun test-minimal-reproducer-guard ()
  (let ((items (loop for i below 5 collect (list i))))
    (check (null (ignore-errors
                   (minimal-reproducer
                    items (lambda (subset) (declare (ignore subset)) nil))))
           "property absent from full input => error, nothing to shrink")))

;; --- #4 cross-floor + #1 general-rewrite (from agent) ---
(defun test-floors-agree ()
  (check (floors-agree-p '((:bin . "42") (:jit . "42") (:oci . "42")))
         "all floors byte-identical => agree")
  (check (floors-agree-p '((:bin . "42") (:jit . nil) (:oci . "42")))
         "unrealised floor skipped, present ones agree")
  (check (not (floors-agree-p '((:bin . "42") (:jit . "99"))))
         "a diverging floor => no agreement")
  (check (not (floors-agree-p '((:bin . nil) (:jit . nil))))
         "no outputs => NIL")
  (check (not (floors-agree-p '())) "empty alist => NIL"))

(defun test-cross-floor-report ()
  (let ((r (cross-floor-report '((:bin . "x") (:jit . "x")))))
    (check (equal '(:bin :jit) (getf r :floors)) "floors listed in order")
    (check (getf r :agree) "agreeing outputs reported as agree")
    (check (equal '((:bin . "x") (:jit . "x")) (getf r :outputs)) "raw outputs echoed"))
  (let ((r (cross-floor-report '((:bin . "x") (:jit . "y")))))
    (check (not (getf r :agree)) "disagreement surfaced in report")))

(defun test-forms-equivalent ()
  (check (forms-equivalent-p '(+ x 0) 'x :rules '(((+ x 0) x)))
         "literal rule proves (+ x 0) == x")
  (check (forms-equivalent-p '(f a b) '(f a b)) "identical forms are equivalent")
  (check (not (forms-equivalent-p '(+ x 0) 'x)) "no rule => not equivalent")
  (check (not (forms-equivalent-p '(+ y 0) 'y :rules '(((+ x 0) x))))
         "literal rule does not generalise (schema is the frontier)"))

(defun test-prove-simplification ()
  (let ((p (prove-simplification '(+ x 0) 'x :rules '(((+ x 0) x)))))
    (check (getf p :equivalent) "rewrite is proven equivalent")
    (check (getf p :smaller) "x is strictly shorter than (+ x 0)")
    (check (equal '(+ x 0) (getf p :from)))
    (check (eq 'x (getf p :to))))
  (let ((p (prove-simplification '(+ x 0) 'x)))
    (check (not (getf p :equivalent)) "no rule => not certified equivalent")))

;; --- #5/#6 jit runtime (from agent) ---
(defun test-runtime-simplify ()
  (multiple-value-bind (opt report) (runtime-simplify "(+ (* 2 x) (* 2 y))")
    (check (< (rosette-sound-self-rewrite:program-size opt)
              (rosette-sound-self-rewrite:program-size '(+ (* 2 x) (* 2 y))))
           "runtime-simplify shrinks the form by op-count")
    (check (plusp (getf report :improvement)) "positive certified improvement")
    (check (getf report :certified) "report marks the runtime rewrite certified")
    (check (equal opt (getf report :output)) "first value is the report output")))

(defun test-apply-patch ()
  (let ((patch (merge-pathnames (format nil "rosette-ship-patch-~A.lisp" (get-universal-time))
                                (uiop:temporary-directory))))
    (makunbound 'cl-user::*rosette-ship-patch-probe*)
    (with-open-file (s patch :direction :output :if-exists :supersede)
      (format s "(defparameter cl-user::*rosette-ship-patch-probe* 42)~%"))
    (multiple-value-bind (status n) (apply-patch patch :verify-thunk (lambda () nil))
      (check (eq :rejected status) "verify veto => :rejected")
      (check (= 0 n) "rejected patch applies zero forms")
      (check (not (boundp 'cl-user::*rosette-ship-patch-probe*)) "veto installs nothing"))
    (multiple-value-bind (status n) (apply-patch patch)
      (check (eq :installed status) "no gate => :installed")
      (check (= 1 n) "one form installed")
      (check (boundp 'cl-user::*rosette-ship-patch-probe*) "patch is live in the image")
      (check (= 42 (symbol-value 'cl-user::*rosette-ship-patch-probe*)) "hot-patch took effect"))))

(defun test-apply-patch-bad-file ()
  (multiple-value-bind (status n)
      (apply-patch (merge-pathnames "rosette-ship-no-such-patch.lisp" (uiop:temporary-directory)))
    (check (eq :error status) "missing patch file => :error")
    (check (stringp n) "error carries a condition string")))

(defun test-deliver-lock ()
  ;; production substrate: .lock content-id reproducibility + audience/maturity.
  (let* ((spec '(:deliverable "demo" :system "rosette-x" :floor :jit
                 :audience :fleet :maturity :staging))
         (build '(:content-id "abc123" :parity :skipped :reduction (:kept 3)))
         (lock (deliverable-lock spec build)))
    (check (string= "demo" (getf lock :deliverable)))
    (check (eq :staging (getf lock :maturity)) "maturity carried from spec")
    (check (eq :fleet (getf lock :audience)) "audience carried from spec")
    (check (string= "abc123" (getf lock :content-id)))
    (let* ((dir (ensure-directories-exist
                 (merge-pathnames (format nil "rosette-ship-deliver-~A/demo/" (get-universal-time))
                                  (uiop:temporary-directory))))
           (root (uiop:pathname-parent-directory-pathname dir))
           (lp (merge-pathnames "demo.lock" dir))
           (sp (merge-pathnames "demo.ship" dir)))
      (with-open-file (s sp :direction :output :if-exists :supersede)
        (let ((*print-case* :downcase)) (prin1 spec s)))
      (write-lock lock lp)
      (check (probe-file lp) "lock written")
      ;; same content-id => reproduced
      (check (eq :reproduced (compare-lock '(:content-id "abc123" :parity :skipped) lp))
             "matching content id => reproduced")
      ;; changed content-id => drifted
      (check (eq :drifted (compare-lock '(:content-id "zzz999" :parity :skipped) lp))
             "changed content id => drifted (not reproducible)")
      ;; parity failure dominates
      (check (eq :parity-fail (compare-lock '(:content-id "abc123" :parity :fail) lp))
             "parity fail => :parity-fail")
      (check (eq :parity-fail
                 (compare-lock
                  '(:content-id "abc123" :parity :skipped
                    :parity-external
                    ((:suite "vs-ref" :matched 1 :total 2 :diverged 1)))
                  lp))
             "external parity divergence => :parity-fail")
      ;; --- the build GAUGE (toolchain.lisp) ------------------------------
      ;; the lock records what built it, so "same source" stops being read as
      ;; "same build"
      (check (getf lock :toolchain) "lock records the build toolchain")
      (check (rosette-content-identity:content-id-long-p (toolchain-id))
             "toolchain has a durable content id")
      (check (eq t (toolchain-match-p (build-toolchain)))
             "a toolchain matches itself")
      (check (eq :unknown (toolchain-match-p nil))
             "an absent toolchain is UNKNOWN, never 'same'")
      (check (null (toolchain-match-p '(:lisp "SBCL" :version "0.0.0-not-real")))
             "a different toolchain does not match")
      ;; a lock predating the field keeps its old verdicts unchanged
      (let ((old-lp (merge-pathnames "old.lock" dir)))
        (write-lock '(:deliverable "demo" :content-id "abc123" :parity :skipped) old-lp)
        (check (eq :reproduced (compare-lock '(:content-id "abc123" :parity :skipped) old-lp))
               "pre-toolchain lock still reads :reproduced")
        (check (eq :drifted (compare-lock '(:content-id "zzz999" :parity :skipped) old-lp))
               "pre-toolchain lock still reads :drifted"))
      ;; a lock built elsewhere: the verdict LOCATES the toolchain instead of
      ;; blaming a source change that never happened
      (let ((foreign (merge-pathnames "foreign.lock" dir)))
        (write-lock '(:deliverable "demo" :content-id "abc123" :parity :skipped
                      :toolchain (:lisp "SBCL" :version "0.0.0-not-real"))
                    foreign)
        (check (eq :reproduced-cross-toolchain
                   (compare-lock '(:content-id "abc123" :parity :skipped) foreign))
               "same id on a different toolchain => cross-toolchain reproduction")
        (check (eq :drifted-toolchain
                   (compare-lock '(:content-id "zzz999" :parity :skipped) foreign))
               "drift on a different toolchain names its own cause")
        (multiple-value-bind (verdict match)
            (compare-lock '(:content-id "abc123" :parity :skipped) foreign)
          (declare (ignore verdict))
          (check (null match) "compare-lock reports the toolchain match"))
        ;; parity failure still dominates every toolchain case
        (check (eq :parity-fail (compare-lock '(:content-id "abc123" :parity :fail) foreign))
               "parity fail dominates the toolchain verdicts"))
      ;; a merged view joins recipe (audience/maturity) with lock (content-id)
      (let ((v (deliverable-view sp)))
        (check (eq :fleet (getf v :audience)) "view reads audience from recipe")
        (check (string= "abc123" (getf v :content-id)) "view reads content-id from lock"))
      ;; STATUS is grouped by audience then maturity, over the recipes
      (let ((status (write-deliverables-status root)))
        (check (probe-file status) "STATUS.md generated")
        (let ((txt (string-downcase (uiop:read-file-string status))))
          (check (search "fleet" txt) "STATUS groups by audience")
          (check (search "staging" txt) "STATUS shows maturity"))))))

(defun test-parity-suite ()
  ;; external parity: rosette vs a third-party reference; verdict is a DISTRIBUTION
  ;; with located divergences, not a boolean.
  (check (parity-agree-p 0.5d0 0.5000000001d0 :rel-tol 1d-8) "within tolerance => agree")
  (check (not (parity-agree-p 0.5d0 0.6d0 :rel-tol 1d-8)) "outside tolerance => diverge")
  (check (parity-agree-p '(1 2 3) '(1 2 3) :rel-tol 1d-9) "elementwise list agree")
  (check (not (parity-agree-p '(1 2 3) '(1 2 4) :rel-tol 1d-9)) "one element diverges")
  ;; run a synthetic suite: two matching cases (sin/sqrt = numpy refs), one bad
  (let* ((suite '(:suite "vs-numpy" :reference "numpy 1.26" :comparator :rel-tol
                  :tolerance 1d-10
                  :cases ((:name "sin-pi/6" :expr "(sin (/ pi 6))" :reference 0.5d0)
                          (:name "sqrt2" :expr "(sqrt 2d0)" :reference 1.4142135623730951d0)
                          (:name "wrong" :expr "(+ 1 1)" :reference 3))))
         (v (run-parity-suite suite)))
    (check (= 3 (getf v :total)) "three cases")
    (check (= 2 (getf v :matched)) "two match the reference")
    (check (= 1 (getf v :diverged)) "one located divergence")
    (check (= 2/3 (getf v :match-rate)) "verdict is a distribution, not a boolean")
    (check (string= "wrong" (getf (first (getf v :divergences)) :name))
           "the divergence is LOCATED (named + magnitude), not hidden")
    (check (string= "vs-numpy 2/3" (parity-suite-summary v)) "compact summary")))

(defun test-deliverable-kinds ()
  ;; a bundle composes atoms + carries external parity; no binary.
  (let* ((spec '(:deliverable "b" :kind :bundle :atoms ("decode" "deconvolve")
                 :audience :product :maturity :candidate))
         (build '(:kind :bundle :content-id "cid" :parity :composed
                  :atom-content-ids ((:deliverable "decode" :content-id "a")
                                     (:deliverable "deconvolve" :content-id "b"))
                  :parity-external ((:suite "vs-scipy" :matched 2 :total 2 :diverged 0))))
         (lock (deliverable-lock spec build)))
    (check (eq :bundle (getf lock :kind)) "kind carried")
    (check (equal '("decode" "deconvolve") (getf lock :atoms)) "atom list carried")
    (check (= 2 (length (getf lock :atom-content-ids)))
           "sealed atom content ids carried")
    (check (eq :composed (getf lock :parity)) "bundle parity is :composed")
    (let ((ext (getf lock :parity-external)))
      (check (= 2 (getf (first ext) :matched)) "external parity summarised in lock"))))

(defun %write-test-form (path form)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (let ((*print-readably* t) (*print-pretty* t) (*print-case* :downcase))
      (prin1 form out)
      (terpri out)))
  path)

(defun %bundle-test-root (tag)
  (ensure-directories-exist
   (merge-pathnames
    (format nil "rosette-ship-bundle-~A-~A-~A/"
            tag (get-universal-time) (random 1000000))
    (uiop:temporary-directory))))

(defun %test-content-id (tag)
  (rosette-content-identity:content-id-long (list :bundle-test tag)))

(defun %write-test-atom
    (root name &key stub (kind :atom) (write-lock-p t)
                      (content-id (%test-content-id name))
                      (parity :skipped) parity-external
                      (lock-name name) (lock-kind kind)
                      (system (format nil "rosette-~A" name))
                      (lock-system system) (floor :jit) (lock-floor floor))
  (let* ((dir (ensure-directories-exist
               (merge-pathnames (format nil "~A/" name) root)))
         (ship (merge-pathnames (format nil "~A.ship" name) dir))
         (lock (merge-pathnames (format nil "~A.lock" name) dir)))
    (%write-test-form
     ship (list :deliverable name :stub stub :kind kind
                :system system :floor floor
                :parity-suites
                (remove-duplicates
                 (mapcar (lambda (receipt) (getf receipt :suite))
                         parity-external)
                 :test #'string=)))
    (when write-lock-p
      (%write-test-form
       lock (list :deliverable lock-name :kind lock-kind
                  :system lock-system :floor lock-floor
                  :content-id content-id :parity parity
                  :parity-external parity-external)))
    (values ship lock)))

(defun %bundle-build-errors-p (bundle-dir atoms &key parity-suites)
  (handler-case
      (progn
        (build-deliverable
         (list :deliverable "bundle" :kind :bundle :atoms atoms
               :parity-suites parity-suites)
         :deliverable-dir bundle-dir)
        nil)
    (error () t)))

(defun test-sealed-bundle-build ()
  ;; A bundle hashes durable atom locks and owns only its local end-to-end
  ;; suites.  Inherited receipts are grouped by suite with an explicit source
  ;; trail, and identical duplicate receipts from one atom count once.
  (let* ((root (%bundle-test-root "sealed"))
         (bundle-dir (ensure-directories-exist (merge-pathnames "bundle/" root)))
         (a-receipt '(:suite "vs-ref" :matched 1 :total 1
                      :diverged 0 :divergences nil)) ; legacy: no :reference
         (b-receipt '(:suite "vs-ref" :reference "reference 2"
                      :matched 2 :total 2 :diverged 0 :divergences nil))
         (cid-a (%test-content-id :a))
         (cid-b (%test-content-id :b))
         (spec '(:deliverable "bundle" :kind :bundle :atoms ("a" "b")
                 :parity-suites ("e2e"))))
    (%write-test-atom root "a" :content-id cid-a
                      :parity-external (list a-receipt a-receipt))
    (%write-test-atom root "b" :content-id cid-b
                      :parity-external (list b-receipt))
    (%write-test-form
     (merge-pathnames "parity/e2e/suite.sexp" bundle-dir)
     '(:suite "e2e" :reference "closed form" :comparator :exact))
    (%write-test-form
     (merge-pathnames "parity/e2e/cases/answer.sexp" bundle-dir)
     '(:name "answer" :expr "(+ 20 22)" :reference 42))
    (let* ((build (build-deliverable spec :deliverable-dir bundle-dir))
           (lock (deliverable-lock spec build))
           (external (getf build :parity-external))
           (shared (find "vs-ref" external :test #'string=
                                      :key (lambda (v) (getf v :suite))))
           (local (find "e2e" external :test #'string=
                                   :key (lambda (v) (getf v :suite))))
           (first-cid (getf build :content-id)))
      (check (rosette-content-identity:content-id-long-p first-cid)
             "bundle has a durable content id")
      (check (equal (list (list :deliverable "a" :content-id cid-a)
                          (list :deliverable "b" :content-id cid-b))
                    (getf build :atom-content-ids))
             "declared atom order + durable ids are explicit")
      (check (equal (getf build :atom-content-ids)
                    (getf lock :atom-content-ids))
             "bundle lock carries the atom-id edge list")
      (check (= 3 (getf shared :matched))
             "same-named inherited suites aggregate distinct atom cases")
      (check (= 3 (getf shared :total))
             "duplicate receipt from one source is counted once")
      (check (equal '("a" "b") (getf shared :sources))
             "inherited parity retains atom sources")
      (check (= 1 (getf local :matched)) "local e2e suite is unioned")
      (check (equal '(:bundle) (getf local :sources))
             "local e2e receipt is attributed to the bundle")
      (check (null (getf (first (getf shared :components)) :reference))
             "legacy atom receipt without :reference remains readable")
      ;; The atom name is unchanged; changing only its sealed content id must
      ;; move the bundle identity.
      (%write-test-atom root "b" :content-id (%test-content-id :b-v2)
                        :parity-external (list b-receipt))
      (let ((second (build-deliverable spec :deliverable-dir bundle-dir)))
        (check (not (string= first-cid (getf second :content-id)))
               "one atom content-id change moves the bundle content-id")))))

(defun test-sealed-bundle-rejections ()
  ;; Every unresolved composition edge is a hard error, never a nil hash or a
  ;; nominal :composed receipt.
  (let* ((root (%bundle-test-root "reject"))
         (bundle-dir (ensure-directories-exist (merge-pathnames "bundle/" root))))
    (check (%bundle-build-errors-p bundle-dir '("missing"))
           "missing atom recipe rejected")
    (%write-test-atom root "stubbed" :stub t)
    (check (%bundle-build-errors-p bundle-dir '("stubbed"))
           "stub atom rejected even if a stale lock exists")
    (%write-test-atom root "unlocked" :write-lock-p nil)
    (check (%bundle-build-errors-p bundle-dir '("unlocked"))
           "unlocked atom rejected")
    (%write-test-atom root "nested" :kind :bundle :lock-kind :bundle)
    (check (%bundle-build-errors-p bundle-dir '("nested"))
           "bundle cannot masquerade as an atom")
    (%write-test-atom root "short-id" :content-id "not-durable")
    (check (%bundle-build-errors-p bundle-dir '("short-id"))
           "non-durable atom content id rejected")
    (%write-test-atom root "wrong-name" :lock-name "somewhere-else")
    (check (%bundle-build-errors-p bundle-dir '("wrong-name"))
           "lock/recipe name mismatch rejected")
    (%write-test-atom root "wrong-system" :lock-system "rosette-other")
    (check (%bundle-build-errors-p bundle-dir '("wrong-system"))
           "lock/recipe coordinate mismatch rejected")
    (%write-test-atom root "failed" :parity :fail)
    (check (%bundle-build-errors-p bundle-dir '("failed"))
           "failed atom internal parity rejected")
    (%write-test-atom
     root "external-failed"
     :parity-external
     '((:suite "vs-ref" :matched 0 :total 1 :diverged 1
        :divergences ((:name "bad")))))
    (check (%bundle-build-errors-p bundle-dir '("external-failed"))
           "failed atom external parity rejected")
    (%write-test-atom root "valid")
    (check (%bundle-build-errors-p bundle-dir '("valid" "valid"))
           "duplicate atom declaration rejected")
    (check (%bundle-build-errors-p bundle-dir '("../valid"))
           "path-like atom name rejected")
    (check (%bundle-build-errors-p bundle-dir '("valid")
                                  :parity-suites '("missing-e2e"))
           "declared but unmeasured local suite rejected")
    (check (handler-case
               (progn
                 (rosette-ship::%validate-local-parity-suites
                  '(:parity-suites ("empty"))
                  '((:suite "empty" :matched 0 :total 0 :diverged 0)))
                 nil)
             (error () t))
           "zero-case local suite cannot certify a bundle")))

(defun test-deliverable-ordering ()
  ;; Alphabetical bundle names must not outrun the atom locks they consume.
  (let* ((root (%bundle-test-root "order"))
         (atom (merge-pathnames "z-atom/z-atom.ship" root))
         (bundle (merge-pathnames "a-bundle/a-bundle.ship" root)))
    (%write-test-form atom '(:deliverable "z-atom" :kind :atom))
    (%write-test-form bundle '(:deliverable "a-bundle" :kind :bundle
                               :atoms ("z-atom")))
    (let ((ordered (rosette-ship::%order-ship-specs (list bundle atom))))
      (check (equal atom (first ordered)) "all-build orders atoms first")
      (check (equal bundle (second ordered)) "bundles follow atoms"))))

(defun %run-deliver-build (deliverables-root &optional target)
  "Run the public delivery CLI in a fresh process and return its exit code/output."
  (let* ((repo-root (or *repo-root*
                        (error "rosette-ship test cannot locate repository root")))
         (tool (or (probe-file (merge-pathnames "tools/deliver" repo-root))
                   (probe-file (merge-pathnames "src/tools/deliver" repo-root))
                   (error "rosette-ship test cannot locate the delivery CLI")))
         ;; Reuse the focused gate's cache: by the time this test runs it holds
         ;; the exact rosette-ship FASLs that each fresh CLI process needs.  The CLI
         ;; executions remain separate real processes; only redundant recompilation
         ;; is removed from the regression's wall time.
         (cache (or (uiop:getenv "XDG_CACHE_HOME")
                    (namestring (merge-pathnames "cache/" deliverables-root))))
         (argv (append
                (list "env"
                      (format nil "ROSETTE_DELIVERABLES=~A"
                              (namestring deliverables-root))
                      (format nil "ROSETTE_XDG_CACHE_HOME=~A" cache)
                      (namestring tool) "build")
                (and target (list (namestring target))))))
    (multiple-value-bind (output ignored-error-output code)
        (uiop:run-program argv :output :string :error-output :output
                               :ignore-error-status t)
      (declare (ignore ignored-error-output))
      (values code output))))

(defun test-deliver-cli-fails-closed ()
  ;; Exercise the real shell wrapper.  An unsupported floor makes DELIVER-FILE
  ;; signal.  The targeted pipeline must preserve that nonzero status, and an
  ;; all-build must turn DELIVER-ALL's structured :ERROR verdict into a nonzero
  ;; process exit rather than reporting success after regenerating STATUS.md.
  (let* ((root (%bundle-test-root "cli-exit"))
         (dir (ensure-directories-exist (merge-pathnames "bad-floor/" root)))
         (ship (merge-pathnames "bad-floor.ship" dir)))
    (%write-test-form
     ship '(:deliverable "bad-floor" :kind :atom
            :system "scalar-core" :floor :invalid-floor
            :audience :internal :maturity :candidate))
    (multiple-value-bind (code output) (%run-deliver-build root ship)
      (declare (ignore output))
      (check (and (integerp code) (not (zerop code)))
             "targeted build preserves a failing Lisp pipeline exit"))
    (multiple-value-bind (code output) (%run-deliver-build root)
      (check (and (integerp code) (not (zerop code)))
             "all-build exits nonzero when any result is :error")
      (check (search "built bad-floor error" output :test #'char-equal)
             "all-build exposes the structured :error verdict"))))

(defun test-capabilities ()
  ;; object-capabilities: a Deliverable DECLARES its OS authority; the lock +
  ;; view carry it so the sandbox can grant exactly that (least-privilege).
  (let ((lock (deliverable-lock
               '(:deliverable "d" :system "rosette-x" :floor :jit
                 :capabilities ((:read "/data") (:net)))
               '(:content-id "c" :parity :skipped))))
    (check (equal '((:read "/data") (:net)) (getf lock :capabilities))
           "declared capabilities carried into the lock (default is none)"))
  (let ((bare (deliverable-lock '(:deliverable "d" :system "rosette-x" :floor :jit)
                                '(:content-id "c" :parity :skipped))))
    (check (null (getf bare :capabilities))
           "no declaration => no capabilities => pure stdio compute")))

(defun test-mcp ()
  ;; the MCP-over-stdio runtime that makes a jit binary a native agent tool.
  ;; JSON round-trip
  (check (equal '(("a" . 1) ("b" . "x"))
                (rosette-ship-mcp:json-parse "{\"a\":1,\"b\":\"x\"}"))
         "json parse: object -> alist")
  (check (equal '(1 2.5d0 "z") (rosette-ship-mcp:json-parse "[1, 2.5, \"z\"]"))
         "json parse: array + int + float + string")
  (check (string= "{\"n\":42}"
                  (with-output-to-string (s) (rosette-ship-mcp:json-encode '(:obj "n" 42) s)))
         "json encode: object")
  ;; initialize
  (let ((r (rosette-ship-mcp:handle-line
            "CL-USER" "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}")))
    (check (search "protocolVersion" r) "initialize returns protocolVersion")
    (check (search "\"id\":1" r) "response echoes the id"))
  ;; a notification produces no response
  (check (null (rosette-ship-mcp:handle-line
                "CL-USER" "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"))
         "notification -> no response")
  ;; tools/list + tools/call over a synthetic package of exported functions
  (let ((p (or (find-package "MCP-TEST") (make-package "MCP-TEST" :use '(:cl)))))
    (let ((sym (intern "ADD2" p)))
      (eval `(defun ,sym (x y) "add two numbers" (+ x y)))
      (export sym p))
    (check (search "add2"
                   (rosette-ship-mcp:handle-line
                    "MCP-TEST" "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"))
           "tools/list surfaces the exported function")
    (check (search "\"text\":\"5\""
                   (rosette-ship-mcp:handle-line
                    "MCP-TEST"
                    "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"add2\",\"arguments\":{\"args\":[2,3]}}}"))
           "tools/call applies the function -> 5")))

(defun test-stub ()
  ;; a stub is declared-but-unbuilt backlog: deliver-file skips it (verify does
  ;; not fail), and the view surfaces its DECLARED parity targets as pending.
  (let* ((tmp (ensure-directories-exist
               (merge-pathnames (format nil "rosette-ship-stub-~A/s/" (get-universal-time))
                                (uiop:temporary-directory))))
         (sp (merge-pathnames "s.ship" tmp)))
    (with-open-file (o sp :direction :output :if-exists :supersede)
      (let ((*print-case* :downcase))
        (prin1 '(:deliverable "s" :stub t :kind :atom :audience :fleet
                 :system "rosette-x" :floor :jit :parity-suites ("vs-foo")
                 :maturity :candidate)
               o)))
    (check (eq :stub (getf (deliver-file sp :mode :verify) :verdict))
           "stub is skipped by verify, not built")
    (let ((v (deliverable-view sp)))
      (check (eq :stub (getf v :parity)) "view shows :stub parity")
      (check (equal '(:suite "vs-foo" :matched 0 :total 0)
                    (first (getf v :parity-external)))
             "declared parity target surfaced as pending"))))

(defun test-web-floor-audit ()
  (let* ((dir (ensure-directories-exist
               (merge-pathnames "rosette-ship-web-audit/" (uiop:temporary-directory))))
         (spec '(:deliverable "test-web" :kind :atom
                 :system "browser-artifact" :floor :web
                 :entry "ROSETTE-SHIP/TESTS:%EMIT-TEST-WEB")))
    (let ((*test-web-html* "<!doctype html><title>test</title><p>sealed</p>"))
      (let ((build (build-deliverable spec :out-dir dir)))
        (check (getf (getf build :web-audit) :ok)
               "the web floor records a successful shared audit")
        (check (eq :offline (getf (getf build :web-audit) :mode))
               "the web floor defaults to the fetch-free offline policy")))
    (let ((*test-web-html*
            "<!doctype html><script>fetch('/hidden-service')</script>"))
      (check
       (handler-case (progn (build-deliverable spec :out-dir dir) nil)
         (rosette-browser-artifact:browser-artifact-error () t))
       "the web floor refuses an undeclared service dependency"))))

(defun run-all-tests ()
  (setf *pass* 0 *fail* 0)
  (test-dep-name)
  (test-closure-order)
  (test-closure-cycle-safe)
  (test-compute-slice-missing)
  (test-reduction-report)
  (test-emit-reduced-file)
  (test-floors-bin)
  (test-floors-jit)
  (test-floors-supported-and-guard)
  (test-deliverable-behavior-cases)
  (test-form-level-reduce)
  (test-ddmin)
  (test-certified-simplify)
  (test-parity)
  (test-ship-manifest)
  (test-parse-ship-args)
  ;; the ten-directions batch
  (test-cache)
  (test-minimal-reproducer)
  (test-property-oracle-alias)
  (test-minimal-reproducer-guard)
  (test-floors-agree)
  (test-cross-floor-report)
  (test-forms-equivalent)
  (test-prove-simplification)
  (test-runtime-simplify)
  (test-apply-patch)
  (test-apply-patch-bad-file)
  (test-deliver-lock)
  (test-parity-suite)
  (test-deliverable-kinds)
  (test-sealed-bundle-build)
  (test-sealed-bundle-rejections)
  (test-deliverable-ordering)
  (test-deliver-cli-fails-closed)
  (test-capabilities)
  (test-mcp)
  (test-stub)
  (test-web-floor-audit)
  (test-real-closure-integration)
  (format t "~&rosette-ship: ~D checks passed, ~D failed.~%" *pass* *fail*)
  (when (plusp *fail*)
    (error "rosette-ship: ~D checks FAILED" *fail*))
  t)
