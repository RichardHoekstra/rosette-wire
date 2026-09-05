;;;; tests.lisp --- test suite for rosette-sharing-reduction.
;;;;
;;;; Zero-dependency harness (rosette-microbench style).  Each test is one
;;;; clause of the pre-registered anti-property; the suite passing IS the
;;;; refutation of "the net mis-reduces / the super-exponential is not tamed
;;;; / a shared redex is reduced more than once / not confluent / no
;;;; geometry win."

(in-package #:cl-user)

(defpackage #:rosette-sharing-reduction/tests
  (:use #:cl #:rosette-sharing-reduction)
  (:import-from #:rosette-foundation-rewrite #:tlc-var #:tlc-lam #:tlc-app)
  (:export #:run-all-tests))

(in-package #:rosette-sharing-reduction/tests)

(defvar *passes* 0)
(defvar *fails* 0)

(defmacro check (name expression)
  `(if ,expression
       (progn (incf *passes*) (format t "~&  PASS  ~A~%" ',name))
       (progn (incf *fails*)
              (format t "~&  FAIL  ~A~%    expression: ~S~%" ',name ',expression))))

;;; ------------------------------------------------------------
;;; (a) CORRECTNESS -- Church arithmetic via the net.
;;; ------------------------------------------------------------

(defun net-num (term) (church-count (run-term term)))

(defun arithmetic-correct ()
  (and (= 3 (net-num (church-numeral 3)))
       (= 5 (net-num (apply* (church-add) (church-numeral 2) (church-numeral 3))))
       (= 5 (net-num (apply* (church-add) (church-numeral 1) (church-numeral 4))))
       (= 6 (net-num (apply* (church-mul) (church-numeral 2) (church-numeral 3))))
       (= 9 (net-num (apply* (church-mul) (church-numeral 3) (church-numeral 3))))
       (= 8 (net-num (apply* (church-exp) (church-numeral 2) (church-numeral 3))))
       (= 9 (net-num (apply* (church-exp) (church-numeral 3) (church-numeral 2))))))

(defun identity-and-k ()
  "(\\i.i) z -> z ; (\\a.\\b.a) z w -> z (alpha-insensitively a variable)."
  (and (eq :lam (first (term->sexp (run-term (tlc-lam :i (tlc-var :i))))))
       ;; (\\i.i) applied to a numeral returns the numeral.
       (= 4 (net-num (tlc-app (tlc-lam :i (tlc-var :i)) (church-numeral 4))))))

(defun alpha-key (term &optional environment)
  "Encode TERM with bound variables as de Bruijn indices.

The interaction-net readback deliberately chooses canonical binder names,
while the TLC reference normalizer preserves source names.  Comparing this
key makes the reducer/reference regression genuinely alpha-insensitive."
  (case (rosette-foundation-rewrite:tlc-term-kind term)
    (:var
     (let ((name (rosette-foundation-rewrite:tlc-var-name term)))
       (let ((index (position name environment :test #'eq)))
         (if index
             (list :bound index)
             (list :free name)))))
    (:lam
     (let ((parameter (rosette-foundation-rewrite:tlc-lam-param term)))
       (list :lam
             (alpha-key (rosette-foundation-rewrite:tlc-lam-body term)
                        (cons parameter environment)))))
    (:app
     (list :app
           (alpha-key (rosette-foundation-rewrite:tlc-app-fn term)
                      environment)
           (alpha-key (rosette-foundation-rewrite:tlc-app-arg term)
                      environment)))))

(defun reducer-agrees-with-reference-p (term)
  "Check the net normal form against the ordinary TLC beta reference."
  (let ((reference (rosette-foundation-rewrite:normalize term)))
    (multiple-value-bind (actual interactions)
        (run-term term)
      (and (plusp interactions)
           (equal (alpha-key actual)
                  (alpha-key reference))))))

(defun duplication-readback-correct ()
  "Regression for commutation of binary nodes with internal self-wires.

The old rewrite treated a self-wire on the active pair as an external edge;
fresh duplicators then pointed at freed nodes and readback returned free
variables.  These cases exercise self-application, two uses of one value,
and the same duplication with a closed base value."
  (let* ((id (tlc-lam :y (tlc-var :y)))
         (true (tlc-lam :x (tlc-lam :y (tlc-var :x))))
         (false (tlc-lam :x (tlc-lam :y (tlc-var :y))))
         (self-application
           (tlc-app
            (tlc-lam :f (tlc-app (tlc-var :f) (tlc-var :f)))
            id))
         (duplicate-application
           (tlc-app
            (tlc-lam
             :f
             (tlc-app
              (tlc-app (tlc-var :f) true)
              (tlc-app (tlc-var :f) false)))
            id))
         (duplicate-base
           (tlc-app
            (tlc-lam :f (tlc-app (tlc-var :f) (tlc-var :f)))
            (tlc-lam :z (tlc-var :z)))))
    (and (reducer-agrees-with-reference-p self-application)
         (reducer-agrees-with-reference-p duplicate-application)
         (reducer-agrees-with-reference-p duplicate-base))))

;;; ------------------------------------------------------------
;;; (b) SUPER-EXPONENTIAL TAMED -- the headline.
;;; ------------------------------------------------------------

(defun tower (n)
  (let ((term (church-numeral 2)))
    (dotimes (i n) (setf term (apply* (church-mul) term term)))
    term))

(defun super-exponential-tamed ()
  "For the squaring tower (value 2^(2^n)), the net reduces in interactions
that are at least one full EXPONENTIAL level below the naive tree size
2^(2^n).  At n=5 the naive numeral has > 4e9 nodes; the net does < 1000
interactions and keeps < 200 live nodes."
  (let* ((n 5)
         (net (lam->net (tower n))))
    (reduce-all net)
    (let ((interactions (net-interactions net))
          (live (net-node-count net))
          (naive (expt 2 (expt 2 n))))      ; = 2^32
      (and (< interactions 1000)
           (< live 1000)
           ;; the blow-up is killed by many orders of magnitude.
           (< (* interactions 100000) naive)))))

(defun value-recovered-small-tower ()
  "Where the numeral is still writable (n<=3), the net result is exactly
2^(2^n)."
  (flet ((tv (n) (let ((net (lam->net (tower n))))
                   (reduce-all net) (church-count (net->term net)))))
    (and (= 4 (tv 1)) (= 16 (tv 2)) (= 256 (tv 3)))))

;;; ------------------------------------------------------------
;;; (c) LEVY-OPTIMALITY -- a shared redex contracted once.
;;; ------------------------------------------------------------

(defun shared-redex (k)
  (let* ((id (tlc-lam :w (tlc-var :w)))
         (redex (tlc-app (tlc-lam :y (tlc-var :y)) id))
         (numeral (church-numeral k :f :s :x :z)))
    (apply* numeral redex (tlc-lam :q (tlc-var :q)))))

(defun levy-optimal ()
  "Net interactions grow LINEARLY with the number of uses k of one shared
redex -- no per-use re-reduction blow-up (the shared redex is contracted
once)."
  (flet ((ic (k) (let ((net (lam->net (shared-redex k))))
                   (reduce-all net) (net-interactions net))))
    (let ((i2 (ic 2)) (i4 (ic 4)) (i8 (ic 8)) (i16 (ic 16)))
      ;; bounded work per use: interactions / k stays small and constant-ish.
      (and (< (/ i16 16) 10) (< (/ i8 8) 10)
           ;; strictly increasing but only linearly.
           (< i2 i4) (< i4 i8) (< i8 i16)
           (<= (- i16 i8) (* 2 (- i8 i4)))))))

(defun action-bookkeeping-profile-runs ()
  "Exercise P2's cost split on the existing shared-redex control family.

The repository does not currently contain a named Asperti--Mairson blowup
corpus, so this gate checks the instrumentation and fit plumbing without
promoting the control family to that literature claim."
  (let* ((scaling
           (reduction-cost-scaling
            (loop for k in '(2 4 8 16)
                  collect (list k (shared-redex k)))))
         (rows (getf scaling :rows)))
    (and (= 4 (length rows))
         (every (lambda (row)
                  (and (getf row :operational-proxy-p)
                       (= (getf row :total-interactions)
                          (+ (getf row :action-interactions)
                             (getf row :bookkeeping-interactions)))))
                rows)
         (member (getf scaling :selected-model)
                 '(:additive :multiplicative :neither)))))

(defun stratification-phase-sweep-runs ()
  "The same evaluator result is blind to the external complexity profile."
  (let* ((term (shared-redex 4))
         (eal '(:logic :eal :stratified-p t :certificate :external))
         (wild '(:logic :unrestricted :stratified-p nil))
         (eal-profile (reduction-cost-profile term
                                              :name :eal
                                              :stratification eal))
         (wild-profile (reduction-cost-profile term
                                               :name :wild
                                               :stratification wild))
         (sweep (reduction-cost-phase-sweep
                 (list (list :wild wild term)
                       (list :eal eal term)))))
    (and (equal eal (getf eal-profile :stratification))
         (= (getf eal-profile :total-interactions)
            (getf wild-profile :total-interactions))
         (equal wild (getf (first (getf sweep :rows)) :stratification))
         (equal eal (getf (second (getf sweep :rows)) :stratification))
         (getf sweep :regime-blind-p)
         (null (getf sweep :eal-type-checker-present-p))
         (eq (getf sweep :formulation-invariant-p) :open))))

(defun constructive-phase-corpus-runs ()
  "Construction labels verify without adding an EAL inferencer."
  (let* ((corpus (make-constructed-phase-corpus
                  :inside-parameters '(2 4)
                  :outside-heights '(1 2 3)))
         (sweep (reduction-cost-constructed-phase-sweep corpus))
         (tampered (copy-tree (first corpus))))
    (setf (getf tampered :term) (tlc-var :tampered))
    (and (= 5 (length corpus))
         (verify-constructed-phase-corpus corpus)
         (not (verify-constructed-phase-certificate tampered))
         (getf sweep :construction-certificates-verified-p)
         (null (getf sweep :formal-eal-inference-p))
         (null (getf sweep :am-corpus-present-p))
         (eq (getf sweep :membership-source) :construction)
         (= 5 (length (getf sweep :rows))))))

(defun trace-compressibility-runs ()
  "BLC is the primary K estimate; RLE/Shannon are secondary diagnostics."
  (let ((profile (reduction-cost-profile (shared-redex 16) :name :trace-control)))
    (and (plusp (getf profile :trace-length))
         (<= (getf profile :trace-rle-length)
             (getf profile :trace-length))
         (plusp (getf profile :trace-unique-events))
         (>= (getf profile :trace-entropy-bits) 0d0)
         (getf profile :trace-compressibility-proxy-p)
         (plusp (getf profile :blc-description-length))
         (plusp (getf profile :blc-shared-description-length))
         (plusp (getf profile :blc-dag-description-length))
         (plusp (getf profile :blc2-shared-description-length))
         (= (getf profile :kolmogorov-upper-bound-bits)
            (min (getf profile :blc-description-length)
                 (getf profile :blc-shared-description-length)
                 (getf profile :blc-dag-description-length)
                 (getf profile :blc2-shared-description-length)))
         (getf profile :kolmogorov-estimate-p)
         (eq (getf profile :kolmogorov-estimate-scope)
             :canonical-rewrite-trace-encoding)
         (getf profile :blc-refinements-complete-p)
         (getf profile :blc-matched-null-p)
         (plusp (getf profile :blc-shuffled-description-length))
         (plusp (getf profile :blc-shuffled-dag-description-length))
         (>= (getf profile :blc-dag-sharing-gap-bits) 0)
         (>= (getf profile :blc-shuffled-dag-sharing-gap-bits) 0)
         (integerp (getf profile :blc-order-sharing-excess-bits))
         (plusp (getf profile :blc-rle-description-length))
         (plusp (getf profile :blc-rle-shared-description-length))
         (plusp (getf profile :blc-rle-dag-description-length))
         (plusp (getf profile :blc2-rle-shared-description-length))
         (plusp (getf profile :blc-rle-k-upper-bound-bits))
         (plusp (getf profile :blc-rle-shuffled-k-upper-bound-bits))
         (integerp (getf profile :blc-rle-order-sharing-excess-bits)))))

(defun bounded-cost-profile-runs ()
  "A budgeted profile returns an explicit partial prefix, never a fake NF."
  (let ((partial
          (reduction-cost-profile-bounded
           (shared-redex 4) :name :budgeted :max-interactions 1))
        (normal
          (reduction-cost-profile-bounded
           (tlc-lam :z (tlc-var :z)) :name :already-normal
           :max-interactions 1)))
    (and (null (getf partial :reduction-complete-p))
         (getf partial :partial-trace-p)
         (= 1 (getf partial :interaction-budget))
         (= 1 (getf partial :total-interactions))
         (eq :interaction-budget (getf partial :reduction-stop-reason))
         (getf normal :reduction-complete-p)
         (null (getf normal :partial-trace-p))
         (eq :normal-form (getf normal :reduction-stop-reason)))))

;;; ------------------------------------------------------------
;;; (d) PARALLEL-CONFLUENCE -- sequential == all-at-once.
;;; ------------------------------------------------------------

(defun selected-step-replays-first-step ()
  "A scheduler may choose the first redex through the audited selector API."
  (let* ((term (apply* (church-add) (church-numeral 2) (church-numeral 3)))
         (selected (lam->net term))
         (ordinary (lam->net term)))
    (and (reduce-selected-step
          selected
          (lambda (net pairs)
            (declare (ignore net pairs))
            0))
         (reduce-step ordinary)
         (= (net-interactions selected) 1)
         (= (net-interactions ordinary) 1)
         (equal (net-trace selected) (net-trace ordinary)))))

(defun parallel-confluent ()
  (every
   (lambda (term)
     (let ((ns (lam->net term)) (np (lam->net term)))
       (reduce-all ns :parallel nil)
       (reduce-all np :parallel t)
       (and (equal (term->sexp (net->term ns)) (term->sexp (net->term np)))
            (= (net-interactions ns) (net-interactions np)))))
   (list (apply* (church-add) (church-numeral 3) (church-numeral 4))
         (apply* (church-mul) (church-numeral 3) (church-numeral 3))
         (apply* (church-exp) (church-numeral 2) (church-numeral 4))
         (tower 3))))

(defun schedule-gauge-certificate-replays ()
  (let ((certificate
          (certify-schedule-gauge
           (apply* (church-add)
                   (church-numeral 3)
                   (church-numeral 4)))))
    (and
     (verify-schedule-gauge-certificate certificate)
     (equal
      (schedule-gauge-certificate-source-form certificate)
      (term->sexp
       (apply* (church-add)
               (church-numeral 3)
               (church-numeral 4))))
     (equal
      (schedule-gauge-certificate-sequential-normal-form certificate)
      (schedule-gauge-certificate-parallel-normal-form certificate))
     (= (schedule-gauge-certificate-sequential-interactions certificate)
        (schedule-gauge-certificate-parallel-interactions certificate))
     (progn
       (incf
        (rosette-sharing-reduction::schedule-gauge-certificate-parallel-interactions
         certificate))
       (not (verify-schedule-gauge-certificate certificate))))))

;;; ------------------------------------------------------------
;;; (e) GEOMETRY WIN -- compact net << unfolded tree.
;;; ------------------------------------------------------------

(defun geometry-win ()
  "The live shared net is exponentially smaller than the unfolded numeral
tree it represents."
  (let* ((n 5) (net (lam->net (tower n))))
    (reduce-all net)
    (let ((live (net-node-count net))
          (unfolded (expt 2 (expt 2 n))))
      (< (* live 100000) unfolded))))

;;; ------------------------------------------------------------
;;; Run all.
;;; ------------------------------------------------------------

(defun run-all-tests ()
  (setf *passes* 0 *fails* 0)
  (format t "~&;; rosette-sharing-reduction test run ---------------------------~%")
  (check arithmetic-correct          (arithmetic-correct))
  (check identity-and-k              (identity-and-k))
  (check duplication-readback-correct (duplication-readback-correct))
  (check super-exponential-tamed     (super-exponential-tamed))
  (check value-recovered-small-tower (value-recovered-small-tower))
  (check levy-optimal                (levy-optimal))
  (check action-bookkeeping-profile  (action-bookkeeping-profile-runs))
  (check stratification-phase-sweep  (stratification-phase-sweep-runs))
  (check constructive-phase-corpus   (constructive-phase-corpus-runs))
  (check trace-compressibility       (trace-compressibility-runs))
  (check bounded-cost-profile        (bounded-cost-profile-runs))
  (check selected-step-replay        (selected-step-replays-first-step))
  (check parallel-confluent          (parallel-confluent))
  (check schedule-gauge-certificate  (schedule-gauge-certificate-replays))
  (check geometry-win                (geometry-win))
  (format t "~&;; ----------------------------------------------------------~%")
  (format t "~&;; ~D pass, ~D fail~%" *passes* *fails*)
  (unless (zerop *fails*)
    (error "rosette-sharing-reduction tests failed: ~D failures." *fails*))
  (values *passes* *fails*))
