;;;; parity-suite.lisp --- external parity: rosette vs third-party reference.
;;;;
;;;; Internal parity (parity.lisp) asks "does the artifact == the in-tree run"
;;;; -- build integrity. EXTERNAL parity asks "does rosette == the reference the
;;;; customer already trusts" -- credibility. A parity SUITE lives beside a
;;;; Deliverable:
;;;;
;;;;   deliverables/<name>/parity/<suite>/
;;;;     suite.sexp     (:suite "vs-scipy" :reference "scipy 1.11"
;;;;                     :comparator :rel-tol :tolerance 1d-10
;;;;                     :load ("rosette-…")  :provenance "…")
;;;;     cases/*.sexp   (:name "…" :expr "(rosette form)" :reference <third-party output>
;;;;                     [:tolerance 1d-8])
;;;;
;;;; The verdict is a DISTRIBUTION, not a boolean: a match rate plus the LOCATED
;;;; divergences (magnitude reported, never hidden). A divergence with a number
;;;; is more credible than a bare "we match" -- the honest wall is the moat.

(in-package #:rosette-ship)

(defun %num-agree-p (a b tol)
  (and (numberp a) (numberp b)
       (<= (abs (- a b)) (* tol (max 1 (abs a) (abs b))))))

(defun parity-agree-p (got ref comparator tol)
  "Compare an rosette result GOT to a reference REF under COMPARATOR:
:exact (equal), :rel-tol (relative tolerance on numbers, elementwise on equal-
length lists). Domain comparators register by extending this."
  (case comparator
    (:exact (equal got ref))
    (:rel-tol
     (cond ((and (numberp got) (numberp ref)) (%num-agree-p got ref tol))
           ((and (listp got) (listp ref) (= (length got) (length ref)))
            (every (lambda (a b) (parity-agree-p a b :rel-tol tol)) got ref))
           (t (equal got ref))))
    (t (equal got ref))))

(defun read-parity-suites (deliverable-dir)
  "Read every parity suite under <DELIVERABLE-DIR>/parity/<suite>/. Returns a
list of suite plists with :cases attached (NIL when there is no parity/ dir)."
  (let ((root (merge-pathnames "parity/"
                               (uiop:ensure-directory-pathname deliverable-dir))))
    (when (probe-file root)
      (loop for suite-file in (sort (copy-list
                                     (directory
                                      (merge-pathnames "*/suite.sexp" root)))
                                    #'string< :key #'namestring)
            for meta = (with-open-file (in suite-file :direction :input) (read in))
            for cases-dir = (merge-pathnames "cases/"
                             (uiop:pathname-directory-pathname suite-file))
            for cases = (loop for cf in (sort (copy-list
                                               (directory
                                                (merge-pathnames "*.sexp"
                                                                 cases-dir)))
                                              #'string< :key #'namestring)
                              collect (with-open-file (in cf :direction :input) (read in)))
            collect (append meta (list :cases cases))))))

(defun run-parity-suite (suite)
  "Run one external-parity SUITE: load its systems, evaluate each case's :expr
(the rosette capability), and compare to :reference (the third-party output) under
the suite comparator. Returns a verdict distribution:
(:suite :reference :total N :matched N :diverged N :match-rate r
 :divergences ((:name … :rosette … :reference …) …))."
  (dolist (s (getf suite :load)) (ignore-errors (asdf:load-system s)))
  (let* ((cmp (or (getf suite :comparator) :rel-tol))
         (tol (or (getf suite :tolerance) 1d-9))
         (cases (getf suite :cases))
         (matched 0) (divs '()))
    (dolist (c cases)
      (let* ((got (ignore-errors
                   (let ((*read-eval* nil)) (eval (read-from-string (getf c :expr))))))
             (ref (getf c :reference))
             (ctol (or (getf c :tolerance) tol)))
        (if (parity-agree-p got ref cmp ctol)
            (incf matched)
            (push (list :name (getf c :name) :rosette got :reference ref) divs))))
    (list :suite (getf suite :suite) :reference (getf suite :reference)
          :provenance (getf suite :provenance)
          :total (length cases) :matched matched :diverged (length divs)
          :match-rate (if cases (/ matched (length cases)) 1)
          :divergences (nreverse divs))))

(defun run-parity-suites (deliverable-dir)
  "Run every parity suite of the Deliverable at DELIVERABLE-DIR. Returns the
list of verdict distributions (empty when there are no suites)."
  (mapcar #'run-parity-suite (read-parity-suites deliverable-dir)))

(defun parity-suite-summary (verdict)
  "A compact 'vs-scipy 2/2' string for STATUS."
  (format nil "~A ~D/~D" (getf verdict :suite)
          (getf verdict :matched) (getf verdict :total)))
