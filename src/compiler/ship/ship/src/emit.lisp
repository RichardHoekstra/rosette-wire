;;;; emit.lisp --- The reduced, self-contained single-file artifact.
;;;;
;;;; Given a SLICE (load-ordered closure), concatenate the source of every
;;;; system's files, deps-first, into one .lisp that loads with NO ASDF and
;;;; NO source registry -- the "reduced lisp" you can hand to someone. Copying
;;;; is verbatim line-by-line so reader macros, comments, and `#.` survive.
;;;;
;;;; This is the *coarse* reduction: whole-substrate -> reachable closure.
;;;; The form-level tree-shake (drop unreached defuns) is a later refinement
;;;; that edits this same emission; until then the form ratio is reported as
;;;; :not-applied rather than asserted.

(in-package #:rosette-ship)

(defun system-source-files (name)
  "Ordered CL source pathnames of system NAME via its parsed ASDF
components (respecting :serial order). NIL if NAME is unknown."
  (let ((sys (ignore-errors (asdf:find-system name nil))))
    (when sys
      (loop for c in (asdf:component-children sys)
            when (typep c 'asdf:cl-source-file)
              collect (asdf:component-pathname c)))))

;;; ---- reduction accounting ---------------------------------------------
(defstruct (reduction-report (:constructor %make-reduction-report))
  "How much the reduction kept. WHOLE = substrate .asd count; KEPT = systems
in the slice; FILES/LINES = size of the emitted artifact; MISSING = systems
whose source could not be located. FORM-LEVEL reduction is :not-applied at
the coarse stage."
  (whole 0 :type integer)
  (kept 0 :type integer)
  (files 0 :type integer)
  (lines 0 :type integer)
  (missing '() :type list)
  (form-level :not-applied))

(defun make-reduction-report (&key (whole 0) (kept 0) (files 0) (lines 0)
                                   (missing '()) (form-level :not-applied))
  (%make-reduction-report :whole whole :kept kept :files files :lines lines
                          :missing missing :form-level form-level))

(defun reduction-report-ratio (report)
  "Fraction of the substrate KEPT (kept / whole). 1 when WHOLE is unknown."
  (let ((whole (reduction-report-whole report)))
    (if (plusp whole)
        (/ (reduction-report-kept report) whole)
        1)))

(defun reduction-report->plist (report)
  (list :whole (reduction-report-whole report)
        :kept (reduction-report-kept report)
        :ratio (reduction-report-ratio report)
        :files (reduction-report-files report)
        :lines (reduction-report-lines report)
        :missing (reduction-report-missing report)
        :form-level (reduction-report-form-level report)))

;;; ---- the emitter -------------------------------------------------------
(defun emit-reduced-file (slice out-path
                          &key (source-fn #'system-source-files)
                               (whole-count-fn #'count-primary-systems))
  "Emit SLICE as a single self-contained loadable file at OUT-PATH.
Returns a REDUCTION-REPORT. SOURCE-FN (name -> file list) and WHOLE-COUNT-FN
() -> integer are injectable for testing."
  (ensure-directories-exist out-path)
  (let ((files 0) (lines 0) (missing '()))
    (with-open-file (out out-path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
      (format out ";;;; rosette-ship reduced artifact -- self-contained; no ASDF, no registry.~%")
      (format out ";;;; root=~A  systems=~D~%" (slice-root slice)
              (length (slice-order slice)))
      (format out ";;;; load order: ~{~A~^ ~}~2%" (slice-order slice))
      (dolist (sysname (slice-order slice))
        (let ((srcs (funcall source-fn sysname)))
          (if srcs
              (dolist (f srcs)
                (incf files)
                (format out ";;; ==== ~A :: ~A ====~%" sysname (file-namestring f))
                (with-open-file (in f :direction :input :external-format :utf-8)
                  (loop for line = (read-line in nil nil)
                        while line
                        do (write-line line out) (incf lines)))
                (terpri out))
              (push sysname missing)))))
    (make-reduction-report :whole (funcall whole-count-fn)
                           :kept (length (slice-order slice))
                           :files files :lines lines
                           :missing (nreverse missing))))
