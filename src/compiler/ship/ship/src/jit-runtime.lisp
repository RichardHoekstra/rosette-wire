;;;; jit-runtime.lisp --- what the LIVE compiler earns at runtime (the :jit floor).
;;;;
;;;; A :bin artifact is frozen: it computes exactly what it was dumped to
;;;; compute. A :jit artifact carries the compiler INSIDE it -- it can
;;;; read->compile->eval at runtime -- so the deliverable can specialise and
;;;; upgrade ITSELF in the field. These two functions are that dividend, called
;;;; by the shipped binary's `main`:
;;;;
;;;;   RUNTIME-SIMPLIFY  (#5, the self-optimizing binary) -- the binary reads an
;;;;   expression at runtime and shrinks it with its OWN embedded certified
;;;;   rewriter (certified-simplify / rosette-sound-self-rewrite). The optimiser
;;;;   ships live, not just the optimised code.
;;;;
;;;;   APPLY-PATCH  (#6, hot-patch as certified delta) -- the binary reads a
;;;;   small patch file, verifies it, and compile+installs its forms into the
;;;;   RUNNING image (redefining functions live). A frozen binary cannot do
;;;;   this; a :jit binary does a zero-downtime field upgrade.
;;;;
;;;; Each is GATED by a check, honestly: runtime-simplify rides the certificate
;;;; chain (every rewrite proven equivalent on a battery); apply-patch installs
;;;; nothing unless the caller's VERIFY-THUNK -- the provenance/parity gate --
;;;; passes. The compiler being live is the capability; the gate is what makes
;;;; exercising it trustworthy.

(in-package #:rosette-ship)

(defun runtime-simplify (expr-string)
  "Read EXPR-STRING to a form and shrink it with the binary's embedded certified
rewriter, AT RUNTIME. Returns (values OPTIMIZED-FORM REPORT-PLIST), where
REPORT-PLIST is CERTIFIED-SIMPLIFY-REPORT's summary (:input :output :size-before
:size-after :improvement :certified). Prints one human line. EXPR-STRING is
external runtime input, so it is read with *READ-EVAL* disabled -- this is the
simplify path, not the eval path (see APPLY-PATCH for the trusted, gated eval)."
  (let* ((form (let ((*package* (find-package :cl-user))
                     (*read-eval* nil))
                 (read-from-string expr-string)))
         (report (certified-simplify-report form))
         (optimized (getf report :output))
         (improvement (getf report :improvement)))
    (format t "~&runtime JIT: ~A  ->  ~A  (op-count -~D, ~A)~%"
            form optimized improvement
            (if (getf report :certified) "certified" "no-op"))
    (values optimized report)))

(defun apply-patch (path &key verify-thunk)
  "Live hot-patch: read every top-level form from file PATH and, iff VERIFY-THUNK
is nil OR returns non-nil, EVAL each -- redefining functions in the running
image (a zero-downtime field upgrade a frozen :bin cannot do). VERIFY-THUNK is
the caller's provenance/parity gate (e.g. a parity-certificate check).

All forms are READ before any is EVAL'd, so a malformed patch installs nothing.
Returns (values :INSTALLED N-FORMS) on success, (values :REJECTED 0) when
VERIFY-THUNK vetoes, or (values :ERROR CONDITION-STRING) if reading or applying
signals (the read-all-then-eval order keeps a syntactically bad patch from being
half-applied; an eval that signals mid-list is reported but cannot be unwound)."
  (handler-case
      (let ((forms (with-open-file (in path :direction :input)
                     (loop for form = (read in nil :eof)
                           until (eq form :eof)
                           collect form))))
        (if (and verify-thunk (not (funcall verify-thunk)))
            (values :rejected 0)
            (progn
              (dolist (form forms) (eval form))
              (values :installed (length forms)))))
    (serious-condition (c)
      (values :error (princ-to-string c)))))
