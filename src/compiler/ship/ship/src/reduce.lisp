;;;; reduce.lisp --- Stage 3 (OPTIMIZE): the *optimal reduced lisp*.
;;;;
;;;; The coarse reduction (closure.lisp) keeps whole systems. This is the fine
;;;; one: a symbol call-graph tree-shake that keeps only the top-level
;;;; definer forms actually reachable from the entrypoint -- the sliver of a
;;;; sliver you would hand-write if you knew exactly what the capability calls.
;;;;
;;;; THE BRILLIANT PART -- reduction bounded by a verifier, not by caution.
;;;; Source-level tree-shaking is unsound in general (macros synthesise names,
;;;; funcall/find-symbol hide edges). We shake aggressively anyway, because the
;;;; parity gauge-check (parity.lisp) FALSIFIES any over-shake: the shaken
;;;; artifact is used only if it (a) loads and (b) reproduces the in-tree run
;;;; byte-for-byte; otherwise the caller falls back to the full reduced file.
;;;; The verifier is what makes the aggressive optimisation safe.
;;;;
;;;; Soundness posture regardless of the backstop:
;;;;   - PRUNABLE (droppable if unreached): defun / defmacro /
;;;;     define-compiler-macro -- pure code definers with no top-level effect.
;;;;   - ANCHOR (always kept, and their referenced symbols seed the live set):
;;;;     everything else -- defpackage, in-package, defstruct/defclass,
;;;;     defgeneric/defmethod, defvar/defparameter/defconstant, eval-when,
;;;;     top-level calls, and any form we don't recognise.
;;;; Kept forms are emitted as VERBATIM SOURCE SPANS so comments and reader
;;;; macros survive untouched.

(in-package #:rosette-ship)

(defparameter *prunable-heads*
  '(defun defmacro define-compiler-macro)
  "Top-level definer heads safe to drop when unreachable.")

(defstruct (rform (:constructor %make-rform))
  "A tagged top-level form with its verbatim source span [start,end)."
  form head defined-symbols anchor-p start end)

;;; ---- symbol collection -------------------------------------------------
(defun %collect-symbols (form acc)
  "Add every symbol appearing anywhere in FORM to hash-set ACC."
  (cond
    ((symbolp form) (when form (setf (gethash form acc) t)))
    ((consp form) (%collect-symbols (car form) acc)
                  (%collect-symbols (cdr form) acc))
    (t nil))
  acc)

(defun %definer-name-symbol (form)
  "The symbol a prunable FORM defines: (defun foo ...) -> FOO;
(defun (setf foo) ...) -> FOO."
  (let ((n (second form)))
    (cond ((symbolp n) n)
          ((and (consp n) (symbolp (second n))) (second n))
          (t nil))))

(defun string-equal-head (a b)
  "Compare a form head against a CL symbol by name, package-agnostically."
  (and (symbolp a) (symbolp b) (string= (symbol-name a) (symbol-name b))))

;;; ---- read a file into tagged, span-annotated forms ---------------------
(defun %read-tagged-forms (text)
  "Read TEXT (one file's source) into RFORMs, tracking IN-PACKAGE so symbols
intern into the right (already-loaded) package, and recording each form's
verbatim character span. Returns NIL on any read error (caller falls back)."
  (handler-case
      (with-input-from-string (in text)
        (let ((pkg *package*) (forms '()))
          (loop
            (let* ((*package* pkg)
                   (start (file-position in))
                   (form (read in nil :eof)))
              (when (eq form :eof) (return))
              (let ((end (file-position in))
                    (head (and (consp form) (first form))))
                ;; a mid-file (in-package X) shifts the reader for later forms
                (when (and (consp form)
                           (member head '(in-package) :test #'string-equal-head))
                  (setf pkg (or (find-package (second form)) pkg)))
                (push (%make-rform
                       :form form :head head
                       :defined-symbols (when (member head *prunable-heads*
                                                       :test #'string-equal-head)
                                          (list (%definer-name-symbol form)))
                       :anchor-p (not (member head *prunable-heads*
                                              :test #'string-equal-head))
                       :start start :end end)
                      forms))))
          (nreverse forms)))
    (error () nil)))

;;; ---- the tree-shake ----------------------------------------------------
(defun %shake (all-rforms seed-symbols)
  "Return the set (hash-table) of RFORMs to KEEP: every anchor, plus every
prunable definer reachable (via referenced symbols) from the anchors and
SEED-SYMBOLS."
  (let ((defines (make-hash-table :test #'eq))   ; symbol -> list of rforms
        (live-sym (make-hash-table :test #'eq))
        (keep (make-hash-table :test #'eq))
        (queue '()))
    ;; index prunable definers by the symbol they define
    (dolist (rf all-rforms)
      (dolist (s (rform-defined-symbols rf))
        (when s (push rf (gethash s defines)))))
    (flet ((liven (sym)
             (when (and sym (symbolp sym) (not (gethash sym live-sym)))
               (setf (gethash sym live-sym) t)
               (push sym queue))))
      ;; seeds: explicit entrypoint/expr symbols ...
      (dolist (s seed-symbols) (liven s))
      ;; ... and every anchor form (kept unconditionally) livens its symbols
      (dolist (rf all-rforms)
        (when (rform-anchor-p rf)
          (setf (gethash rf keep) t)
          (let ((syms (make-hash-table :test #'eq)))
            (%collect-symbols (rform-form rf) syms)
            (maphash (lambda (s _) (declare (ignore _)) (liven s)) syms))))
      ;; BFS over live symbols -> keep their definer forms -> liven their refs
      (loop while queue do
        (let ((sym (pop queue)))
          (dolist (rf (gethash sym defines))
            (unless (gethash rf keep)
              (setf (gethash rf keep) t)
              (let ((syms (make-hash-table :test #'eq)))
                (%collect-symbols (rform-form rf) syms)
                (maphash (lambda (s _) (declare (ignore _)) (liven s)) syms)))))))
    keep))

(defun %resolve-seed-symbols (seed-strings)
  "Read each SEED string (an entrypoint \"PKG:FN\" or an --expr form) and
collect the symbols it names. Runs in the loaded image so reads succeed."
  (let ((acc (make-hash-table :test #'eq)))
    (dolist (s seed-strings)
      (when (and s (plusp (length s)))
        (handler-case (%collect-symbols (read-from-string s) acc)
          (error () nil))))
    (loop for k being the hash-keys of acc collect k)))

;;; ---- parse a slice into per-file tagged forms --------------------------
(defun parse-slice-forms (slice &key (source-fn #'system-source-files))
  "Load SLICE's systems (so packages exist) and parse every source file into
tagged, span-annotated RFORMs. Returns a list of (text . rforms) in load
order, or NIL if any file could not be parsed. Shared by the tree-shake and
by ddmin."
  (dolist (sys (slice-order slice)) (ignore-errors (asdf:load-system sys)))
  (let ((per-file '()) (ok t))
    (dolist (sys (slice-order slice))
      (dolist (f (funcall source-fn sys))
        (let* ((text (uiop:read-file-string f))
               (rfs (%read-tagged-forms text)))
          (if rfs (push (cons text rfs) per-file) (setf ok nil)))))
    (when ok (nreverse per-file))))

;;; ---- emit a chosen subset of forms as one verbatim file ----------------
(defun emit-kept-forms (slice per-file keep-hash out-path &key (header "OPTIMISED"))
  "Write every RFORM in PER-FILE for which KEEP-HASH holds, as verbatim source
spans, to OUT-PATH. Returns (values nlines nfiles)."
  (ensure-directories-exist out-path)
  (let ((nlines 0) (nfiles (length per-file)))
    (with-open-file (out out-path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
      (format out ";;;; rosette-ship ~A artifact -- self-contained; no ASDF/registry.~%"
              header)
      (format out ";;;; root=~A~2%" (slice-root slice))
      (dolist (pf per-file)
        (destructuring-bind (text . rfs) pf
          (dolist (rf rfs)
            (when (gethash rf keep-hash)
              (let ((span (subseq text (rform-start rf) (rform-end rf))))
                (write-string span out)
                (incf nlines (1+ (count #\Newline span))))
              (terpri out))))))
    (values nlines nfiles)))

;;; ---- public: emit the shaken single file -------------------------------
(defun form-level-reduce (slice out-path
                          &key seeds (source-fn #'system-source-files))
  "Emit a form-level tree-shaken artifact for SLICE to OUT-PATH, keeping only
anchors + prunable definers reachable from SEEDS (a list of entrypoint/expr
strings). Returns a plist (:total N :kept N :dropped N :pruned-ratio r) on
success, or NIL if analysis could not run (caller falls back to the full
reduced file). Correctness is guaranteed by the downstream parity check, not
by this pass alone."
  (let ((per-file (parse-slice-forms slice :source-fn source-fn)))
    (unless per-file (return-from form-level-reduce nil))
    (let* ((all (loop for (nil . rfs) in per-file append rfs))
           (seed-syms (%resolve-seed-symbols seeds))
           (keep (%shake all seed-syms))
           (total-prunable (count-if-not #'rform-anchor-p all))
           (kept-prunable (count-if (lambda (rf)
                                      (and (not (rform-anchor-p rf))
                                           (gethash rf keep)))
                                    all)))
      (multiple-value-bind (nlines nfiles)
          (emit-kept-forms slice per-file keep out-path :header "OPTIMISED")
        (list :total total-prunable
              :kept kept-prunable
              :dropped (- total-prunable kept-prunable)
              :pruned-ratio (if (plusp total-prunable)
                                (/ (- total-prunable kept-prunable) total-prunable)
                                0)
              :files nfiles :lines nlines)))))
