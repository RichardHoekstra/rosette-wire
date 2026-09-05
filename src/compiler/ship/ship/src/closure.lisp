;;;; closure.lisp --- Stage 2 (REDUCE, coarse): the reachable dependency
;;;; closure of a system, in load order.
;;;;
;;;; This is the honest, always-correct floor of the reduction story: from
;;;; the whole substrate we keep ONLY the systems transitively reachable from
;;;; the shipped root via :depends-on. The finer, form-level tree-shake
;;;; (egraph + sound-self-rewrite) is a later, *optional* refinement layered
;;;; on top of this set -- never a replacement for it.
;;;;
;;;; Everything here is pure over an injectable DEPS-FN so the graph logic is
;;;; testable without loading any target library.

(in-package #:rosette-ship)

;;; ---- repo anchor -------------------------------------------------------
;;; Walk up from where this file was loaded/compiled to the .rosette-wire-root root,
;;; the same convention as tools/build-rosette-image.lisp. Used only as the
;;; default denominator for the reduction ratio; callers may override.
(defun %find-repo-root (&optional starts)
  "First .rosette-wire-root-bearing ancestor of any START directory, else NIL.
Tries several candidates because at ASDF compile time *COMPILE-FILE-PATHNAME*
points into the fasl cache (no .rosette-wire-root above it); the launch cwd usually is
the repo root."
  (labels ((walk-up (start)
             (loop for dir = (ignore-errors (truename start))
                     then (ignore-errors (truename (merge-pathnames "../" dir)))
                   while (and dir (cdr (pathname-directory dir)))
                   when (probe-file (merge-pathnames ".rosette-wire-root" dir))
                     return dir
                   finally (return nil))))
    (loop for cand in (or starts
                          (list *load-pathname* *compile-file-pathname*
                                *default-pathname-defaults*))
          for root = (and cand (walk-up (uiop:pathname-directory-pathname cand)))
          when root return root)))

(defparameter *repo-root* (%find-repo-root)
  "Repository root (directory holding .rosette-wire-root), or NIL if not found.")

(defun count-primary-systems (&optional (root (or *repo-root* (%find-repo-root))))
  "Count .asd files under ROOT, excluding hidden / worktree copies -- the
whole-substrate denominator for the reduction ratio. Returns 0 when ROOT is
NIL (ratio then degrades to 1)."
  (if root
      (count-if-not (lambda (p)
                      (let ((s (namestring p)))
                        (or (search "/.claude/" s) (search "/worktrees/" s))))
                    (directory (merge-pathnames "**/*.asd" root)))
      0))

;;; ---- dependency-name normalisation -------------------------------------
(defun dep-name (entry)
  "Normalise one :depends-on ENTRY (string / symbol / (:version name ...) /
(:feature f name) / (:require name)) to a lowercase system-name string."
  (cond
    ((stringp entry) (string-downcase entry))
    ((symbolp entry) (string-downcase (symbol-name entry)))
    ((consp entry)
     (case (first entry)
       (:version (dep-name (second entry)))
       (:feature (dep-name (third entry)))
       (:require (dep-name (second entry)))
       (t        (dep-name (second entry)))))
    (t (string-downcase (princ-to-string entry)))))

;;; ---- ASDF-backed direct deps (parses, does NOT load target code) -------
(defun asdf-system-deps (name)
  "Direct :depends-on of system NAME as lowercase name strings, read from a
*parsed* (not loaded/compiled) ASDF system. NIL if the system is unknown."
  (let ((sys (ignore-errors (asdf:find-system name nil))))
    (when sys
      (remove-duplicates
       (mapcar #'dep-name (asdf:system-depends-on sys))
       :test #'string=))))

(defun system-resolvable-p (name)
  "True when NAME names a findable ASDF system."
  (and (ignore-errors (asdf:find-system name nil)) t))

;;; ---- the closure -------------------------------------------------------
(defun library-closure (root &key (deps-fn #'asdf-system-deps))
  "Transitive :depends-on closure of ROOT in LOAD ORDER (dependencies before
dependents; ROOT last). Names are lowercased. Pure over DEPS-FN, which maps
a name to its list of direct dependency names. Cycle-safe."
  (let ((seen (make-hash-table :test #'equal))
        (order '()))
    (labels ((visit (name)
               (let ((key (dep-name name)))
                 (unless (gethash key seen)
                   (setf (gethash key seen) t)      ; mark before descent: cycle-safe
                   (mapc #'visit (funcall deps-fn key))
                   (push key order)))))             ; post-order => deps first after nreverse
      (visit root))
    (nreverse order)))

;;; ---- the slice ---------------------------------------------------------
(defstruct (slice (:constructor %make-slice))
  "A shippable reachable slice: the root system, its load-ordered closure,
and any dependency names that could not be resolved."
  (root "" :type string)
  (order '() :type list)
  (missing '() :type list))

(defun make-slice (&key (root "") (order '()) (missing '()))
  (%make-slice :root (dep-name root) :order order :missing missing))

(defun slice-lib-count (slice)
  "Number of systems kept in SLICE."
  (length (slice-order slice)))

(defun compute-slice (root &key (deps-fn #'asdf-system-deps)
                                (resolvable-fn #'system-resolvable-p))
  "Build the shippable SLICE for system ROOT: its load-ordered dependency
closure plus any unresolvable names (flagged, never silently dropped)."
  (let* ((order (library-closure root :deps-fn deps-fn))
         (missing (remove-if resolvable-fn order)))
    (%make-slice :root (dep-name root) :order order :missing missing)))
