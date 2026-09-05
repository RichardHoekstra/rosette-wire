;;;; ddmin.lisp --- Stage 3+ : the PROVABLE minimum (delta-debugging).
;;;;
;;;; The static tree-shake (reduce.lisp) drops what it can PROVE unreachable;
;;;; it conservatively keeps everything else. ddmin goes past that: it uses the
;;;; parity oracle itself as the decision procedure. Remove a chunk of forms;
;;;; if the artifact STILL reproduces the entrypoint's output, the chunk was
;;;; dead for this capability -- keep it dropped. Iterate to a 1-minimal set.
;;;; The result is not "provably reachable" but "provably sufficient": the
;;;; fewest forms that still pass parity.
;;;;
;;;; THE CHEAP-ORACLE TRICK. A save-lisp-and-die per candidate would be far too
;;;; slow. But minimisation doesn't need a binary -- it needs to know whether a
;;;; subset still computes the right answer. So the oracle just writes the
;;;; subset to a temp file and runs a fresh SBCL that LOADS it and evaluates the
;;;; expr (load = native-compile). Dozens of ~1s loads, not dozens of 30s core
;;;; dumps. Only the final minimal set is lowered to a real floor.

(in-package #:rosette-ship)

;;; ---- Zeller delta-debugging: 1-minimise a set under a monotone-ish test --
(defun ddmin (items predicate)
  "Return a 1-minimal sublist of ITEMS for which PREDICATE still holds.
PREDICATE maps a sublist to a boolean and MUST hold for the whole ITEMS list.
Classic ddmin: try each chunk, then each complement, doubling granularity when
stuck. PREDICATE is the expensive oracle."
  (labels ((split (lst n)
             (let* ((len (length lst)) (size (max 1 (floor len n)))
                    (out '()) (i 0))
               (loop while (< i len) do
                 (push (subseq lst i (min len (+ i size))) out)
                 (incf i size))
               (nreverse out))))
    (let ((items (coerce items 'list)) (n 2))
      (loop
        (when (< (length items) 2) (return))
        (let ((chunks (split items n)) (reduced nil))
          ;; 1. does a single chunk alone still pass?  (fast collapse)
          (dolist (ch chunks)
            (when (funcall predicate ch)
              (setf items ch n 2 reduced t) (return)))
          ;; 2. else does removing a single chunk still pass?
          (unless reduced
            (dolist (ch chunks)
              (let ((complement (remove-if (lambda (x) (member x ch)) items)))
                (when (and complement (funcall predicate complement))
                  (setf items complement n (max 2 (1- n)) reduced t)
                  (return)))))
          (unless reduced
            (if (>= n (length items)) (return)
                (setf n (min (* 2 n) (length items)))))))
      items)))

;;; ---- keep-hash for a chosen prunable subset (anchors always kept) -------
(defun %subset-keep-hash (per-file subset)
  (let ((h (make-hash-table :test #'eq)))
    (dolist (pf per-file)
      (dolist (rf (cdr pf))
        (when (or (rform-anchor-p rf) (member rf subset))
          (setf (gethash rf h) t))))
    h))

;;; ---- the cheap load-and-run parity oracle ------------------------------
(defun make-load-run-oracle (slice per-file expr in-tree-output
                             &key (sbcl "sbcl") (tmp "/tmp/rosette-ship-ddmin"))
  "PREDICATE(subset) -> T iff a fresh SBCL loading (anchors + SUBSET) and
evaluating EXPR prints IN-TREE-OUTPUT. The gauge-check that drives ddmin."
  (let ((n 0) (want (string-right-trim '(#\Newline #\Space) in-tree-output)))
    (lambda (subset)
      (let* ((keep (%subset-keep-hash per-file subset))
             (file (merge-pathnames (format nil "cand-~D.lisp" (incf n))
                                    (uiop:ensure-directory-pathname tmp))))
        (emit-kept-forms slice per-file keep file :header "DDMIN-CANDIDATE")
        (let ((out (ignore-errors
                     (uiop:run-program
                      (list sbcl "--noinform" "--non-interactive"
                            "--no-sysinit" "--no-userinit"
                            "--load" (namestring file)
                            "--eval" (format nil "(progn (prin1 ~A) (terpri))" expr))
                      :output '(:string :stripped t)
                      :error-output nil :ignore-error-status t))))
          (and out (string= (string-right-trim '(#\Newline #\Space) out) want)))))))

;;; ---- top-level: minimise a slice, emit the minimal file ----------------
(defun ddmin-reduce (slice out-path
                     &key seeds expr in-tree-output (sbcl "sbcl")
                          (source-fn #'system-source-files))
  "Minimise SLICE to the fewest forms that still reproduce EXPR's
IN-TREE-OUTPUT (via the load-run oracle), starting from the static tree-shake,
and emit the minimal single file to OUT-PATH. Returns a plist
(:start N :minimal N :dropped N :files N :lines N), or NIL if parsing failed or
the shaken start set does not itself pass the oracle."
  (let ((per-file (parse-slice-forms slice :source-fn source-fn)))
    (unless per-file (return-from ddmin-reduce nil))
    (let* ((all (loop for (nil . rfs) in per-file append rfs))
           (shaken (%shake all (%resolve-seed-symbols seeds)))
           (candidates (remove-if-not
                        (lambda (rf) (and (not (rform-anchor-p rf))
                                          (gethash rf shaken)))
                        all))
           (oracle (make-load-run-oracle slice per-file expr in-tree-output
                                         :sbcl sbcl)))
      (unless (funcall oracle candidates)   ; no valid starting point => bail
        (return-from ddmin-reduce nil))
      (let* ((minimal (ddmin candidates oracle))
             (keep (%subset-keep-hash per-file minimal)))
        (multiple-value-bind (nlines nfiles)
            (emit-kept-forms slice per-file keep out-path :header "DDMIN-MINIMAL")
          (list :start (length candidates) :minimal (length minimal)
                :dropped (- (length candidates) (length minimal))
                :files nfiles :lines nlines))))))
