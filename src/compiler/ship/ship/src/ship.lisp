;;;; ship.lisp --- the verb: point at any system, get a planned deliverable.
;;;;
;;;; `ship` composes the stages: compute the reachable slice (REDUCE), lower
;;;; it to a delivery floor (LOWER; emits the reduced file + build driver),
;;;; and return a structured report. Realising the build and running the
;;;; parity gauge-check are the CLI's job (they need subprocesses); this stays
;;;; pure planning so it is testable and side-effect-scoped to file emission.

(in-package #:rosette-ship)

(defun ship (root &key (floor :bin) entrypoint expr package out-dir name sbcl
                       optimize (deps-fn #'asdf-system-deps))
  "Plan a shipment of system ROOT to FLOOR. Emits the reduced self-contained
file and the floor's build driver as a side effect; returns a FLOOR-ARTIFACT.
Does NOT run the build -- the CLI realises FLOOR-ARTIFACT-BUILD-COMMAND and
runs the parity gauge-check."
  (unless (member floor (supported-floors))
    (error "rosette-ship: unsupported floor ~S (supported: ~{~S~^ ~})"
           floor (supported-floors)))
  (let ((slice (compute-slice root :deps-fn deps-fn)))
    (apply #'lower-to-floor slice floor
           (append (when entrypoint (list :entrypoint entrypoint))
                   (when expr       (list :expr expr))
                   (when optimize   (list :optimize optimize))
                   (when package    (list :package package))
                   (when out-dir    (list :out-dir out-dir))
                   (when name       (list :name name))
                   (when sbcl       (list :sbcl sbcl))))))

(defun ship-ddmin (root &key (floor :bin) expr entrypoint package out-dir name sbcl
                             in-tree-output)
  "Ship ROOT minimised by ddmin -- the fewest forms that still reproduce EXPR's
IN-TREE-OUTPUT under the parity oracle -- then lower to FLOOR over that minimal
file. Requires EXPR + IN-TREE-OUTPUT (the CLI runs the in-tree form). Returns
(values FLOOR-ARTIFACT DDMIN-STATS), or NIL if ddmin could not run."
  (let* ((slice (compute-slice root))
         (nm (or name (slice-root slice)))
         (dir (%ship-out-dir out-dir nm))
         (reduced (merge-pathnames (format nil "~A.reduced.lisp" nm) dir))
         (stats (ddmin-reduce slice reduced
                              :seeds (list entrypoint expr)
                              :expr expr :in-tree-output in-tree-output
                              :sbcl (or sbcl "sbcl"))))
    (when stats
      (values
       (apply #'lower-to-floor slice floor :reduced-ready t
              (append (when entrypoint (list :entrypoint entrypoint))
                      (when expr (list :expr expr))
                      (when package (list :package package))
                      (when out-dir (list :out-dir out-dir))
                      (list :name nm)
                      (when sbcl (list :sbcl sbcl))))
       stats))))

(defun ship-report (artifact)
  "A human/plist summary of a planned FLOOR-ARTIFACT."
  (let ((report (getf (floor-artifact-metadata artifact) :reduction)))
    (list :floor (floor-artifact-floor artifact)
          :reduced-file (namestring (floor-artifact-reduced-file artifact))
          :driver (namestring (floor-artifact-driver artifact))
          :output (let ((o (floor-artifact-output artifact)))
                    (if (pathnamep o) (namestring o) o))
          :build-command (floor-artifact-build-command artifact)
          :reduction (when report (reduction-report->plist report)))))
