;;;; identity.lisp --- Stage 6 (IDENTITY): the self-describing deliverable.
;;;;
;;;; A shipment should not be an anonymous blob. This emits a manifest that
;;;; makes the artifact FINDABLE and VERIFIABLE on its own terms:
;;;;   - a content id (rosette-content-identity) over the EXACT reduced source that
;;;;     ships -- two builds of the same slice get the same id; a changed byte
;;;;     changes the id;
;;;;   - the reduction accounting (what was kept vs the whole substrate);
;;;;   - the parity certificate (rosette-proof-witness) -- the artifact carries its
;;;;     own falsifiable proof that it reproduces the in-tree run.
;;;;
;;;; The manifest is a plain readable s-expr you can hand along with the binary;
;;;; anyone can recompute the content id of the reduced source and check it, and
;;;; re-run the parity claim. This is the FAIR (Findable/Accessible/Inter-
;;;; operable/Reusable) counterpart of the raw executable.

(in-package #:rosette-ship)

(defun ship-manifest (artifact &key parity-certificate)
  "A self-describing, content-addressed manifest plist for a FLOOR-ARTIFACT.
The :content-id is rosette-content-identity:content-id-long over the exact reduced
source string that ships (NIL if the source is unreadable)."
  (let* ((reduced (floor-artifact-reduced-file artifact))
         (src (and reduced (probe-file reduced) (uiop:read-file-string reduced)))
         (cid (and src (ignore-errors (rosette-content-identity:content-id-long src))))
         (report (getf (floor-artifact-metadata artifact) :reduction)))
    (list :rosette-ship-manifest 1
          :floor (floor-artifact-floor artifact)
          :output (let ((o (floor-artifact-output artifact)))
                    (if (pathnamep o) (namestring o) o))
          :reduced-source (and reduced (namestring reduced))
          :content-id cid
          :reduction (and report (reduction-report->plist report))
          :parity (and parity-certificate
                       (rosette-proof-witness:certificate->plist parity-certificate)))))

(defun write-ship-manifest (artifact path &key parity-certificate)
  "Write SHIP-MANIFEST for ARTIFACT to PATH as a readable s-expr. Returns the
manifest plist."
  (let ((m (ship-manifest artifact :parity-certificate parity-certificate)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-readably* nil) (*print-pretty* t) (*print-case* :downcase))
        (prin1 m out) (terpri out)))
    m))

(defun manifest-from-run (&key reduced output floor in-tree-file artifact-file
                               manifest-path)
  "Build (and optionally write) a manifest from a completed run's captured
outputs. The CLI runs the two processes and passes their stdout as files, plus
the reduced-source path and floor; this assembles the content id + parity cert
+ manifest without needing the in-Lisp FLOOR-ARTIFACT object. Returns the plist."
  (flet ((slurp (p) (and p (probe-file p) (uiop:read-file-string p))))
    (let* ((src (slurp reduced))
           (cid (and src (ignore-errors (rosette-content-identity:content-id-long src))))
           (fkw (intern (string-upcase (string floor)) :keyword))
           (cert (make-parity-certificate (slurp in-tree-file) (slurp artifact-file)
                                          :floor fkw))
           (m (list :rosette-ship-manifest 1
                    :floor fkw :output output
                    :reduced-source (and reduced (probe-file reduced)
                                         (namestring (truename reduced)))
                    :content-id cid
                    :parity (rosette-proof-witness:certificate->plist cert))))
      (when manifest-path
        (with-open-file (out manifest-path :direction :output :if-exists :supersede
                                           :if-does-not-exist :create)
          (let ((*print-readably* nil) (*print-pretty* t) (*print-case* :downcase))
            (prin1 m out) (terpri out))))
      m)))

(defun verify-ship-manifest (manifest-path)
  "Re-derive the content id of the reduced source named in MANIFEST-PATH and
check it against the recorded :content-id. Returns T when they agree (the
deliverable is intact), NIL otherwise. This is the on-its-own-terms check a
recipient runs."
  (let* ((m (with-open-file (in manifest-path :direction :input)
              (read in)))
         (reduced (getf m :reduced-source))
         (recorded (getf m :content-id)))
    (and recorded reduced (probe-file reduced)
         (equal recorded
                (ignore-errors
                 (rosette-content-identity:content-id-long
                  (uiop:read-file-string reduced))))
         t)))
