;;;; cache.lisp --- #2: content-addressed reproducible-build cache.
;;;;
;;;; The reduced source already has a canonical content id (identity.lisp).
;;;; Promote it from a label to a CACHE KEY: an artifact is up to date exactly
;;;; when a sidecar records the content id of the source it was built from and
;;;; that id still matches. Identical closure => identical source => identical
;;;; id => skip the (expensive) rebuild. This is reproducibility AS a cache:
;;;; two builds of the same slice are the same bytes, and the proof is the id.

(in-package #:rosette-ship)

(defun reduced-content-id (reduced-path)
  "Canonical content id of the reduced source at REDUCED-PATH, or NIL."
  (and reduced-path (probe-file reduced-path)
       (ignore-errors
        (rosette-content-identity:content-id-long (uiop:read-file-string reduced-path)))))

(defun %cache-tag-path (artifact)
  "Sidecar path recording the content id an artifact was built from."
  (let ((out (floor-artifact-output artifact)))
    (when (or (pathnamep out) (stringp out))
      (concatenate 'string (namestring out) ".cid"))))

(defun cache-hit-p (artifact)
  "T iff ARTIFACT's OUTPUT exists and its sidecar records the CURRENT content
id of its reduced source -- i.e. a rebuild would reproduce the same bytes."
  (let ((tag (%cache-tag-path artifact))
        (out (floor-artifact-output artifact))
        (key (reduced-content-id (floor-artifact-reduced-file artifact))))
    (and key tag out (probe-file out) (probe-file tag)
         (string= key (string-trim '(#\Newline #\Space)
                                   (uiop:read-file-string tag)))
         t)))

(defun cache-record (artifact)
  "Record ARTIFACT's reduced-source content id in its sidecar, so a later
CACHE-HIT-P can skip an identical rebuild. Returns the recorded id."
  (let ((tag (%cache-tag-path artifact))
        (key (reduced-content-id (floor-artifact-reduced-file artifact))))
    (when (and tag key)
      (with-open-file (s tag :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
        (write-string key s)))
    key))
