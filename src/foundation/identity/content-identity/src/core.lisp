;;;; core.lisp --- shared deterministic content identity.

(in-package #:rosette-content-identity)

(defun canonical-content-form (content)
  "Return the canonical identity form for CONTENT.

The first implementation deliberately preserves the substrate's existing
readable Lisp data convention: callers pass the precise form whose structural
address is meant to be stable."
  content)

(defun content-id (content)
  "Return CONTENT's deterministic 16-hex-digit structural content id."
  (format nil "~(~16,'0x~)"
          (form-address-of (canonical-content-form content))))

(defun content-id-long (content)
  "Return CONTENT's versioned 64-hex-digit canonical structural content id.

This is the long-term evidence identity.  It preserves the same canonical
content-form contract as `content-id`, but expands the address space by hashing
four domain-separated structural forms.  It is deterministic and dependency
light; callers that need compact display handles can continue to use
`content-id` as a legacy short alias."
  (let ((addresses
          (form-addresses-of-domain-separated
           (canonical-content-form content)
           '((:rosette-content-id/v1 0)
             (:rosette-content-id/v1 1)
             (:rosette-content-id/v1 2)
             (:rosette-content-id/v1 3)))))
    (format nil "~(~16,'0x~16,'0x~16,'0x~16,'0x~)"
            (first addresses) (second addresses)
            (third addresses) (fourth addresses))))

(defun content-id-p (object)
  "Return true when OBJECT is a rendered 16-hex-digit content id."
  (and (stringp object)
       (= 16 (length object))
       (every (lambda (ch)
                (or (digit-char-p ch)
                    (find ch "abcdefABCDEF" :test #'char=)))
              object)))

(defun content-id-long-p (object)
  "Return true when OBJECT is a rendered 64-hex-digit canonical content id."
  (and (stringp object)
       (= 64 (length object))
       (every (lambda (ch)
                (or (digit-char-p ch)
                    (find ch "abcdefABCDEF" :test #'char=)))
              object)))

(defun content-id-string (content-or-id)
  "Return a content id string for CONTENT-OR-ID.

Rendered 16-hex-digit ids are preserved.  Other strings are ordinary content
and are structurally hashed like any other value."
  (if (content-id-p content-or-id)
      content-or-id
      (content-id content-or-id)))

(defun content-id-string-long (content-or-id)
  "Return a canonical long content id string for CONTENT-OR-ID.

Rendered 64-hex-digit ids are preserved.  Other strings, including legacy
16-hex ids, are ordinary content for the long-id namespace unless the caller
passes the original content form."
  (if (content-id-long-p content-or-id)
      content-or-id
      (content-id-long content-or-id)))

(defun content-id-short (content-or-long-id &key (width 16))
  "Return a compact display handle for CONTENT-OR-LONG-ID.

WIDTH must be between 1 and 64.  The handle is always a prefix of the canonical
long identity, never an authority by itself.  Callers that persist or resolve
handles must keep the corresponding full `content-id-long` value or perform
scope-local collision checks."
  (unless (and (integerp width) (<= 1 width 64))
    (error "Short content id width must be an integer in [1,64], got ~S"
           width))
  (subseq (content-id-string-long content-or-long-id) 0 width))

(defun content-id-handle
    (content-or-long-id &key (width 16) (collision-policy :widen))
  "Return a structured display handle for CONTENT-OR-LONG-ID.

The returned plist carries the full canonical identity plus its current display
prefix.  Persisting this descriptor is safe because the canonical id remains
the authority; the prefix is only an affordance for humans, logs, filenames,
and cockpit surfaces.  COLLISION-POLICY names the expected resolver behavior
when the prefix is ambiguous."
  (let* ((canonical-id (content-id-string-long content-or-long-id))
         (prefix (content-id-short canonical-id :width width)))
    (list :canonical-content-id canonical-id
          :short-id prefix
          :short-id-width width
          :short-id-algorithm :canonical-prefix
          :collision-policy collision-policy)))

(defun content-id-handle-p (object)
  "Return true when OBJECT is a structured canonical-prefix display handle."
  (and (listp object)
       (let ((canonical-id (getf object :canonical-content-id))
             (short-id (getf object :short-id))
             (width (getf object :short-id-width))
             (algorithm (getf object :short-id-algorithm)))
         (and (durable-content-id-p canonical-id)
              (stringp short-id)
              (integerp width)
              (<= 1 width 64)
              (= (length short-id) width)
              (eq algorithm :canonical-prefix)
              (string= short-id canonical-id :end2 width)))))

(defun durable-content-id-p (object)
  "Return true when OBJECT is acceptable as a durable content identity.

Durable identities must be rendered canonical long ids.  Short ids and display
prefixes are intentionally excluded: they are local handles that require store
resolution before they can authorize evidence, replay, or cross-store links."
  (content-id-long-p object))

(defun legacy-content-id-only-p (object)
  "Return true when OBJECT is only a legacy short content handle."
  (and (content-id-p object)
       (not (content-id-long-p object))))

(defun assert-durable-content-id (object &optional (context :content-id))
  "Return OBJECT when it is durable, otherwise signal an identity error.

CONTEXT is included in the condition text so callers can name the artifact,
certificate field, or replay edge that attempted to persist a non-durable id."
  (unless (durable-content-id-p object)
    (error "Non-durable content identity for ~S: ~S. Persist a 64-hex canonical id; use short ids only as display handles."
           context object))
  object)

(defun content-equal-p (left right)
  "Return true when LEFT and RIGHT have the same content identity."
  (string= (content-id-string left) (content-id-string right)))

(defun content-equal-long-p (left right)
  "Return true when LEFT and RIGHT have the same canonical long identity.

Unlike `content-equal-p`, this uses the durable 64-hex namespace.  Legacy
16-hex ids are treated as ordinary string content because a short id cannot be
promoted to a canonical long id without the original content."
  (string= (content-id-string-long left) (content-id-string-long right)))
