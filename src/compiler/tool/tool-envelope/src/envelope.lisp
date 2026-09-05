;;;; envelope.lisp --- result envelope primitive.

(in-package #:rosette-tool-envelope)

(defstruct (tool-envelope
            (:constructor %make-tool-envelope
                (ok status data metadata warnings))
            (:copier nil))
  ok
  status
  data
  metadata
  warnings)

(defun %warning-list (warnings)
  (cond
    ((null warnings) nil)
    ((listp warnings) (copy-list warnings))
    (t (list warnings))))

(defun make-tool-envelope (&key ok (status (if ok :ok :error)) data metadata warnings)
  "Construct a normalized tool result envelope."
  (unless (typep ok 'boolean)
    (error "Envelope OK must be a boolean, got ~S." ok))
  (unless (keywordp status)
    (error "Envelope STATUS must be a keyword, got ~S." status))
  (%make-tool-envelope
   ok
   status
   data
   (rosette-metadata-core:copy-plist metadata :label "Envelope METADATA")
   (%warning-list warnings)))

(defun ok-envelope (&key data metadata warnings (status :ok))
  "Construct a successful tool envelope."
  (make-tool-envelope :ok t
                      :status status
                      :data data
                      :metadata metadata
                      :warnings warnings))

(defun error-envelope (&key data metadata warnings (status :error))
  "Construct an unsuccessful tool envelope."
  (make-tool-envelope :ok nil
                      :status status
                      :data data
                      :metadata metadata
                      :warnings warnings))

(defun tool-envelope->plist (envelope)
  "Return ENVELOPE as the canonical plist shape."
  (list :ok (tool-envelope-ok envelope)
        :status (tool-envelope-status envelope)
        :data (tool-envelope-data envelope)
        :metadata (rosette-metadata-core:copy-plist
                   (tool-envelope-metadata envelope)
                   :label "Envelope METADATA")
        :warnings (copy-list (tool-envelope-warnings envelope))))

(defun merge-envelope-warning (envelope warning)
  "Return a copy of ENVELOPE with WARNING appended."
  (make-tool-envelope
   :ok (tool-envelope-ok envelope)
   :status (tool-envelope-status envelope)
   :data (tool-envelope-data envelope)
   :metadata (rosette-metadata-core:copy-plist
              (tool-envelope-metadata envelope)
              :label "Envelope METADATA")
   :warnings (append (tool-envelope-warnings envelope)
                     (list warning))))
