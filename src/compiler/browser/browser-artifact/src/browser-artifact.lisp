;;;; browser-artifact.lisp --- rosette-browser-artifact implementation.

(in-package #:rosette-browser-artifact)

(define-condition browser-artifact-error (error)
  ((reason :initarg :reason :reader browser-artifact-error-reason)
   (detail :initarg :detail :initform nil :reader browser-artifact-error-detail))
  (:report (lambda (condition stream)
             (format stream "browser-artifact: ~A~@[ (~A)~]"
                     (browser-artifact-error-reason condition)
                     (browser-artifact-error-detail condition)))))

(defstruct (browser-artifact
             (:constructor %make-browser-artifact
                 (html mode title capabilities)))
  (html "" :type string :read-only t)
  (mode :offline :type keyword :read-only t)
  (title "" :type string :read-only t)
  (capabilities '() :type list :read-only t))

(defun %fail (reason &optional detail)
  (error 'browser-artifact-error :reason reason :detail detail))

(defun html-escape (value)
  "Escape VALUE for HTML text or double-quoted attribute content."
  (with-output-to-string (out)
    (loop for ch across (princ-to-string value) do
      (case ch
        (#\& (write-string "&amp;" out))
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\" (write-string "&quot;" out))
        (#\' (write-string "&#39;" out))
        (t (write-char ch out))))))

(defun js-string-literal (value)
  "Encode VALUE as one double-quoted JavaScript string literal.

Angle brackets are hex-escaped, so embedded text cannot terminate the page's
SCRIPT element.  This function is for data spliced into JavaScript, not for
authoring executable JavaScript."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across (princ-to-string value) do
      (case ch
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (#\Backspace (write-string "\\b" out))
        (#\Page (write-string "\\f" out))
        (#\< (write-string "\\x3c" out))
        (#\> (write-string "\\x3e" out))
        (#\& (write-string "\\x26" out))
        (#\` (write-string "\\u0060" out))
        (t (let ((code (char-code ch)))
             (if (or (< code 32) (= code #x2028) (= code #x2029))
                 (format out "\\u~4,'0X" code)
                 (write-char ch out))))))
    (write-char #\" out)))

(defun %count-substring (needle haystack)
  (loop with count = 0
        with start = 0
        for position = (search needle haystack :start2 start)
        while position
        do (incf count)
           (setf start (+ position (length needle)))
        finally (return count)))

(defun %replace-exactly-once (text marker replacement)
  (let ((count (%count-substring marker text)))
    (unless (= count 1)
      (%fail "template marker must occur exactly once"
             (format nil "~S occurred ~D times" marker count)))
    (let ((position (search marker text)))
      (concatenate 'string
                   (subseq text 0 position)
                   replacement
                   (subseq text (+ position (length marker)))))))

(defun %assert-mode (mode)
  (unless (member mode '(:offline :local-service :static))
    (%fail "unknown execution mode" mode))
  mode)

(defun %assert-safe-element-content (content closing-tag kind)
  (when (search closing-tag content :test #'char-equal)
    (%fail (format nil "~A resource can terminate its element" kind)
           closing-tag))
  content)

(defun %named-content (entry kind)
  (etypecase entry
    (string (values nil entry))
    (cons
     (unless (and (stringp (car entry)) (stringp (cdr entry)))
       (%fail (format nil "~A resource must be STRING or (NAME . STRING)" kind)
              entry))
     (values (car entry) (cdr entry)))))

(defun %emit-resources (resources tag closing-tag &key type)
  (with-output-to-string (out)
    (dolist (entry resources)
      (multiple-value-bind (name content) (%named-content entry tag)
        (%assert-safe-element-content content closing-tag tag)
        (format out "<~A~@[ type=\"~A\"~]~@[ data-rosette-resource=\"~A\"~]>~A</~A>~%"
                tag type (and name (html-escape name)) content tag)))))

(defun make-browser-artifact
    (&key (title "browser artifact") (body "") (head "")
          styles scripts data (mode :offline) capabilities (lang "en"))
  "Construct a deterministic standalone HTML document.

BODY and HEAD are trusted HTML fragments.  STYLES and SCRIPTS contain strings
or (NAME . STRING) pairs.  DATA contains JSON strings or named pairs and is
emitted as inert application/json.  MODE is :OFFLINE, :LOCAL-SERVICE, or
:STATIC; AUDIT-BROWSER-ARTIFACT enforces the corresponding runtime policy."
  (%assert-mode mode)
  (when (and (eq mode :static) scripts)
    (%fail "static artifacts cannot contain script resources"))
  (let ((html
          (with-output-to-string (out)
            (format out "<!doctype html>~%<html lang=\"~A\">~%<head>~%"
                    (html-escape lang))
            (format out "<meta charset=\"utf-8\">~%")
            (format out "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">~%<title>~A</title>~%"
                    (html-escape title))
            (write-string head out)
            (unless (or (zerop (length head)) (char= #\Newline (char head (1- (length head)))))
              (terpri out))
            (write-string (%emit-resources styles "style" "</style>") out)
            (format out "</head>~%<body>~%")
            (write-string body out)
            (unless (or (zerop (length body)) (char= #\Newline (char body (1- (length body)))))
              (terpri out))
            (write-string (%emit-resources data "script" "</script>"
                                           :type "application/json") out)
            (write-string (%emit-resources scripts "script" "</script>") out)
            (format out "</body>~%</html>~%"))))
    (let ((artifact (%make-browser-artifact html mode (princ-to-string title)
                                            (copy-list capabilities))))
      (audit-browser-artifact artifact)
      artifact)))

(defun browser-artifact-from-template
    (template replacements &key (mode :offline) (title "") capabilities)
  "Construct an artifact by replacing every declared marker exactly once.

REPLACEMENTS is an alist of (MARKER . STRING).  Callers remain responsible for
encoding each replacement for its HTML/JS/CSS context; HTML-ESCAPE and
JS-STRING-LITERAL cover the common cases."
  (check-type template string)
  (%assert-mode mode)
  (let ((html template))
    (dolist (entry replacements)
      (unless (and (consp entry) (stringp (car entry)) (stringp (cdr entry)))
        (%fail "template replacement must be (MARKER . STRING)" entry))
      (setf html (%replace-exactly-once html (car entry) (cdr entry))))
    (when (%contains-any-p html '("__ROSETTE_" "<!--__" "/*__"))
      (%fail "template contains an unresolved marker"))
    (let ((artifact (%make-browser-artifact html mode (princ-to-string title)
                                            (copy-list capabilities))))
      (audit-browser-artifact artifact)
      artifact)))

(defun %contains-any-p (text needles)
  (some (lambda (needle) (search needle text :test #'char-equal)) needles))

(defun audit-browser-artifact (artifact)
  "Validate ARTIFACT's document and runtime policy; return an audit receipt."
  (check-type artifact browser-artifact)
  (let ((html (browser-artifact-html artifact))
        (mode (browser-artifact-mode artifact)))
    (%assert-mode mode)
    (unless (%contains-any-p html '("<!doctype html" "<!DOCTYPE html"))
      (%fail "artifact is not a complete HTML document"))
    (when (%contains-any-p html '("src=\"http://" "src=\"https://"
                                  "src='http://" "src='https://"
                                  "href=\"http://" "href=\"https://"
                                  "href='http://" "href='https://"
                                  "url(http://" "url(https://" "@import"))
      (%fail "artifact references an external runtime resource"))
    (when (%contains-any-p
           html
           (list (concatenate 'string "/" "home" "/")
                 (concatenate 'string "/" "Users" "/")
                 "file://"))
      (%fail "artifact contains a private absolute path"))
    (when (and (eq mode :offline)
               (%contains-any-p html '("fetch(" "fetch (" "XMLHttpRequest"
                                           "WebSocket(" "EventSource("
                                           "hx-post=" "hx-get=")))
      (%fail "offline artifact contains a network/service action"))
    (when (and (eq mode :static)
               (search "<script" html :test #'char-equal))
      (%fail "static artifact contains executable or data script elements"))
    (list :ok t :mode mode :bytes (length html)
          :capabilities (copy-list (browser-artifact-capabilities artifact)))))

(defun render-browser-artifact (artifact)
  "Return ARTIFACT's audited HTML bytes as a string."
  (audit-browser-artifact artifact)
  (browser-artifact-html artifact))

(defun write-browser-artifact (artifact path)
  "Audit ARTIFACT, write its deterministic HTML to PATH, and return PATH."
  (check-type path (or string pathname))
  (let ((html (render-browser-artifact artifact)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string html out)))
  path)
