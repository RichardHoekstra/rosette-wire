;;;; encode.lisp --- Lisp data -> JSON string.
;;;;
;;;; Data model (chosen so parse . encode is the identity on parsed data):
;;;;
;;;;   JSON object  <->  alist  (("key" . value) ...)   keys are strings
;;;;   JSON array   <->  list   (value ...)             or a vector
;;;;   JSON string  <->  string
;;;;   JSON number  <->  integer / double-float
;;;;   true / false <->  :true / :false
;;;;   null         <->  :null
;;;;
;;;; The sentinels :true :false :null are distinct keywords so that the
;;;; boolean/null cases survive a round trip without colliding with NIL
;;;; (which is reserved for the empty array).

(in-package #:rosette-json)

(defconstant json-true  :true)
(defconstant json-false :false)
(defconstant json-null  :null)

(defun %json-scalar-character-p (character)
  (let ((code (char-code character)))
    (and (<= 0 code #x10ffff)
         (not (<= #xd800 code #xdfff)))))

(defun json-string (string)
  "Return STRING encoded as a JSON string literal (with surrounding quotes)."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across string
          do (unless (%json-scalar-character-p ch)
               (error "Cannot JSON-encode non-scalar character U+~4,'0X"
                      (char-code ch)))
             (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Backspace (write-string "\\b" out))
               (#\Page (write-string "\\f" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (let ((code (char-code ch)))
                    (if (< code 32)
                        (format out "\\u~4,'0x" code)
                        (write-char ch out))))))
    (write-char #\" out)))

(defun %object-alist-p (value)
  "True when VALUE is a non-empty alist with string/symbol keys (a JSON object)."
  (and (consp value)
       (every (lambda (entry)
                (and (consp entry)
                     (let ((k (car entry)))
                       (or (stringp k) (symbolp k)))))
              value)))

(defun %key-string (key)
  (etypecase key
    (string key)
    (keyword (string-downcase (symbol-name key)))
    (symbol (string-downcase (symbol-name key)))))

(defun %write-number (value stream)
  (cond
    ((integerp value) (format stream "~D" value))
    ((floatp value)
     (unless
         (handler-case
             (let ((double (coerce value 'double-float)))
               (and (= double double)
                    (<= (- most-positive-double-float)
                        double most-positive-double-float)))
           (error () nil))
       (error "Cannot JSON-encode non-finite float ~S" value))
     ;; Emit a plain decimal with an explicit 'e' exponent marker rather
     ;; than Lisp's 'd0' so the text is valid JSON.
     (let ((*read-default-float-format* 'double-float)
           (*print-base* 10)
           (*print-radix* nil))
       (write-string (substitute #\e #\D
                                 (substitute
                                  #\e #\d
                                  (princ-to-string
                                   (coerce value 'double-float))))
                     stream)))
    ((rationalp value)
     (%write-number (coerce value 'double-float) stream))
    (t (error "Cannot JSON-encode non-real number ~S" value))))

(defun write-json (value stream)
  "Write VALUE as JSON to STREAM and return VALUE."
  (cond
    ((eq value json-null)  (write-string "null"  stream))
    ((eq value json-true)  (write-string "true"  stream))
    ((eq value json-false) (write-string "false" stream))
    ((eq value :empty-object) (write-string "{}" stream))
    ;; NIL is the empty JSON array (objects are always non-empty alists).
    ((null value) (write-string "[]" stream))
    ((stringp value) (write-string (json-string value) stream))
    ((numberp value) (%write-number value stream))
    ((%object-alist-p value)
     (write-char #\{ stream)
     (loop for (entry . rest) on value
           do (write-string (json-string (%key-string (car entry))) stream)
              (write-char #\: stream)
              (write-json (cdr entry) stream)
              (when rest (write-char #\, stream)))
     (write-char #\} stream))
    ((listp value)
     (write-char #\[ stream)
     (loop for (v . rest) on value
           do (write-json v stream)
              (when rest (write-char #\, stream)))
     (write-char #\] stream))
    ((vectorp value)
     (write-char #\[ stream)
     (loop for i below (length value)
           do (when (plusp i) (write-char #\, stream))
              (write-json (aref value i) stream))
     (write-char #\] stream))
    ((symbolp value) (write-string (json-string (%key-string value)) stream))
    (t (error "Cannot JSON-encode ~S" value)))
  value)

(defun json-encode (value)
  "Return VALUE encoded as a JSON string."
  (with-output-to-string (stream)
    (write-json value stream)))
