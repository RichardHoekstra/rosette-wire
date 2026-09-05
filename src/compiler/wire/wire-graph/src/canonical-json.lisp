;;;; canonical-json.lisp --- deterministic protocol projection and identity.

(in-package #:rosette-wire)

(defconstant +max-safe-json-integer+ 9007199254740991)

(defun %enum-name (value)
  (string-downcase (symbol-name value)))

(defun %field->value (field)
  `(("name" . ,(%wire-field-name field))
    ("type" . ,(wire-type->value (%wire-field-type field)))))

(defun wire-type->value (type)
  (check-type type wire-type)
  (let ((kind (%wire-type-kind type))
        (payload (%wire-type-payload type)))
    (ecase kind
      (:scalar `(("kind" . "scalar") ("name" . ,(%enum-name payload))))
      ((:list :option)
       `(("element" . ,(wire-type->value payload))
         ("kind" . ,(%enum-name kind))))
      (:result
       `(("error" . ,(wire-type->value (second payload)))
         ("kind" . "result")
         ("ok" . ,(wire-type->value (first payload)))))
      (:tuple
       `(("elements" . ,(mapcar #'wire-type->value payload))
         ("kind" . "tuple")))
      ((:record :variant)
       `((,(if (eq kind :record) "fields" "cases")
           . ,(mapcar #'%field->value payload))
         ("kind" . ,(%enum-name kind))))
      (:tensor
       (destructuring-bind (dtype dimensions layout device mutability) payload
         `(("device" . ,(%enum-name device))
           ("dimensions" . ,(mapcar (lambda (dimension)
                                      (if (eq dimension :dynamic)
                                          "dynamic" dimension))
                                    dimensions))
           ("dtype" . ,(%enum-name dtype))
           ("kind" . "tensor")
           ("layout" . ,(%enum-name layout))
           ("mutability" . ,(%enum-name mutability))
           ("rank" . ,(length dimensions)))))
      (:resource
       (destructuring-bind (name owner ownership) payload
         `(("kind" . "resource")
           ("name" . ,name)
           ("owner" . ,owner)
           ("ownership" . ,(%enum-name ownership))))))))

(defun %operation->value (operation)
  `(("inputs" . ,(mapcar #'%field->value (%wire-operation-inputs operation)))
    ("name" . ,(%wire-operation-name operation))
    ("output" . ,(wire-type->value (%wire-operation-output operation)))))

(defun %port->value (port)
  `(("name" . ,(%wire-port-name port))
    ("operations" . ,(mapcar #'%operation->value (%wire-port-operations port)))))

(defun wire-descriptor->value (descriptor)
  (check-type descriptor wire-descriptor)
  `(("adapter" . ,(%freeze-value (%wire-descriptor-adapter descriptor)))
    ("capabilities" . ,(mapcar #'copy-seq
                                (%wire-descriptor-capabilities descriptor)))
    ("effects" . ,(mapcar #'%enum-name (%wire-descriptor-effects descriptor)))
    ("exports" . ,(mapcar #'%port->value (%wire-descriptor-exports descriptor)))
    ("imports" . ,(mapcar #'%port->value (%wire-descriptor-imports descriptor)))
    ("name" . ,(%wire-descriptor-name descriptor))
    ("schema" . ,+wire-schema+)
    ("verifiers" . ,(mapcar #'copy-seq (%wire-descriptor-verifiers descriptor)))
    ("version" . ,(%wire-descriptor-version descriptor))))

(defun %object-p (value)
  (and (consp value)
       (every (lambda (entry)
                (and (consp entry)
                     (or (stringp (car entry)) (symbolp (car entry)))))
              value)))

(defun %key-string (key)
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))))

(defun %utf16-units (string)
  (loop for character across string
        for code = (char-code character)
        if (< code #x10000)
          collect code
        else
          append (let ((offset (- code #x10000)))
                   (list (+ #xd800 (ash offset -10))
                         (+ #xdc00 (logand offset #x3ff))))))

(defun %utf16-string< (left right)
  (loop for l in (%utf16-units left)
        for r in (%utf16-units right)
        when (< l r) do (return t)
        when (> l r) do (return nil)
        finally (return (< (length (%utf16-units left))
                           (length (%utf16-units right))))))

(defun %canonical-object-entries (value path)
  (let ((entries
          (mapcar (lambda (entry)
                    (cons (%key-string (car entry)) (cdr entry)))
                  value)))
    (%require-unique-names entries #'car path)
    (sort entries #'%utf16-string< :key #'car)))

(defun %write-canonical-json (value stream path)
  (cond
    ((eq value json-null) (write-string "null" stream))
    ((eq value json-true) (write-string "true" stream))
    ((eq value json-false) (write-string "false" stream))
    ((eq value :empty-object) (write-string "{}" stream))
    ((null value) (write-string "[]" stream))
    ((stringp value) (write-string (json-string value) stream))
    ((integerp value)
     (unless (<= (- +max-safe-json-integer+) value +max-safe-json-integer+)
       (%wire-error :unsafe-json-integer path
                    "integer ~D is outside the interoperable I-JSON range" value))
     (format stream "~D" value))
    ((floatp value)
     (%wire-error :untyped-float path
                  "floats must use an explicit typed encoding before identity"))
    ((%object-p value)
     (write-char #\{ stream)
     (loop for entry in (%canonical-object-entries value path)
           for index from 0
           do (when (plusp index) (write-char #\, stream))
              (write-string (json-string (car entry)) stream)
              (write-char #\: stream)
              (%write-canonical-json
               (cdr entry) stream (format nil "~A.~A" path (car entry))))
     (write-char #\} stream))
    ((listp value)
     (write-char #\[ stream)
     (loop for item in value
           for index from 0
           do (when (plusp index) (write-char #\, stream))
              (%write-canonical-json item stream
                                     (format nil "~A[~D]" path index)))
     (write-char #\] stream))
    ((vectorp value)
     (write-char #\[ stream)
     (dotimes (index (length value))
       (when (plusp index) (write-char #\, stream))
       (%write-canonical-json (aref value index) stream
                              (format nil "~A[~D]" path index)))
     (write-char #\] stream))
    ((symbolp value)
     ;; Keywords are a Common Lisp spelling of JSON string enums, never an
     ;; implicit boolean or null except for the explicit JSON sentinels above.
     (write-string (json-string (%enum-name value)) stream))
    (t (%wire-error :unsupported-json-value path
                    "cannot canonicalize value of type ~S" (type-of value)))))

(defun canonical-json (value)
  "Serialize VALUE in the RFC 8785 object/string/integer subset used by Wire.

Object keys are sorted by UTF-16 code units. Duplicate keys, non-I-JSON
integers, untyped floats, and values outside the protocol domain are rejected."
  (with-output-to-string (stream)
    (%write-canonical-json value stream "$")))

(defun %typed-protocol-value (value)
  "Turn runtime floats into an explicit, exact binary-value envelope."
  (typecase value
    (float
     (handler-case
         (multiple-value-bind (significand exponent sign)
             (integer-decode-float value)
           `(("$float" .
              (("exponent" . ,exponent)
               ("format" . ,(string-downcase
                              (princ-to-string (type-of value))))
               ("sign" . ,sign)
               ("significand" . ,(format nil "~D" significand))))))
       (error ()
         (%wire-error :non-finite-float "$"
                      "non-finite float requires an application-defined value"))))
    (string (copy-seq value))
    (cons (cons (%typed-protocol-value (car value))
                (%typed-protocol-value (cdr value))))
    (vector (map 'vector #'%typed-protocol-value value))
    (t value)))

(defun canonical-id (value)
  (format nil "sha256:~A"
          (sha256-hex (string->bytes (canonical-json value)))))

(defun wire-contract-id (descriptor)
  (canonical-id (wire-descriptor->value descriptor)))
