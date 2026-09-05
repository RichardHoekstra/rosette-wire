;;;; parse.lisp --- JSON string -> Lisp data.
;;;;
;;;; A small recursive-descent parser over a string + index. Produces the
;;;; data model documented in encode.lisp:
;;;;   object -> alist of (string . value)   array -> list
;;;;   string -> string   number -> integer/double-float
;;;;   true/false/null -> :true / :false / :null
;;;; Malformed input signals JSON-PARSE-ERROR with the offending position.

(in-package #:rosette-json)

(define-condition json-parse-error (error)
  ((message  :initarg :message  :reader json-parse-error-message)
   (position :initarg :position :reader json-parse-error-position :initform nil))
  (:report (lambda (c stream)
             (format stream "JSON parse error at ~A: ~A"
                     (or (json-parse-error-position c) "?")
                     (json-parse-error-message c)))))

(defconstant +default-json-max-depth+ 128)

(defstruct (parser (:constructor %make-parser (string len max-depth)))
  (string "" :type simple-string)
  (pos 0   :type fixnum)
  (len 0   :type fixnum)
  (depth 0 :type fixnum)
  (max-depth +default-json-max-depth+ :type (or null integer)))

(declaim (inline %peek %eof-p))
(defun %peek (p)
  (if (< (parser-pos p) (parser-len p))
      (char (parser-string p) (parser-pos p))
      nil))

(defun %eof-p (p) (>= (parser-pos p) (parser-len p)))

(defun %fail (p fmt &rest args)
  (error 'json-parse-error
         :position (parser-pos p)
         :message (apply #'format nil fmt args)))

(defun %advance (p) (incf (parser-pos p)))

(defun %skip-ws (p)
  (loop for ch = (%peek p)
        while (and ch (member ch '(#\Space #\Tab #\Newline #\Return)))
        do (%advance p)))

(defun %expect (p ch)
  (if (eql (%peek p) ch)
      (%advance p)
      (%fail p "expected ~C but got ~A" ch
             (if (%eof-p p) "end of input" (%peek p)))))

(defun %match-literal (p literal value)
  "If the upcoming text is LITERAL, consume it and return VALUE."
  (let ((end (+ (parser-pos p) (length literal))))
    (when (and (<= end (parser-len p))
               (string= (parser-string p) literal
                        :start1 (parser-pos p) :end1 end))
      (setf (parser-pos p) end)
      (return-from %match-literal value)))
  (%fail p "invalid literal (expected ~A)" literal))

(defun %parse-hex4 (p)
  "Parse exactly four hexadecimal digits after a `\\u` introducer."
  (when (> (+ (parser-pos p) 4) (parser-len p))
    (%fail p "truncated \\u escape"))
  (let ((code 0))
    (dotimes (index 4 code)
      (let ((digit (digit-char-p (%peek p) 16)))
        (unless digit
          (%fail p "invalid hexadecimal digit in \\u escape"))
        (setf code (+ (* code 16) digit))
        (%advance p)))))

(defun %write-codepoint (code out p)
  (let ((character (code-char code)))
    (unless character
      (%fail p "host Lisp cannot represent Unicode code point U+~4,'0X" code))
    (write-char character out)))

(defun %parse-string (p)
  (%expect p #\")
  (let ((out (make-string-output-stream)))
    (loop
      (when (%eof-p p) (%fail p "unterminated string"))
      (let ((ch (%peek p)))
        (cond
          ((char= ch #\") (%advance p) (return))
          ((char= ch #\\)
           (%advance p)
           (when (%eof-p p) (%fail p "unterminated escape"))
           (let ((e (%peek p)))
             (%advance p)
             (case e
               (#\" (write-char #\" out))
               (#\\ (write-char #\\ out))
               (#\/ (write-char #\/ out))
               (#\b (write-char #\Backspace out))
               (#\f (write-char #\Page out))
               (#\n (write-char #\Newline out))
               (#\r (write-char #\Return out))
               (#\t (write-char #\Tab out))
               (#\u
                (let ((code (%parse-hex4 p)))
                  (cond
                    ((<= #xd800 code #xdbff)
                     ;; JSON transports supplementary code points as an exact
                     ;; high/low UTF-16 surrogate pair.  Normalize that pair
                     ;; into one host character; never leak surrogate code
                     ;; units into the Lisp data model.
                     (unless (and (eql (%peek p) #\\)
                                  (< (1+ (parser-pos p)) (parser-len p))
                                  (eql (char (parser-string p)
                                             (1+ (parser-pos p)))
                                       #\u))
                       (%fail p "high surrogate lacks a following \\u low surrogate"))
                     (%advance p)
                     (%advance p)
                     (let ((low (%parse-hex4 p)))
                       (unless (<= #xdc00 low #xdfff)
                         (%fail p "high surrogate is followed by non-low surrogate"))
                       (%write-codepoint
                        (+ #x10000
                           (ash (- code #xd800) 10)
                           (- low #xdc00))
                        out p)))
                    ((<= #xdc00 code #xdfff)
                     (%fail p "unpaired low surrogate"))
                    (t (%write-codepoint code out p)))))
               (t (%fail p "invalid escape \\~C" e)))))
          ((< (char-code ch) #x20)
           (%fail p "unescaped control character in string"))
          ((<= #xd800 (char-code ch) #xdfff)
           (%fail p "raw surrogate code unit in string"))
          (t (write-char ch out) (%advance p)))))
    (get-output-stream-string out)))

(defun %parse-number (p)
  (let ((start (parser-pos p))
        (floatp nil))
    ;; RFC 8259 number grammar:
    ;;   -? (0 / [1-9][0-9]*) (.[0-9]+)? ([eE][+-]?[0-9]+)?
    (when (eql (%peek p) #\-)
      (%advance p))
    (cond
      ((eql (%peek p) #\0)
       (%advance p)
       (when (and (%peek p) (digit-char-p (%peek p)))
         (%fail p "leading zero in number")))
      ((and (%peek p) (digit-char-p (%peek p))
            (not (zerop (digit-char-p (%peek p)))))
       (loop while (and (%peek p) (digit-char-p (%peek p)))
             do (%advance p)))
      (t (%fail p "expected digit in number")))
    (when (eql (%peek p) #\.)
      (setf floatp t)
      (%advance p)
      (unless (and (%peek p) (digit-char-p (%peek p)))
        (%fail p "fraction requires at least one digit"))
      (loop while (and (%peek p) (digit-char-p (%peek p)))
            do (%advance p)))
    (when (member (%peek p) '(#\e #\E))
      (setf floatp t)
      (%advance p)
      (when (member (%peek p) '(#\+ #\-))
        (%advance p))
      (unless (and (%peek p) (digit-char-p (%peek p)))
        (%fail p "exponent requires at least one digit"))
      (loop while (and (%peek p) (digit-char-p (%peek p)))
            do (%advance p)))
    (let ((text (subseq (parser-string p) start (parser-pos p))))
      (handler-case
          (if floatp
              (let ((*read-default-float-format* 'double-float)
                    (*read-base* 10)
                    (*read-suppress* nil)
                    (*readtable* (copy-readtable nil))
                    (*read-eval* nil))
                (multiple-value-bind (number end)
                    (read-from-string text nil nil)
                  (unless (and (= end (length text)) (realp number))
                    (%fail p "malformed number ~S" text))
                  (coerce number 'double-float)))
              (parse-integer text :radix 10))
        (error () (%fail p "malformed number ~S" text))))))

(defun %parse-container (p function)
  (when (and (parser-max-depth p)
             (>= (parser-depth p) (parser-max-depth p)))
    (%fail p "JSON nesting exceeds configured depth ~D"
           (parser-max-depth p)))
  (incf (parser-depth p))
  (unwind-protect (funcall function p)
    (decf (parser-depth p))))

(defun %parse-array (p)
  (%expect p #\[)
  (%skip-ws p)
  (when (eql (%peek p) #\])
    (%advance p)
    (return-from %parse-array nil))
  (let ((acc '()))
    (loop
      (push (%parse-value p) acc)
      (%skip-ws p)
      (case (%peek p)
        (#\, (%advance p) (%skip-ws p))
        (#\] (%advance p) (return))
        (t (%fail p "expected , or ] in array"))))
    (nreverse acc)))

(defun %parse-object (p)
  (%expect p #\{)
  (%skip-ws p)
  (when (eql (%peek p) #\})
    (%advance p)
    ;; Empty object: distinguish from empty array via :empty-object sentinel?
    ;; We use NIL-free representation: an empty object is the keyword :{}.
    (return-from %parse-object :empty-object))
  (let ((acc '()))
    (loop
      (%skip-ws p)
      (unless (eql (%peek p) #\")
        (%fail p "expected string key in object"))
      (let ((key (%parse-string p)))
        (%skip-ws p)
        (%expect p #\:)
        (%skip-ws p)
        (push (cons key (%parse-value p)) acc))
      (%skip-ws p)
      (case (%peek p)
        (#\, (%advance p))
        (#\} (%advance p) (return))
        (t (%fail p "expected , or } in object"))))
    (nreverse acc)))

(defun %parse-value (p)
  (%skip-ws p)
  (let ((ch (%peek p)))
    (cond
      ((null ch) (%fail p "unexpected end of input"))
      ((char= ch #\{) (%parse-container p #'%parse-object))
      ((char= ch #\[) (%parse-container p #'%parse-array))
      ((char= ch #\") (%parse-string p))
      ((char= ch #\t) (%match-literal p "true"  json-true))
      ((char= ch #\f) (%match-literal p "false" json-false))
      ((char= ch #\n) (%match-literal p "null"  json-null))
      ((or (digit-char-p ch) (char= ch #\-)) (%parse-number p))
      (t (%fail p "unexpected character ~C" ch)))))

(defun json-parse (string &key (max-depth +default-json-max-depth+))
  "Parse STRING as JSON and return the corresponding Lisp data.
Signals JSON-PARSE-ERROR on malformed input.

Mapping: object -> alist (string . value), array -> list, string -> string,
number -> integer/double-float, true/false/null -> :true/:false/:null,
empty object -> :empty-object. MAX-DEPTH defaults to 128 nested arrays/objects;
a positive integer changes the ceiling and NIL explicitly disables it."
  (unless (or (null max-depth)
              (typep max-depth '(integer 1 #.most-positive-fixnum)))
    (error 'json-parse-error :position 0
           :message "MAX-DEPTH must be a positive integer or NIL"))
  (let ((p (%make-parser (coerce string 'simple-string)
                         (length string) max-depth)))
    (let ((value (%parse-value p)))
      (%skip-ws p)
      (unless (%eof-p p)
        (%fail p "trailing content after JSON value"))
      value)))

(defun json-ref (data &rest path)
  "Walk DATA (a parsed JSON value) along PATH. Each step is a string/symbol
object key or an integer array index. Returns the value, or :missing if a
step does not resolve."
  (dolist (step path data)
    (setf data
          (cond
            ((and (integerp step) (<= 0 step)
                  (listp data) (< step (length data)))
             (nth step data))
            ((and (consp data) (consp (car data)))
             (let ((cell (assoc (if (stringp step) step (%key-string step))
                                data :test #'equal)))
               (if cell (cdr cell) (return :missing))))
            (t (return :missing))))))
