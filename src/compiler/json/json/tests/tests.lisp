;;;; tests.lisp --- tests for rosette-json.

(in-package #:rosette-json/tests)

(defparameter *chat-response*
  "{\"id\":\"chatcmpl-7\",\"object\":\"chat.completion\",\"created\":1700000000,
    \"model\":\"gpt-4\",\"choices\":[{\"index\":0,
      \"message\":{\"role\":\"assistant\",\"content\":\"Hello, world!\"},
      \"finish_reason\":\"stop\"}],
    \"usage\":{\"prompt_tokens\":9,\"completion_tokens\":12,\"total_tokens\":21}}"
  "An OpenAI-chat-style response body for the parse + nested-extract law.")

(defun round-trips-p (value)
  "True when (parse (encode value)) is EQUAL to VALUE."
  (equal (json-parse (json-encode value)) value))

(defun signals-error-p (thunk &optional (type 'error))
  (handler-case (progn (funcall thunk) nil)
    (error (condition) (typep condition type))))

(defun run-all-tests ()
  (with-test-run (run "rosette-json")
    ;; --- scalars -------------------------------------------------------
    (check run (= 42 (json-parse "42")) "parse integer")
    (check run (= -7 (json-parse "-7")) "parse negative integer")
    (check run (< (abs (- 3.14d0 (json-parse "3.14"))) 1d-12) "parse float")
    (check run (< (abs (- 1.5d3 (json-parse "1.5e3"))) 1d-9) "parse exponent")
    (check run (eq :true  (json-parse "true"))  "parse true")
    (check run (eq :false (json-parse "false")) "parse false")
    (check run (eq :null  (json-parse "null"))  "parse null")
    (check run (string= "hi" (json-parse "\"hi\"")) "parse string")

    ;; --- string escapes ------------------------------------------------
    (check run (string= (format nil "a~Cb" #\Newline)
                        (json-parse "\"a\\nb\""))
           "parse \\n escape")
    (check run (string= "q\"q" (json-parse "\"q\\\"q\"")) "parse escaped quote")
    (check run (char= #\A (char (json-parse "\"\\u0041\"") 0))
           "parse \\u unicode escape")
    (let ((astral (code-char #x1f642)))
      (check run
             (and astral
                  (let ((parsed (json-parse "\"\\ud83d\\ude42\"")))
                    (and (= 1 (length parsed))
                         (= #x1f642 (char-code (char parsed 0))))))
             "parse a UTF-16 surrogate pair as one supplementary code point"))
    (check run (string= "\"a\\nb\"" (json-string (format nil "a~Cb" #\Newline)))
           "json-string escapes newline")
    (check run (string= "\"q\\\"q\"" (json-string "q\"q"))
           "json-string escapes quote")
    (check run (string= "\"slash\\\\slash\"" (json-string "slash\\slash"))
           "json-string escapes backslash")
    (check run (string= "\"\\u0001\"" (json-string (string (code-char 1))))
           "json-string escapes control characters as unicode")

    ;; --- containers ----------------------------------------------------
    (check run (equal '(1 2 3) (json-parse "[1,2,3]")) "parse array")
    (check run (equal '(1 (:true :false) :null)
                      (json-parse "[1,[true,false],null]"))
           "parse nested array with sentinels")
    (check run (null (json-parse "[]")) "parse empty array -> NIL")
    (check run (eq :empty-object (json-parse "{}")) "parse empty object")
    (check run (equal '(("a" . 1) ("b" . 2))
                      (json-parse "{\"a\":1,\"b\":2}"))
           "parse object -> alist")
    (check run (equal '(("x" . (1 (("y" . :true)))))
                      (json-parse "{\"x\":[1,{\"y\":true}]}"))
           "parse nested object+array")

    ;; --- encode --------------------------------------------------------
    (check run (string= "42" (json-encode 42)) "encode integer")
    (check run (string= "true" (json-encode :true)) "encode true")
    (check run (string= "null" (json-encode :null)) "encode null")
    (check run (string= "[1,2,3]" (json-encode '(1 2 3))) "encode array")
    (check run (string= "[]" (json-encode nil)) "encode NIL -> []")
    (check run (string= "{}" (json-encode :empty-object)) "encode empty object")
    (check run (string= "{\"a\":1}" (json-encode '(("a" . 1)))) "encode object")
    (check run (string= "[1,2,3]" (json-encode #(1 2 3))) "encode vector as array")
    (check run (string= "\"hello\"" (json-encode 'hello)) "encode symbol as lowercase string")
    (check run (string= "{\"hello\":1}" (json-encode '((hello . 1))))
           "encode symbol object key as lowercase string")
    (check run (string= "{\"answer\":42}" (json-encode '((:answer . 42))))
           "encode keyword object key as lowercase string")
    (check run (= 0.5d0 (json-parse (json-encode 1/2)))
           "encode rational through controlled float boundary")
    (let ((*print-base* 16) (*print-radix* t))
      (check run (string= "20" (json-encode 20))
             "integer encoding is decimal under hostile print radix"))
    (let ((*read-base* 16))
      (check run (= 100.0d0 (json-parse "1e2"))
             "number parsing is decimal under hostile read radix"))
    (let ((*read-suppress* t))
      (check run (= 100.0d0 (json-parse "1e2"))
             "number parsing ignores ambient reader suppression"))
    (let ((*readtable* (copy-readtable nil)))
      (set-macro-character #\1
                           (lambda (stream character)
                             (declare (ignore stream character))
                             :poison))
      (check run (= 100.0d0 (json-parse "1e2"))
             "number parsing uses a standard readtable"))
    (let ((surrogate (code-char #xd800)))
      (check run (and surrogate
                      (signals-error-p
                       (lambda () (json-string (string surrogate)))))
             "encoder rejects a raw surrogate code unit"))
    (multiple-value-bind (infinity status)
        (let ((package (find-package "SB-EXT")))
          (if package
              (find-symbol "DOUBLE-FLOAT-POSITIVE-INFINITY" package)
              (values nil nil)))
      (declare (ignore status))
      (if (and infinity (boundp infinity))
          (check run
                 (signals-error-p
                  (lambda () (json-encode (symbol-value infinity))))
                 "encoder rejects a non-finite float")
          (check run t
                 "implementation exposes no constructible infinity fixture")))
    (let ((out (make-string-output-stream))
          (value '(("a" . 1))))
      (check run (eq (write-json value out) value) "write-json returns original value")
      (check run (string= "{\"a\":1}" (get-output-stream-string out))
             "write-json writes encoded JSON to stream"))

    ;; --- LAW: round-trip on a corpus -----------------------------------
    (dolist (v (list 0 42 -17 3.5d0
                     "" "plain" (format nil "tab~Ctab" #\Tab)
                     :true :false :null
                     nil '(1 2 3) '(1 (2 3) 4)
                     '(("name" . "alice") ("age" . 30))
                     '(("nested" . (("a" . 1) ("b" . (10 20 :null)))))
                     '(("escapes" . "a\"b\\c") ("arr" . (:true :false)))))
      (check run (round-trips-p v)
             (format nil "round-trip: ~S" v)))

    ;; --- LAW: parse real-world JSON + extract a nested field -----------
    (let ((resp (json-parse *chat-response*)))
      (check run (string= "chat.completion" (json-ref resp "object"))
             "extract top-level field")
      (check run (string= "Hello, world!"
                          (json-ref resp "choices" 0 "message" "content"))
             "extract deeply nested field (the oracle-reply content)")
      (check run (string= "Hello, world!"
                          (json-ref resp :choices 0 :message :content))
             "extract deeply nested field with symbol keys")
      (check run (= 21 (json-ref resp "usage" "total_tokens"))
             "extract nested integer")
      (check run (eq :missing (json-ref resp "nope" "nope"))
             "missing path -> :missing"))
    (check run (eq :missing (json-ref (json-parse "[10,20]") 5))
           "array index past end -> :missing")
    (check run (eq :missing (json-ref (json-parse "[10,20]") -1))
           "negative array index -> :missing")
    (check run (eq :missing (json-ref (json-parse "{}") "anything"))
           "empty object sentinel has no keys")
    (check run (eq :missing (json-ref (json-parse "42") "anything"))
           "scalar path lookup -> :missing")

    ;; --- LAW: malformed JSON signals a clear error ---------------------
    (dolist (bad '("{" "[1,2" "{\"a\":}" "tru" "\"unterminated"
                   "[1 2]" "{\"a\":1 \"b\":2}" "" "nul" "12,34"
                   "\"\\u00xz\"" "\"\\q\""
                   "01" "-01" "00" "1." "1e" "1e+" "1e-"
                   "+1" ".1" "1.e2" "1e--2"
                   "\"raw
newline\""
                   "\"\\ud800\"" "\"\\ud800\\u0041\""
                   "\"\\udc00\""))
      (check run (handler-case (progn (json-parse bad) nil)
                   (json-parse-error () t))
             (format nil "malformed signals json-parse-error: ~S" bad)))

    (dolist (row '(("0" . 0) ("-0" . 0) ("10" . 10)
                   ("0.0" . 0.0d0) ("1e-2" . 0.01d0)
                   ("-2.5E+2" . -250.0d0)))
      (check run (= (cdr row) (json-parse (car row)))
             (format nil "strict valid number parses: ~A" (car row))))

    (check run (equal '(((0))) (json-parse "[[[0]]]" :max-depth 3))
           "configured nesting depth admits its exact boundary")
    (check run
           (signals-error-p
            (lambda () (json-parse "[[[0]]]" :max-depth 2))
            'json-parse-error)
           "configured nesting depth rejects the next container")
    (let ((too-deep
            (concatenate 'string
                         (make-string 129 :initial-element #\[)
                         "0"
                         (make-string 129 :initial-element #\]))))
      (check run
             (signals-error-p (lambda () (json-parse too-deep))
                              'json-parse-error)
             "default nesting wall fails before recursive stack exhaustion"))

    ;; --- error carries a position --------------------------------------
    (check run (handler-case (progn (json-parse "[1,2,]") nil)
                 (json-parse-error (e) (integerp (json-parse-error-position e))))
           "parse error reports a position")))
