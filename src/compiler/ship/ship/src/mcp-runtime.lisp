;;;; mcp-runtime.lisp --- MCP-over-stdio, baked into every :jit binary.
;;;;
;;;; A jit Deliverable answers `--mcp` by speaking JSON-RPC 2.0 / MCP over
;;;; stdin+stdout, so ANY agent's MCP client can use it as a native tool with
;;;; zero integration: the tools are the shipped package's exported functions,
;;;; discovered via `tools/list` and invoked via `tools/call`. Combined with the
;;;; content-id + parity (trust) and `--describe` (HATEOAS), this is the "share a
;;;; binary → their agent uses it immediately" surface.
;;;;
;;;; Self-contained (CL only, own package) so it compiles into any binary. The
;;;; tool-argument convention is pragmatic: each tool takes {"args":[...]}, a
;;;; positional array; precise per-arg JSON Schema from CL lambda-lists is a
;;;; refinement (the arglist is still surfaced via --describe).

(defpackage #:rosette-ship-mcp
  (:use #:cl)
  (:export #:serve #:handle-line #:json-encode #:json-parse))

(in-package #:rosette-ship-mcp)

;;; ======================================================================
;;; Minimal JSON — encode (a tiny DSL) and parse (recursive descent).
;;; ======================================================================
(defun %json-escape (s stream)
  (write-char #\" stream)
  (loop for ch across s do
    (case ch
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (#\Newline (write-string "\\n" stream))
      (#\Return (write-string "\\r" stream))
      (#\Tab (write-string "\\t" stream))
      (t (if (< (char-code ch) 32)
             (format stream "\\u~4,'0x" (char-code ch))
             (write-char ch stream)))))
  (write-char #\" stream))

(defun json-encode (obj &optional (stream *standard-output*))
  "Encode OBJ. Objects: (:obj \"k\" v ...); arrays: (:arr v ...); booleans/null:
:true :false :null; else string / number."
  (let ((*read-default-float-format* 'double-float))  ; print floats JSON-clean
    (labels ((enc (obj)
               (cond
                 ((eq obj :true) (write-string "true" stream))
                 ((eq obj :false) (write-string "false" stream))
                 ((eq obj :null) (write-string "null" stream))
                 ((null obj) (write-string "null" stream))
                 ((stringp obj) (%json-escape obj stream))
                 ((integerp obj) (format stream "~D" obj))
                 ((numberp obj) (format stream "~A" (coerce obj 'double-float)))
                 ((and (consp obj) (eq (car obj) :obj))
                  (write-char #\{ stream)
                  (loop for (k v) on (cdr obj) by #'cddr for first = t then nil
                        do (unless first (write-char #\, stream))
                           (%json-escape (string k) stream)
                           (write-char #\: stream) (enc v))
                  (write-char #\} stream))
                 ((and (consp obj) (eq (car obj) :arr))
                  (write-char #\[ stream)
                  (loop for v in (cdr obj) for first = t then nil
                        do (unless first (write-char #\, stream)) (enc v))
                  (write-char #\] stream))
                 (t (%json-escape (princ-to-string obj) stream)))))
      (enc obj)))
  obj)

(defstruct (jst (:constructor jst (str))) (str "" :type string) (pos 0 :type fixnum))
(defun %peek (st) (when (< (jst-pos st) (length (jst-str st))) (char (jst-str st) (jst-pos st))))
(defun %next (st) (prog1 (char (jst-str st) (jst-pos st)) (incf (jst-pos st))))
(defun %ws (st) (loop for c = (%peek st)
                      while (and c (member c '(#\Space #\Tab #\Newline #\Return)))
                      do (%next st)))

(defun json-parse (string)
  "Parse a JSON STRING. Objects -> alist of (string . value); arrays -> list;
true/false/null -> :true/:false/:null; else string / number."
  (let ((st (jst string))) (%ws st) (%pval st)))

(defun %pval (st)
  (%ws st)
  (let ((c (%peek st)))
    (cond ((null c) (error "json: unexpected end"))
          ((char= c #\{) (%pobj st))
          ((char= c #\[) (%parr st))
          ((char= c #\") (%pstr st))
          ((or (digit-char-p c) (char= c #\-)) (%pnum st))
          ((char= c #\t) (%plit st "true" :true))
          ((char= c #\f) (%plit st "false" :false))
          ((char= c #\n) (%plit st "null" :null))
          (t (error "json: unexpected ~A" c)))))

(defun %plit (st word val)
  (loop for ch across word do (unless (eql (%next st) ch) (error "json: bad literal")))
  val)

(defun %pstr (st)
  (%next st)                            ; opening "
  (let ((out (make-string-output-stream)))
    (loop for c = (%next st) do
      (cond ((char= c #\") (return))
            ((char= c #\\)
             (let ((e (%next st)))
               (case e
                 (#\" (write-char #\" out)) (#\\ (write-char #\\ out))
                 (#\/ (write-char #\/ out)) (#\n (write-char #\Newline out))
                 (#\r (write-char #\Return out)) (#\t (write-char #\Tab out))
                 (#\b (write-char #\Backspace out)) (#\f (write-char #\Page out))
                 (#\u (let ((code (parse-integer (jst-str st) :start (jst-pos st)
                                                 :end (+ (jst-pos st) 4) :radix 16)))
                        (incf (jst-pos st) 4) (write-char (code-char code) out)))
                 (t (write-char e out)))))
            (t (write-char c out))))
    (get-output-stream-string out)))

(defun %pnum (st)
  (let ((start (jst-pos st)))
    (loop for c = (%peek st)
          while (and c (or (digit-char-p c) (member c '(#\- #\+ #\. #\e #\E))))
          do (%next st))
    (let ((*read-eval* nil) (*read-default-float-format* 'double-float))
      (read-from-string (subseq (jst-str st) start (jst-pos st))))))

(defun %parr (st)
  (%next st) (%ws st)                   ; [
  (when (eql (%peek st) #\]) (%next st) (return-from %parr nil))
  (let ((out '()))
    (loop (push (%pval st) out) (%ws st)
          (let ((c (%next st)))
            (cond ((char= c #\]) (return (nreverse out)))
                  ((char= c #\,) (%ws st))
                  (t (error "json: bad array")))))))

(defun %pobj (st)
  (%next st) (%ws st)                   ; {
  (when (eql (%peek st) #\}) (%next st) (return-from %pobj nil))
  (let ((out '()))
    (loop (%ws st)
          (let ((key (%pstr st)))
            (%ws st) (unless (char= (%next st) #\:) (error "json: expected :"))
            (push (cons key (%pval st)) out))
          (%ws st)
          (let ((c (%next st)))
            (cond ((char= c #\}) (return (nreverse out)))
                  ((char= c #\,) nil)
                  (t (error "json: bad object")))))))

;;; ======================================================================
;;; MCP dispatch — tools = the shipped package's exported functions.
;;; ======================================================================
(defun %get (alist key) (cdr (assoc key alist :test #'string=)))

(defun %tools (pkg-name)
  (let ((pkg (find-package pkg-name)) (tools '()))
    (when pkg
      (do-external-symbols (s pkg)
        (when (fboundp s)
          (push (list :obj
                      "name" (string-downcase (symbol-name s))
                      "description" (or (documentation s 'function) "")
                      "inputSchema"
                      (list :obj "type" "object"
                            "properties" (list :obj "args"
                                               (list :obj "type" "array"
                                                     "description" "positional arguments"))))
                tools))))
    (cons :arr (nreverse tools))))

(defun %call (pkg-name name args)
  (let* ((pkg (find-package pkg-name))
         (sym (and pkg (find-symbol (string-upcase name) pkg))))
    (unless (and sym (fboundp sym)) (error "no such tool: ~A" name))
    (princ-to-string (apply sym (if (listp args) args (list args))))))

(defun %handle (pkg-name req)
  (let ((id (%get req "id")) (method (%get req "method")) (params (%get req "params")))
    (flet ((ok (result) (list :obj "jsonrpc" "2.0" "id" id "result" result)))
      (cond
        ((string= method "initialize")
         (ok (list :obj "protocolVersion" "2024-11-05"
                   "capabilities" (list :obj "tools" (list :obj))
                   "serverInfo" (list :obj "name" pkg-name "version" "0.1.0"))))
        ((string= method "notifications/initialized") nil)
        ((string= method "ping") (ok (list :obj)))
        ((string= method "tools/list") (ok (list :obj "tools" (%tools pkg-name))))
        ((string= method "tools/call")
         (let ((name (%get params "name"))
               (args (%get (%get params "arguments") "args")))
           (handler-case
               (ok (list :obj "content"
                         (list :arr (list :obj "type" "text"
                                          "text" (%call pkg-name name
                                                        (if (eq args :null) nil args))))))
             (error (e)
               (ok (list :obj "isError" :true
                         "content" (list :arr (list :obj "type" "text"
                                                    "text" (princ-to-string e)))))))))
        (id (list :obj "jsonrpc" "2.0" "id" id
                  "error" (list :obj "code" -32601 "message"
                                (format nil "method not found: ~A" method))))
        (t nil)))))

(defun handle-line (pkg-name line)
  "Parse one JSON-RPC LINE, dispatch to PKG-NAME's tools, return the response
JSON string (or NIL for a notification)."
  (handler-case
      (let ((resp (%handle pkg-name (json-parse line))))
        (when resp (with-output-to-string (s) (json-encode resp s))))
    (error (e)
      (with-output-to-string (s)
        (json-encode (list :obj "jsonrpc" "2.0" "id" :null
                           "error" (list :obj "code" -32700
                                         "message" (princ-to-string e)))
                     s)))))

(defun serve (pkg-name &optional (in *standard-input*) (out *standard-output*))
  "MCP-over-stdio loop: read newline-delimited JSON-RPC from IN, dispatch to
PKG-NAME's exported functions, write responses to OUT. Runs until EOF."
  (loop for line = (read-line in nil :eof)
        until (eq line :eof)
        when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
          do (let ((resp (handle-line pkg-name line)))
               (when resp (write-string resp out) (write-char #\Newline out)
                     (finish-output out)))))
