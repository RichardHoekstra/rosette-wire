;;;; toml.lisp --- Strict, configuration-oriented TOML subset.

(in-package #:rosette-toml)

(define-condition toml-error (error)
  ((line :initarg :line :reader toml-error-line)
   (column :initarg :column :reader toml-error-column)
   (reason :initarg :reason :reader toml-error-reason))
  (:report (lambda (condition stream)
             (format stream "TOML error at ~D:~D: ~A"
                     (toml-error-line condition)
                     (toml-error-column condition)
                     (toml-error-reason condition)))))

(defun %fail (line column control &rest arguments)
  (error 'toml-error :line line :column column
         :reason (apply #'format nil control arguments)))

(defun %trim (string)
  (string-trim '(#\Space #\Tab #\Return #\Newline) string))

(defun %split-top-level (string separator line)
  (let ((parts '()) (start 0) (depth 0) (quoted nil) (escaped nil))
    (loop for index below (length string)
          for char = (char string index)
          do (cond
               (escaped (setf escaped nil))
               ((and quoted (char= char #\\)) (setf escaped t))
               ((char= char #\") (setf quoted (not quoted)))
               (quoted nil)
               ((char= char #\[) (incf depth))
               ((char= char #\]) (decf depth))
               ((and (zerop depth) (char= char separator))
                (push (%trim (subseq string start index)) parts)
                (setf start (1+ index)))))
    (when (or quoted (/= depth 0))
      (%fail line 1 "unclosed string or array"))
    (push (%trim (subseq string start)) parts)
    (nreverse parts)))

(defun %strip-comment (line)
  (let ((quoted nil) (escaped nil))
    (loop for index below (length line)
          for char = (char line index)
          do (cond
               (escaped (setf escaped nil))
               ((and quoted (char= char #\\)) (setf escaped t))
               ((char= char #\") (setf quoted (not quoted)))
               ((and (not quoted) (char= char #\#))
                (return-from %strip-comment (subseq line 0 index)))))
    line))

(defun %valid-key-p (key)
  (and (plusp (length key))
       (every (lambda (char)
                (or (alphanumericp char) (find char "_-" :test #'char=)))
              key)))

(defun %parse-key-path (text line)
  (let ((parts (%split-top-level text #\. line)))
    (dolist (part parts)
      (unless (%valid-key-p part)
        (%fail line 1 "unsupported key ~S" part)))
    parts))

(defun %parse-basic-string (text line)
  (unless (and (>= (length text) 2)
               (char= (char text 0) #\")
               (char= (char text (1- (length text))) #\"))
    (%fail line 1 "malformed string"))
  (with-output-to-string (out)
    (loop with escaped = nil
          for index from 1 below (1- (length text))
          for char = (char text index)
          do (cond
               (escaped
                (write-char
                 (case char
                   (#\n #\Newline) (#\r #\Return) (#\t #\Tab)
                   (#\" #\") (#\\ #\\)
                   (otherwise (%fail line (1+ index)
                                     "unsupported escape \\~C" char)))
                 out)
                (setf escaped nil))
               ((char= char #\\) (setf escaped t))
               (t (write-char char out)))
          finally (when escaped
                    (%fail line (length text) "dangling string escape")))))

(defun %number-token-p (text)
  (and (plusp (length text))
       (every (lambda (char)
                (or (digit-char-p char)
                    (find char "+-.eE_" :test #'char=)))
              text)
       (some #'digit-char-p text)))

(defun %parse-number (text line)
  (unless (%number-token-p text)
    (%fail line 1 "unsupported value ~S" text))
  (let ((clean (remove #\_ text)))
    (handler-case
        (multiple-value-bind (value position) (read-from-string clean nil nil)
          (unless (and value (= position (length clean)) (realp value))
            (%fail line 1 "invalid finite number ~S" text))
          (let ((number (if (integerp value) value
                            (coerce value 'double-float))))
            (when (and (floatp number)
                       (or (not (= number number))
                           (> (abs number) most-positive-double-float)))
              (%fail line 1 "number is not finite"))
            number))
      (error () (%fail line 1 "invalid number ~S" text)))))

(defun %parse-value (text line)
  (let ((value (%trim text)))
    (cond
      ((zerop (length value)) (%fail line 1 "missing value"))
      ((char= (char value 0) #\") (%parse-basic-string value line))
      ((string= value "true") t)
      ((string= value "false") nil)
      ((char= (char value 0) #\[)
       (unless (char= (char value (1- (length value))) #\])
         (%fail line 1 "unclosed array"))
       (let* ((body (%trim (subseq value 1 (1- (length value)))))
              (items (if (zerop (length body)) '()
                         (%split-top-level body #\, line)))
              (parsed (mapcar (lambda (item) (%parse-value item line)) items)))
         (when parsed
           (let ((type (type-of (first parsed))))
             (unless (every (lambda (item)
                              (or (and (numberp item) (numberp (first parsed)))
                                  (and (stringp item)
                                       (stringp (first parsed)))
                                  (equal (type-of item) type)))
                            parsed)
               (%fail line 1 "arrays must be homogeneous"))))
         parsed))
      (t (%parse-number value line)))))

(defun %ensure-table (root path line)
  (let ((table root))
    (dolist (key path table)
      (let ((cell (assoc key (cdr table) :test #'string=)))
        (cond
          ((null cell)
           (let ((new-table '()))
             (push (cons key new-table) (cdr table))
             (setf table (assoc key (cdr table) :test #'string=))))
          ((and (listp (cdr cell))
                (or (null (cdr cell)) (consp (first (cdr cell)))))
           (setf table cell))
          (t (%fail line 1 "~A is already a scalar" key)))))))

(defun %put (root path value line)
  (let* ((parent-path (butlast path))
         (key (car (last path)))
         (table (%ensure-table root parent-path line)))
    (when (assoc key (cdr table) :test #'string=)
      (%fail line 1 "duplicate key ~A" key))
    (push (cons key value) (cdr table)))
  value)

(defun parse-toml (text)
  "Parse the supported TOML configuration subset into a string-keyed alist.
The root is an ordinary alist; tables are nested alists."
  (let ((root (list (cons "__root__" nil)))
        (section '()))
    (with-input-from-string (stream text)
      (loop for raw = (read-line stream nil nil)
            for line-number from 1
            while raw
            for line = (%trim (%strip-comment raw))
            unless (zerop (length line))
              do (if (char= (char line 0) #\[)
                     (progn
                       (unless (and (> (length line) 2)
                                    (char= (char line (1- (length line))) #\]))
                         (%fail line-number 1 "malformed table header"))
                       (setf section
                             (%parse-key-path
                              (%trim (subseq line 1 (1- (length line))))
                              line-number))
                       (%ensure-table root section line-number))
                     (let ((parts (%split-top-level line #\= line-number)))
                       (unless (= (length parts) 2)
                         (%fail line-number 1 "expected one key = value assignment"))
                       (%put root
                             (append section
                                     (%parse-key-path (first parts) line-number))
                             (%parse-value (second parts) line-number)
                             line-number)))))
    (nreverse (cdr root))))

(defun read-toml (pathname)
  "Read and parse PATHNAME."
  (with-open-file (stream pathname :direction :input)
    (let ((text (make-string (file-length stream))))
      (read-sequence text stream)
      (parse-toml text))))

(defun toml-get (document path &optional default)
  "Return a value from DOCUMENT. PATH is a dotted string or a list of strings."
  (let ((parts (if (stringp path) (%parse-key-path path 1) path))
        (value document))
    (dolist (key parts value)
      (let ((cell (and (listp value) (assoc key value :test #'string=))))
        (unless cell (return-from toml-get default))
        (setf value (cdr cell))))))

(defun %emit-string (value stream)
  (write-char #\" stream)
  (loop for char across value
        do (case char
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Tab (write-string "\\t" stream))
             (otherwise (write-char char stream))))
  (write-char #\" stream))

(defun %emit-value (value stream)
  (cond
    ((stringp value) (%emit-string value stream))
    ((eq value t) (write-string "true" stream))
    ((null value) (write-string "false" stream))
    ((integerp value) (format stream "~D" value))
    ((floatp value) (format stream "~,16G" value))
    ((listp value)
     (write-char #\[ stream)
     (loop for item in value for first = t then nil
           unless first do (write-string ", " stream)
           do (%emit-value item stream))
     (write-char #\] stream))
    (t (error "Cannot encode TOML value ~S" value))))

(defun %table-p (value)
  (and (listp value) value
       (every (lambda (cell) (and (consp cell) (stringp (car cell)))) value)))

(defun %write-table (table stream path root-p)
  (unless root-p
    (format stream "[~{~A~^.~}]~%" path))
  (dolist (cell table)
    (unless (%table-p (cdr cell))
      (format stream "~A = " (car cell))
      (%emit-value (cdr cell) stream)
      (terpri stream)))
  (dolist (cell table)
    (when (%table-p (cdr cell))
      (unless root-p (terpri stream))
      (%write-table (cdr cell) stream (append path (list (car cell))) nil))))

(defun write-toml (document destination)
  "Write DOCUMENT deterministically to a stream or pathname."
  (if (streamp destination)
      (%write-table document destination '() t)
      (with-open-file (stream destination :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (%write-table document stream '() t)))
  destination)
