;;;; reader.lisp --- Read Lisp source into hash-consed forms.

(in-package #:rosette-sexpr-dag)

(defun read-lisp-forms (pathname)
  "Read top-level forms from PATHNAME.

This uses the host Lisp reader and therefore intentionally treats source as
Lisp syntax, not raw text. Files that require unavailable reader packages are
best handled after their systems are loaded; INDEX-LISP-FILE records the read
error instead of failing the whole scan."
  (with-open-file (stream pathname :direction :input)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun %ensure-package (name)
  (or (find-package name)
      (make-package name :use nil)))

(defun %package-name-string (designator)
  (string-upcase
   (etypecase designator
     (string designator)
     (symbol (symbol-name designator)))))

(defun %ensure-exported-symbol (package-name symbol-name)
  (let* ((package (%ensure-package package-name))
         (symbol (or (find-symbol (string-upcase (symbol-name symbol-name)) package)
                     (intern (string-upcase (symbol-name symbol-name)) package))))
    (export symbol package)
    symbol))

(defun %bootstrap-defpackage (form)
  "Predeclare packages and exported/imported symbols in a DEFPACKAGE form.

This lets cold scans read later PACKAGE:SYMBOL references without loading the
target systems. It is intentionally structural; the real DEFPACKAGE is still
evaluated afterward when possible."
  (when (and (consp form) (eq (car form) 'cl:defpackage))
    (let ((package-name (%package-name-string (second form))))
      (%ensure-package package-name)
      (dolist (clause (cddr form))
        (when (consp clause)
          (case (car clause)
            ((:use :shadow :nicknames :documentation :size :intern) nil)
            (:export
             (dolist (symbol-name (cdr clause))
               (%ensure-exported-symbol package-name symbol-name)))
            ((:import-from :shadowing-import-from)
             (let ((source-package (%package-name-string (second clause))))
               (%ensure-package source-package)
               (dolist (symbol-name (cddr clause))
                 (%ensure-exported-symbol source-package symbol-name))))))))))

(defun %ensure-reader-packages-for-path (pathname)
  "Create empty package placeholders inferred from Rosette path components.

This is only for reading source. It lets the Lisp reader accept qualified
symbols such as ROSETTE-FOO:BAR in examples before ROSETTE-FOO has been loaded."
  (dolist (part (pathname-directory pathname))
    (when (and (stringp part)
               (uiop:string-prefix-p "rosette-" part))
      (let ((name (string-upcase part)))
        (%ensure-package name)
        (%ensure-package (concatenate 'string name "/TESTS"))))))

(defun %reader-package-char-p (ch)
  (or (alphanumericp ch)
      (member ch '(#\- #\_ #\/ #\.) :test #'char=)))

(defun %reader-symbol-char-p (ch)
  (or (alphanumericp ch)
      (member ch '(#\- #\_ #\* #\+ #\/ #\< #\> #\= #\? #\! #\$ #\% #\& #\~ #\^)
              :test #'char=)))

(defun %bootstrap-qualified-reader-symbols (pathname)
  "Predeclare package-qualified Rosette symbols found textually in PATHNAME.

The scanner indexes cold source trees without loading every optional system.
When a file contains ROSETTE-FOO/BAR:BAZ and no package form for ROSETTE-FOO/BAR is
available, the Lisp reader fails before the DAG code can recover per form.
This pass creates only Rosette package placeholders and their referenced external
symbols; it does not evaluate user code."
  (with-open-file (stream pathname :direction :input)
    (let* ((size (file-length stream))
           (text (make-string size)))
      (read-sequence text stream)
      (loop with n = (length text)
            for colon = (position #\: text :start 0)
              then (position #\: text :start (1+ colon))
            while colon
            do (let ((start colon))
                 (loop while (and (> start 0)
                                  (%reader-package-char-p
                                   (char text (1- start))))
                       do (decf start))
                 (when (and (< start colon)
                            (uiop:string-prefix-p
                             "ROSETTE-" (string-upcase
                                     (subseq text start colon))))
                   (let* ((double-colon-p (and (< (1+ colon) n)
                                               (char= (char text (1+ colon))
                                                      #\:)))
                          (symbol-start (+ colon (if double-colon-p 2 1)))
                          (end symbol-start))
                     (loop while (and (< end n)
                                      (%reader-symbol-char-p
                                       (char text end)))
                           do (incf end))
                     (when (< symbol-start end)
                       (let* ((package-name (string-upcase
                                             (subseq text start colon)))
                              (symbol-name (string-upcase
                                            (subseq text symbol-start end)))
                              (package (%ensure-package package-name))
                              (symbol (or (find-symbol
                                           symbol-name package)
                                          (intern symbol-name package))))
                         (unless double-colon-p
                           (export symbol package)))))))))))

(defun %maybe-eval-defpackage (form)
  "Evaluate DEFPACKAGE forms so later package-qualified symbols can read.

No other top-level forms are evaluated by the source scanner."
  (when (and (consp form) (eq (car form) 'cl:defpackage))
    (%bootstrap-defpackage form)
    (handler-bind ((warning #'muffle-warning))
      (handler-case
          (eval form)
        (error () nil)))))

(defun %maybe-eval-in-package (form)
  "Honor IN-PACKAGE while cold-reading a file.

The scanner does not evaluate user code, but the host reader resolves
package-local nicknames through *PACKAGE*.  Updating *PACKAGE* after reading an
IN-PACKAGE form lets later forms use local nicknames declared by DEFPACKAGE."
  (when (and (consp form) (eq (car form) 'cl:in-package))
    (let ((package (%ensure-package (%package-name-string (second form)))))
      (setf *package* package)
      package)))

(defun index-lisp-file (pathname &key (index (make-dag-index)))
  "Read PATHNAME and add every top-level form to INDEX.

Returns two values: INDEX and an error object or NIL. Reader errors are
contained so project scans can continue."
  (handler-case
      (progn
        (%ensure-reader-packages-for-path pathname)
        (%bootstrap-qualified-reader-symbols pathname)
        (let ((*package* *package*)
              (*read-eval* nil))
          (with-open-file (stream pathname :direction :input)
            (loop for form = (read stream nil :eof)
                  for i from 0
                  until (eq form :eof)
                  do (%maybe-eval-defpackage form)
                  do (%maybe-eval-in-package form)
                  do (sexp->dag form
                                :index index
                                :root t
                                :provenance (list :file (namestring pathname)
                                                  :form-index i))
                  finally (return (values index nil))))))
    (error (condition)
      (values index condition))))

(defun index-lisp-files (pathnames &key (index (make-dag-index)))
  "Index every pathname in PATHNAMES. Returns INDEX and a list of read errors."
  (let ((errors nil))
    (dolist (pathname pathnames (values index (nreverse errors)))
      (multiple-value-bind (_ error) (index-lisp-file pathname :index index)
        (declare (ignore _))
        (when error
          (push (list :file (namestring pathname) :error error) errors))))))

(defun %rosette-directory-name-p (name)
  (and (stringp name)
       (uiop:string-prefix-p "rosette-" name)))

(defun %reader-repo-root (directory)
  (loop with current = (uiop:ensure-directory-pathname directory)
        for probe = (probe-file current)
        while probe
        when (and (probe-file (merge-pathnames "INDEX.md" current))
                  (probe-file (merge-pathnames "CAPABILITIES.json" current)))
          return current
        do (let* ((parts (pathname-directory current))
                  (parent-parts (butlast parts)))
             (if (equal parts parent-parts)
                 (return nil)
                 (setf current (make-pathname :defaults current
                                               :directory parent-parts))))))

(defun %reader-package-root (directory)
  (or (%reader-repo-root directory)
      (let* ((dir (uiop:ensure-directory-pathname directory))
             (parts (pathname-directory dir))
             (leaf (car (last parts))))
        (if (%rosette-directory-name-p leaf)
            (make-pathname :defaults dir
                           :directory (butlast parts))
            dir))))

(defun %bootstrap-reader-packages (directory)
  "Evaluate sibling rosette-*/src/package.lisp forms before a scan.

Targeted scans such as `tools/sexpr-dag-report.lisp rosette-ring-tensor` still
contain qualified references to packages from other libraries. Preloading only
DEFPACKAGE forms gives the reader enough external symbols without loading or
executing those systems."
  (let ((root (%reader-package-root directory)))
    (when (probe-file root)
      (labels ((load-package-file (package-file)
                 (when (probe-file package-file)
                   (handler-case
                       (dolist (form (read-lisp-forms package-file))
                         (%maybe-eval-defpackage form))
                     (error () nil))))
               (walk (dir)
                 (dolist (subdir (uiop:subdirectories dir))
                   (let ((leaf (car (last (pathname-directory subdir)))))
                     (if (%rosette-directory-name-p leaf)
                         (load-package-file
                          (merge-pathnames "src/package.lisp" subdir))
                         (walk subdir))))))
        (walk root)))))

(defun %lisp-source-pathname-p (pathname)
  (let ((type (pathname-type pathname)))
    (and type (member (string-downcase type) '("lisp" "asd" "cl")
                      :test #'string=))))

(defun index-directory (directory &key (index (make-dag-index))
                                    (exclude-directories '(".git" "_reference")))
  "Recursively index .lisp, .asd, and .cl files under DIRECTORY.

Directories named by EXCLUDE-DIRECTORIES are ignored. Returns INDEX and
read errors."
  (let ((files nil))
    (%bootstrap-reader-packages directory)
    (labels ((walk (dir)
               (dolist (p (uiop:directory-files dir))
                 (when (%lisp-source-pathname-p p)
                   (push p files)))
               (dolist (subdir (uiop:subdirectories dir))
                 (unless (member (car (last (pathname-directory subdir)))
                                 exclude-directories
                                 :test #'string=)
                   (walk subdir)))))
      (walk (uiop:ensure-directory-pathname directory)))
    (labels ((file-priority (pathname)
               (let ((name (pathname-name pathname)))
                 (cond
                   ((and name (string= name "package")) 0)
                   ((string= (or (pathname-type pathname) "") "asd") 1)
                   (t 2))))
             (source< (a b)
               (let ((pa (file-priority a))
                     (pb (file-priority b)))
                 (if (= pa pb)
                     (string< (namestring a) (namestring b))
                     (< pa pb)))))
      (index-lisp-files (sort files #'source<) :index index))))
