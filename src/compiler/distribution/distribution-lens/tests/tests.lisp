;;;; distribution-lens/tests/tests.lisp --- Executable lens laws.

(defpackage #:distribution-lens/tests
  (:use #:cl #:rosette-distribution-lens)
  (:export #:run-all-tests))

(in-package #:distribution-lens/tests)

(defvar *assertions* 0)
(defvar *failures* 0)

(defmacro check (form label)
  `(progn (incf *assertions*)
          (unless ,form
            (incf *failures*)
            (format t "  FAIL ~A: ~S~%" ,label ',form))))

(defun garden-lower ()
  (coerce (mapcar #'code-char '(114 115 104)) 'string))

(defun garden-upper ()
  (string-upcase (garden-lower)))

(defun rules ()
  (list (make-lens-rule :exact (format nil "~A-" (garden-lower))
                                   "rosette-")
        (make-lens-rule :exact (format nil "~A_" (garden-upper))
                                   "ROSETTE_")
        (make-lens-rule :token (garden-upper) "Rosette")
        (make-lens-rule :token (garden-lower) "rosette")))

(defun line (control &rest arguments)
  (concatenate 'string (apply #'format nil control arguments)
               (string #\Newline)))

(defun adjustable-text (text)
  (make-array (length text) :element-type 'base-char
              :adjustable t :fill-pointer (length text)
              :initial-contents text))

(defun temporary-directory ()
  (let ((path (merge-pathnames
               (format nil "distribution-lens-test-~36R-~36R/"
                       (get-universal-time) (random most-positive-fixnum))
               (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "placeholder" path)) path))

(defun write-text (root relative text)
  (let ((path (merge-pathnames relative root)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string text out))))

(defun signals-code-p (thunk code)
  (handler-case (progn (funcall thunk) nil)
    (distribution-lens-error (condition)
      (eq code (distribution-lens-error-code condition)))))

(defun has-obstruction-p (code obstructions)
  (some (lambda (item) (eq code (lens-obstruction-code item))) obstructions))

(defun fixture-map (rules source public)
  (make-export-map
   :profile (adjustable-text "rosette")
   :source-cut-id (adjustable-text "cut")
   :distribution-id (adjustable-text "distribution")
   :rules rules
   :entries
   (list (make-export-entry
          :path "src/demo.lisp" :class :component
          :source-path "src/demo.lisp" :source-id (text-id source)
          :public-id (text-id public) :editable-p t :lift-mode :namespace)
         (make-export-entry
          :path "README.md" :class :generated
          :public-id (text-id (line "generated"))))))

(defun test-text-laws ()
  (let* ((rules (rules))
         (source (concatenate 'string
                              (line "(defpackage #:~A-demo)" (garden-lower))
                              (line "(defun ~A-run () ~A_VALUE)"
                                    (garden-lower) (garden-upper))))
         (public (forward-text rules source))
         (edited (concatenate 'string
                              (line "(defpackage #:rosette-demo)")
                              (line "(defun rosette-run () (+ ROSETTE_VALUE 1))"))))
    (check (search "rosette-demo" public) "forward namespace")
    (check (string= source (reverse-text rules public)) "simple inverse")
    (let ((lifted (lift-edited-text source edited rules :namespace)))
      (check (search (format nil "(+ ~A_VALUE 1)" (garden-upper)) lifted)
             "namespace edit lifted")
      (check (string= edited (forward-text rules lifted)) "PutGet")
      (check (string= source (lift-edited-text source public rules :namespace))
             "GetPut"))
    (let* ((doc-source (concatenate 'string (line "# Rosette")
                                    (line "~A internals." (garden-upper))))
           (doc-edit (concatenate 'string (line "# Rosette")
                                  (line "A public Rosette manual."))))
      (check (string= doc-edit
                      (lift-edited-text doc-source doc-edit rules :literal))
             "literal documentation stays public"))
    (check (signals-code-p
            (lambda ()
              (lift-edited-text (line "x")
                                (line "~A secret" (garden-upper))
                                rules :literal))
            :literal-crosses-namespace)
           "literal mode refuses garden namespace")))

(defun test-tree-and-bundle ()
  (let* ((rules (rules))
         (source (line "(defun ~A-f (x) (+ x 1))" (garden-lower)))
         (public (forward-text rules source))
         (edited (line "(defun rosette-f (x) (+ x 2))"))
         (root (temporary-directory))
         (source-root (temporary-directory))
         (base-id (text-id "export")))
    (unwind-protect
         (progn
           (write-text root "src/demo.lisp" public)
           (write-text root "README.md" (line "generated"))
           (write-text source-root "src/demo.lisp" source)
           (let* ((map (fixture-map rules source public))
                  (scan (scan-export-tree root map base-id)))
             (check (scan-result-admitted-p scan) "unchanged tree scans")
             (check (null (change-bundle-changes (scan-result-bundle scan)))
                    "unchanged tree yields empty bundle")
             (write-text root "src/demo.lisp" edited)
             (let* ((changed (scan-export-tree root map base-id))
                    (bundle (scan-result-bundle changed))
                    (lift (lift-change-bundle bundle map source-root base-id)))
               (check (scan-result-admitted-p changed) "editable scan admitted")
               (check (= 1 (length (change-bundle-changes bundle)))
                      "one change captured")
               (check (lift-result-admitted-p lift) "change lifted")
               (check (search "(+ x 2)"
                              (lifted-change-content
                               (first (lift-result-changes lift))))
                      "source replacement recovered")
               (check (string= edited
                               (forward-text rules
                                             (lifted-change-content
                                              (first (lift-result-changes lift)))))
                      "lift re-exports exactly")
               (check (has-obstruction-p
                       :base-export-id-mismatch
                       (lift-result-obstructions
                        (lift-change-bundle bundle map source-root
                                            (text-id "other-export"))))
                      "export identity mismatch refused")
               (let ((overlay (merge-pathnames
                               (format nil "distribution-lens-overlay-~36R/"
                                       (random most-positive-fixnum))
                               (uiop:temporary-directory))))
                 (unwind-protect
                      (progn
                        (materialize-lift-overlay lift overlay)
                        (check (probe-file
                                (merge-pathnames "LIFT-RECEIPT.sexp" overlay))
                               "admitted lift materializes receipt")
                        (check (string=
                                (lifted-change-content
                                 (first (lift-result-changes lift)))
                                (uiop:read-file-string
                                 (merge-pathnames "src/demo.lisp" overlay)))
                               "overlay contains exact source replacement"))
                   (when (probe-file overlay)
                     (uiop:delete-directory-tree overlay :validate t))))
               (let ((bundle-path (merge-pathnames "bundle.sexp" source-root)))
                 (write-change-bundle bundle bundle-path)
                 (check (string= (change-bundle-id bundle)
                                 (change-bundle-id
                                  (read-change-bundle bundle-path)))
                        "bundle round-trip")
                 (check (null (search "#A(" (uiop:read-file-string bundle-path)))
                        "bundle uses portable string syntax")))
             (write-text root "README.md" (line "tampered"))
             (let ((refusal (scan-export-tree root map base-id)))
               (check (has-obstruction-p
                       :generated-file-edited
                       (scan-result-obstructions refusal))
                      "generated edit refused"))
             (write-text root "NEW.lisp" (line "surprise"))
             (let ((refusal (scan-export-tree root map base-id)))
               (check (has-obstruction-p
                       :added-file (scan-result-obstructions refusal))
                      "addition refused"))
             (let ((map-path (merge-pathnames "map.sexp" source-root)))
               (write-export-map map map-path)
               (check (string= (export-map-id map)
                               (export-map-id (read-export-map map-path)))
                      "map round-trip")
               (check (null (search "#A(" (uiop:read-file-string map-path)))
                      "map uses portable string syntax"))
             (delete-file (merge-pathnames "src/demo.lisp" root))
             (check (has-obstruction-p
                     :deleted-file
                     (scan-result-obstructions
                      (scan-export-tree root map base-id)))
                    "deletion refused")
             (write-text source-root "src/demo.lisp" (line "drift"))
             (let* ((clean-root (temporary-directory)))
               (unwind-protect
                    (progn
                      (write-text clean-root "src/demo.lisp" edited)
                      (write-text clean-root "README.md" (line "generated"))
                      (let* ((bundle (scan-result-bundle
                                      (scan-export-tree clean-root map base-id)))
                             (refusal (lift-change-bundle
                                       bundle map source-root base-id)))
                        (check (has-obstruction-p
                                :source-base-drift
                                (lift-result-obstructions refusal))
                               "source drift refused")))
                 (uiop:delete-directory-tree clean-root
                                             :validate t
                                             :if-does-not-exist :ignore))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree source-root
                                  :validate t :if-does-not-exist :ignore)))))

(defun test-inert-reader-boundary ()
  (let* ((root (temporary-directory))
         (path (merge-pathnames "input.sexp" root)))
    (unwind-protect
         (progn
           (dolist (text (list "#1000000000(0)" "#1=(:changes #1#)"
                               "#.(error \"must not execute\")" "'(:changes nil)"
                               "(:changes . nil)" "(:changes #+sbcl nil)"
                               "(:changes nil) (:changes nil)"
                               "(:changes cl-user::attacker-symbol)"
                               (concatenate 'string (make-string 65 :initial-element #\()
                                            "nil" (make-string 65 :initial-element #\)))
                               (concatenate 'string ":" (make-string 129 :initial-element #\a))))
             (write-text root "input.sexp" text)
             (check (handler-case (progn (read-change-bundle path) nil)
                      (distribution-lens-error () t))
                    "unsupported or over-budget reader syntax is refused before READ"))
           (let ((name (format nil "LENS-UNTRUSTED-~36R" (random most-positive-fixnum))))
             (check (null (find-symbol name :keyword)) "fresh keyword starts uninterned")
             (write-text root "input.sexp" (format nil "(:~A nil)" name))
             (check (handler-case (progn (read-change-bundle path) nil)
                      (distribution-lens-error () t))
                    "unknown bundle keyword refused")
             (check (null (find-symbol name :keyword))
                    "rejected bundle does not pollute the keyword package"))
           (write-text root "input.sexp"
                       (with-output-to-string (out)
                         (write-char #\( out)
                         (dotimes (index 200000) (write-string "nil " out))
                         (write-char #\) out)))
           (check (handler-case (progn (read-change-bundle path) nil)
                    (distribution-lens-error (condition)
                      (eq :input-too-large (distribution-lens-error-code condition))))
                  "node budget is enforced before host reader cons allocation")
           (write-text root "input.sexp"
                       (make-string (1+ rosette-distribution-lens::+maximum-bundle-bytes+)
                                    :initial-element #\Space))
           (check (handler-case (progn (read-change-bundle path) nil)
                    (distribution-lens-error (condition)
                      (eq :input-too-large (distribution-lens-error-code condition))))
                  "oversized input is refused before byte-buffer allocation")
           (write-text root "input.sexp" "(:profile \"literal #. and \\\"quote\\\"\")")
           (let ((*readtable* (copy-readtable nil)) (*read-suppress* t)
                 (*read-base* 16) (called nil))
             (set-macro-character #\( (lambda (&rest ignored)
                                        (declare (ignore ignored))
                                        (setf called t) (error "custom reader ran")))
             (check (equal '(:profile "literal #. and \"quote\"")
                           (rosette-distribution-lens::%read-one-form path))
                    "reader controls are isolated and source text remains inert")
             (check (not called) "custom reader macro was not invoked")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun run-all-tests ()
  (setf *assertions* 0 *failures* 0)
  (test-text-laws)
  (test-tree-and-bundle)
  (test-inert-reader-boundary)
  (format t "distribution-lens: ~D assertions, ~D failures~%"
          *assertions* *failures*)
  (when (plusp *failures*) (error "distribution-lens tests failed"))
  t)
