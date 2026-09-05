;;;; distribution-compiler.lisp --- compile Rosette closures into repository plans.

(in-package #:rosette-distribution-compiler)

(define-condition distribution-error (error)
  ((code :initarg :code :reader distribution-error-code)
   (detail :initarg :detail :reader distribution-error-detail))
  (:report (lambda (condition stream)
             (format stream "Distribution ~A: ~A"
                     (distribution-error-code condition)
                     (distribution-error-detail condition)))))

(defun %fail (code control &rest arguments)
  (error 'distribution-error :code code
                             :detail (apply #'format nil control arguments)))

(defun %normalize-name (value field)
  (unless (and (stringp value) (plusp (length value)))
    (%fail :invalid-definition "~A must be a non-empty string" field))
  (string-downcase value))

(defun %normalize-set (values field &key allow-empty)
  (unless (and (listp values) (every #'stringp values))
    (%fail :invalid-definition "~A must be a list of strings" field))
  (let ((result (sort (remove-duplicates
                       (mapcar (lambda (value)
                                 (%normalize-name value field))
                               values)
                       :test #'string=)
                      #'string<)))
    (when (and (null result) (not allow-empty))
      (%fail :invalid-definition "~A must not be empty" field))
    result))

(defstruct (distribution-definition
             (:constructor %make-distribution-definition))
  (name "" :type string :read-only t)
  (version "" :type string :read-only t)
  (roots nil :type list :read-only t)
  (expected-systems nil :type list :read-only t)
  (forbidden-systems nil :type list :read-only t)
  (forbidden-prefixes nil :type list :read-only t)
  (max-systems 0 :type (integer 1 *) :read-only t)
  (public-license "" :type string :read-only t)
  (target-language "common-lisp" :type string :read-only t)
  (language-profile "asdf-source-v1" :type string :read-only t)
  (required-floor "sbcl" :type string :read-only t)
  (candidate-floors nil :type list :read-only t)
  (source-prefix "" :type string :read-only t)
  (public-prefix "" :type string :read-only t)
  (source-environment-prefix "" :type string :read-only t)
  (public-environment-prefix "" :type string :read-only t)
  (forbidden-tokens nil :type list :read-only t)
  (cut-policy-id "unbound" :type string :read-only t)
  (export-policy-id "unbound" :type string :read-only t)
  (license-grants-id "unbound" :type string :read-only t))

(defun make-distribution-definition
    (&key name version roots expected-systems forbidden-systems
          forbidden-prefixes max-systems public-license
          (target-language "common-lisp")
          (language-profile "asdf-source-v1") (required-floor "sbcl")
          candidate-floors
          (source-prefix "") (public-prefix "")
          (source-environment-prefix "") (public-environment-prefix "")
          forbidden-tokens (cut-policy-id "unbound")
          (export-policy-id "unbound") (license-grants-id "unbound"))
  "Construct the immutable policy IR consumed by the distribution compiler."
  (let ((name (%normalize-name name "name"))
        (version (%normalize-name version "version"))
        (roots (%normalize-set roots "roots"))
        (expected (%normalize-set expected-systems "expected systems"))
        (forbidden (%normalize-set forbidden-systems "forbidden systems"
                                    :allow-empty t))
        (prefixes (%normalize-set forbidden-prefixes "forbidden prefixes"
                                  :allow-empty t))
        (tokens (%normalize-set forbidden-tokens "forbidden tokens"
                                :allow-empty t))
        (candidate-floors (%normalize-set candidate-floors "candidate floors"
                                          :allow-empty t)))
    (unless (and (integerp max-systems) (plusp max-systems))
      (%fail :invalid-definition "max-systems must be a positive integer"))
    (unless (and (stringp public-license) (plusp (length public-license)))
      (%fail :invalid-definition "public-license must be non-empty"))
    (when (and (plusp (length source-prefix))
               (zerop (length public-prefix)))
      (%fail :invalid-definition
             "a source namespace prefix requires a public prefix"))
    (%make-distribution-definition
     :name name :version version :roots roots :expected-systems expected
     :forbidden-systems forbidden :forbidden-prefixes prefixes
     :max-systems max-systems :public-license (copy-seq public-license)
     :target-language (%normalize-name target-language "target language")
     :language-profile (%normalize-name language-profile "language profile")
     :required-floor (%normalize-name required-floor "required floor")
     :candidate-floors candidate-floors
     :source-prefix (copy-seq source-prefix)
     :public-prefix (copy-seq public-prefix)
     :source-environment-prefix (copy-seq source-environment-prefix)
     :public-environment-prefix (copy-seq public-environment-prefix)
     :forbidden-tokens tokens
     :cut-policy-id (copy-seq cut-policy-id)
     :export-policy-id (copy-seq export-policy-id)
     :license-grants-id (copy-seq license-grants-id))))

(defun %split-set (value)
  (if (and (stringp value) (plusp (length value)))
      (remove "" (uiop:split-string value :separator '(#\|)) :test #'string=)
      nil))

(defun %file-id (pathname)
  (format nil "sha256:~A"
          (sha256-hex (string->bytes (uiop:read-file-string pathname)))))

(defun %repository-root-for-policy (cut-policy-path)
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-parent-directory-pathname
    (uiop:pathname-directory-pathname cut-policy-path))))

(defun read-distribution-definition (cut-policy-path export-policy-path)
  "Read the established semantic-cut and source-export policies into one IR."
  (let* ((cut (read-toml cut-policy-path))
         (export (read-toml export-policy-path))
         (grant-relative (toml-get cut "release.relicense_manifest"))
         (grant-path
           (merge-pathnames grant-relative
                            (%repository-root-for-policy cut-policy-path))))
    (unless (probe-file grant-path)
      (%fail :missing-license-grants "grant manifest is unavailable: ~A"
             grant-relative))
    (make-distribution-definition
     :name (toml-get cut "release.name")
     :version (princ-to-string (toml-get cut "release.version"))
     :roots (%split-set (toml-get cut "roots.systems"))
     :expected-systems (%split-set
                        (toml-get cut "boundary.expected_systems"))
     :forbidden-systems (%split-set
                         (toml-get cut "boundary.forbidden_systems" ""))
     :forbidden-prefixes (%split-set
                          (toml-get cut "boundary.forbidden_prefixes" ""))
     :max-systems (toml-get cut "release.max_systems")
     :public-license (toml-get cut "release.public_license")
     :target-language (toml-get export "language.target" "common-lisp")
     :language-profile (toml-get export "language.profile" "asdf-source-v1")
     :required-floor (toml-get export "language.required_floor" "sbcl")
     :candidate-floors
     (%split-set (toml-get export "language.candidate_floors" ""))
     :source-prefix (toml-get export "namespace.garden_prefix" "")
     :public-prefix (toml-get export "namespace.public_prefix" "")
     :source-environment-prefix
     (toml-get export "namespace.garden_environment_prefix" "")
     :public-environment-prefix
     (toml-get export "namespace.public_environment_prefix" "")
     :forbidden-tokens
     (%split-set (toml-get export "boundary.forbidden_tokens" ""))
     :cut-policy-id (%file-id cut-policy-path)
     :export-policy-id (%file-id export-policy-path)
     :license-grants-id (%file-id grant-path))))

(defstruct (distribution-plan (:constructor %make-distribution-plan))
  (definition nil :type distribution-definition :read-only t)
  (systems nil :type list :read-only t)
  (root-closures nil :type list :read-only t)
  (id "" :type string :read-only t))

(defun %prefixp (prefix value)
  (and (<= (length prefix) (length value))
       (string= prefix value :end2 (length prefix))))

(defun %forbidden-name-p (definition name)
  (or (find name (distribution-definition-forbidden-systems definition)
            :test #'string=)
      (some (lambda (prefix) (%prefixp prefix name))
            (distribution-definition-forbidden-prefixes definition))))

(defun %stable-closure (root deps-fn)
  (library-closure root :deps-fn
                   (lambda (name)
                     (sort (copy-list (funcall deps-fn name)) #'string<))))

(defun %plan-form (definition systems root-closures)
  (list :distribution (distribution-definition-name definition)
        :version (distribution-definition-version definition)
        :public-license (distribution-definition-public-license definition)
        :language (list (distribution-definition-target-language definition)
                        (distribution-definition-language-profile definition)
                        (distribution-definition-required-floor definition)
                        (distribution-definition-candidate-floors definition))
        :namespace
        (list (distribution-definition-source-prefix definition)
              (distribution-definition-public-prefix definition)
              (distribution-definition-source-environment-prefix definition)
              (distribution-definition-public-environment-prefix definition))
        :boundary
        (list :expected (distribution-definition-expected-systems definition)
              :forbidden
              (distribution-definition-forbidden-systems definition)
              :forbidden-prefixes
              (distribution-definition-forbidden-prefixes definition)
              :forbidden-tokens
              (distribution-definition-forbidden-tokens definition)
              :max-systems
              (distribution-definition-max-systems definition))
        :policy-identities
        (list :cut (distribution-definition-cut-policy-id definition)
              :export (distribution-definition-export-policy-id definition)
              :license-grants
              (distribution-definition-license-grants-id definition))
        :systems systems :roots root-closures))

(defun %form-text (form)
  (with-output-to-string (stream)
    (let ((*print-case* :downcase) (*print-pretty* nil) (*print-readably* t))
      (write form :stream stream))))

(defun compile-distribution-plan
    (definition &key (deps-fn #'rosette-ship:asdf-system-deps)
                     (resolvable-fn
                       (lambda (name) (asdf:find-system name nil))))
  "Compile an exact closure plan or reject membrane drift before emission."
  (check-type definition distribution-definition)
  (let ((seen (make-hash-table :test #'equal))
        (systems nil) (root-closures nil))
    (dolist (root (distribution-definition-roots definition))
      (let ((closure (%stable-closure root deps-fn)))
        (push (cons root closure) root-closures)
        (dolist (system closure)
          (unless (funcall resolvable-fn system)
            (%fail :missing-system "~A, reachable from ~A, is unavailable"
                   system root))
          (unless (gethash system seen)
            (setf (gethash system seen) t)
            (push system systems)))))
    (setf systems (sort systems #'string<)
          root-closures (sort root-closures #'string< :key #'car))
    (dolist (system systems)
      (when (%forbidden-name-p definition system)
        (%fail :forbidden-system "~A crossed the distribution membrane"
               system)))
    (when (> (length systems)
             (distribution-definition-max-systems definition))
      (%fail :system-ceiling "closure contains ~D systems; ceiling is ~D"
             (length systems)
             (distribution-definition-max-systems definition)))
    (let ((expected (distribution-definition-expected-systems definition)))
      (unless (equal systems expected)
        (%fail :closure-drift "added=~S removed=~S"
               (set-difference systems expected :test #'string=)
               (set-difference expected systems :test #'string=))))
    (let* ((form (%plan-form definition systems root-closures))
           (id (format nil "sha256:~A"
                       (sha256-hex (string->bytes (%form-text form))))))
      (%make-distribution-plan :definition definition :systems systems
                               :root-closures root-closures :id id))))

(defun distribution-plan->form (plan)
  (check-type plan distribution-plan)
  (copy-tree (%plan-form (distribution-plan-definition plan)
                         (distribution-plan-systems plan)
                         (distribution-plan-root-closures plan))))

(defun %replace-all (text from to)
  (if (zerop (length from)) text
      (with-output-to-string (out)
        (loop with start = 0
              for position = (search from text :start2 start :test #'char=)
              do (if position
                     (progn (write-string text out :start start :end position)
                            (write-string to out)
                            (setf start (+ position (length from))))
                     (progn (write-string text out :start start) (return)))))))

(defun lower-distribution-name (definition name)
  (check-type definition distribution-definition)
  (%replace-all name (distribution-definition-source-prefix definition)
                (distribution-definition-public-prefix definition)))

(defun lower-distribution-text (definition text)
  "Apply explicit namespace maps; hygiene and licensing remain separate passes."
  (check-type definition distribution-definition)
  (%replace-all
   (%replace-all text (distribution-definition-source-prefix definition)
                 (distribution-definition-public-prefix definition))
   (distribution-definition-source-environment-prefix definition)
   (distribution-definition-public-environment-prefix definition)))
