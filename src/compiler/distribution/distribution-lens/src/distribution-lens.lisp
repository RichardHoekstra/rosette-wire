;;;; distribution-lens.lisp --- Checked bidirectional compiled repositories.

(in-package #:rosette-distribution-lens)

(defconstant +maximum-changes+ 64)
(defconstant +maximum-file-bytes+ (* 1024 1024))
(defconstant +maximum-bundle-bytes+ (* 8 1024 1024))

(define-condition distribution-lens-error (error)
  ((code :initarg :code :reader distribution-lens-error-code)
   (detail :initarg :detail :reader distribution-lens-error-detail))
  (:report (lambda (condition stream)
             (format stream "Distribution lens ~A: ~A"
                     (distribution-lens-error-code condition)
                     (distribution-lens-error-detail condition)))))

(defun %fail (code control &rest arguments)
  (error 'distribution-lens-error :code code
         :detail (apply #'format nil control arguments)))

(defun %stable-string (value)
  ;; Normalize adjustable/fill-pointer strings so portable readable forms use
  ;; ordinary string syntax instead of implementation-specific array syntax.
  (let ((result (make-string (length value))))
    (replace result value)
    result))

(defun text-id (text)
  "Return the algorithm-qualified SHA-256 identity of UTF-8 TEXT."
  (unless (stringp text) (%fail :invalid-text "expected a string"))
  (%stable-string
   (format nil "sha256:~A"
           (crypto:sha256-hex
            (sb-ext:string-to-octets text :external-format :utf-8)))))

(defun %sha-id-p (value)
  (and (stringp value) (= (length value) 71)
       (string= "sha256:" value :end2 7)
       (every (lambda (character)
                (or (digit-char-p character)
                    (find character "abcdef" :test #'char=)))
              (subseq value 7))))

(defun %hex-encode (text)
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets text
                                                   :external-format :utf-8)
          do (format out "~2,'0x" byte))))

(defun %hex-decode (text)
  (unless (and (stringp text) (evenp (length text))
               (every (lambda (character) (digit-char-p character 16)) text))
    (%fail :invalid-map "malformed hexadecimal rule source"))
  (let ((bytes (make-array (/ (length text) 2)
                           :element-type '(unsigned-byte 8))))
    (loop for index below (length bytes)
          for offset = (* index 2)
          do (setf (aref bytes index)
                   (parse-integer text :start offset :end (+ offset 2)
                                        :radix 16)))
    (sb-ext:octets-to-string bytes :external-format :utf-8)))

(defun %safe-relative-path-p (path)
  (and (stringp path) (plusp (length path))
       (not (member (char path 0) '(#\/ #\\) :test #'char=))
       (not (find #\\ path :test #'char=))
       (every (lambda (part)
                (and (plusp (length part))
                     (not (member part '("." ".." ".git")
                                  :test #'string=))))
              (uiop:split-string path :separator '(#\/)))))

(defun %check-path (path field)
  (unless (%safe-relative-path-p path)
    (%fail :unsafe-path "~A is unsafe: ~S" field path))
  (%stable-string path))

(defstruct (lens-rule (:constructor %make-lens-rule))
  (kind :exact :type keyword :read-only t)
  (source "" :type string :read-only t)
  (public "" :type string :read-only t))

(defun make-lens-rule (kind source public)
  (case kind
    ((:exact :token))
    (otherwise (%fail :invalid-rule "unsupported rule kind ~S" kind)))
  (unless (and (stringp source) (plusp (length source))
               (stringp public) (plusp (length public))
               (not (string= source public)))
    (%fail :invalid-rule "rule sides must be distinct non-empty strings"))
  (%make-lens-rule :kind kind :source (%stable-string source)
                   :public (%stable-string public)))

(defun %replace-all (text from to)
  (with-output-to-string (out)
    (loop with start = 0
          for position = (search from text :start2 start :test #'char=)
          do (if position
                 (progn (write-string text out :start start :end position)
                        (write-string to out)
                        (setf start (+ position (length from))))
                 (progn (write-string text out :start start) (return))))))

(defun %token-boundary-p (text index)
  (or (< index 0) (>= index (length text))
      (not (alphanumericp (char text index)))))

(defun %replace-token (text token replacement)
  (let ((token-length (length token)) (text-length (length text)))
    (with-output-to-string (out)
      (loop with index = 0 while (< index text-length)
            do (if (and (<= (+ index token-length) text-length)
                        (%token-boundary-p text (1- index))
                        (%token-boundary-p text (+ index token-length))
                        (string= token text :start2 index
                                :end2 (+ index token-length)))
                   (progn (write-string replacement out)
                          (incf index token-length))
                   (progn (write-char (char text index) out)
                          (incf index)))))))

(defun %apply-rule (text rule direction)
  (let ((from (if (eq direction :forward)
                  (lens-rule-source rule) (lens-rule-public rule)))
        (to (if (eq direction :forward)
                (lens-rule-public rule) (lens-rule-source rule))))
    (ecase (lens-rule-kind rule)
      (:exact (%replace-all text from to))
      (:token (%replace-token text from to)))))

(defun forward-text (rules text)
  "Apply ordered RULES from authoritative to public text."
  (reduce (lambda (value rule) (%apply-rule value rule :forward))
          rules :initial-value text))

(defun reverse-text (rules text)
  "Apply the declared partial inverse in reverse rule order."
  (reduce (lambda (value rule) (%apply-rule value rule :reverse))
          (reverse rules) :initial-value text))

(defstruct (export-entry (:constructor %make-export-entry))
  (path "" :type string :read-only t)
  (class :generated :type keyword :read-only t)
  source-path source-id
  (public-id "" :type string :read-only t)
  (editable-p nil :type boolean :read-only t)
  (lift-mode :none :type keyword :read-only t))

(defun make-export-entry (&key path (class :generated) source-path source-id
                               public-id editable-p (lift-mode :none))
  (let ((path (%check-path path "public path")))
    (when source-path (%check-path source-path "source path"))
    (unless (%sha-id-p public-id)
      (%fail :invalid-entry "invalid public identity for ~A" path))
    (when (or source-path source-id editable-p)
      (unless (and source-path (%sha-id-p source-id))
        (%fail :invalid-entry "source provenance is incomplete for ~A" path)))
    (case lift-mode
      ((:none :namespace :literal))
      (otherwise (%fail :invalid-entry "invalid lift mode for ~A" path)))
    (when (and editable-p (eq lift-mode :none))
      (%fail :invalid-entry "editable entry ~A has no lift mode" path))
    (%make-export-entry :path path :class class
                        :source-path (and source-path (%stable-string source-path))
                        :source-id (and source-id (%stable-string source-id))
                        :public-id (%stable-string public-id)
                        :editable-p (not (null editable-p))
                        :lift-mode lift-mode)))

(defun %rule-form (rule)
  ;; Encode the authoritative spelling so the map itself crosses the public
  ;; repository's namespace-hygiene boundary without leaking private tokens.
  (list :kind (lens-rule-kind rule)
        :source-hex (%hex-encode (lens-rule-source rule))
        :public (lens-rule-public rule)))

(defun %entry-form (entry)
  (list :path (export-entry-path entry) :class (export-entry-class entry)
        :source-path (export-entry-source-path entry)
        :source-id (export-entry-source-id entry)
        :public-id (export-entry-public-id entry)
        :editable (export-entry-editable-p entry)
        :lift-mode (export-entry-lift-mode entry)))

(defstruct (export-map (:constructor %make-export-map))
  (profile "" :type string :read-only t)
  (source-cut-id "" :type string :read-only t)
  (distribution-id "" :type string :read-only t)
  (rules nil :type list :read-only t)
  (entries nil :type list :read-only t)
  (id "" :type string :read-only t))

(defun %export-map-payload (profile source-cut-id distribution-id rules entries)
  (list :schema :distribution-export-map/v1 :profile profile
        :source-cut-id source-cut-id :distribution-id distribution-id
        :rules (mapcar #'%rule-form rules)
        :entries (mapcar #'%entry-form entries)))

(defun %form-id (form)
  (text-id (with-output-to-string (out)
             (let ((*print-pretty* nil) (*print-readably* t)
                   (*print-case* :downcase))
               (write form :stream out)))))

(defun make-export-map (&key profile source-cut-id distribution-id rules entries)
  (unless (and (stringp profile) (plusp (length profile)))
    (%fail :invalid-map "profile must be non-empty"))
  (unless (and (stringp source-cut-id) (plusp (length source-cut-id))
               (stringp distribution-id) (plusp (length distribution-id)))
    (%fail :invalid-map "source and distribution identities are required"))
  (unless (every #'lens-rule-p rules) (%fail :invalid-map "invalid rules"))
  (unless (every #'export-entry-p entries) (%fail :invalid-map "invalid entries"))
  (let ((paths (mapcar #'export-entry-path entries)))
    (unless (= (length paths) (length (remove-duplicates paths :test #'string=)))
      (%fail :invalid-map "duplicate public paths")))
  (let* ((profile (%stable-string profile))
         (source-cut-id (%stable-string source-cut-id))
         (distribution-id (%stable-string distribution-id))
         (ordered (sort (copy-list entries) #'string< :key #'export-entry-path))
         (payload (%export-map-payload profile source-cut-id distribution-id
                                       rules ordered)))
    (%make-export-map :profile profile
                      :source-cut-id source-cut-id
                      :distribution-id distribution-id
                      :rules (copy-list rules) :entries ordered
                      :id (%form-id payload))))

(defun export-map->form (map)
  (append (%export-map-payload
           (export-map-profile map) (export-map-source-cut-id map)
           (export-map-distribution-id map) (export-map-rules map)
           (export-map-entries map))
          (list :map-id (export-map-id map))))

(defun %write-form (path form)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (let ((*print-pretty* t) (*print-readably* t) (*print-case* :downcase))
      (write form :stream out) (terpri out))))

(defun write-export-map (map path)
  (%write-form path (export-map->form map)) map)

(defun %preflight-data-form (text &optional allowed-keywords)
  "Bound inert printed data before the host reader can allocate or intern.
No dispatch macros, dotted lists, package references, escaped symbols, numbers,
comments, or quote forms are part of this wire format."
  (let ((index 0) (depth 0) (nodes 0) (roots 0) (size (length text)))
    (labels ((spacep (c) (find c '(#\Space #\Tab #\Newline #\Return #\Page)))
             (node ()
               (when (> (incf nodes) 200000)
                 (%fail :input-too-large "data node budget exceeded"))
               (when (zerop depth) (incf roots)))
             (token-char-p (c)
               (or (alphanumericp c) (find c "-_/+*"))))
      (loop while (< index size) for c = (char text index) do
        (cond
          ((spacep c) (incf index))
          ((char= c #\()
           (node)
           (when (> (incf depth) 64)
             (%fail :input-too-large "data nesting budget exceeded"))
           (incf index))
          ((char= c #\))
           (when (minusp (decf depth)) (%fail :invalid-form "unmatched close"))
           (incf index))
          ((char= c #\")
           (node) (incf index)
           (loop
             (when (>= index size) (%fail :invalid-form "unterminated string"))
             (let ((current (char text index)))
               (incf index)
               (cond ((char= current #\") (return))
                     ((char= current #\\)
                      (when (>= index size) (%fail :invalid-form "incomplete escape"))
                      (incf index))))))
          (t
           (node)
           (let ((start index))
             (when (char= c #\:) (incf index))
             (loop while (and (< index size) (token-char-p (char text index)))
                   do (incf index)
                      (when (> (- index start) 128)
                        (%fail :input-too-large "data atom budget exceeded")))
             (unless (and (> index start)
                          (or (and (char= c #\:) (> (- index start) 1))
                              (member (subseq text start index) '("nil" "t")
                                      :test #'string-equal))
                          (or (= index size)
                              (spacep (char text index))
                              (find (char text index) "()\"")))
               (%fail :invalid-form "unsupported data reader syntax"))
             (when (and allowed-keywords (char= c #\:)
                        (not (find (subseq text (1+ start) index) allowed-keywords
                                   :key #'symbol-name :test #'string-equal)))
               (%fail :invalid-form "unknown bundle keyword"))))))
      (unless (and (zerop depth) (= roots 1))
        (%fail :invalid-form "expected one balanced data form"))))
  text)

(defun %read-one-form (path &optional allowed-keywords)
  ;; Size and read use the same descriptor. Refuse concurrent growth rather
  ;; than reopening a pathname after checking a different file's size.
  (let* ((bytes
           (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
             (let ((size (file-length in)))
               (unless (and size (<= size +maximum-bundle-bytes+))
                 (%fail :input-too-large "input exceeds byte budget"))
               (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
                 (unless (and (= size (read-sequence bytes in))
                              (eq :eof (read-byte in nil :eof)))
                   (%fail :invalid-form "input changed during read"))
                 bytes))))
         (text (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (%preflight-data-form text allowed-keywords)
    (let ((*read-eval* nil) (*read-suppress* nil) (*read-base* 10)
          (*readtable* (copy-readtable nil)) (*package* (find-package :cl-user)))
      (with-input-from-string (in text)
        (let ((form (read in nil :eof)) (tail (read in nil :eof)))
          (when (or (eq form :eof) (not (eq tail :eof)))
            (%fail :invalid-form "expected exactly one readable form"))
          form)))))

(defun %parse-rule (form)
  (unless (and (listp form)
               (case (getf form :kind)
                 ((:exact :token) t)
                 (otherwise nil))
               (stringp (getf form :source-hex))
               (stringp (getf form :public)))
    (%fail :invalid-map "malformed rule"))
  (make-lens-rule (getf form :kind)
                  (%hex-decode (getf form :source-hex))
                  (getf form :public)))

(defun %parse-entry (form)
  (unless (listp form) (%fail :invalid-map "malformed entry"))
  (make-export-entry :path (getf form :path) :class (getf form :class)
                     :source-path (getf form :source-path)
                     :source-id (getf form :source-id)
                     :public-id (getf form :public-id)
                     :editable-p (getf form :editable)
                     :lift-mode (getf form :lift-mode :none)))

(defun read-export-map (path)
  (let ((form (%read-one-form path)))
    (unless (eq (getf form :schema) :distribution-export-map/v1)
      (%fail :invalid-map "unsupported schema"))
    (let ((map (make-export-map
                :profile (getf form :profile)
                :source-cut-id (getf form :source-cut-id)
                :distribution-id (getf form :distribution-id)
                :rules (mapcar #'%parse-rule (getf form :rules))
                :entries (mapcar #'%parse-entry (getf form :entries)))))
      (unless (string= (export-map-id map) (getf form :map-id ""))
        (%fail :map-id-mismatch "export map identity mismatch"))
      map)))

(defstruct (export-change (:constructor %make-export-change))
  (path "" :type string :read-only t)
  (base-id "" :type string :read-only t)
  (content "" :type string :read-only t)
  (new-id "" :type string :read-only t))

(defun %make-change (path base-id content)
  (unless (%sha-id-p base-id)
    (%fail :invalid-bundle "invalid base identity for ~A" path))
  (when (> (length (sb-ext:string-to-octets content
                                            :external-format :utf-8))
           +maximum-file-bytes+)
    (%fail :file-too-large "changed file exceeds limit: ~A" path))
  (%make-export-change :path (%check-path path "change path")
                       :base-id (%stable-string base-id)
                       :content (%stable-string content) :new-id (text-id content)))

(defun %change-form (change)
  (list :path (export-change-path change)
        :base-id (export-change-base-id change)
        :new-id (export-change-new-id change)
        :content (export-change-content change)))

(defstruct (change-bundle (:constructor %make-change-bundle))
  (profile "" :type string :read-only t)
  (base-export-id "" :type string :read-only t)
  (map-id "" :type string :read-only t)
  (changes nil :type list :read-only t)
  (id "" :type string :read-only t))

(defun %bundle-payload (profile base-export-id map-id changes)
  (list :schema :distribution-change-bundle/v1 :profile profile
        :base-export-id base-export-id :map-id map-id
        :changes (mapcar #'%change-form changes)))

(defun make-change-bundle (&key profile base-export-id map-id changes)
  (unless (and (stringp profile) (plusp (length profile))
               (%sha-id-p base-export-id) (%sha-id-p map-id))
    (%fail :invalid-bundle "profile and durable identities are required"))
  (unless (and (listp changes) (<= (length changes) +maximum-changes+)
               (every #'export-change-p changes))
    (%fail :invalid-bundle "invalid or over-budget change set"))
  (let* ((profile (%stable-string profile))
         (base-export-id (%stable-string base-export-id))
         (map-id (%stable-string map-id))
         (ordered (sort (copy-list changes) #'string< :key #'export-change-path))
         (paths (mapcar #'export-change-path ordered)))
    (unless (= (length paths) (length (remove-duplicates paths :test #'string=)))
      (%fail :invalid-bundle "duplicate changed paths"))
    (let ((payload (%bundle-payload profile base-export-id map-id ordered)))
      (%make-change-bundle :profile profile
                           :base-export-id base-export-id
                           :map-id map-id :changes ordered
                           :id (%form-id payload)))))

(defun change-bundle->form (bundle)
  (append (%bundle-payload
           (change-bundle-profile bundle) (change-bundle-base-export-id bundle)
           (change-bundle-map-id bundle) (change-bundle-changes bundle))
          (list :bundle-id (change-bundle-id bundle))))

(defun write-change-bundle (bundle path)
  (%write-form path (change-bundle->form bundle)) bundle)

(defun %parse-change (form)
  (let ((change (%make-change (getf form :path) (getf form :base-id)
                              (getf form :content))))
    (unless (string= (export-change-new-id change) (getf form :new-id ""))
      (%fail :invalid-bundle "change identity mismatch"))
    change))

(defun read-change-bundle (path)
  (let ((form (%read-one-form
               path '(:schema :distribution-change-bundle/v1 :profile
                      :base-export-id :map-id :changes :path :base-id
                      :new-id :content :bundle-id))))
    (unless (eq (getf form :schema) :distribution-change-bundle/v1)
      (%fail :invalid-bundle "unsupported schema"))
    (let ((bundle (make-change-bundle
                   :profile (getf form :profile)
                   :base-export-id (getf form :base-export-id)
                   :map-id (getf form :map-id)
                   :changes (mapcar #'%parse-change (getf form :changes)))))
      (unless (string= (change-bundle-id bundle) (getf form :bundle-id ""))
        (%fail :bundle-id-mismatch "change bundle identity mismatch"))
      bundle)))

(defstruct (lens-obstruction (:constructor %make-obstruction))
  (code :unknown :type keyword :read-only t) path detail)

(defun %obstruction (code &optional path detail)
  (%make-obstruction :code code :path path :detail detail))

(defstruct (scan-result (:constructor %make-scan-result))
  (admitted-p nil :type boolean :read-only t)
  (obstructions nil :type list :read-only t)
  bundle)

(defun %all-relative-files (root &optional (directory root))
  (append
   (mapcar (lambda (path) (enough-namestring path root))
           (uiop:directory-files directory))
   (loop for child in (uiop:subdirectories directory)
         for leaf = (car (last (pathname-directory child)))
         unless (or (string= leaf ".git") (string= leaf "dist"))
           append (%all-relative-files root child))))

(defun %read-text (path)
  (handler-case (uiop:read-file-string path) (error () nil)))

(defun scan-export-tree (root map base-export-id)
  "Build a bounded change bundle or return typed tree obstructions."
  (let* ((root (uiop:ensure-directory-pathname root))
         (entries (export-map-entries map))
         (known (mapcar #'export-entry-path entries))
         (ignored '("release/EXPORT-ID" "release/EXPORT-MAP.sexp"))
         (obstructions nil) (changes nil))
    (unless (%sha-id-p base-export-id)
      (push (%obstruction :invalid-base-export-id) obstructions))
    (dolist (entry entries)
      (let* ((relative (export-entry-path entry))
             (path (merge-pathnames relative root)))
        (cond
          ((not (probe-file path))
           (push (%obstruction :deleted-file relative) obstructions))
          (t
           (let ((text (%read-text path)))
             (cond
               ((null text)
                (when (export-entry-editable-p entry)
                  (push (%obstruction :non-text-edit relative) obstructions)))
               ((not (string= (text-id text) (export-entry-public-id entry)))
                (if (export-entry-editable-p entry)
                    (handler-case
                        (push (%make-change relative
                                            (export-entry-public-id entry) text)
                              changes)
                      (distribution-lens-error (condition)
                        (push (%obstruction
                               (distribution-lens-error-code condition)
                               relative (distribution-lens-error-detail condition))
                              obstructions)))
                    (push (%obstruction :generated-file-edited relative)
                          obstructions)))))))))
    (dolist (relative (%all-relative-files root))
      (unless (or (member relative known :test #'string=)
                  (member relative ignored :test #'string=))
        (push (%obstruction :added-file relative) obstructions)))
    (when (> (length changes) +maximum-changes+)
      (push (%obstruction :change-budget-exceeded nil (length changes))
            obstructions))
    (if obstructions
        (%make-scan-result :obstructions (nreverse obstructions))
        (%make-scan-result
         :admitted-p t
         :bundle (make-change-bundle
                  :profile (export-map-profile map)
                  :base-export-id base-export-id :map-id (export-map-id map)
                  :changes (nreverse changes))))))

(defun %line-segments (text)
  (let ((segments nil) (start 0) (size (length text)))
    (loop for index from 0 below size
          when (char= (char text index) #\Newline)
            do (push (subseq text start (1+ index)) segments)
               (setf start (1+ index)))
    (when (< start size) (push (subseq text start) segments))
    (coerce (nreverse segments) 'vector)))

(defun %lcs-table (left right)
  (let* ((n (length left)) (m (length right))
         (table (make-array (list (1+ n) (1+ m))
                            :element-type 'fixnum :initial-element 0)))
    (loop for i downfrom (1- n) to 0 do
      (loop for j downfrom (1- m) to 0 do
        (setf (aref table i j)
              (if (string= (aref left i) (aref right j))
                  (1+ (aref table (1+ i) (1+ j)))
                  (max (aref table (1+ i) j)
                       (aref table i (1+ j)))))))
    table))

(defun %raise-inserted-segment (segment rules mode)
  (ecase mode
    (:namespace (reverse-text rules segment))
    (:literal
     (unless (string= segment (forward-text rules segment))
       (%fail :literal-crosses-namespace
              "literal edit contains an authoritative namespace token"))
     segment)))

(defun lift-edited-text (source-base public-edited rules mode)
  "Lift PUBLIC-EDITED through the line-preserving forward view of SOURCE-BASE."
  (case mode
    ((:namespace :literal))
    (otherwise (%fail :unsupported-lift-mode "cannot lift mode ~S" mode)))
  (let* ((source (%line-segments source-base))
         (public-base (%line-segments (forward-text rules source-base)))
         (edited (%line-segments public-edited)))
    (unless (= (length source) (length public-base))
      (%fail :non-line-preserving-transform "forward transform changed line count"))
    (loop for index below (length source)
          unless (string= (forward-text rules (aref source index))
                          (aref public-base index))
            do (%fail :non-local-transform "line ~D is not independently mapped"
                      (1+ index)))
    (let ((table (%lcs-table public-base edited)) (i 0) (j 0))
      (let ((candidate
              (with-output-to-string (out)
                (loop while (or (< i (length public-base))
                                (< j (length edited)))
                      do (cond
                           ((and (< i (length public-base)) (< j (length edited))
                                 (string= (aref public-base i) (aref edited j)))
                            (write-string (aref source i) out) (incf i) (incf j))
                           ((and (< i (length public-base))
                                 (or (= j (length edited))
                                     (>= (aref table (1+ i) j)
                                         (aref table i (1+ j)))))
                            (incf i))
                           (t
                            (write-string
                             (%raise-inserted-segment (aref edited j) rules mode)
                             out)
                            (incf j)))))))
        (unless (string= (forward-text rules candidate) public-edited)
          (%fail :roundtrip-residual
                 "re-export differs from edited public text"))
        candidate))))

(defstruct (lifted-change (:constructor %make-lifted-change))
  (public-path "" :type string :read-only t)
  (source-path "" :type string :read-only t)
  (before-id "" :type string :read-only t)
  (after-id "" :type string :read-only t)
  (content "" :type string :read-only t))

(defun %lifted-change-form (change)
  (list :public-path (lifted-change-public-path change)
        :source-path (lifted-change-source-path change)
        :before-id (lifted-change-before-id change)
        :after-id (lifted-change-after-id change)
        :content (lifted-change-content change)))

(defstruct (lift-result (:constructor %make-lift-result))
  (admitted-p nil :type boolean :read-only t)
  (obstructions nil :type list :read-only t)
  (changes nil :type list :read-only t)
  (id "" :type string :read-only t))

(defun %obstruction-form (obstruction)
  (list :code (lens-obstruction-code obstruction)
        :path (lens-obstruction-path obstruction)
        :detail (lens-obstruction-detail obstruction)))

(defun %lift-payload (admitted-p obstructions changes)
  (list :schema :distribution-lift-receipt/v1 :admitted admitted-p
        :obstructions (mapcar #'%obstruction-form obstructions)
        :changes (mapcar #'%lifted-change-form changes)))

(defun %finish-lift (obstructions changes)
  (let* ((admitted-p (null obstructions))
         (payload (%lift-payload admitted-p obstructions changes)))
    (%make-lift-result :admitted-p admitted-p :obstructions obstructions
                       :changes changes :id (%form-id payload))))

(defun lift-result->form (result)
  (append (%lift-payload (lift-result-admitted-p result)
                         (lift-result-obstructions result)
                         (lift-result-changes result))
          (list :receipt-id (lift-result-id result))))

(defun %entry-by-path (map path)
  (find path (export-map-entries map) :test #'string=
        :key #'export-entry-path))

(defun lift-change-bundle (bundle map source-root expected-export-id)
  "Lift BUNDLE to immutable source replacements or a typed refusal receipt."
  (let ((obstructions nil) (lifted nil)
        (source-root (uiop:ensure-directory-pathname source-root)))
    (unless (string= (change-bundle-profile bundle) (export-map-profile map))
      (push (%obstruction :profile-mismatch) obstructions))
    (unless (string= (change-bundle-map-id bundle) (export-map-id map))
      (push (%obstruction :map-id-mismatch) obstructions))
    (unless (string= (change-bundle-base-export-id bundle) expected-export-id)
      (push (%obstruction :base-export-id-mismatch) obstructions))
    (unless obstructions
      (dolist (change (change-bundle-changes bundle))
        (let* ((path (export-change-path change))
               (entry (%entry-by-path map path)))
          (cond
            ((null entry)
             (push (%obstruction :unmapped-change path) obstructions))
            ((not (export-entry-editable-p entry))
             (push (%obstruction :noneditable-change path) obstructions))
            ((not (string= (export-change-base-id change)
                           (export-entry-public-id entry)))
             (push (%obstruction :public-base-drift path) obstructions))
            (t
             (let* ((source-path (export-entry-source-path entry))
                    (absolute (merge-pathnames source-path source-root))
                    (source (and (uiop:subpathp absolute source-root)
                                 (probe-file absolute) (%read-text absolute))))
               (cond
                 ((null source)
                  (push (%obstruction :source-unavailable source-path)
                        obstructions))
                 ((not (string= (text-id source)
                                (export-entry-source-id entry)))
                  (push (%obstruction :source-base-drift source-path)
                        obstructions))
                 (t
                  (handler-case
                      (let ((candidate
                              (lift-edited-text
                               source (export-change-content change)
                               (export-map-rules map)
                               (export-entry-lift-mode entry))))
                        (push (%make-lifted-change
                               :public-path path :source-path source-path
                               :before-id (export-entry-source-id entry)
                               :after-id (text-id candidate) :content candidate)
                              lifted))
                    (distribution-lens-error (condition)
                      (push (%obstruction
                             (distribution-lens-error-code condition) path
                             (distribution-lens-error-detail condition))
                            obstructions)))))))))))
    (%finish-lift (nreverse obstructions)
                  (if obstructions nil (nreverse lifted)))))

(defun materialize-lift-overlay (result output-root)
  "Write an admitted immutable lift plan beneath a fresh OUTPUT-ROOT."
  (unless (lift-result-admitted-p result)
    (%fail :refused-lift "cannot materialize a refused result"))
  (let ((root (uiop:ensure-directory-pathname output-root)))
    (when (probe-file root)
      (%fail :target-exists "overlay target already exists: ~A" root))
    (dolist (change (lift-result-changes result))
      (let ((path (merge-pathnames (lifted-change-source-path change) root)))
        (unless (uiop:subpathp path root) (%fail :unsafe-path "overlay escape"))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
          (write-string (lifted-change-content change) out))))
    (%write-form (merge-pathnames "LIFT-RECEIPT.sexp" root)
                 (lift-result->form result))
    root))
