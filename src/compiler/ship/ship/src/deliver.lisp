;;;; deliver.lisp --- the production substrate: Deliverables + locks.
;;;;
;;;; A DELIVERABLE is the production dual of a Witness: a witness lowered to a
;;;; floor and BUILT. Its committable source of truth is a `.ship` spec (a
;;;; small plist); the binary itself is a regenerable build product (never
;;;; committed). A `.lock` records the content id of the exact shipped source
;;;; plus the parity verdict and reduction stats -- so a rebuild PROVES
;;;; reproducibility (same slice => same content id) and re-verifies the
;;;; gauge-check. Maturity (:candidate/:staging/:production) is a FIELD in the
;;;; spec, governed by promotion -- never a directory (that would bake a gauge
;;;; into the coordinate). This mirrors the research substrate: structural
;;;; truth in files, navigable views generated over it.
;;;;
;;;; .ship spec (hand-authored, committed):
;;;;   (:deliverable "name" :system "rosette-foo" :floor :jit
;;;;    :entry "ROSETTE-FOO:MAIN" | :expr "(rosette-foo:bar 1)"
;;;;    :optimize t | :ddmin t | :maturity :staging :description "...")
;;;; .lock (generated, committed):
;;;;   (:deliverable "name" :system ... :floor ... :maturity ...
;;;;    :content-id "<64hex>" :parity :pass|:skipped|:fail :reduction (...))

(in-package #:rosette-ship)

(defun read-ship-spec (path)
  "Read a .ship spec (one plist) from PATH."
  (with-open-file (in path :direction :input) (read in)))

(defun %read-lock (path)
  "Read one Deliverable lock plist from PATH."
  (with-open-file (in path :direction :input) (read in)))

(defun %safe-deliverable-name-p (name)
  "True when NAME is a flat Deliverables-namespace component.

Bundle atom references are names, never paths.  Restricting the spelling here
keeps a tracked recipe from escaping the Deliverables root via `..' or a path
separator."
  (and (stringp name)
       (plusp (length name))
       (every (lambda (ch) (or (alphanumericp ch) (char= ch #\-))) name)))

(defun %deliverables-root (deliverable-dir)
  "The flat Deliverables namespace containing DELIVERABLE-DIR."
  (unless deliverable-dir
    (error "deliver: a bundle requires its deliverable directory"))
  (uiop:pathname-parent-directory-pathname
   (uiop:ensure-directory-pathname deliverable-dir)))

(defun %atom-paths (root name)
  "Return NAME's recipe and lock paths under Deliverables ROOT."
  (unless (%safe-deliverable-name-p name)
    (error "deliver: unsafe atom name ~S (atom references are flat names)" name))
  (let ((dir (merge-pathnames (format nil "~A/" name)
                              (uiop:ensure-directory-pathname root))))
    (values (merge-pathnames (format nil "~A.ship" name) dir)
            (merge-pathnames (format nil "~A.lock" name) dir))))

(defun %validate-sealed-atom-parity (name spec receipts)
  "Require NAME's lock to cover every declared external suite, all green."
  (let* ((declared (getf spec :parity-suites))
         (measured (mapcar (lambda (receipt) (getf receipt :suite)) receipts))
         (measured-unique (remove-duplicates measured :test #'string=)))
    (unless (and (null (set-difference declared measured-unique :test #'string=))
                 (null (set-difference measured-unique declared :test #'string=)))
      (error "deliver: atom ~A recipe/lock external-suite mismatch (~S vs ~S)"
             name declared measured-unique))
    (dolist (receipt receipts)
      (unless (and (plusp (or (getf receipt :total) 0))
                   (zerop (or (getf receipt :diverged)
                              (- (getf receipt :total)
                                 (getf receipt :matched)))))
        (error "deliver: atom ~A has unproven or diverged external parity ~S"
               name receipt)))
    t))

(defun %resolve-atom-receipt (root name)
  "Resolve one named atom to a sealed recipe+lock receipt, or fail closed.

The lock is not treated as an anonymous hash: its name, kind, system, floor,
durable content id, and internal-parity verdict must agree with the recipe."
  (multiple-value-bind (ship-path lock-path) (%atom-paths root name)
    (unless (probe-file ship-path)
      (error "deliver: bundle atom ~A has no recipe at ~A" name ship-path))
    (let ((spec (read-ship-spec ship-path)))
      (unless (equal name (getf spec :deliverable))
        (error "deliver: atom reference ~A resolves to recipe for ~S"
               name (getf spec :deliverable)))
      (when (getf spec :stub)
        (error "deliver: bundle atom ~A is still a stub" name))
      (unless (eq (or (getf spec :kind) :atom) :atom)
        (error "deliver: bundle member ~A is not an atom" name))
      (unless (probe-file lock-path)
        (error "deliver: bundle atom ~A is unlocked" name))
      (let* ((lock (%read-lock lock-path))
             (cid (getf lock :content-id))
             (parity (getf lock :parity)))
        (unless (equal name (getf lock :deliverable))
          (error "deliver: atom ~A lock names ~S"
                 name (getf lock :deliverable)))
        ;; Old atom locks predate :kind; NIL has always meant :atom.
        (unless (eq (or (getf lock :kind) :atom) :atom)
          (error "deliver: atom ~A lock has non-atom kind ~S"
                 name (getf lock :kind)))
        (dolist (key '(:system :floor))
          (unless (equal (getf spec key) (getf lock key))
            (error "deliver: atom ~A recipe/lock ~A mismatch (~S vs ~S)"
                   name key (getf spec key) (getf lock key))))
        (unless (rosette-content-identity:content-id-long-p cid)
          (error "deliver: atom ~A lock lacks a durable 64-hex content id" name))
        (unless (member parity '(:pass :skipped) :test #'eq)
          (error "deliver: atom ~A has unacceptable internal parity ~S"
                 name parity))
        (%validate-sealed-atom-parity name spec (getf lock :parity-external))
        (list :deliverable name
              :content-id cid
              :parity parity
              :parity-external (copy-tree (getf lock :parity-external)))))))

(defun %resolve-atom-receipts (deliverable-dir atoms)
  "Resolve ordered ATOMS under DELIVERABLE-DIR's parent namespace."
  (unless (and (listp atoms) atoms)
    (error "deliver: a bundle requires a non-empty :atoms list"))
  (let ((root (%deliverables-root deliverable-dir))
        (seen (make-hash-table :test #'equal)))
    (loop for name in atoms
          do (when (gethash name seen)
               (error "deliver: duplicate atom ~S in bundle" name))
             (setf (gethash name seen) t)
          collect (%resolve-atom-receipt root name))))

(defun %parity-component (source receipt)
  "Normalise one external-parity RECEIPT and attribute it to SOURCE."
  (let* ((suite (getf receipt :suite))
         (matched (getf receipt :matched))
         (total (getf receipt :total))
         (diverged (or (getf receipt :diverged)
                       (and (integerp matched) (integerp total)
                            (- total matched))))
         (divergences (copy-tree (getf receipt :divergences))))
    (unless (and (stringp suite)
                 (integerp matched) (not (minusp matched))
                 (integerp total) (not (minusp total))
                 (<= matched total)
                 (integerp diverged) (not (minusp diverged))
                 (= diverged (- total matched)))
      (error "deliver: malformed external-parity receipt from ~S: ~S"
             source receipt))
    (list :source source :suite suite
          :reference (getf receipt :reference)
          :provenance (getf receipt :provenance)
          :matched matched :total total :diverged diverged
          :divergences divergences)))

(defun %bundle-parity-components (atom-receipts local-suites)
  "Flatten inherited atom receipts and bundle-local suite verdicts."
  (append
   (loop for atom in atom-receipts
         append (loop for receipt in (getf atom :parity-external)
                      collect (%parity-component (getf atom :deliverable)
                                                 receipt)))
   (loop for receipt in local-suites
         collect (%parity-component :bundle receipt))))

(defun %source-divergence (source divergence)
  "Attach SOURCE to a located divergence without mutating its receipt."
  (if (and (listp divergence) (getf divergence :source))
      (copy-tree divergence)
      (append (list :source source) (copy-tree divergence))))

(defun %merge-external-parity-receipts (atom-receipts local-suites)
  "Union parity receipts into one deterministic, source-attributed view.

There is one output row per suite name.  Distinct atom/local components add
their case counts; byte-identical duplicate components from the same source
are counted once.  :COMPONENTS preserves the audit trail behind each sum."
  (let* ((components
           (remove-duplicates
            (%bundle-parity-components atom-receipts local-suites)
            :test #'equal :from-end t))
         (table (make-hash-table :test #'equal))
         (order '()))
    (dolist (component components)
      (let* ((suite (getf component :suite))
             (group (gethash suite table)))
        (unless group
          (setf group (list :suite suite :matched 0 :total 0 :diverged 0
                            :divergences nil :sources nil :references nil
                            :provenances nil :components nil)
                (gethash suite table) group)
          (push suite order))
        (incf (getf group :matched) (getf component :matched))
        (incf (getf group :total) (getf component :total))
        (incf (getf group :diverged) (getf component :diverged))
        (let ((source (getf component :source))
              (reference (getf component :reference))
              (provenance (getf component :provenance)))
          (unless (member source (getf group :sources) :test #'equal)
            (setf (getf group :sources)
                  (append (getf group :sources) (list source))))
          (when (and reference
                     (not (member reference (getf group :references)
                                  :test #'equal)))
            (setf (getf group :references)
                  (append (getf group :references) (list reference))))
          (when (and provenance
                     (not (member provenance (getf group :provenances)
                                  :test #'equal)))
            (setf (getf group :provenances)
                  (append (getf group :provenances) (list provenance))))
          (setf (getf group :divergences)
                (append (getf group :divergences)
                        (mapcar (lambda (d) (%source-divergence source d))
                                (getf component :divergences))))
          (setf (getf group :components)
                (append (getf group :components) (list component))))))
    (mapcar (lambda (suite) (gethash suite table)) (nreverse order))))

(defun %validate-local-parity-suites (spec suites)
  "Require a Deliverable's declared local suites to equal measured suites."
  (let ((declared (getf spec :parity-suites))
        (measured (mapcar (lambda (v) (getf v :suite)) suites)))
    (when (/= (length declared)
              (length (remove-duplicates declared :test #'string=)))
      (error "deliver: recipe declares a local parity suite more than once"))
    (when (/= (length measured)
              (length (remove-duplicates measured :test #'string=)))
      (error "deliver: measured a local parity suite more than once"))
    (let ((missing (set-difference declared measured :test #'string=))
          (undeclared (set-difference measured declared :test #'string=)))
      (when missing
        (error "deliver: declared local parity suites not measured: ~S" missing))
      (when undeclared
        (error "deliver: measured undeclared local parity suites: ~S" undeclared)))
    (dolist (suite suites)
      (unless (plusp (getf suite :total))
        (error "deliver: local parity suite ~A has no cases"
               (getf suite :suite))))
    t))

(defun %atom-content-ids (atom-receipts)
  "The auditable atom-id edge list stored in and hashed by a bundle lock."
  (mapcar (lambda (receipt)
            (list :deliverable (getf receipt :deliverable)
                  :content-id (getf receipt :content-id)))
          atom-receipts))

(defun %build-web-deliverable (spec &key out-dir name suites)
  "Build the :web floor -- a self-contained interactive page.

The recipe names an :ENTRY function of one keyword argument, :PATH, which
writes the page and returns its path (`rosette-webgpu-figure:emit-webgpu-figure`
and its wrappers have exactly this shape).  We load the recipe's :SYSTEM, call
it into the build directory, and take the content id over the emitted bytes.

Why the emitted page and not a reduced source slice: for the binary floors the
shipped thing is a tree-shaken Lisp image, so the reduced source is what must
be reproducible.  Here the shipped thing IS the page -- shaders, host harness
and all, inlined -- so hashing anything else would certify the wrong object."
  (let* ((system (getf spec :system))
         (entry (or (getf spec :entry)
                    (error "deliver: the :web floor needs :entry \"PKG:EMIT-FN\" for ~A" name)))
         (dir (or out-dir (merge-pathnames (format nil "rosette-ship/~A/" name)
                                           (uiop:temporary-directory))))
         (path (merge-pathnames (format nil "~A.html" name) dir)))
    (ensure-directories-exist dir)
    (asdf:load-system system)
    (let* ((colon (position #\: entry))
           (pkg (string-upcase (subseq entry 0 colon)))
           (fn (string-upcase (remove #\: (subseq entry colon))))
           (sym (or (find-symbol fn (or (find-package pkg)
                                        (error "deliver: no package ~A for ~A" pkg name)))
                    (error "deliver: no function ~A in ~A" fn pkg))))
      (funcall sym :path path))
    (unless (probe-file path)
      (error "deliver: :web entry for ~A wrote no page at ~A" name path))
    (let* ((text (uiop:read-file-string path))
           (mode (or (getf spec :web-mode) :offline))
           (artifact
             (rosette-browser-artifact:browser-artifact-from-template
              text '() :mode mode :title name))
           (audit (rosette-browser-artifact:audit-browser-artifact artifact))
           (cid (rosette-content-identity:content-id-long
                 (rosette-browser-artifact:render-browser-artifact artifact))))
      (list :kind :atom :content-id cid
            ;; A page is not run by the builder, so it has no in-tree/artifact
            ;; comparison to make.  Real-device parity is measured by a separate
            ;; suite (tools/rosette-webgpu-parity); absent that, say :skipped.
            :parity (if suites :measured :skipped)
            :parity-external suites
            :web-output path
            :web-bytes (length text)
            :web-audit audit
            :reduction nil))))

(defun %run-behavior-cases (artifact cases)
  "Run declarative CASES against ARTIFACT without a shell and return receipts."
  (let ((seen (make-hash-table :test #'equal))
        (output (floor-artifact-output artifact)))
    (unless (or (null cases) (probe-file output))
      (error "deliver: behavior cases require a built executable"))
    (loop for case in cases
          for name = (getf case :name)
          for args = (or (getf case :args) nil)
          for expected-exit = (or (getf case :exit) 0)
          for contains = (or (getf case :contains) nil)
          do (unless (and (stringp name) (plusp (length name)))
               (error "deliver: behavior case needs a nonempty :name: ~S" case))
             (when (gethash name seen)
               (error "deliver: duplicate behavior case name ~S" name))
             (setf (gethash name seen) t)
             (unless (and (listp args) (every #'stringp args))
               (error "deliver: behavior case ~A has non-string :args" name))
             (unless (and (integerp expected-exit) (<= 0 expected-exit 255))
               (error "deliver: behavior case ~A has invalid :exit" name))
             (unless (and (listp contains) contains (every #'stringp contains))
               (error "deliver: behavior case ~A needs nonempty string :contains" name))
          collect
          (multiple-value-bind (text code)
              (%run (cons (namestring output) args))
            (let ((passed (and (= code expected-exit)
                               (every (lambda (fragment) (search fragment text))
                                      contains))))
              (list :name name :args (copy-list args)
                    :expected-exit expected-exit :observed-exit code
                    :contains (copy-list contains)
                    :output-content-id
                    (rosette-content-identity:content-id-long
                     (list :rosette-deliverable-behavior-output/v1 text))
                    :passed (and passed t)))))))

(defun %behavior-green-p (receipts)
  (every (lambda (receipt) (eq (getf receipt :passed) t)) receipts))

(defun build-deliverable (spec &key out-dir (sbcl "sbcl") deliverable-dir)
  "Build SPEC (a .ship plist). An :atom builds its floor binary (internal
parity vs the in-tree run) + runs its external parity suites; a :bundle builds
no binary -- it composes named :atoms, and its identity is over that manifest +
its suite verdicts. Returns a plist (:kind :content-id :parity :parity-external
… ). External parity suites (rosette vs third-party) live in <deliverable-dir>/
parity/ and are run for both kinds."
  (let* ((kind (or (getf spec :kind) :atom))
         (name (getf spec :deliverable))
         (suites (and deliverable-dir (run-parity-suites deliverable-dir))))
    (%validate-local-parity-suites spec suites)
    (if (eq kind :bundle)
        ;; A bundle composes sealed atom receipts: no binary.  Its identity
        ;; changes when any atom content id or measured parity receipt changes.
        (let* ((atoms (getf spec :atoms))
               (atom-receipts (%resolve-atom-receipts deliverable-dir atoms))
               (atom-ids (%atom-content-ids atom-receipts))
               (merged-suites
                 (%merge-external-parity-receipts atom-receipts suites))
               (manifest (list :rosette-ship-bundle 1
                               :deliverable name
                               :atom-content-ids atom-ids
                               :parity-external merged-suites))
               (cid (rosette-content-identity:content-id-long manifest)))
          (list :kind :bundle :content-id cid :parity :composed
                :atoms atoms :atom-content-ids atom-ids
                :parity-external merged-suites :reduction nil))
        ;; the :web floor -- a Witness lowered to the BROWSER.
        ;;
        ;; Unlike the binary floors this ships no executable and unlike the
        ;; kernel floors it ships more than one emitted module: the artifact is
        ;; a self-contained page carrying SEVERAL lowered kernel-specs plus the
        ;; host that drives them.  Its identity is the content id over the exact
        ;; emitted page, which is the whole shipped source -- there is nothing
        ;; else to hash, and a rebuild that produces a different page is a drift
        ;; the lock catches.
        ;;
        ;; Parity here is deliberately NOT the binary floors' "in-tree run ==
        ;; artifact run": a page does not run under `%run`.  It is :skipped
        ;; unless the recipe names a real-device suite, because claiming parity
        ;; a build never measured would be worse than admitting none.
        (if (eq (getf spec :floor) :web)
            (%build-web-deliverable spec :out-dir out-dir :name name :suites suites)
        ;; an atom: build the floor binary as before.
        (let* ((system (getf spec :system)) (floor (getf spec :floor))
               (expr (getf spec :expr)) (entry (getf spec :entry))
               (in-tree (and expr (%eval-in-tree system expr)))
               (artifact (if (getf spec :ddmin)
                             (ship-ddmin system :floor floor :expr expr :entrypoint entry
                                         :out-dir out-dir :name name :sbcl sbcl
                                         :in-tree-output in-tree)
                             (ship system :floor floor :expr expr :entrypoint entry
                                   :out-dir out-dir :name name :sbcl sbcl
                                   :optimize (getf spec :optimize)))))
          (unless artifact (error "deliver: planning failed for ~A" name))
          (multiple-value-bind (bout bcode) (%run (floor-artifact-build-command artifact))
            (declare (ignore bout))
            (unless (eql bcode 0)
              (error "deliver: build failed for ~A (code ~A)" name bcode)))
          (let* ((cid (reduced-content-id (floor-artifact-reduced-file artifact)))
                 (art-out (and expr (member floor '(:bin :jit))
                               (%run (list (namestring (floor-artifact-output artifact))))))
                 (parity (cond ((null expr) :skipped)
                               ((and art-out (string= (%trim in-tree) (%trim art-out))) :pass)
                               (t :fail)))
                 (behavior (%run-behavior-cases artifact
                                                (getf spec :behavior-cases)))
                 (reduction (getf (floor-artifact-metadata artifact) :reduction)))
            (list :kind :atom :artifact artifact :content-id cid :parity parity
                  :in-tree in-tree :artifact-out art-out
                  :behavior behavior
                  :parity-external suites
                  :reduction (and reduction (reduction-report->plist reduction)))))))))

(defun %external-parity-summary (suites)
  "Condense suite verdicts to (:suite match/total …) for the lock."
  (mapcar (lambda (v) (list :suite (getf v :suite)
                            :reference (getf v :reference)
                            :references (getf v :references)
                            :provenance (getf v :provenance)
                            :provenances (getf v :provenances)
                            :matched (getf v :matched) :total (getf v :total)
                            :diverged (getf v :diverged)
                            :divergences (getf v :divergences)
                            :sources (getf v :sources)
                            :components (getf v :components)))
          suites))

(defun deliverable-lock (spec build)
  "Assemble the committable .lock plist for SPEC from a BUILD result."
  (list :deliverable (getf spec :deliverable)
        :kind (or (getf build :kind) (getf spec :kind) :atom)
        :system (getf spec :system)
        :floor (getf spec :floor)
        :atoms (getf spec :atoms)
        :atom-content-ids (getf build :atom-content-ids)
        :audience (or (getf spec :audience) :internal)
        :maturity (or (getf spec :maturity) :candidate)
        ;; object-capabilities the Deliverable REQUESTS (default none = pure
        ;; stdio compute). The sandbox grants exactly these, least-privilege.
        :capabilities (getf spec :capabilities)
        :content-id (getf build :content-id)
        ;; the GAUGE the content id above was measured in. Without it a lock
        ;; claims "same source" and is read as "same build" -- see toolchain.lisp.
        :toolchain (build-toolchain)
        :parity (getf build :parity)
        :parity-external (%external-parity-summary (getf build :parity-external))
        :behavior (copy-tree (getf build :behavior))
        :reduction (getf build :reduction)))

(defun deliverable-view (ship-path)
  "A merged view of a Deliverable: the recipe fields (audience/maturity are
recipe truth, read from the .ship) joined with the build receipt (content-id,
parity) from the .lock if present."
  (let* ((spec (read-ship-spec ship-path))
         (dir (%deliverable-dir ship-path))
         (name (getf spec :deliverable))
         (lock-path (merge-pathnames (format nil "~A.lock" name) dir))
         (lock (and (probe-file lock-path)
                    (with-open-file (in lock-path :direction :input) (read in)))))
    (list :deliverable name
          :kind (or (getf spec :kind) (getf lock :kind) :atom)
          :system (getf spec :system) :floor (getf spec :floor)
          :audience (or (getf spec :audience) :internal)
          :maturity (or (getf spec :maturity) :candidate)
          :capabilities (or (getf spec :capabilities) (getf lock :capabilities))
          :content-id (getf lock :content-id)
          :parity (cond ((getf spec :stub) :stub)
                        ((getf lock :parity))
                        (t :unbuilt))
          ;; a stub shows its DECLARED parity suites as planned (pending);
          ;; a built one shows the measured verdicts from its lock.
          :parity-external
          (or (getf lock :parity-external)
              (and (getf spec :parity-suites)
                   (mapcar (lambda (s) (list :suite s :matched 0 :total 0))
                           (getf spec :parity-suites)))))))

(defun write-lock (lock path)
  "Write a .lock plist to PATH as a readable, downcased s-expr."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
    (let ((*print-readably* nil) (*print-pretty* t) (*print-case* :downcase))
      (prin1 lock out) (terpri out)))
  path)

(defun compare-lock (build lock-path)
  "Compare a fresh BUILD against the committed lock at LOCK-PATH.

Returns one of :parity-fail, :reproduced, :reproduced-cross-toolchain,
:drifted, :drifted-toolchain -- and, as a second value, the recorded toolchain
match (T / NIL / :unknown).

The two toolchain-qualified verdicts exist because a content id alone cannot
distinguish them.  Reproducing on a DIFFERENT toolchain is a stronger result
than reproducing on the same one, and drifting on a different toolchain names
its own most likely cause instead of sending someone to hunt a source change
that never happened.  Both still gate exactly as before: a reproduction passes,
a drift fails."
  (let* ((old (with-open-file (in lock-path :direction :input) (read in)))
         (old-cid (getf old :content-id))
         (new-cid (getf build :content-id))
         (same-toolchain (toolchain-match-p (getf old :toolchain)))
         (parity (getf build :parity))
         (external (getf build :parity-external))
         (behavior (getf build :behavior))
         (old-behavior (getf old :behavior)))
    (values
     (cond ((or (eq parity :fail)
                (some (lambda (receipt)
                        (or (not (plusp (or (getf receipt :total) 0)))
                            (plusp (or (getf receipt :diverged)
                                       (- (getf receipt :total)
                                          (getf receipt :matched))))))
                      external))
            :parity-fail)
           ((not (%behavior-green-p behavior)) :parity-fail)
           ((and (equal old-cid new-cid) (equal old-behavior behavior))
            (if (null same-toolchain) :reproduced-cross-toolchain :reproduced))
           (t
            (if (null same-toolchain) :drifted-toolchain :drifted)))
     same-toolchain)))

;;; ---- per-file + whole-tree orchestration -------------------------------
(defun %deliverable-dir (ship-path)
  (uiop:pathname-directory-pathname ship-path))

(defun deliver-file (ship-path &key (mode :build) (sbcl "sbcl"))
  "Build the Deliverable at SHIP-PATH (a .ship file). MODE :build writes the
lock beside it; MODE :verify rebuilds and compares against the committed lock.
Returns a plist (:name :parity :content-id :verdict). Binaries land in a
`build/` subdir (gitignored)."
  (let ((spec (read-ship-spec ship-path)))
    ;; a STUB is a declared-but-unbuilt Deliverable (backlog for a later
    ;; agent): it carries intent (audience/floor/parity target/:todo) but is
    ;; not built or locked, and the verify gate skips it.
    (when (getf spec :stub)
      (return-from deliver-file
        (list :name (getf spec :deliverable) :verdict :stub :parity :stub))))
  (let* ((spec (read-ship-spec ship-path))
         (dir (%deliverable-dir ship-path))
         (name (getf spec :deliverable))
         (out-dir (namestring (merge-pathnames "build/" dir)))
         (lock-path (merge-pathnames (format nil "~A.lock" name) dir))
         (build (build-deliverable spec :out-dir out-dir :sbcl sbcl
                                        :deliverable-dir dir)))
    (ecase mode
      (:build (write-lock (deliverable-lock spec build) lock-path)
              (list :name name :parity (getf build :parity)
                    :content-id (getf build :content-id) :verdict :built))
      (:verify (multiple-value-bind (verdict same-toolchain)
                   (if (probe-file lock-path)
                       (compare-lock build lock-path)
                       (values :no-lock :unknown))
                 (list :name name :parity (getf build :parity)
                       :content-id (getf build :content-id) :verdict verdict
                       :toolchain (toolchain-fingerprint)
                       :toolchain-match same-toolchain))))))

(defun find-ship-specs (root)
  "All .ship files under ROOT, sorted."
  (sort (directory (merge-pathnames "**/*.ship" (uiop:ensure-directory-pathname root)))
        #'string< :key #'namestring))

(defun deliver-all (root &key (mode :build) (sbcl "sbcl"))
  "Build/verify every Deliverable under ROOT. Returns the list of per-file
result plists."
  (let ((unavailable-atoms (make-hash-table :test #'equal))
        (results '()))
    ;; Stable two-phase order: atom locks are refreshed/verified before a
    ;; bundle is permitted to consume them.  This avoids minting a bundle from
    ;; yesterday's atom receipt during an all-build.
    (dolist (ship (%order-ship-specs (find-ship-specs root)))
      (let* ((spec (read-ship-spec ship))
             (name (getf spec :deliverable))
             (kind (or (getf spec :kind) :atom))
             (blocked (and (eq kind :bundle)
                           (not (getf spec :stub))
                           (find-if (lambda (atom)
                                      (gethash atom unavailable-atoms))
                                    (getf spec :atoms))))
             (result
               (if blocked
                   (list :name name :verdict :error
                         :error (format nil "dependency atom ~A is unavailable"
                                        blocked))
                   (handler-case (deliver-file ship :mode mode :sbcl sbcl)
                     (error (e) (list :name name :verdict :error
                                      :error (princ-to-string e)))))))
        ;; a cross-toolchain reproduction is a REPRODUCTION -- stronger, in
        ;; fact -- so it must not block a bundle that depends on this atom.
        (when (and (eq kind :atom)
                   (not (member (getf result :verdict)
                                (if (eq mode :build)
                                    '(:built)
                                    '(:reproduced :reproduced-cross-toolchain))
                                :test #'eq)))
          (setf (gethash name unavailable-atoms) t))
        (push result results)))
    (nreverse results)))

(defun %order-ship-specs (ships)
  "Stable atom-before-bundle order for all-build/all-verify orchestration."
  (stable-sort (copy-list ships) #'<
               :key (lambda (ship)
                      (if (eq (or (getf (read-ship-spec ship) :kind) :atom)
                              :bundle)
                          1 0))))

;;; ---- the generated navigable view (grouped by maturity) ----------------
(defparameter *audiences* '(:product :fleet :agent :personal :web :internal)
  "The audience axis -- who runs a Deliverable and why. A field, surfaced by
the generated view, never a directory.")

(defun write-deliverables-status (root)
  "Regenerate STATUS.md under ROOT, grouped by :audience then :maturity. The
navigable view over structural truth: audience and maturity are read from the
`.ship` recipes (joined with each `.lock`), never foldered."
  (let ((views (mapcar #'deliverable-view (find-ship-specs root)))
        (path (merge-pathnames "STATUS.md" (uiop:ensure-directory-pathname root))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (format out "# Deliverables — production substrate~%~%")
      (format out "Generated view over the committed `.ship` recipes + `.lock` receipts.~%")
      (format out "**Audience** (who runs it) and **maturity** (how ready) are fields~%")
      (format out "governed by promotion, not directories. Binaries live in gitignored~%")
      (format out "`build/` dirs; the `.ship` and `.lock` (content id + parity) are the~%")
      (format out "committed source of truth.~2%")
      (dolist (a *audiences*)
        (let ((group (remove a views :key (lambda (v) (getf v :audience)) :test-not #'eq)))
          (when group
            (format out "## ~:(~A~)~%~%" a)
            (format out "| deliverable | kind | system | floor | maturity | parity | vs third-party | content-id |~%")
            (format out "|---|---|---|---|---|---|---|---|~%")
            (dolist (m '(:production :staging :candidate))
              (dolist (v (remove m group :key (lambda (x) (getf x :maturity)) :test-not #'eq))
                (format out "| ~A | ~A | ~A | ~A | ~A | ~A | ~A | `~A` |~%"
                        (getf v :deliverable) (getf v :kind)
                        (or (getf v :system) "—") (getf v :floor)
                        (getf v :maturity) (getf v :parity)
                        (let ((ex (getf v :parity-external)))
                          (if ex
                              (format nil "~{~A~^, ~}"
                                      (mapcar (lambda (s)
                                                (if (plusp (getf s :total))
                                                    (format nil "~A ~D/~D" (getf s :suite)
                                                            (getf s :matched) (getf s :total))
                                                    (format nil "~A pending" (getf s :suite))))
                                              ex))
                              "—"))
                        (let ((c (getf v :content-id)))
                          (if c (subseq c 0 (min 16 (length c))) "—")))))
            (terpri out)))))
    path))
