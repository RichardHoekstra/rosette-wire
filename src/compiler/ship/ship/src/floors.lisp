;;;; floors.lisp --- Stage 4 (LOWER): delivery floor = the gauge.
;;;;
;;;; The substrate already lowers one compute IR to every silicon floor
;;;; (CPU/PTX/WASM/WGSL/Verilog), each oracle-gated bit-equivalent. Shipping
;;;; is simply the floor that story never had: a reachable slice is the
;;;; invariant, the delivery format is the gauge, and the parity certificate
;;;; (parity.lisp) is the gauge-check.
;;;;
;;;; `lower-to-floor` is the pluggable seam. Each method EMITS the build
;;;; artifacts (a self-contained reduced .lisp plus a driver the CLI runs)
;;;; and returns a FLOOR-ARTIFACT describing how to realise the deliverable.
;;;; The library never spawns subprocesses; realisation is the CLI's job, so
;;;; the core stays pure and testable.

(in-package #:rosette-ship)

(defstruct (floor-artifact (:constructor %make-floor-artifact))
  "A planned deliverable. REDUCED-FILE is the self-contained source; DRIVER
is the script that, when run by the toolchain, produces OUTPUT; BUILD-COMMAND
is the argv that realises it; METADATA carries the reduction report + notes."
  (floor :bin :type keyword)
  reduced-file
  driver
  output
  (build-command '() :type list)
  (metadata '() :type list))

(defun make-floor-artifact (&rest args)
  (apply #'%make-floor-artifact args))

;;; Absolute path to the MCP runtime source, baked at compile time so the jit
;;; driver can (load ...) it into the binary. (Valid while the repo stays put.)
(defparameter *mcp-runtime-path*
  #.(namestring (merge-pathnames "mcp-runtime.lisp"
                                 (or *compile-file-pathname* *load-pathname*)))
  "Where the baked-in MCP-over-stdio runtime source lives.")

(defun supported-floors ()
  "Delivery floors rosette-ship can lower to today.
  :bin  standalone executable; runs the entrypoint and exits (frozen behaviour)
  :jit  standalone executable with the Lisp compiler LIVE inside it -- it can
        read -> compile -> eval fresh forms at runtime (SBCL JITs each to
        native code), so the shipped capability can specialise/extend itself
        in the field. A sovereign, homoiconic binary.
  :oci  container image baking the executable
  :mcp  live service exposing the root package's exports as MCP tools"
  '(:bin :jit :oci :mcp))

(defgeneric lower-to-floor (slice floor &key &allow-other-keys)
  (:documentation
   "Lower SLICE to delivery FLOOR (a keyword in SUPPORTED-FLOORS). Emits the
build artifact(s) and returns a FLOOR-ARTIFACT. Keyword args:
  :entrypoint  package-qualified 0-arg function name to run (\"PKG:MAIN\")
  :package     root package name (defaults from the slice root)
  :out-dir     where to write artifacts (default /tmp/rosette-ship/<root>)
  :name        artifact basename (defaults to the slice root)
  :sbcl        sbcl binary for the build command."))

;;; ---- shared helpers ----------------------------------------------------
(defun %ship-out-dir (out-dir name)
  (uiop:ensure-directory-pathname
   (or out-dir (format nil "/tmp/rosette-ship/~A" name))))

(defun %default-package-name (root)
  "rosette convention: a system's primary package shares its name, upcased."
  (string-upcase (dep-name root)))

(defun %emit-slice-source (slice reduced-path optimize seeds)
  "Emit the reduced single file to REDUCED-PATH and return a REDUCTION-REPORT.
When OPTIMIZE, form-level tree-shake to SEEDS (entrypoint/expr strings); if
the analysis can't run, statically fall back to the full closure. (The parity
gauge-check is the dynamic backstop the CLI applies on top of this.)"
  (let ((opt (and optimize (form-level-reduce slice reduced-path :seeds seeds))))
    (if opt
        (make-reduction-report :whole (count-primary-systems)
                               :kept (slice-lib-count slice)
                               :files (getf opt :files) :lines (getf opt :lines)
                               :form-level opt)
        (emit-reduced-file slice reduced-path))))

(defun %emit-run-call (out entry expr)
  "Emit the toplevel's run line: evaluate EXPR (a form string) and print it,
or funcall the 0-arg ENTRY function. EXPR wins when both are given."
  (if expr
      (format out "      (progn (prin1 (eval (read-from-string ~S))) (terpri)~%" expr)
      (format out "      (progn (funcall (read-from-string ~S))~%" entry)))

;;; ======================================================================
;;; :bin -- one standalone executable via save-lisp-and-die.
;;; ======================================================================
(defmethod lower-to-floor (slice (floor (eql :bin))
                           &key entrypoint expr package out-dir name sbcl optimize
                                reduced-ready
                           &allow-other-keys)
  (let* ((name (or name (slice-root slice)))
         (pkg (or package (%default-package-name (slice-root slice))))
         (entry (or entrypoint (format nil "~A:MAIN" pkg)))
         (dir (%ship-out-dir out-dir name))
         (reduced (merge-pathnames (format nil "~A.reduced.lisp" name) dir))
         (driver  (merge-pathnames (format nil "~A.build.lisp" name) dir))
         (exe     (merge-pathnames name dir))
         ;; reduced-ready: the reduced file was pre-emitted (e.g. by ddmin);
         ;; do not overwrite it.
         (report  (unless reduced-ready
                    (%emit-slice-source slice reduced optimize (list entry expr)))))
    (with-open-file (out driver :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
      (format out ";;;; rosette-ship :bin build driver for ~A~%" name)
      (format out "(require :asdf)  ; provides UIOP for libs that use it~%")
      (format out "(load ~S)~%" (namestring reduced))
      (format out "(defun %rosette-ship-toplevel ()~%")
      (format out "  (handler-case~%")
      (%emit-run-call out entry expr)
      (format out "             (finish-output) (sb-ext:exit :code 0))~%")
      (format out "    (serious-condition (c)~%")
      (format out "      (format *error-output* \"~~&rosette-ship bin: ~~A~~%\" c)~%")
      (format out "      (sb-ext:exit :code 1))))~%")
      (format out "(sb-ext:save-lisp-and-die ~S :executable t :toplevel #'%rosette-ship-toplevel)~%"
              (namestring exe)))
    (make-floor-artifact
     :floor :bin :reduced-file reduced :driver driver :output exe
     :build-command (list (or sbcl "sbcl") "--no-sysinit" "--no-userinit"
                          "--non-interactive" "--load" (namestring driver))
     :metadata (list :reduction report :entrypoint (or expr entry)))))

;;; ======================================================================
;;; :jit -- a self-contained executable with the Lisp compiler LIVE inside it.
;;; SBCL's cores retain the full compiler; this floor EXPOSES it: the binary
;;; can `read -> compile -> eval` fresh forms at runtime, so the shipped
;;; capability is not frozen -- it can specialise and extend itself in the
;;; field. `./app` runs the entrypoint; `./app --eval "(form)"` JIT-compiles
;;; and runs one form; `./app --repl` is a live compile-eval loop over stdin.
;;; (Compute-IR witnesses can additionally carry rosette-gpu-kernel-dsl so the
;;; live JIT lowers fresh kernels to the host's silicon -- the deep JIT.)
;;; ======================================================================
(defmethod lower-to-floor (slice (floor (eql :jit))
                           &key entrypoint expr package out-dir name sbcl optimize
                                reduced-ready
                           &allow-other-keys)
  (let* ((name (or name (slice-root slice)))
         (pkg (or package (%default-package-name (slice-root slice))))
         (entry (or entrypoint (format nil "~A:MAIN" pkg)))
         (dir (%ship-out-dir out-dir name))
         (reduced (merge-pathnames (format nil "~A.reduced.lisp" name) dir))
         (driver  (merge-pathnames (format nil "~A.build.lisp" name) dir))
         (exe     (merge-pathnames name dir))
         (report  (unless reduced-ready
                    (%emit-slice-source slice reduced optimize (list entry expr)))))
    (with-open-file (out driver :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
      (format out ";;;; rosette-ship :jit build driver for ~A (compiler LIVE in binary)~%" name)
      (format out "(require :asdf)  ; provides UIOP for libs that use it~%")
      (format out "(load ~S)~%" (namestring reduced))
      (format out "(load ~S)  ; MCP-over-stdio runtime~%" *mcp-runtime-path*)
      ;; :save-runtime-options t => the executable owns its argv (no SBCL
      ;; flag processing); the toplevel routes to entrypoint / eval / repl.
      (format out "(defun %rosette-ship-jit-toplevel ()~%")
      (format out "  (let ((args (rest sb-ext:*posix-argv*)))~%")
      (format out "    (handler-case~%")
      (format out "        (cond~%")
      ;; --describe: HATEOAS self-documentation. Ask the binary what it can do;
      ;; it answers from its own exports + arglists + docstrings. No man page.
      (format out "          ((member \"--describe\" args :test #'string=)~%")
      (format out "           (ignore-errors (require :sb-introspect))~%")
      (format out "           (format t \"~~&deliverable: ~A~~%floor: jit~~%package: ~A~~%operations:~~%\")~%"
              name pkg)
      (format out "           (let ((p (find-package ~S)))~%" pkg)
      (format out "             (when p (do-external-symbols (s p)~%")
      (format out "               (when (fboundp s)~%")
      (format out "                 (format t \"  ~~(~~A~~)~~@[ ~~A~~]~~@[  -- ~~A~~]~~%\"~%")
      (format out "                         (symbol-name s)~%")
      (format out "                         (ignore-errors (funcall (find-symbol \"FUNCTION-LAMBDA-LIST\" \"SB-INTROSPECT\") s))~%")
      (format out "                         (documentation s 'function))))))~%")
      (format out "           (finish-output) (sb-ext:exit :code 0))~%")
      ;; --mcp: speak MCP (JSON-RPC) over stdio so any agent's MCP client can
      ;; use this binary as a native, self-describing, verified tool.
      (format out "          ((member \"--mcp\" args :test #'string=)~%")
      (format out "           (funcall (read-from-string \"ROSETTE-SHIP-MCP:SERVE\") ~S)~%" pkg)
      (format out "           (sb-ext:exit :code 0))~%")
      (format out "          ((member \"--repl\" args :test #'string=)~%")
      (format out "           ;; live lisp: JIT-compile & run forms from stdin~%")
      (format out "           (loop for form = (read *standard-input* nil :eof)~%")
      (format out "                 until (eq form :eof)~%")
      (format out "                 do (handler-case (prin1 (eval form))~%")
      (format out "                      (serious-condition (c)~%")
      (format out "                        (format t \"~~&;; ~~A~~%\" c)))~%")
      (format out "                    (terpri))~%")
      (format out "           (sb-ext:exit :code 0))~%")
      (format out "          ((member \"--eval\" args :test #'string=)~%")
      (format out "           (prin1 (eval (read-from-string~%")
      (format out "                         (second (member \"--eval\" args :test #'string=)))))~%")
      (format out "           (terpri) (sb-ext:exit :code 0))~%")
      ;; A recipe may deliberately carry both an ENTRY (the public CLI) and an
      ;; EXPR (the deterministic no-argument parity smoke). Positional input
      ;; belongs to the CLI; it must never be swallowed by the smoke path.
      (when expr
        (format out "          (args (funcall (read-from-string ~S)))~%" entry))
      (format out "          (t ~A~%"
              (if expr
                  (format nil "(prin1 (eval (read-from-string ~S))) (terpri)" expr)
                  (format nil "(funcall (read-from-string ~S))" entry)))
      (format out "             (finish-output) (sb-ext:exit :code 0)))~%")
      (format out "      (serious-condition (c)~%")
      (format out "        (format *error-output* \"~~&rosette-ship jit: ~~A~~%\" c)~%")
      (format out "        (sb-ext:exit :code 1)))))~%")
      (format out "(sb-ext:save-lisp-and-die ~S :executable t~%" (namestring exe))
      (format out "                          :toplevel #'%rosette-ship-jit-toplevel~%")
      (format out "                          :save-runtime-options t)~%"))
    (make-floor-artifact
     :floor :jit :reduced-file reduced :driver driver :output exe
     :build-command (list (or sbcl "sbcl") "--no-sysinit" "--no-userinit"
                          "--non-interactive" "--load" (namestring driver))
     :metadata (list :reduction report :entrypoint entry
                     :note "compiler live in binary: --eval / --repl JIT at runtime"))))

;;; ======================================================================
;;; :oci -- an OCI image that bakes the reduced file + executable entrypoint.
;;; Emits a Dockerfile; the CLI runs `docker build`.
;;; ======================================================================
(defmethod lower-to-floor (slice (floor (eql :oci))
                           &key entrypoint package out-dir name sbcl
                                (base "docker.io/library/debian:stable-slim")
                           &allow-other-keys)
  (declare (ignore sbcl))
  (let* ((name (or name (slice-root slice)))
         (pkg (or package (%default-package-name (slice-root slice))))
         (entry (or entrypoint (format nil "~A:MAIN" pkg)))
         (dir (%ship-out-dir out-dir name))
         ;; reuse the :bin lowering to get the reduced file + build driver
         (bin (lower-to-floor slice :bin :entrypoint entry :package pkg
                                          :out-dir dir :name name))
         (dockerfile (merge-pathnames "Dockerfile" dir)))
    (with-open-file (out dockerfile :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
      (format out "# rosette-ship :oci image for ~A~%" name)
      (format out "FROM ~A AS build~%" base)
      (format out "RUN apt-get update && apt-get install -y --no-install-recommends sbcl && rm -rf /var/lib/apt/lists/*~%")
      (format out "WORKDIR /ship~%")
      (format out "COPY ~A ~A ./~%"
              (file-namestring (floor-artifact-reduced-file bin))
              (file-namestring (floor-artifact-driver bin)))
      (format out "RUN sbcl --no-sysinit --no-userinit --non-interactive --load ~A~%"
              (file-namestring (floor-artifact-driver bin)))
      (format out "~%FROM ~A~%" base)
      (format out "COPY --from=build /ship/~A /usr/local/bin/~A~%" name name)
      (format out "ENTRYPOINT [\"/usr/local/bin/~A\"]~%" name))
    (make-floor-artifact
     :floor :oci :reduced-file (floor-artifact-reduced-file bin)
     :driver dockerfile :output (format nil "rosette-ship/~A:latest" name)
     :build-command (list "docker" "build" "-t" (format nil "rosette-ship/~A:latest" name)
                          (namestring dir))
     :metadata (append (floor-artifact-metadata bin) (list :base base)))))

;;; ======================================================================
;;; :mcp -- expose the root package's exports as MCP tools (a live service).
;;; Emits a driver that loads the reduced file, introspects external symbols,
;;; and prints a JSON tool manifest; the CLI hosts it via tools/rosette-mcp.
;;; ======================================================================
(defmethod lower-to-floor (slice (floor (eql :mcp))
                           &key package out-dir name
                           &allow-other-keys)
  (let* ((name (or name (slice-root slice)))
         (pkg (or package (%default-package-name (slice-root slice))))
         (dir (%ship-out-dir out-dir name))
         (reduced (merge-pathnames (format nil "~A.reduced.lisp" name) dir))
         (driver  (merge-pathnames (format nil "~A.mcp.lisp" name) dir))
         (manifest (merge-pathnames (format nil "~A.tools.sexp" name) dir))
         (report  (emit-reduced-file slice reduced)))
    (with-open-file (out driver :direction :output :if-exists :supersede
                                :if-does-not-exist :create)
      (format out ";;;; rosette-ship :mcp manifest driver for ~A~%" name)
      (format out "(load ~S)~%" (namestring reduced))
      (format out "(with-open-file (m ~S :direction :output :if-exists :supersede :if-does-not-exist :create)~%"
              (namestring manifest))
      (format out "  (let ((tools '()))~%")
      (format out "    (do-external-symbols (s (find-package ~S))~%" pkg)
      (format out "      (when (fboundp s)~%")
      (format out "        (push (list :name (string-downcase (symbol-name s))~%")
      (format out "                    :package ~S) tools)))~%" pkg)
      (format out "    (let ((*print-readably* nil) (*print-pretty* t))~%")
      (format out "      (write (nreverse tools) :stream m :case :downcase))))~%")
      (format out "(sb-ext:exit :code 0)~%"))
    (make-floor-artifact
     :floor :mcp :reduced-file reduced :driver driver :output manifest
     :build-command (list "sbcl" "--no-sysinit" "--no-userinit"
                          "--non-interactive" "--load" (namestring driver))
     :metadata (list :reduction report :package pkg))))
