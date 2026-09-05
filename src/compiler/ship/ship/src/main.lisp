;;;; main.lisp --- the self-sufficient entrypoint (the ouroboros).
;;;;
;;;; `main` is rosette-ship's CLI *in Lisp* -- no bash. It parses argv, plans a
;;;; shipment, runs the build (a child SBCL, via uiop:run-program), runs the
;;;; parity gauge-check, and writes the content-addressed manifest. Because it
;;;; is self-contained, rosette-ship can ship ITSELF: lower rosette-ship's own closure
;;;; to a :jit binary whose entrypoint is this function, and the resulting
;;;; binary is a working rosette-ship that ships other libraries -- and, run on
;;;; `rosette-ship`, reproduces a rosette-ship binary. Deployment closes on itself.
;;;;
;;;; A shipped rosette-ship needs the rosette source tree + SBCL on disk to read other
;;;; libraries' source and dump their cores (it is a dev tool, honestly scoped);
;;;; ENSURE-REGISTRY re-initialises the ASDF source registry from the cwd's
;;;; .rosette-wire-root root at runtime, so the baked-in build-time path never matters.

(in-package #:rosette-ship)

(defun parse-ship-args (args)
  "Parse a rosette-ship argv list into a plist
(:system :floor :expr :entry :out :optimize :ddmin). Signals on unknown --flag."
  (let ((sys nil) (floor :bin) (expr nil) (entry nil) (out nil)
        (optimize nil) (ddmin nil) (simplify nil) (patch nil) (rest args))
    (loop while rest do
      (let ((a (pop rest)))
        (cond
          ((string= a "--floor") (setf floor (intern (string-upcase (or (pop rest) "bin"))
                                                      :keyword)))
          ((string= a "--expr")  (setf expr (pop rest)))
          ((string= a "--entry") (setf entry (pop rest)))
          ((string= a "--out")   (setf out (pop rest)))
          ((string= a "--optimize") (setf optimize t))
          ((string= a "--ddmin")    (setf ddmin t))
          ;; #5/#6 runtime verbs of a shipped :jit binary (live compiler):
          ((string= a "--simplify") (setf simplify (pop rest)))
          ((string= a "--patch")    (setf patch (pop rest)))
          ((and (>= (length a) 2) (string= (subseq a 0 2) "--"))
           (error "rosette-ship: unknown flag ~A" a))
          (t (setf sys a)))))
    (list :system sys :floor floor :expr expr :entry entry :out out
          :optimize optimize :ddmin ddmin :simplify simplify :patch patch)))

(defun ensure-registry ()
  "Re-scan the rosette source tree from the current working directory so a shipped
binary can find libraries' source at runtime."
  (let ((root (%find-repo-root (list *default-pathname-defaults*))))
    (when root
      (asdf:initialize-source-registry
       `(:source-registry (:tree ,root) :inherit-configuration)))
    root))

(defun %trim (s) (string-trim '(#\Newline #\Space #\Return #\Tab) (or s "")))

(defun %eval-in-tree (system expr)
  "Load SYSTEM and evaluate EXPR in-image; return the printed result string."
  (ignore-errors (asdf:load-system system))
  (let ((*package* (find-package :cl-user)))
    (prin1-to-string (eval (read-from-string expr)))))

(defun %run (argv)
  "Run ARGV; return (values stdout exit-code)."
  (multiple-value-bind (out err code)
      (uiop:run-program argv :output '(:string :stripped t)
                             :error-output nil :ignore-error-status t)
    (declare (ignore err))
    (values out code)))

(defun %manifest-path (artifact)
  (let ((rs (namestring (floor-artifact-reduced-file artifact)))
        (suffix ".reduced.lisp"))
    (concatenate 'string
                 (if (and (>= (length rs) (length suffix))
                          (string= suffix rs :start2 (- (length rs) (length suffix))))
                     (subseq rs 0 (- (length rs) (length suffix)))
                     rs)
                 ".manifest.sexp")))

(defun run-ship (plist)
  "Execute a parsed shipment PLIST end to end. Returns a Unix exit code."
  (let ((sys (getf plist :system)) (floor (getf plist :floor))
        (expr (getf plist :expr)))
    ;; #5/#6 runtime verbs: a shipped :jit binary using its LIVE compiler --
    ;; not a shipment. Handle them before anything else.
    (let ((simplify (getf plist :simplify)) (patch (getf plist :patch)))
      (when simplify (runtime-simplify simplify) (return-from run-ship 0))
      (when patch
        (multiple-value-bind (status n) (apply-patch patch)
          (format t "rosette-ship: patch ~A (~A form~:P)~%" status n)
          (return-from run-ship (if (eq status :installed) 0 1)))))
    (unless sys
      (format t "usage: rosette-ship <system> --floor bin|jit|oci|mcp ~
                 [--expr E] [--optimize|--ddmin] [--entry PKG:FN] [--out DIR]~%")
      (return-from run-ship 2))
    (ensure-registry)
    (let* ((in-tree (and expr (%eval-in-tree sys expr)))
           (artifact
             (cond ((getf plist :ddmin)
                    (ship-ddmin sys :floor floor :expr expr
                                    :entrypoint (getf plist :entry)
                                    :out-dir (getf plist :out) :in-tree-output in-tree))
                   (t (ship sys :floor floor :expr expr
                                :entrypoint (getf plist :entry)
                                :out-dir (getf plist :out)
                                :optimize (getf plist :optimize))))))
      (unless artifact (format t "rosette-ship: planning failed~%") (return-from run-ship 1))
      (format t "rosette-ship: building -> ~A~%" (floor-artifact-output artifact))
      (multiple-value-bind (bout bcode) (%run (floor-artifact-build-command artifact))
        (declare (ignore bout))
        (unless (eql bcode 0)
          (format t "rosette-ship: build failed (code ~A)~%" bcode) (return-from run-ship 1)))
      ;; parity gauge-check + manifest (executable floor + a form to run)
      (when (and (member floor '(:bin :jit)) expr)
        (let ((art-out (%run (list (namestring (floor-artifact-output artifact))))))
          (format t "  in-tree : ~A~%  artifact: ~A~%" in-tree art-out)
          (cond
            ((and art-out (string= (%trim in-tree) (%trim art-out)))
             (format t "  PARITY: PASS (byte-identical)~%")
             (write-ship-manifest
              artifact (%manifest-path artifact)
              :parity-certificate (make-parity-certificate in-tree art-out :floor floor))
             (format t "  manifest: ~A~%" (%manifest-path artifact)))
            (t (format t "  PARITY: FAIL~%") (return-from run-ship 1)))))
      (format t "rosette-ship: done -> ~A~%" (floor-artifact-output artifact))
      0)))

(defun main ()
  "Top-level for a shipped rosette-ship binary: parse argv, ship, exit."
  (handler-case
      (sb-ext:exit :code (run-ship (parse-ship-args (rest sb-ext:*posix-argv*))))
    (serious-condition (c)
      (format *error-output* "rosette-ship: ~A~%" c)
      (sb-ext:exit :code 1))))
