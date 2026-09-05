;;;; toolchain.lisp --- the gauge the reproducibility claim was quantified over.
;;;;
;;;; A `.lock` records a content id over the exact shipped SOURCE, and rebuilding
;;;; to the same id is called reproducible.  But the id says nothing about WHAT
;;;; BUILT IT -- which Lisp, which version, which platform.  Meanwhile CI runs
;;;; `apt-get install -y sbcl` against a rolling base image, so "the same build"
;;;; is quantified over a toolchain nobody wrote down.
;;;;
;;;; This is Nix's actual thesis, and the one place rosette-ship was behind it:
;;;; a derivation hash must cover the toolchain, not just the inputs.  In this
;;;; substrate's own vocabulary the omission has a name -- the build has a GAUGE
;;;; GROUP (implementation, version, platform, features) that the content id
;;;; never gauge-fixed, so the reproducibility verdict carried an unmeasured
;;;; residue.
;;;;
;;;; What this adds is deliberately small: RECORD the toolchain in the lock and
;;;; let the verdict SAY which case it is.  Recording turns two verdicts that
;;;; used to look identical into different, and more informative, results:
;;;;
;;;;   same id,      same toolchain  ->  :reproduced
;;;;   same id,  DIFFERENT toolchain ->  :reproduced-cross-toolchain
;;;;                                     STRONGER evidence, not weaker: the
;;;;                                     slice reproduced across a gauge change.
;;;;   id moved,     same toolchain  ->  :drifted          (the source moved)
;;;;   id moved, DIFFERENT toolchain ->  :drifted-toolchain
;;;;                                     still a failure -- the lock did not
;;;;                                     reproduce -- but the cause is LOCATED,
;;;;                                     so nobody hunts a source change that
;;;;                                     never happened.
;;;;
;;;; Locks written before this field exists have no :toolchain.  That is read as
;;;; UNKNOWN, never as "same": an old lock keeps its old verdict rather than
;;;; being granted a cross-toolchain claim it never measured.

(in-package #:rosette-ship)

(defparameter *toolchain-features*
  '(:sb-thread :sb-unicode :64-bit :little-endian :big-endian)
  "The *FEATURES* keywords recorded in a lock.  A curated few, not all of
*FEATURES*: the whole list is noisy, machine-specific and changes for reasons
that cannot affect emitted source, and a fingerprint that changes for
irrelevant reasons is one people learn to ignore.")

(defun build-toolchain ()
  "The build gauge, as a readable plist: what would have to be equal for two
builds of the same slice to be the same event rather than merely the same
bytes."
  (list :lisp (lisp-implementation-type)
        :version (lisp-implementation-version)
        :machine (machine-type)
        :os (string (uiop:operating-system))
        :features (remove-if-not (lambda (f) (member f *features*))
                                 *toolchain-features*)))

(defun toolchain-id (&optional (toolchain (build-toolchain)))
  "A durable content id over a toolchain plist -- the same 64-hex identity the
substrate uses everywhere else, so a toolchain can be compared, indexed and
quoted exactly like a shipped slice."
  (rosette-content-identity:content-id-long
   (with-output-to-string (s)
     (let ((*print-case* :downcase) (*print-readably* nil) (*print-pretty* nil))
       (prin1 toolchain s)))))

(defun toolchain-fingerprint (&optional (toolchain (build-toolchain)))
  "A one-line human fingerprint, e.g. \"SBCL 2.4.9 (X86-64 linux)\"."
  (format nil "~A ~A (~A ~A)"
          (getf toolchain :lisp) (getf toolchain :version)
          (getf toolchain :machine) (getf toolchain :os)))

(defun toolchain-match-p (recorded &optional (current (build-toolchain)))
  "T when RECORDED and CURRENT are the same toolchain, NIL when they differ,
and :unknown when RECORDED is absent (a lock predating this field).  The three
cases are kept distinct on purpose: absence is not agreement."
  (cond ((null recorded) :unknown)
        ((equal (toolchain-id recorded) (toolchain-id current)) t)
        (t nil)))
