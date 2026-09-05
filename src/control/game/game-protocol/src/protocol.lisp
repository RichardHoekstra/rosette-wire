;;;; protocol.lisp --- generic-function contract + one portable replay verifier.
;;;;
;;;; A "game backend" is anything that can advance a world one deterministic
;;;; tick under an opaque per-tick input, expose an integer fingerprint of its
;;;; state, and report an integer tick counter.  Backends differ in a single
;;;; redundant choice -- the GAUGE -- namely whether GAME-ADVANCE returns a
;;;; fresh world (functional stepper) or mutates and returns the same world
;;;; (in-place stepper).  GAME-CAPTURE / GAME-RESTORE fix that gauge: a
;;;; functional backend leaves them at their identity defaults; a mutating
;;;; backend supplies a snapshot / from-snapshot pair so the verifier can
;;;; re-run from a pristine initial world.  The replay law itself lives here
;;;; once, not in each backend.

(in-package #:rosette-game-protocol)

;;; ---------------------------------------------------------------------------
;;; The backend contract.
;;; ---------------------------------------------------------------------------

(defgeneric game-advance (world input &key dt)
  (:documentation
   "Advance WORLD one deterministic tick under the opaque per-tick INPUT and
return the post-tick world.  DT is the fixed timestep; a backend that has no
continuous time ignores it.  A functional backend returns a fresh world; an
in-place backend mutates WORLD and returns it.  Must be a pure function of
(WORLD, INPUT, DT) up to that gauge choice -- no hidden state, no randomness."))

(defgeneric game-hash (world)
  (:documentation
   "Return a deterministic non-negative integer fingerprint of WORLD's state.
Two worlds with the same observable state must hash equal; any observable
difference should change the hash.  This is the observable the replay law
compares -- it need not be collision-free, only deterministic."))

(defgeneric game-tick (world)
  (:documentation "Return WORLD's integer tick counter."))

(defgeneric game-capture (world)
  (:documentation
   "Return a snapshot from which GAME-RESTORE reconstructs a world equal to
WORLD, independent of any later mutation of WORLD.  Defaults to identity, which
is correct for a functional backend whose worlds are never mutated in place.")
  (:method (world) world))

(defgeneric game-restore (snapshot)
  (:documentation
   "Reconstruct a fresh world from a GAME-CAPTURE snapshot.  Defaults to
identity, the inverse of the default GAME-CAPTURE.")
  (:method (snapshot) snapshot))

;;; ---------------------------------------------------------------------------
;;; The portable run + replay verifier (the shared law).
;;; ---------------------------------------------------------------------------

(defstruct (game-run (:constructor %make-game-run) (:copier nil))
  "A replayable record of advancing a world through a fixed input sequence.
INITIAL is a GAME-CAPTURE snapshot taken before the first tick; INPUTS is the
per-tick input vector; TICKS and HASHES are length (1+ steps) traces recorded
at tick 0 and after each advance."
  (initial nil)
  (inputs #() :type simple-vector)
  (dt 1d0)
  (ticks #() :type simple-vector)
  (hashes #() :type simple-vector))

(declaim (inline game-run-final-hash))
(defun game-run-final-hash (run)
  "The last recorded fingerprint in RUN (its terminal state)."
  (let ((h (game-run-hashes run)))
    (aref h (1- (length h)))))

(defun run-game (world inputs &key (dt 1d0))
  "Advance WORLD through INPUTS -- one opaque input per tick -- capturing a
replayable GAME-RUN.  The initial world is snapshotted via GAME-CAPTURE before
stepping, so a mutating backend (which advances WORLD in place, exactly as its
own transcript runner would) stays replayable.  Returns the GAME-RUN."
  (let* ((inputs (coerce (or inputs '()) 'simple-vector))
         (steps (length inputs))
         (hashes (make-array (1+ steps)))
         (ticks (make-array (1+ steps)))
         (initial (game-capture world)))
    (setf (aref hashes 0) (game-hash world)
          (aref ticks 0) (game-tick world))
    (dotimes (i steps)
      (setf world (game-advance world (aref inputs i) :dt dt))
      (setf (aref hashes (1+ i)) (game-hash world)
            (aref ticks (1+ i)) (game-tick world)))
    (%make-game-run :initial initial :inputs inputs :dt dt
                    :ticks ticks :hashes hashes)))

(defun replay-p (run)
  "T iff RUN replays deterministically: restoring its captured initial world and
re-advancing through the same inputs reproduces every recorded fingerprint and
tick.  This is the falsifiable gauge-check -- a nondeterministic or
state-leaking GAME-ADVANCE makes it fail."
  (check-type run game-run)
  (let* ((inputs (game-run-inputs run))
         (steps (length inputs))
         (dt (game-run-dt run))
         (hashes (game-run-hashes run))
         (ticks (game-run-ticks run))
         (world (game-restore (game-run-initial run))))
    (and (eql (game-hash world) (aref hashes 0))
         (eql (game-tick world) (aref ticks 0))
         (dotimes (i steps t)
           (setf world (game-advance world (aref inputs i) :dt dt))
           (unless (and (eql (game-hash world) (aref hashes (1+ i)))
                        (eql (game-tick world) (aref ticks (1+ i))))
             (return nil))))))

(defun conformant-p (world &optional (input nil input-supplied-p))
  "Cheap smoke check that WORLD's backend implements the contract: GAME-HASH and
GAME-TICK return numbers, GAME-CAPTURE/GAME-RESTORE round-trip the fingerprint,
and -- when a sample INPUT is supplied -- a single-tick run replays.  Returns T
or signals via the underlying generics; intended for tests and bring-up, not a
hot path."
  (and (integerp (game-hash world))
       (integerp (game-tick world))
       (eql (game-hash world)
            (game-hash (game-restore (game-capture world))))
       (or (not input-supplied-p)
           (replay-p (run-game world (list input))))))
