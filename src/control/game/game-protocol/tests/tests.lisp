;;;; tests/tests.lisp --- law tests for rosette-game-protocol.
;;;;
;;;; Two toy backends stand in for the two real ones: FWORLD is functional
;;;; (GAME-ADVANCE returns a fresh world, like rosette-game-engine's
;;;; step-game-state) and MWORLD is in-place mutating (GAME-ADVANCE mutates and
;;;; returns the same world, like rosette-minecraft-clone's step-world).  Both must
;;;; satisfy the same replay law through the same verifier.

(in-package #:rosette-game-protocol/tests)

;;; --- Functional backend: default (identity) capture/restore is correct.

(defstruct (fworld (:copier nil)) (tick 0) (pos 0))

(defmethod game-advance ((w fworld) input &key dt)
  (declare (ignore dt))
  (make-fworld :tick (1+ (fworld-tick w))
               :pos (+ (fworld-pos w) input)))

(defmethod game-hash ((w fworld))
  (logxor (* 1000003 (fworld-pos w)) (fworld-tick w)))

(defmethod game-tick ((w fworld)) (fworld-tick w))

;;; --- Mutating backend: needs a real snapshot capture/restore pair.

(defstruct (mworld (:copier nil)) (tick 0) (pos 0))

(defmethod game-advance ((w mworld) input &key dt)
  (declare (ignore dt))
  (incf (mworld-tick w))
  (incf (mworld-pos w) input)
  w)                                    ; same object, mutated in place

(defmethod game-hash ((w mworld))
  (logxor (* 1000003 (mworld-pos w)) (mworld-tick w)))

(defmethod game-tick ((w mworld)) (mworld-tick w))

(defmethod game-capture ((w mworld))
  (cons (mworld-tick w) (mworld-pos w)))

(defmethod game-restore ((snap cons))
  (make-mworld :tick (car snap) :pos (cdr snap)))

;;; --- A deliberately broken mutating backend: capture is left at the identity
;;; default even though advance mutates.  This is exactly the bug the
;;; capture/restore pair exists to prevent, so replay-p MUST reject it.

(defstruct (bworld (:copier nil)) (tick 0) (pos 0))

(defmethod game-advance ((w bworld) input &key dt)
  (declare (ignore dt))
  (incf (bworld-tick w))
  (incf (bworld-pos w) input)
  w)

(defmethod game-hash ((w bworld))
  (logxor (* 1000003 (bworld-pos w)) (bworld-tick w)))

(defmethod game-tick ((w bworld)) (bworld-tick w))
;; NB: no game-capture / game-restore methods -> identity default, which is
;; wrong for an in-place backend.

(defun run-all-tests ()
  (with-test-run (run "rosette-game-protocol")
    (let ((inputs '(3 -1 4 -1 5 -9)))

      ;; --- functional backend replays through the shared verifier
      (let ((r (run-game (make-fworld) inputs)))
        (check run (game-run-p r) "run-game returns a game-run (functional)")
        (check run (= (length (game-run-hashes r)) (1+ (length inputs)))
               "hash trace has one entry per tick plus the initial")
        (check run (replay-p r) "functional backend satisfies replay-p")
        (check run (= (game-tick (game-restore (game-run-initial r))) 0)
               "captured initial world is at tick 0"))

      ;; --- mutating backend replays through the SAME verifier
      (let* ((w (make-mworld))
             (r (run-game w inputs)))
        (check run (replay-p r) "mutating backend satisfies replay-p")
        (check run (= (mworld-tick w) (length inputs))
               "run-game advanced the mutating world in place")
        (check run (= (game-run-final-hash r) (game-hash w))
               "final recorded hash matches the advanced world"))

      ;; --- both backends agree on the same computation (gauge invariance)
      (check run (equalp (game-run-hashes (run-game (make-fworld) inputs))
                         (game-run-hashes (run-game (make-mworld) inputs)))
             "functional and mutating backends produce identical hash traces")

      ;; --- capture isolates a run from later mutation of the source world
      (let* ((w (make-mworld))
             (r (run-game w inputs)))
        (incf (mworld-pos w) 1000)      ; clobber the source after the run
        (check run (replay-p r)
               "replay-p survives post-run mutation of the source world"))

      ;; --- the broken (identity-capture) mutating backend is rejected
      (let ((r (run-game (make-bworld) inputs)))
        (check run (not (replay-p r))
               "replay-p falsifies an in-place backend with identity capture"))

      ;; --- empty input: a zero-step run trivially replays
      (let ((r (run-game (make-fworld) '())))
        (check run (= (length (game-run-hashes r)) 1) "empty run has one hash")
        (check run (replay-p r) "empty run replays"))

      ;; --- conformance smoke check
      (check run (conformant-p (make-fworld) 7)
             "conformant-p accepts a well-formed functional backend")
      (check run (conformant-p (make-mworld) 7)
             "conformant-p accepts a well-formed mutating backend"))))
