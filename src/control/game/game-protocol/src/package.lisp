;;;; package.lisp --- Public API for rosette-game-protocol.
;;;;
;;;; The deterministic-replay contract shared by every Rosette game backend.

(defpackage #:rosette-game-protocol
  (:use #:cl)
  (:export
   ;; the backend contract (generic functions a backend implements)
   #:game-advance
   #:game-hash
   #:game-tick
   #:game-capture
   #:game-restore
   ;; the portable run/replay verifier (shared law, implemented once)
   #:game-run
   #:game-run-p
   #:game-run-initial
   #:game-run-inputs
   #:game-run-dt
   #:game-run-ticks
   #:game-run-hashes
   #:game-run-final-hash
   #:run-game
   #:replay-p
   #:conformant-p))
(in-package #:rosette-game-protocol)
