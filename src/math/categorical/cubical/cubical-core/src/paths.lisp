;;;; paths.lisp --- cubical paths: a path IS a function out of the interval.
;;;;
;;;; This is the defining cubical move.  In book-HoTT a path is an opaque
;;;; identity witness with endpoints recorded as data; in cubical type
;;;; theory  Path A x y  is the type of functions  p : I -> A  with the
;;;; JUDGEMENTAL constraints  p i0 = x  and  p i1 = y.  Application is
;;;; ordinary function application, so  p @ i0  and  p @ i1  REDUCE to the
;;;; endpoints definitionally -- there is no separate "compute the
;;;; endpoint" step, it is just (funcall p :0).

(in-package #:rosette-cubical-core)

(defstruct (cpath (:constructor %make-cpath (type function)))
  "A cubical path in TYPE: FUNCTION maps an interval endpoint (:0/:1) or
a dimension term to an inhabitant of TYPE.  The endpoints are recovered
by application, not stored separately."
  (type nil :type hott-type :read-only t)
  (function nil :type function :read-only t))

(defun make-cpath (type function)
  "Construct a cubical path  I -> TYPE.  FUNCTION is checked to land in
TYPE at both endpoints (the only constraint cubical imposes on a line)."
  (let ((at0 (funcall function :0))
        (at1 (funcall function :1)))
    (unless (and (in-type-p type at0) (in-type-p type at1))
      (error "Cubical path endpoints ~S, ~S are not both in type ~S."
             at0 at1 (hott-type-name type))))
  (%make-cpath type function))

(defun path-app (path r)
  "Apply PATH at interval term R.  At an endpoint this reduces
definitionally to that endpoint value; the headline cubical reduction."
  (let ((v (eval-interval r nil)))
    (case v
      ((:0 :1) (funcall (cpath-function path) v))
      ;; non-endpoint: keep it symbolic by applying the closure to the
      ;; (reduced) term -- a closure may know how to interpret it.
      (t (funcall (cpath-function path) v)))))

(defun cpath-i0 (path) "PATH's 0-endpoint (definitional)." (path-app path :0))
(defun cpath-i1 (path) "PATH's 1-endpoint (definitional)." (path-app path :1))
(defun cpath-from (path) (cpath-i0 path))
(defun cpath-to (path) (cpath-i1 path))

(defun crefl (type value)
  "Reflexivity as the CONSTANT line  lambda i. VALUE.  Both endpoints
reduce to VALUE by computation."
  (make-cpath type (constantly value)))
