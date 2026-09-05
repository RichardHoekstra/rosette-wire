;;;; interval.lisp --- the interval I as a primitive De Morgan algebra.
;;;;
;;;; In cubical type theory the interval is PRIMITIVE: a distributive
;;;; lattice with endpoints 0,1, a reversal (1-r), and connections
;;;; (meet r/\s, join r\/s).  Dimension variables are formal, so a path
;;;; is a term living in a context with one fresh dimension.  We give the
;;;; minimal computational core: interval terms evaluate under an
;;;; environment binding dimension variables to endpoints, and they
;;;; reduce to 0/1 definitionally at the endpoints -- which is exactly
;;;; what makes  p @ i0  and  p @ i1  compute.

(in-package #:rosette-cubical-core)

;;; Interval terms.  Constants i0 / i1 are the keywords :0 / :1; a
;;; dimension variable is (:dim name); reversal/meet/join are tagged
;;; conses.  We keep the algebra in De Morgan normal-ish form only as far
;;; as the endpoints need -- the point is the endpoint computation, not a
;;; lattice decision procedure.

(defparameter i0 :0 "The 0 endpoint of the interval I.")
(defparameter i1 :1 "The 1 endpoint of the interval I.")

(defun idim (name)
  "A formal dimension variable NAME : I."
  (list :dim name))

(defun ineg (r)
  "Interval reversal 1 - R."
  (list :neg r))

(defun imeet (r s)
  "Interval connection (meet) R /\\ S."
  (list :meet r s))

(defun ijoin (r s)
  "Interval connection (join) R \\/ S."
  (list :join r s))

(defun interval-term-p (r)
  "Return true when R is a well-formed interval term."
  (cond
    ((member r '(:0 :1)) t)
    ((and (consp r) (eq (first r) :dim) (symbolp-or-keyword (second r))) t)
    ((and (consp r) (eq (first r) :neg)) (interval-term-p (second r)))
    ((and (consp r) (member (first r) '(:meet :join)))
     (and (interval-term-p (second r)) (interval-term-p (third r))))
    (t nil)))

(defun symbolp-or-keyword (x)
  (or (symbolp x) (keywordp x)))

(defun eval-interval (r env)
  "Evaluate interval term R under ENV (an alist dim-name -> :0/:1).

Returns :0 or :1 when R is determined, else the partially reduced term.
At the endpoints this is the De Morgan algebra:
reversal swaps, meet is min (0 absorbs), join is max (1 absorbs)."
  (cond
    ((eq r :0) :0)
    ((eq r :1) :1)
    ((and (consp r) (eq (first r) :dim))
     (let ((cell (assoc (second r) env)))
       (if cell (cdr cell) r)))
    ((and (consp r) (eq (first r) :neg))
     (let ((v (eval-interval (second r) env)))
       (case v (:0 :1) (:1 :0) (t (list :neg v)))))
    ((and (consp r) (eq (first r) :meet))
     (let ((a (eval-interval (second r) env))
           (b (eval-interval (third r) env)))
       (cond ((or (eq a :0) (eq b :0)) :0)   ; 0 absorbs in meet
             ((eq a :1) b)                     ; 1 is unit in meet
             ((eq b :1) a)
             (t (list :meet a b)))))
    ((and (consp r) (eq (first r) :join))
     (let ((a (eval-interval (second r) env))
           (b (eval-interval (third r) env)))
       (cond ((or (eq a :1) (eq b :1)) :1)   ; 1 absorbs in join
             ((eq a :0) b)                     ; 0 is unit in join
             ((eq b :0) a)
             (t (list :join a b)))))
    (t (error "Not an interval term: ~S." r))))

(defun interval-endpoint-p (r)
  "Return :0, :1, or NIL according to whether R is a definite endpoint."
  (let ((v (eval-interval r nil)))
    (and (member v '(:0 :1)) v)))
