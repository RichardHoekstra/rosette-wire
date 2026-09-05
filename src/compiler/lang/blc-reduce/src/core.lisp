;;;; rosette-blc-reduce/src/core.lisp
;;;;
;;;; BLC EXECUTION (rung 1): a lazy combinator-GRAPH reducer for the
;;;; substrate's Binary Lambda Calculus (rosette-blc terms).  This is the
;;;; *make-it-run* side of BLC, paired with rosette-blc's *measure-it-in-bits*
;;;; side.  It is a faithful port of John Tromp's `gblc.c` ION machine
;;;; (Ben Lynn's ION / Oleg Kiselyov bracket abstraction):
;;;;
;;;;   1. rosette-blc de-Bruijn term  --term->graph-->  V/L/app graph in a heap.
;;;;   2. Kiselyov bracket abstraction (convert-k / combine-k / zip + the
;;;;      clapp peephole rewrites: S K I B C R T D M Y F :) -> combinators.
;;;;   3. LAZY GRAPH REDUCTION: a left-spine walk with in-place node update
;;;;      (= sharing: a shared redex is contracted once) + a copying
;;;;      (Cheney) garbage collector.
;;;;
;;;; The machine is a set of file-internal special variables (gblc.c uses
;;;; file-globals too); the exported entry points each (re)initialise it,
;;;; so callers see a pure function from an rosette-blc term to a result.

(in-package #:rosette-blc-reduce)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

;;; ==================================================================
;;; Combinator codes (ASCII, < +NCOMB+).  Mirrors gblc.c so the clapp /
;;; bracket-abstraction port reads side-by-side with the reference.
;;; ==================================================================

(defconstant +ncomb+    128)
(defconstant +stackrail+ 2)

(defmacro defcc (name ch) `(defconstant ,name ,(char-code ch)))
(defcc +S+ #\S) (defcc +K+ #\K) (defcc +I+ #\I) (defcc +B+ #\B)
(defcc +C+ #\C) (defcc +R+ #\R) (defcc +T+ #\T) (defcc +D+ #\D)
(defcc +M+ #\M) (defcc +Y+ #\Y) (defcc +F+ #\F) (defcc +CONS+ #\:)
(defcc +V+ #\V) (defcc +L+ #\L)
;; our own driver atoms (not in gblc's I/O set):
(defcc +INC+  #\^)   ; church-count successor (acts like I + a host counter)
(defcc +HALT+ #\Z)   ; church base / halt sentinel
(defcc +MT+   #\1)   ; boolean TRUE  marker (inert atom)
(defcc +MF+   #\0)   ; boolean FALSE marker (inert atom)

;;; combinator code -> readback keyword (and back), for combinators->term.
(defparameter *code->keyword*
  (let ((tbl (make-hash-table)))
    (loop for (code . kw) in `((,+S+ . :s) (,+K+ . :k) (,+I+ . :i) (,+B+ . :b)
                               (,+C+ . :c) (,+R+ . :r) (,+T+ . :t) (,+D+ . :d)
                               (,+M+ . :m) (,+Y+ . :y) (,+F+ . :f)
                               (,+CONS+ . :cons) (,+V+ . :v) (,+L+ . :l)
                               (,+INC+ . :inc) (,+HALT+ . :halt)
                               (,+MT+ . :true) (,+MF+ . :false))
          do (setf (gethash code tbl) kw))
    tbl))
(defparameter *keyword->code*
  (let ((tbl (make-hash-table)))
    (maphash (lambda (code kw) (setf (gethash kw tbl) code)) *code->keyword*)
    tbl))

;;; ==================================================================
;;; The heap.  Two equal fixnum arrays (main + GC scratch).  A value
;;; < +ncomb+ is a combinator; a value >= +ncomb+ is an (even) index of
;;; a 2-cell application node (cell0 = function, cell1 = argument).
;;; ==================================================================

(declaim (type (simple-array fixnum (*)) *mem* *gcmem*))
(declaim (type fixnum *hp* *sp* *sptop* *memsize* *steps* *ngc* *church-count*))
(defparameter *memsize* (ash 1 23))             ; 8M cells / heap (~64MB each)
(defparameter *mem*   (make-array *memsize* :element-type 'fixnum :initial-element 0))
(defparameter *gcmem* (make-array *memsize* :element-type 'fixnum :initial-element 0))
(defparameter *hp* +ncomb+)
(defparameter *sp* 0)
(defparameter *sptop* (- *memsize* +stackrail+))
(defparameter *steps* 0)
(defparameter *ngc* 0)
(defparameter *church-count* 0)

(defun last-step-count ()
  "Reduction steps performed by the most recent reduce-* entry point."
  *steps*)

(declaim (inline comb-p m mk-app))
(defun comb-p (n) (declare (type fixnum n)) (< n +ncomb+))
(defun m (i) (declare (type fixnum i)) (aref *mem* i))
(defun (setf m) (v i) (declare (type fixnum v i)) (setf (aref *mem* i) v))

(defun mk-app (f x)
  "Allocate an application node (F X) at the heap front; return its index."
  (declare (type fixnum f x))
  (let ((h *hp*))
    (setf (aref *mem* h) f
          (aref *mem* (1+ h)) x)
    (setf *hp* (+ h 2))
    h))

(defun init-machine ()
  "Reset to a fresh program: zero the +ncomb+ combinator slots of BOTH
heaps (clapp reads mem[comb] and needs 0 there) and rewind the heap."
  (fill *mem*   0 :start 0 :end +ncomb+)
  (fill *gcmem* 0 :start 0 :end +ncomb+)
  (setf *hp* +ncomb+ *sp* *sptop* *steps* 0 *ngc* 0 *church-count* 0))

;;; ==================================================================
;;; clapp -- the combinator-application smart constructor (peephole
;;; rewrites that gblc.c notes "are known to help").  Non -q subset.
;;; ==================================================================

(defun clapp (f a)
  (declare (type fixnum f a))
  (cond
    ((and (= f +K+) (= a +I+)) +F+)                                   ; K I -> F
    ((and (= f +B+) (= a +K+)) +D+)                                   ; B K -> D
    ((and (= f +C+) (= a +I+)) +T+)                                   ; C I -> T
    ((and (= f +D+) (= a +I+)) +K+)                                   ; D I -> K
    ((and (= (m f) +B+) (= a +I+)) (m (1+ f)))                        ; B M I -> M
    ((and (= (m f) +R+) (= a +I+)) (mk-app +T+ (m (1+ f))))           ; R M I -> T M
    ((and (= (m f) +B+) (= (m (1+ f)) +C+) (= a +T+)) +CONS+)         ; B C T -> :
    ((and (= (m f) +S+) (= (m (1+ f)) +I+) (= a +I+)) +M+)            ; S I I -> M
    (t (mk-app f a))))

;;; ==================================================================
;;; Kiselyov bracket abstraction (de-Bruijn lambda -> combinators).
;;; Bool-lists ("which variables are used") are foldr-encoded as app
;;; nodes with NULL = index 0; see gblc.c convertK/combineK/zip.
;;; ==================================================================

(defun zip (nf na)
  (declare (type fixnum nf na))
  (cond ((= nf 0) na)
        ((= na 0) nf)
        (t (mk-app (zip (m nf) (m na))
                   (logior (m (1+ nf)) (m (1+ na)))))))

(defun combine-k (n1 d1 n2 d2)
  (declare (type fixnum n1 d1 n2 d2))
  (cond
    ((= n1 0)
     (cond ((= n2 0) (clapp d1 d2))
           ((/= (m (1+ n2)) 0) (combine-k 0 (clapp +B+ d1) (m n2) d2))
           (t (combine-k 0 d1 (m n2) d2))))
    ((/= (m (1+ n1)) 0)
     (cond ((= n2 0) (combine-k 0 (clapp +R+ d2) (m n1) d1))
           ((/= (m (1+ n2)) 0)
            (combine-k (m n1) (combine-k 0 +S+ (m n1) d1) (m n2) d2))
           (t (combine-k (m n1) (combine-k 0 +C+ (m n1) d1) (m n2) d2))))
    (t
     (cond ((= n2 0) (combine-k (m n1) d1 0 d2))
           ((/= (m (1+ n2)) 0)
            (if (and (= (m n2) 0) (= d2 +I+))             ; eta: B-eliminate
                d1
                (combine-k (m n1) (combine-k 0 +B+ (m n1) d1) (m n2) d2)))
           (t (combine-k (m n1) d1 (m n2) d2))))))

(defun un-double-var (n db)
  "If every Var N in DB occurs doubled, return the undoubled term, else 0."
  (declare (type fixnum n db))
  (let ((f (m db)))
    (if (= f +V+)
        db
        (let ((a (m (1+ db))))
          (if (= f +L+)
              (let ((uda (un-double-var (1+ n) a)))
                (if (/= uda 0) (mk-app +L+ uda) 0))
              (let ((qf (and (= (m f) +V+) (= (m (1+ f)) n)))
                    (qa (and (= (m a) +V+) (= (m (1+ a)) n))))
                (cond
                  ((and qf qa) (mk-app +V+ n))
                  ((or qf qa) 0)
                  (t (let ((udf (un-double-var n f)))
                       (if (/= udf 0)
                           (let ((uda (un-double-var n a)))
                             (if (/= uda 0) (mk-app udf uda) 0))
                           0))))))))))

(defun recursive-template (f a)
  "Recognise (\\x.x x)(\\x. body) and turn it into a Y-combinator body."
  (declare (type fixnum f a))
  (if (and (= f +M+) (= (m a) +L+))
      (let ((g (m (1+ a))))
        (if (/= (m g) +V+)
            (let ((g2 (un-double-var 0 g)))
              (if (/= g2 0) (mk-app +L+ g2) 0))
            0))
      0))

(defun convert-k (db)
  "Kiselyov convert: return (values cl-term used-var-bool-list)."
  (declare (type fixnum db))
  (let ((f (m db)) (a (m (1+ db))))
    (declare (type fixnum f a))
    (cond
      ((= f +V+)
       (let ((nf (mk-app 0 1)))
         (dotimes (i a) (setf nf (mk-app nf 0)))
         (values +I+ nf)))
      ((= f +L+)
       (multiple-value-bind (ca na) (convert-k a)
         (declare (type fixnum ca na))
         (if (= na 0)
             (values (clapp +K+ ca) 0)
             (if (/= (m (1+ na)) 0)
                 (values ca (m na))
                 (values (combine-k 0 +K+ (m na) ca) (m na))))))
      (t
       (multiple-value-bind (cf nf) (convert-k f)
         (declare (type fixnum cf nf))
         (when (= nf 0)
           (let ((ca (recursive-template cf a)))
             (when (/= ca 0) (setf cf +Y+ a ca))))
         (multiple-value-bind (ca na) (convert-k a)
           (declare (type fixnum ca na))
           (values (combine-k nf cf na ca) (zip nf na))))))))

(defun to-cl (db)
  (multiple-value-bind (cl n) (convert-k db)
    (when (/= n 0) (error "to-cl: program is not a closed term"))
    cl))

;;; rosette-blc de-Bruijn term -> the V/L/app heap graph gblc parses into.
(defun term->graph (term)
  (cond
    ((term-var-p term) (mk-app +V+ (term-index term)))
    ((term-lam-p term) (mk-app +L+ (term->graph (term-body term))))
    ((term-app-p term) (mk-app (term->graph (term-fun term))
                               (term->graph (term-arg term))))
    (t (error "term->graph: not a term: ~S" term))))

;;; ==================================================================
;;; Copying garbage collector (Cheney) -- ported from gblc.c evac/gc.
;;; The whole live graph is reachable from the single root at *sptop*.
;;; ==================================================================

(defun evac (n)
  (declare (type fixnum n))
  (if (comb-p n)
      n
      (let ((x (aref *mem* n))
            (y 0))
        (declare (type fixnum x y))
        (setf y (aref *mem* x))
        (loop while (= y +T+) do            ; migrate T M N as N M
          (let ((nn (aref *mem* (1+ n))))
            (setf (aref *mem* n) nn) (setf y nn)
            (setf (aref *mem* (1+ n)) (aref *mem* (1+ x)))
            (setf x y) (setf y (aref *mem* x))))
        (cond
          ((= y +K+) (setf (aref *mem* (1+ n)) (aref *mem* (1+ x))
                           x +I+ (aref *mem* n) +I+))
          ((= y +F+) (setf x +I+ (aref *mem* n) +I+)))
        (setf y (aref *mem* (1+ n)))
        (cond
          ((= x 0) y)                                       ; already forwarded
          ((= x +I+) (setf (aref *mem* n) 0)
                     (setf (aref *mem* (1+ n)) (evac y)))   ; indirection
          (t (let ((hp0 *hp*))
               (setf (aref *gcmem* *hp*) x) (incf *hp*)
               (setf (aref *gcmem* *hp*) y) (incf *hp*)
               (setf (aref *mem* n) 0 (aref *mem* (1+ n)) hp0)
               hp0))))))

(defun gc ()
  (incf *ngc*)
  (let ((newtop (- *memsize* +stackrail+)))
    (setf *hp* +ncomb+)
    (setf (aref *gcmem* newtop) (evac (aref *mem* *sptop*)))
    (let ((di +ncomb+))
      (declare (type fixnum di))
      (loop while (< di *hp*) do
        (setf (aref *gcmem* di) (evac (aref *gcmem* di)))
        (incf di)))
    (setf *sptop* newtop *sp* newtop)
    (rotatef *mem* *gcmem*))
  (when (> (+ *hp* 64) *sp*)
    (error "heap exhausted after GC (hp=~D sp=~D memsize=~D)" *hp* *sp* *memsize*)))

;;; ==================================================================
;;; The lazy spine-reducer.  Reduce to (weak) head normal form, updating
;;; redex nodes IN PLACE (sharing).  Returns the head combinator.
;;; ==================================================================

(defmacro arg (n)   `(aref *mem* (1+ (aref *mem* (+ *sp* ,n)))))
(defmacro apparg (i j) `(mk-app (arg ,i) (arg ,j)))

(declaim (inline lazy lazy1))
(defun lazy (delta f x)              ; overwrite redex root node (both cells)
  (declare (type fixnum delta f x))
  (incf *sp* delta)
  (let ((node (aref *mem* (1+ *sp*))))
    (setf (aref *mem* node) f (aref *mem* (1+ node)) x)))
(defun lazy1 (delta f)               ; redirect parent's function cell
  (declare (type fixnum delta f))
  (incf *sp* delta)
  (when (< *sp* *sptop*)
    (setf (aref *mem* (aref *mem* (1+ *sp*))) f)))

(defun arity (x)
  (declare (type fixnum x))
  (cond ((or (= x +I+) (= x +F+) (= x +M+) (= x +INC+) (= x +Y+)) 1)
        ((or (= x +K+) (= x +T+)) 2)
        ((or (= x +S+) (= x +B+) (= x +C+) (= x +R+) (= x +D+) (= x +CONS+)) 3)
        (t most-positive-fixnum)))   ; markers / HALT -> never enough args

(defun run (root)
  "Reduce ROOT to head normal form; return the head combinator."
  (declare (type fixnum root))
  (setf *sp* *sptop* (aref *mem* *sptop*) root)
  (let ((x root))
    (declare (type fixnum x))
    (loop
      (when (> (+ *hp* 64) *sp*)
        (gc) (setf x (aref *mem* *sptop*)))
      ;; descend the left spine, caching app-node indices on the stack
      (loop while (not (comb-p x)) do
        (setf (aref *mem* *sp*) x)
        (decf *sp*)
        (setf x (aref *mem* (aref *mem* (1+ *sp*)))))
      ;; x is the head combinator
      (cond
        ((= x +HALT+) (return x))
        ((< (- *sptop* *sp*) (arity x)) (return x))   ; WHNF / inert marker
        (t
         (incf *steps*)
         (cond
           ((= x +I+)   (lazy1 1 (setf x (arg 1))))
           ((= x +INC+) (incf *church-count*) (lazy1 1 (setf x (arg 1))))
           ((= x +F+)   (lazy1 1 (setf x +I+)))
           ((= x +M+)   (lazy1 0 (setf x (arg 1))))
           ((= x +Y+)   (lazy 0 (setf x (arg 1)) (mk-app +Y+ (arg 1))))
           ((= x +K+)   (lazy 1 (setf x +I+) (arg 1)))
           ((= x +T+)   (lazy 1 (setf x (arg 2)) (arg 1)))
           ((= x +D+)   (lazy 2 (setf x (arg 1)) (arg 2)))
           ((= x +B+)   (lazy 2 (setf x (arg 1)) (apparg 2 3)))
           ((= x +C+)   (lazy 2 (setf x (apparg 1 3)) (arg 2)))
           ((= x +R+)   (lazy 2 (setf x (apparg 2 3)) (arg 1)))
           ((= x +CONS+)(lazy 2 (setf x (apparg 3 1)) (arg 2)))
           ((= x +S+)   (lazy 2 (setf x (apparg 1 3)) (apparg 2 3)))
           (t (error "run: unknown combinator ~D" x))))))))

;;; ==================================================================
;;; Readback + loading the combinator s-expr representation.
;;;
;;; A combinator term is an s-expr: a leaf is a combinator keyword
;;; (:s :k :i :b :c :r :t :d :m :y :f :cons ...); an application is a
;;; two-element list (FN ARG), left-nested.  combinators->term reads a
;;; heap root back out; csexpr->graph builds a heap graph from one.
;;; ==================================================================

(defun combinators->term (root)
  "Read the combinator graph at heap index ROOT back to a combinator s-expr."
  (declare (type fixnum root))
  (labels ((rb (n)
             (declare (type fixnum n))
             (if (comb-p n)
                 (or (gethash n *code->keyword*)
                     (error "combinators->term: unknown combinator code ~D" n))
                 (list (rb (aref *mem* n)) (rb (aref *mem* (1+ n)))))))
    (rb root)))

(defun csexpr->graph (cs)
  "Build a heap application graph from a combinator s-expr; return its index."
  (cond
    ((keywordp cs)
     (or (gethash cs *keyword->code*)
         (error "csexpr->graph: unknown combinator keyword ~S" cs)))
    ((and (consp cs) (= (length cs) 2))
     (mk-app (csexpr->graph (first cs)) (csexpr->graph (second cs))))
    (t (error "csexpr->graph: not a combinator s-expr: ~S" cs))))

;;; ==================================================================
;;; Public entry points.
;;; ==================================================================

(defun lambda->combinators (term)
  "Bracket-abstract an rosette-blc lambda TERM to a combinator s-expr (no
reduction).  This is the Kiselyov compilation, read back into Lisp."
  (init-machine)
  (combinators->term (to-cl (term->graph term))))

(defun reduce-whnf (term)
  "Reduce an rosette-blc TERM to weak head normal form.  Returns
(values head-keyword combinator-s-expr), where head-keyword is the head
combinator and the s-expr is the whole WHNF graph.  last-step-count
reports the reductions performed."
  (init-machine)
  (let* ((root (to-cl (term->graph term)))
         (head (run root)))
    (values (or (gethash head *code->keyword*)
                (error "reduce-whnf: unknown head ~D" head))
            (combinators->term root))))

(defun reduce-to-normal-form (term)
  "Reduce an rosette-blc TERM to full (combinator) normal form: WHNF, then
recursively normalise each spine argument.  Returns the combinator
s-expr.  last-step-count reports the total reductions performed.
Terminates for normalising terms; diverges for non-normalising ones."
  (let ((total 0))
    (declare (type fixnum total))
    (labels ((spine (cs)
               (if (keywordp cs)
                   (values cs nil)
                   (multiple-value-bind (h args) (spine (first cs))
                     (values h (append args (list (second cs)))))))
             (nf (cs)
               (init-machine)
               (run (csexpr->graph cs))
               (incf total *steps*)
               (let ((whnf (combinators->term (aref *mem* *sptop*))))
                 (multiple-value-bind (head args) (spine whnf)
                   (reduce (lambda (acc a) (list acc (nf a)))
                           args :initial-value head)))))
      (let ((result (nf (lambda->combinators term))))
        (setf *steps* total)
        result))))

(defun reduce-church (lam-term)
  "Reduce LAM-TERM (a closed term denoting a Church numeral) and decode
the integer by applying it to (INC, HALT) and counting the INC peels."
  (init-machine)
  (let* ((cl (to-cl (term->graph lam-term)))
         (root (mk-app (mk-app cl +INC+) +HALT+)))
    (run root)
    *church-count*))

(defun reduce-bool (lam-term)
  "Reduce LAM-TERM (a closed term denoting a Church boolean); apply it to
two inert markers and read which survives.  Returns T or NIL."
  (init-machine)
  (let* ((cl (to-cl (term->graph lam-term)))
         (root (mk-app (mk-app cl +MT+) +MF+))
         (head (run root)))
    (cond ((= head +MT+) t)
          ((= head +MF+) nil)
          (t (error "reduce-bool: non-marker head ~D" head)))))
