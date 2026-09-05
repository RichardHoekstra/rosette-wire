;;;; objects.lisp --- tagged value representation, precise heap, copying GC.
;;;;
;;;; Low-3-bit lowtag scheme on a 64-bit machine word (SBCL-style):
;;;;   000 FIXNUM     signed 61-bit payload = word >> 3 (two's complement)
;;;;   001 CONS       heap base index = word >> 3 ; cell = [car, cdr]
;;;;   010 CHAR       code point = word >> 3
;;;;   011 SINGLETON  nil / true / ... selected by payload
;;;;   100 CLOSURE    heap base ; object = [code-id(fix), ncaps(fix), cap0 ...]
;;;;
;;;; The load-bearing invariant: every word is SELF-DESCRIBING -- its tag says
;;;; pointer-or-not -- so the heap is exactly traceable (precise GC, no false
;;;; roots) and cycles terminate by marking.

(in-package #:rosette-lisp-objects)

(defconstant +tag-bits+ 3)
(defconstant +payload-bits+ 61)
(defconstant +tag-fixnum+    #b000)
(defconstant +tag-cons+      #b001)
(defconstant +tag-char+      #b010)
(defconstant +tag-singleton+ #b011)
(defconstant +tag-closure+   #b100)

(declaim (inline u64 tag-of base-of make-ptr))
(defun u64 (x) (logand x #xFFFFFFFFFFFFFFFF))
(defun tag-of (w) (logand w #b111))
(defun base-of (w) (ash w (- +tag-bits+)))
(defun make-ptr (base tag) (u64 (logior (ash base +tag-bits+) tag)))

;;; --- immediates --------------------------------------------------------------
(defun mk-fixnum (v)
  "Encode integer V as a tagged fixnum word (tag 000)."
  (u64 (ash (logand v (1- (ash 1 +payload-bits+))) +tag-bits+)))
(defun fixnum-val (w)
  "Decode a fixnum word to a signed integer."
  (let ((p (ash w (- +tag-bits+))))
    (if (logbitp (1- +payload-bits+) p) (- p (ash 1 +payload-bits+)) p)))
(defun fixnump (w) (= (tag-of w) +tag-fixnum+))

(defun mk-char (code) (u64 (logior (ash code +tag-bits+) +tag-char+)))
(defun char-code-of (w) (ash w (- +tag-bits+)))
(defun charp (w) (= (tag-of w) +tag-char+))

(defun mk-singleton (selector) (u64 (logior (ash selector +tag-bits+) +tag-singleton+)))
(defparameter +nil+  (mk-singleton 0) "The canonical empty-list / false singleton.")
(defparameter +true+ (mk-singleton 1) "The canonical true singleton.")
(defun singletonp (w) (= (tag-of w) +tag-singleton+))
(defun nilp (w) (= w +nil+))
(defun truep (w) (= w +true+))

(defun consp* (w) (= (tag-of w) +tag-cons+))
(defun closurep (w) (= (tag-of w) +tag-closure+))
(defun pointerp (w) (or (consp* w) (closurep w)))

(defun objtype (w)
  (ecase (tag-of w)
    (#.+tag-fixnum+ :fixnum) (#.+tag-cons+ :cons) (#.+tag-char+ :char)
    (#.+tag-singleton+ :singleton) (#.+tag-closure+ :closure)))

;;; --- the heap: a flat, growable vector of machine words ----------------------
(defstruct (heap (:constructor %make-heap))
  (words (make-array 0 :element-type '(unsigned-byte 64) :adjustable t :fill-pointer 0)))

(defun make-heap () (%make-heap))
(defun heap-word-count (heap) (fill-pointer (heap-words heap)))

(defun heap-alloc (heap &rest ws)
  "Append WS to the heap; return the base index of the first."
  (let* ((v (heap-words heap)) (base (fill-pointer v)))
    (dolist (w ws base) (vector-push-extend (u64 w) v))))

(declaim (inline %ref %set))
(defun %ref (heap i) (aref (heap-words heap) i))
(defun %set (heap i w) (setf (aref (heap-words heap) i) (u64 w)))

;;; --- cons cells --------------------------------------------------------------
(defun heap-cons (heap a d) (make-ptr (heap-alloc heap a d) +tag-cons+))
(defun obj-car (heap w) (%ref heap (base-of w)))
(defun obj-cdr (heap w) (%ref heap (1+ (base-of w))))
(defun obj-set-car (heap w v) (%set heap (base-of w) v) v)
(defun obj-set-cdr (heap w v) (%set heap (1+ (base-of w)) v) v)

;;; --- flat closures -----------------------------------------------------------
(defun heap-closure (heap code-id caps)
  "Allocate a flat closure: code-id + the captured free variables CAPS (a list
of tagged words).  Returns a tagged closure pointer."
  (let ((base (apply #'heap-alloc heap (mk-fixnum code-id) (mk-fixnum (length caps)) caps)))
    (make-ptr base +tag-closure+)))
(defun closure-code (heap w) (fixnum-val (%ref heap (base-of w))))
(defun closure-ncaps (heap w) (fixnum-val (%ref heap (1+ (base-of w)))))
(defun closure-cap (heap w i) (%ref heap (+ (base-of w) 2 i)))

;;; --- precise tracing ---------------------------------------------------------
(defun heap-trace (heap roots)
  "Mark every object reachable from ROOTS (a word or a list of words).  Returns
a hash-table whose keys are the base indices of the live objects.  Exact: only
pointer-tagged words are followed; immediates are never mistaken for pointers;
cycles terminate via the mark set."
  (let ((live (make-hash-table)))
    (labels ((visit (w)
               (case (tag-of w)
                 (#.+tag-cons+
                  (let ((b (base-of w)))
                    (unless (gethash b live)
                      (setf (gethash b live) t)
                      (visit (%ref heap b)) (visit (%ref heap (1+ b))))))
                 (#.+tag-closure+
                  (let ((b (base-of w)))
                    (unless (gethash b live)
                      (setf (gethash b live) t)
                      (dotimes (i (fixnum-val (%ref heap (1+ b))))
                        (visit (%ref heap (+ b 2 i))))))))))
      (dolist (r (if (listp roots) roots (list roots))) (visit r)))
    live))

(defun heap-live-count (heap roots) (hash-table-count (heap-trace heap roots)))

;;; --- copying (compacting) collector ------------------------------------------
(defun heap-gc (heap roots)
  "Copy every object reachable from ROOTS into a fresh, compact heap; drop
garbage; renumber pointers (forwarding pointers handle cycles).  Returns
(values NEW-HEAP FORWARDED-ROOTS), the roots list rewritten into the new heap.
A single root may be passed instead of a list; the result list is parallel."
  (let ((new (%make-heap)) (fwd (make-hash-table))
        (root-list (if (listp roots) roots (list roots))))
    (labels ((copy (w)
               (case (tag-of w)
                 (#.+tag-cons+
                  (let ((ob (base-of w)))
                    (or (gethash ob fwd)
                        (let ((nb (heap-alloc new 0 0)))
                          (let ((nw (make-ptr nb +tag-cons+)))
                            (setf (gethash ob fwd) nw)
                            (%set new nb (copy (%ref heap ob)))
                            (%set new (1+ nb) (copy (%ref heap (1+ ob))))
                            nw)))))
                 (#.+tag-closure+
                  (let ((ob (base-of w)))
                    (or (gethash ob fwd)
                        (let* ((nc (fixnum-val (%ref heap (1+ ob))))
                               (nb (apply #'heap-alloc new
                                          (make-list (+ 2 nc) :initial-element 0))))
                          (let ((nw (make-ptr nb +tag-closure+)))
                            (setf (gethash ob fwd) nw)
                            (%set new nb (%ref heap ob))            ; code-id (immediate)
                            (%set new (1+ nb) (%ref heap (1+ ob)))  ; ncaps  (immediate)
                            (dotimes (i nc)
                              (%set new (+ nb 2 i) (copy (%ref heap (+ ob 2 i)))))
                            nw)))))
                 (t w))))                                          ; immediates: verbatim
      (values new (mapcar #'copy root-list)))))
