;;;; core.lisp --- Disjoint-set forest with union by rank + path compression.
;;;;
;;;; Elements are non-negative integers 0 .. (uf-size uf) - 1.  The forest
;;;; supports near-constant-time (inverse-Ackermann amortized) FIND and
;;;; UNION, an O(1) component count maintained incrementally, and a
;;;; growable element set via UF-ADD-ELEMENT.

(in-package #:rosette-union-find-core)

(defstruct (union-find
            (:constructor %make-union-find (parent rank size component-count))
            (:copier nil)
            (:predicate union-find-p))
  "Disjoint-set forest over integer elements 0 .. SIZE-1."
  (parent nil :type (vector fixnum))
  (rank   nil :type (vector fixnum))
  (size 0 :type fixnum)
  (component-count 0 :type fixnum))

(defun make-union-find (n)
  "Create a union-find structure over N singleton elements 0 .. N-1."
  (check-type n (integer 0))
  (let ((parent (make-array n :element-type 'fixnum
                              :adjustable t :fill-pointer n))
        (rank (make-array n :element-type 'fixnum
                            :adjustable t :fill-pointer n
                            :initial-element 0)))
    (dotimes (i n)
      (setf (aref parent i) i))
    (%make-union-find parent rank n n)))

(defun uf-add-element (uf)
  "Grow UF by one new singleton element; return its index."
  (let* ((i (union-find-size uf)))
    (vector-push-extend i (union-find-parent uf))
    (vector-push-extend 0 (union-find-rank uf))
    (incf (union-find-size uf))
    (incf (union-find-component-count uf))
    i))

(declaim (inline %check-element))
(defun %check-element (uf i)
  (unless (and (integerp i) (<= 0 i) (< i (union-find-size uf)))
    (error "rosette-union-find-core: element ~A out of range [0, ~A)"
           i (union-find-size uf))))

(defun uf-find (uf i)
  "Return the canonical representative of element I, compressing the path."
  (%check-element uf i)
  (let ((parent (union-find-parent uf)))
    ;; Pass 1: find the root.
    (let ((root i))
      (loop until (= root (aref parent root))
            do (setf root (aref parent root)))
      ;; Pass 2: full path compression.
      (loop for j = i then next
            for next = (aref parent j)
            until (= j root)
            do (setf (aref parent j) root))
      root)))

(defun uf-union (uf i j)
  "Merge the components of I and J (union by rank).
Return T if a merge happened, NIL if they were already connected."
  (let ((ri (uf-find uf i))
        (rj (uf-find uf j)))
    (if (= ri rj)
        nil
        (let ((rank (union-find-rank uf))
              (parent (union-find-parent uf)))
          (when (< (aref rank ri) (aref rank rj))
            (rotatef ri rj))
          (setf (aref parent rj) ri)
          (when (= (aref rank ri) (aref rank rj))
            (incf (aref rank ri)))
          (decf (union-find-component-count uf))
          t))))

(defun uf-connected-p (uf i j)
  "Return T iff elements I and J are in the same component."
  (= (uf-find uf i) (uf-find uf j)))

(defun uf-size (uf)
  "Return the number of elements in UF."
  (union-find-size uf))

(defun uf-component-count (uf)
  "Return the current number of disjoint components (O(1))."
  (union-find-component-count uf))

(defun uf-component-sizes (uf)
  "Return an alist of (REPRESENTATIVE . SIZE), one entry per component,
sorted by representative."
  (let* ((n (union-find-size uf))
         (counts (make-array n :element-type 'fixnum :initial-element 0))
         (out '()))
    (dotimes (i (union-find-size uf))
      (incf (aref counts (uf-find uf i))))
    (loop for root from (1- n) downto 0
          for size = (aref counts root)
          unless (zerop size)
            do (push (cons root size) out))
    out))

(defun uf-components (uf)
  "Return the components of UF as a list of lists of element indices.
Each component lists its elements in increasing order; components are
ordered by their smallest element."
  (let ((groups (make-hash-table :test #'eql)))
    ;; Walk indices downward so each pushed list ends up in increasing order.
    (loop for i from (1- (union-find-size uf)) downto 0
          do (push i (gethash (uf-find uf i) groups)))
    (sort (loop for members being the hash-values of groups
                collect members)
          #'< :key #'first)))
