;;;; net.lisp --- interaction-net arena: nodes, ports, wires.
;;;;
;;;; The interaction combinators (Lafont 1990) realised as a mutable
;;;; port-graph.  Three agent symbols:
;;;;
;;;;   +CON+   constructor   (arity 2: principal + 2 aux)
;;;;   +DUP+   duplicator    (arity 2: principal + 2 aux)  -- the SHARING agent
;;;;   +ERA+   eraser        (arity 0: principal only)
;;;;
;;;; A node carries an integer LABEL.  For CON the label distinguishes the
;;;; structural role (lambda vs application -- both are binary constructors,
;;;; tagged so read-back can tell them apart).  For DUP the label is the
;;;; "colour": two duplicators of the SAME colour annihilate, of DIFFERENT
;;;; colours commute -- this colouring is exactly what makes the duplication
;;;; sound (no two distinct fan-outs accidentally annihilate).
;;;;
;;;; PORTS.  Every node has 3 port slots (0 = principal, 1, 2 = auxiliary).
;;;; A wire is a symmetric link between two ports.  We store, per port, the
;;;; port it is wired to (PARTNER-NODE, PARTNER-SLOT).  An *active pair* is a
;;;; wire connecting the PRINCIPAL ports (slot 0) of two live nodes.
;;;;
;;;; The arena is the rung-1 hash-cons made operational at the graph level:
;;;; node identity is its arena index, and a duplicator SHARES a subnet by
;;;; pointing two wires at the same node rather than copying it.

(in-package #:rosette-sharing-reduction)

(defconstant +con+ :con "Constructor agent (lambda / application).")
(defconstant +dup+ :dup "Duplicator agent -- the maximal-sharing fan node.")
(defconstant +era+ :era "Eraser agent.")
(defconstant +root+ :root
  "Inert anchor: holds the net's result on an AUXILIARY port so it never
forms an active pair (no rule mentions +ROOT+).  Read-back starts here.")

(defconstant +slot-principal+ 0)
(defconstant +slot-aux1+ 1)
(defconstant +slot-aux2+ 2)

;;; --- node ------------------------------------------------------------

(defstruct (node (:constructor %make-node) (:copier nil))
  "One interaction-net agent.

  INDEX   arena index (the node's identity)
  SYM     one of +CON+ / +DUP+ / +ERA+
  LABEL   integer tag (CON: role; DUP: colour)
  PORTS   simple-vector of 3 partner cells; each cell is NIL or a PORT."
  (index -1 :type fixnum)
  (sym :con)
  (label 0 :type fixnum)
  (ports (make-array 3 :initial-element nil) :type simple-vector)
  (alive t :type boolean))

(declaim (inline node-symbol))
(defun node-symbol (node) (node-sym node))

;;; --- port ------------------------------------------------------------

(defstruct (port (:constructor make-port (node slot)) (:copier nil))
  "A reference to one slot of a node."
  (node nil :type node)
  (slot 0 :type (integer 0 2)))

(defun port= (a b)
  (and (eq (port-node a) (port-node b))
       (= (port-slot a) (port-slot b))))

;;; --- net (arena) -----------------------------------------------------

(defstruct (net (:constructor %make-net) (:copier nil))
  "An interaction-net arena plus reduction counters."
  (nodes (make-array 0 :adjustable t :fill-pointer 0) :type (vector t))
  (free-list nil :type list)
  (interactions 0 :type fixnum)   ; active pairs rewritten
  (rewrites 0 :type fixnum)       ; alias of interactions (HVM "rewrites")
  (annihilations 0 :type fixnum)  ; same-kind / beta-like local reductions
  (commutations 0 :type fixnum)   ; fan bookkeeping / propagation rules
  (trace-events nil :type list)   ; immutable-on-read rewrite observables
  (root nil))                     ; PORT into the net's principal result

(defun net-trace (net)
  "Return the chronological local-rewrite trace recorded by NET."
  (nreverse (copy-tree (net-trace-events net))))

(defun make-net () (%make-net))

(defun net-node-count (net)
  "Number of LIVE nodes in NET."
  (count-if #'node-alive (net-nodes net)))

(defun alloc-node (net sym &optional (label 0))
  "Allocate a fresh node with SYM and LABEL, return it."
  (let ((node (%make-node :sym sym :label label
                          :ports (make-array 3 :initial-element nil))))
    (if (net-free-list net)
        (let ((idx (pop (net-free-list net))))
          (setf (node-index node) idx
                (aref (net-nodes net) idx) node))
        (progn
          (setf (node-index node) (fill-pointer (net-nodes net)))
          (vector-push-extend node (net-nodes net))))
    node))

(defun free-node (net node)
  "Mark NODE dead and recycle its arena slot."
  (when (node-alive node)
    (setf (node-alive node) nil)
    (fill (node-ports node) nil)
    (push (node-index node) (net-free-list net)))
  (values))

;;; --- wiring ----------------------------------------------------------

(defun connect (pa pb)
  "Wire ports PA and PB together (symmetric)."
  (setf (svref (node-ports (port-node pa)) (port-slot pa)) pb
        (svref (node-ports (port-node pb)) (port-slot pb)) pa)
  (values))

(defun disconnect (port)
  "Remove the wire incident on PORT, if any, from both endpoints.

This is the inverse of CONNECT for graph construction and controlled
rewiring.  It is deliberately symmetric: no stale back-pointer may remain
at the partner endpoint."
  (let ((partner (enter port)))
    (setf (svref (node-ports (port-node port)) (port-slot port)) nil)
    (when partner
      (setf (svref (node-ports (port-node partner)) (port-slot partner)) nil)))
  (values))

(defun enter (port)
  "Return the PORT wired to PORT (its partner), or NIL."
  (svref (node-ports (port-node port)) (port-slot port)))

(defun p0 (node) (make-port node +slot-principal+))
(defun p1 (node) (make-port node +slot-aux1+))
(defun p2 (node) (make-port node +slot-aux2+))
