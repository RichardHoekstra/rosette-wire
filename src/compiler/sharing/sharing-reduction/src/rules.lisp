;;;; rules.lisp --- local active-pair rewrite rules.
;;;;
;;;; An ACTIVE PAIR is a wire joining the principal ports of two nodes a, b.
;;;; The interaction-combinator rewrite is LOCAL (touches only a, b and their
;;;; six aux wires) and there are exactly two cases:
;;;;
;;;;   ANNIHILATION  (same symbol, same label):
;;;;       a.aux1 -- b.aux? : the two nodes vanish and their auxiliary ports
;;;;       are cross-wired.  For two binary nodes CON~CON / DUP~DUP this wires
;;;;       a1-b1 and a2-b2 (a "straight-through" identity); for ERA~ERA both
;;;;       just disappear.  This is beta-reduction (CON lambda meets CON app)
;;;;       and the collapse of a fan meeting its matching fan.
;;;;
;;;;   COMMUTATION   (different symbol OR different label):
;;;;       each node is DUPLICATED through the other: 2x2 fresh nodes wired in
;;;;       a complete bipartite pattern.  This is how a DUP pushes PAST a CON
;;;;       (sharing propagates lazily), and how distinct-colour fans cross.
;;;;       ERA commuting with a binary node erases both its aux subnets.
;;;;
;;;; STRONG CONFLUENCE.  Each rule consumes its active pair and only rewires
;;;; the immediate neighbourhood; distinct active pairs touch disjoint nodes,
;;;; so any two are independent.  Hence the system is strongly confluent and
;;;; the order of contraction (sequential, or all-at-once in parallel) cannot
;;;; change the normal form -- the intrinsic-parallel / GPU-ready property.

(in-package #:rosette-sharing-reduction)

(defun %connect-if (pa pb)
  "Connect PA and PB when both endpoints exist; NIL is a free boundary."
  (when (and pa pb)
    (connect pa pb))
  (values))

(defun binary-symbol-p (sym)
  (or (eq sym +con+) (eq sym +dup+)))

(defun same-kind-p (a b)
  "Do A and B annihilate (T) or commute (NIL)?"
  (and (eq (node-sym a) (node-sym b))
       (= (node-label a) (node-label b))))

(defun %external (port a b)
  "Resolve PORT to its eventual EXTERNAL endpoint, chasing wires that loop
back through the two nodes A,B currently being annihilated.

The straight-through identifies a.aux_i with b.aux_i.  If PORT lands on an
aux slot of A (slot s), the identification sends it to B's slot s, whose
partner we then chase; symmetrically for B.  A principal slot or any node
other than A/B is genuinely external and returned as-is.  Terminates
because each hop strictly alternates A<->B and the auxiliary wiring of a
single redex cannot cycle without an external anchor."
  (loop
    (unless port (return nil))
    (let ((node (port-node port)) (slot (port-slot port)))
      (cond
        ((and (eq node a) (/= slot +slot-principal+))
         (setf port (enter (make-port b slot))))
        ((and (eq node b) (/= slot +slot-principal+))
         (setf port (enter (make-port a slot))))
        (t (return port))))))

(defun %annihilate (net a b)
  "Same-symbol, same-label active pair: cross-wire aux ports, free a,b.

Loops through A/B (e.g. the identity lambda whose body IS its variable)
are resolved by %EXTERNAL so the two genuine external endpoints are wired
directly and nothing dangles into a freed node."
  (when (and (binary-symbol-p (node-sym a)) (binary-symbol-p (node-sym b)))
    ;; straight-through: external(a.p1)<->external(b.p1),
    ;;                   external(a.p2)<->external(b.p2).
    (let ((e-a1 (%external (enter (p1 a)) a b))
          (e-b1 (%external (enter (p1 b)) a b))
          (e-a2 (%external (enter (p2 a)) a b))
          (e-b2 (%external (enter (p2 b)) a b)))
      (%connect-if e-a1 e-b1)
      (%connect-if e-a2 e-b2)))
  ;; ERA~ERA: nothing to rewire.
  (free-node net a)
  (free-node net b))

(defun %commute (net a b)
  "Different active pair: duplicate each node through the other.

For two binary nodes this produces 2x2 fresh nodes (a-copies carry A's
symbol/label, b-copies carry B's), cross-wired completely; the external
aux ports of a,b are rewired onto the new nodes.  ERA against a binary
node erases the binary node's two subnets."
  (let ((sa (node-sym a)) (sb (node-sym b)))
    (cond
      ;; ERA vs binary: erase both subnets of the binary node.
      ((and (eq sa +era+) (binary-symbol-p sb))
       (%era-binary net a b))
      ((and (eq sb +era+) (binary-symbol-p sa))
       (%era-binary net b a))
      ;; ERA vs ERA handled by annihilation; this branch is binary~binary.
      ((and (binary-symbol-p sa) (binary-symbol-p sb))
       (let* ((la (node-label a)) (lb (node-label b))
              ;; B-copies (the symbol of B), one per aux port of A.
              (b1 (alloc-node net sb lb))
              (b2 (alloc-node net sb lb))
              ;; A-copies (the symbol of A), one per aux port of B.
              (a1 (alloc-node net sa la))
              (a2 (alloc-node net sa la))
              ;; A's auxiliary ports are copied into B1/B2 principals;
              ;; B's auxiliary ports are copied into A1/A2 principals.
              ;; Keep the old ports as keys so internal self-wires can be
              ;; remapped before A and B are freed.
              (port-map (list (cons (p1 a) (p0 b1))
                              (cons (p2 a) (p0 b2))
                              (cons (p1 b) (p0 a1))
                              (cons (p2 b) (p0 a2)))))
         ;; An auxiliary wire that leaves the active pair is redirected to
         ;; the corresponding fresh principal.  An auxiliary wire whose two
         ;; endpoints are both on A/B is a local loop; redirect both ends to
         ;; their mapped principals.  Treating such a loop as an external
         ;; endpoint leaves a fresh principal pointing into a freed node.
         (labels ((same-port-p (left right)
                  (and (eq (port-node left) (port-node right))
                       (= (port-slot left) (port-slot right))))
                (mapped-port (port)
                  (cdr (find-if
                        (lambda (entry)
                          (same-port-p port (car entry)))
                        port-map))))
           (dolist (entry port-map)
             (let* ((old (car entry))
                    (fresh (cdr entry))
                    (partner (enter old))
                    (fresh-partner (and partner (mapped-port partner))))
               (when partner
                 (if fresh-partner
                     (connect fresh fresh-partner)
                     (%connect-if partner fresh))))))
         ;; complete bipartite cross-wiring of the new aux ports.
         (connect (p1 a1) (p1 b1))
         (connect (p2 a1) (p1 b2))
         (connect (p1 a2) (p2 b1))
         (connect (p2 a2) (p2 b2))
         (free-node net a)
         (free-node net b)))
      (t (error "unhandled commutation: ~S ~S" sa sb)))))

(defun %era-binary (net era bin)
  "ERA against binary node BIN: BIN disappears, two fresh ERA nodes are
attached to BIN's two auxiliary subnets."
  (let ((e1 (alloc-node net +era+))
        (e2 (alloc-node net +era+))
        (x1 (enter (p1 bin)))
        (x2 (enter (p2 bin))))
    (%connect-if x1 (p0 e1))
    (%connect-if x2 (p0 e2))
    (free-node net era)
    (free-node net bin)))

(defun apply-rule (net a b)
  "Rewrite the active pair (A principal -- B principal).  Returns :ANN or
  :COM.  Increments the interaction/rewrite counters."
  (incf (net-interactions net))
  (incf (net-rewrites net))
  (let ((kind (if (or (and (eq (node-sym a) +era+)
                          (eq (node-sym b) +era+))
                      (same-kind-p a b))
                  :ann
                  :com)))
    (push (list kind
                (node-sym a) (node-label a)
                (node-sym b) (node-label b))
          (net-trace-events net))
    (ecase kind
      (:ann
       (%annihilate net a b)
       (incf (net-annihilations net)))
      (:com
       (%commute net a b)
       (incf (net-commutations net))))
    kind))
