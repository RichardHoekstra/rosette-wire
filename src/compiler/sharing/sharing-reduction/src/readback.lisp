;;;; readback.lisp --- net -> TLC term, and net -> sexp geometry.
;;;;
;;;; READBACK.  Given a (normal-form) net, recover a lambda term by walking
;;;; from the root.  We ENTER a port to find the node delivering that value:
;;;;
;;;;   LAM-CON reached via its principal -> (lambda v. <read p2>), and the
;;;;           binder's variable port p1 names a fresh v threaded so that
;;;;           variable occurrences inside the body read back as v.
;;;;   APP-CON reached via its principal -> (<read p0> <read p1>).
;;;;   free-var CON stub                 -> the original variable name.
;;;;   DUP node reached via an aux port  -> transparent: follow its principal
;;;;           (the shared value).  In a fully-normalised closed term the dup
;;;;           fans have been consumed; any residual dup on the read path is a
;;;;           sharing node and we read through it.
;;;;
;;;; We tag each LAM node we descend into with a fresh variable name in a
;;;; visit table, so when read-back reaches the binder's variable port (the
;;;; back-edge) it yields that name.

(in-package #:rosette-sharing-reduction)

(defun %fresh-var (n)
  (intern (format nil "V~D" n) :keyword))

(defun net->term (net)
  "Read back a TLC term from NET (assumed in normal form).

The classic Geometry-of-Interaction readback (Asperti-Guerrini / Mackie).
We walk from the root.  Each step ENTERs a port to reach the (node, slot)
on the other side, carrying a STACK of duplicator-slot choices:

  - LAM via principal : (lambda v. <read body via p2>), v bound to this LAM.
  - LAM via aux1      : a bound-variable occurrence -> v.
  - APP via aux2      : (<read fn via p0> <read arg via p1>).
  - DUP via principal : pop a slot from the stack, exit via that aux
                        (we are coming OUT of the shared value).
  - DUP via aux i     : push i, continue through the principal
                        (we are going TOWARD the shared value).
  - free-var stub     : the original name.

The DUP stack records, for each fan we pass *into* (aux side), which copy
we are; coming back *out* (principal side) we pop it to take the matching
copy -- this is exactly how a shared subterm reads back to its multiple
occurrences without unfolding the sharing in the net."
  (let ((binder-var (make-hash-table :test 'eq))   ; lam-node -> var symbol
        (counter 0))
    (labels
        ((read-from (port stack)
           (let ((q (enter port)))
             (unless q (error "net->term: dangling port"))
             (read-node q stack)))
         (read-node (port stack)
           (let* ((node (port-node port))
                  (slot (port-slot port))
                  (sym (node-sym node))
                  (label (node-label node)))
             (cond
               ((eq sym +era+) (error "net->term: reached ERA in readback"))
               ((eq sym +root+) (error "net->term: reached ROOT in readback"))
               ;; --- free variable stub (inert CON, label >= 1000) ---
               ((and (eq sym +con+) (>= label 1000))
                (tlc-var (or (free-var-name label)
                             (%fresh-var (incf counter)))))
               ;; --- structural CON: lambda OR application, told apart by
               ;;     the PORT we arrive through (GoI polarity) ---
               ((eq sym +con+)
                (cond
                  ;; principal: this CON is a LAMBDA value.
                  ((= slot +slot-principal+)
                   (let ((v (%fresh-var (incf counter))))
                     (setf (gethash node binder-var) v)
                     (tlc-lam v (read-from (p2 node) stack))))
                  ;; aux1: a bound-variable occurrence (the lambda's var port).
                  ((= slot +slot-aux1+)
                   (tlc-var (or (gethash node binder-var)
                                (%fresh-var (incf counter)))))
                  ;; aux2: an APPLICATION result -> (fn arg).
                  (t
                   (tlc-app (read-from (p0 node) stack)
                            (read-from (p1 node) stack)))))
               ;; --- DUP : the sharing fan ---
               ((eq sym +dup+)
                (cond
                  ((= slot +slot-principal+)
                   ;; coming OUT of the shared value: pick the copy on top.
                   (if stack
                       (let ((choice (car stack)))
                         (read-from (if (= choice +slot-aux1+) (p1 node) (p2 node))
                                    (cdr stack)))
                       ;; no choice recorded: default to aux1.
                       (read-from (p1 node) nil)))
                  (t
                   ;; going TOWARD the shared value via aux SLOT: record it.
                   (read-from (p0 node) (cons slot stack)))))
               (t (error "net->term: unexpected node ~S slot ~D" sym slot))))))
      (read-from (net-root net) nil))))

;;; --- geometry: net -> sexp (compose rosette-sexpr-dag) -------------------
;;;
;;; To MEASURE the compact geometry we project the net's value into an
;;; s-expression and compare TREE size (the unfolded term) against DAG size
;;; (hash-consed, rung-2 sharing).  The net itself is already maximally
;;; shared; the unfolded TREE is what naive tree reduction would have built.

(defun net->sexp (net)
  "Project the net's read-back term into a plain s-expression tree."
  (term->sexp (net->term net)))

(defun term->sexp (term)
  (ecase (tlc-term-kind term)
    (:var (tlc-var-name term))
    (:lam (list :lam (tlc-lam-param term) (term->sexp (tlc-lam-body term))))
    (:app (list (term->sexp (tlc-app-fn term))
                (term->sexp (tlc-app-arg term))))))

(defun net-tree-size (net)
  "Tree-node count of the net's read-back term (the UNFOLDED size)."
  (rosette-sexpr-dag:tree-node-count (net->sexp net)))

(defun net-dag-size (net)
  "Hash-consed DAG-node count of the read-back term (the SHARED size)."
  (rosette-sexpr-dag:dag-node-count (net->sexp net)))
