;;;; lambda.lisp --- lambda-term <-> interaction-net encoding + read-back.
;;;;
;;;; Encoding (Lamping/Asperti optimal reduction, the HVM convention).
;;;; Both abstraction and application are the binary constructor +CON+;
;;;; a beta redex is a CON(lam)~CON(app) ANNIHILATION on their principal
;;;; ports.  Slot convention:
;;;;
;;;;   LAM node:  p0 = the function value (faces whoever applies it)
;;;;              p1 = the bound variable's single use-port
;;;;              p2 = the body's root
;;;;
;;;;   APP node:  p0 = the function position (faces the LAM it applies)
;;;;              p1 = the argument's root
;;;;              p2 = the application's result (faces the continuation)
;;;;
;;;; Annihilation of LAM~APP wires lam.p1<->app.p1 (x := arg) and
;;;; lam.p2<->app.p2 (body := result) -- exactly beta, with sharing
;;;; preserved because the argument subnet is POINTED AT, never copied.
;;;;
;;;; A variable used k>1 times is fanned by a tree of +DUP+ nodes; a
;;;; variable used 0 times is closed by an +ERA+.  Each binder gets a
;;;; FRESH dup colour so independent fans never spuriously annihilate.
;;;;
;;;; We reuse the TLC-TERM type from rosette-foundation-rewrite as the source
;;;; AST (rung: the rewrite kernel), so a net reduction can be checked
;;;; against that library's reference NORMALIZE.

(in-package #:rosette-sharing-reduction)

;;; --- CON-label conventions ------------------------------------------
;;; CRUCIAL: a lambda and an application are the SAME interaction
;;; combinator -- both +CON+ with the SAME structural label +STRUCT-LABEL+
;;; -- so that a beta redex (lambda meets application on their principals)
;;; ANNIHILATES.  Read-back tells lambda from application not by a label
;;; but by the PORT it is reached through (principal = lambda value, aux2 =
;;; application result) -- the Geometry-of-Interaction polarity.
;;;
;;; Free variables get stable labels >= 1000 so their inert stubs never
;;; annihilate with structural CONs (only meaningful for OPEN terms; all
;;; certificates use closed terms).

(defconstant +struct-label+ 0
  "The single structural-CON label shared by lambda and application.")
;; Back-compat aliases used by the compiler/readback (all the same label
;; now -- lambda/application are distinguished by traversal polarity).
(defconstant +lam-label+ +struct-label+)
(defconstant +app-label+ +struct-label+)
(defconstant +root-label+ 2)

(defvar *free-var-labels* (make-hash-table :test 'eq))
(defvar *free-var-next* 1000)
(defun free-var-label (name)
  "Stable integer label for a free-variable stub (>=1000, distinct from
+lam-label+/+app-label+/+root-label+)."
  (or (gethash name *free-var-labels*)
      (setf (gethash name *free-var-labels*)
            (incf *free-var-next*))))
(defun free-var-name (label)
  (maphash (lambda (k v) (when (= v label) (return-from free-var-name k)))
           *free-var-labels*)
  nil)

(defvar *dup-colour* 0)
(defun next-colour () (incf *dup-colour*))

;;; --- compile TLC term -> net ----------------------------------------
;;;
;;; We thread an ENV mapping each bound variable name to a list of "demand"
;;; ports that want that variable's value.  When a binder closes, we wire its
;;; p1 to a DUP-tree fanning out to all demands (or ERA if none).

(defun %fan-out (net target-port demand-ports colour)
  "Wire TARGET-PORT to all DEMAND-PORTS, inserting a balanced DUP tree
when there is more than one demand, an ERA when there are none."
  (let ((n (length demand-ports)))
    (cond
      ((= n 0)
       (let ((e (alloc-node net +era+)))
         (connect target-port (p0 e))))
      ((= n 1)
       (connect target-port (first demand-ports)))
      (t
       ;; Build a left-leaning tree of DUP nodes (all same colour: copies
       ;; of one binder's value are interchangeable).
       (let ((d (alloc-node net +dup+ colour)))
         (connect target-port (p0 d))
         (%fan-out net (p1 d) (list (first demand-ports)) colour)
         (%fan-out net (p2 d) (rest demand-ports) colour))))))

(defun %compile (net term env out-port)
  "Compile TERM so that its value is delivered to OUT-PORT.  ENV maps a
bound-variable name to a CONS cell whose CAR accumulates demand ports.
Returns nothing; side-effects the net."
  (ecase (tlc-term-kind term)
    (:var
     (let* ((name (tlc-var-name term))
            (cell (cdr (assoc name env :test #'eq))))
       (if cell
           ;; a bound variable: register OUT-PORT as a demand.
           (push out-port (car cell))
           ;; a free variable: materialise a fresh CON stub labelled by name.
           (let ((stub (alloc-node net +con+ (free-var-label name))))
             (connect out-port (p0 stub))))))
    (:lam
     (let* ((param (tlc-lam-param term))
            (cell (cons nil nil))      ; (demands . _)
            (lam (alloc-node net +con+ +lam-label+)))
       (connect out-port (p0 lam))
       ;; body delivers its value to lam.p2
       (%compile net (tlc-lam-body term)
                 (acons param cell env)
                 (p2 lam))
       ;; close the binder: fan lam.p1 to all demands collected.
       (%fan-out net (p1 lam) (car cell) (next-colour))))
    (:app
     (let* ((app (alloc-node net +con+ +app-label+)))
       ;; the function value faces app.p0 (principal): compile FN to deliver
       ;; its value into app's principal port.
       (%compile net (tlc-app-fn term) env (p0 app))
       ;; the argument delivers to app.p1.
       (%compile net (tlc-app-arg term) env (p1 app))
       ;; the result of the application is app.p2 -> OUT-PORT.
       (connect out-port (p2 app))))))

(defun lam->net (term)
  "Compile TLC TERM into a fresh net.  The net's ROOT port receives the
term's value.  Returns the net."
  (let* ((*dup-colour* 0)
         (net (make-net))
         ;; ROOT is an inert anchor (no interaction rule names +ROOT+).  The
         ;; term's value is delivered to its AUXILIARY port p1, so the value's
         ;; principal faces an aux port and never forms an active pair against
         ;; the root.  Read-back starts at this port.
         (root (alloc-node net +root+)))
    (setf (net-root net) (p1 root))
    (%compile net term nil (p1 root))
    net))
