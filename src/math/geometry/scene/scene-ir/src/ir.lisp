;;;; ir.lisp --- the homoiconic IR: reader/printer + validation.
;;;;
;;;; The internal representation IS the s-expr (homoiconic): parse-scene
;;;; validates + canonicalises (structure-copies to a fresh, deterministic
;;;; tree), unparse-scene returns it.  ROUND-TRIP:
;;;;   (parse-scene (unparse-scene s)) == s   on any canonical scene.
;;;;
;;;;   scene ::= (:scene (:size W H) node)
;;;;   node  ::= (:group node*) | (:transform xform node*)
;;;;          |  (:clip rect node*) | (:opacity A1000 node*)
;;;;          |  (:scroll (:offset DX DY) rect node*)
;;;;          |  (:layer NAME (:size W H) node*)
;;;;          |  (:draw op)
;;;;   op    ::= (:clear R G B) | (:line X0 Y0 X1 Y1 R G B THICK A1000)
;;;;          |  (:fill-rect X Y W H R G B A1000)
;;;;          |  (:text X Y SCALE1000 R G B THICK "STR")
;;;;          |  (:blit NAME OX OY A1000)

(in-package #:rosette-scene-ir)

(defparameter *node-heads*
  '(:group :transform :clip :opacity :scroll :layer :draw)
  "Valid property-tree node heads (Face B).")

(defparameter *ops-vocabulary*
  '(:clear :line :fill-rect :text :blit)
  "Paint ops (Face A) -- each reified as a pure (frame)->frame closure.")

(defun scene-p (x)
  "True if X is a (:scene (:size W H) node) form."
  (and (consp x) (eq (car x) :scene)
       (consp (cadr x)) (eq (car (cadr x)) :size)))

(defun scene-size (scene)
  "Return (values W H) for a scene."
  (destructuring-bind (w h) (cdr (cadr scene)) (values w h)))

(defun scene-root (scene)
  "The single root node of a scene (wrapped in an implicit :group if the
scene lists several top-level nodes)."
  (let ((rest (cddr scene)))
    (if (= (length rest) 1) (car rest) (cons :group rest))))

(defun node-head (node)
  "Head keyword of a node form, or NIL."
  (and (consp node) (car node)))

;;; ---- validation + canonical copy ----

(defun %validate-op (op)
  (unless (and (consp op) (member (car op) *ops-vocabulary*))
    (error "rosette-scene-ir: not a paint op: ~S" op))
  (ecase (car op)
    (:clear (assert (= 3 (length (cdr op)))))
    (:line (assert (= 9 (length (cdr op)))))
    (:fill-rect (assert (= 8 (length (cdr op)))))
    (:text (assert (= 8 (length (cdr op))))
           (assert (stringp (nth 8 op))))
    (:blit (assert (= 4 (length (cdr op))))))
  (copy-tree op))

(defun %validate-node (node)
  (unless (consp node)
    (error "rosette-scene-ir: not a node: ~S" node))
  (case (car node)
    (:draw (list :draw (%validate-op (cadr node))))
    (:transform (list* :transform (copy-tree (cadr node))
                       (mapcar #'%validate-node (cddr node))))
    (:clip (list* :clip (copy-tree (cadr node))
                  (mapcar #'%validate-node (cddr node))))
    (:opacity (list* :opacity (cadr node)
                     (mapcar #'%validate-node (cddr node))))
    (:scroll (list* :scroll (copy-tree (cadr node)) (copy-tree (caddr node))
                    (mapcar #'%validate-node (cdddr node))))
    (:layer (list* :layer (cadr node) (copy-tree (caddr node))
                   (mapcar #'%validate-node (cdddr node))))
    (:group (cons :group (mapcar #'%validate-node (cdr node))))
    (t (error "rosette-scene-ir: unknown node head ~S" (car node)))))

(defun parse-scene (form)
  "Validate FORM as a scene and return a canonical (fresh) copy.
Idempotent: (parse-scene (parse-scene s)) == (parse-scene s)."
  (unless (scene-p form)
    (error "rosette-scene-ir: not a (:scene (:size W H) ...) form: ~S" form))
  (multiple-value-bind (w h) (scene-size form)
    (list :scene (list :size w h)
          (%validate-node (scene-root form)))))

(defun unparse-scene (scene)
  "Return the canonical s-expr of SCENE (identity on a parsed scene)."
  scene)
