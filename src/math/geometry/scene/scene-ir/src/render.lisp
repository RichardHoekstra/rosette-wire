;;;; render.lisp --- the executors + the promoted seL4 compositor.
;;;;
;;;;   render-node/render-scene  : DIRECT executor (pre-order z-order walk).
;;;;   flatten-scene             : node -> flat op stream (translate baked,
;;;;                               clips as :clip-push/:clip-pop control ops).
;;;;   render-display-list       : the FLAT executor (== render-node . flatten
;;;;                               on the group/transform/clip/draw subset).
;;;;   flatten-to-layers/compose-layers : the LAYERED executor -- each
;;;;                               top-level surface -> its own frame, then
;;;;                               fb-blit onto the scanout in z-order (the
;;;;                               promoted seL4 fb_blit compositor).
;;;;
;;;; INVARIANTS (gated byte-exact in tests):
;;;;   FLATTEN-PARITY : render-node(n) == render-display-list(flatten-scene n)
;;;;   DIRECT==LAYERED: render-scene(s) == compose-layers(flatten-to-layers s)
;;;;
;;;; NOTE (stated deviation from the design's example): layer PLACEMENT and
;;;; OPACITY come from the accumulated ancestor context (a (:transform
;;;; (:translate ox oy) ...) / (:opacity a ...) WRAPPING the (:layer ...)),
;;;; not from children inside the layer.  This makes accumulated-context
;;;; placement unambiguous and makes DIRECT==LAYERED provable by construction
;;;; (render-scene and compose-layers run the identical sub-render + fb-blit).

(in-package #:rosette-scene-ir)

;;; ---- coordinate + alpha transport ----

(defun %xf-op (op xf)
  "Transform an op's coordinates by affine XF.  Integer-translate is exact
(the byte-exact path); other affines transform explicit coord pairs and are
OUT of the parity laws."
  (multiple-value-bind (dx dy) (%translate-delta xf)
    (flet ((tp (x y)
             (if dx (values (+ x dx) (+ y dy))
                 (multiple-value-bind (x2 y2) (xform-apply xf x y)
                   (values (round x2) (round y2))))))
      (ecase (car op)
        (:clear (copy-list op))
        (:line (destructuring-bind (x0 y0 x1 y1 r g b th a) (cdr op)
                 (multiple-value-bind (nx0 ny0) (tp x0 y0)
                   (multiple-value-bind (nx1 ny1) (tp x1 y1)
                     (list :line nx0 ny0 nx1 ny1 r g b th a)))))
        (:fill-rect (destructuring-bind (x y w h r g b a) (cdr op)
                      (multiple-value-bind (nx ny) (tp x y)
                        (list :fill-rect nx ny w h r g b a))))
        (:text (destructuring-bind (x y s r g b th str) (cdr op)
                 (multiple-value-bind (nx ny) (tp x y)
                   (list :text nx ny s r g b th str))))
        (:blit (destructuring-bind (name ox oy a) (cdr op)
                 (multiple-value-bind (nx ny) (tp ox oy)
                   (list :blit name nx ny a))))))))

(defun %scale-alpha (op op1000)
  "Scale an op's A1000 alpha by OP1000/1000 (accumulated opacity)."
  (if (>= op1000 1000)
      op
      (ecase (car op)
        (:clear op)                     ; clear has no alpha
        (:line (let ((c (copy-list op)))
                 (setf (nth 9 c) (round (* (nth 9 op) op1000) 1000)) c))
        (:fill-rect (let ((c (copy-list op)))
                      (setf (nth 8 c) (round (* (nth 8 op) op1000) 1000)) c))
        (:text (let ((c (copy-list op))) c)) ; text alpha is opaque strokes
        (:blit (let ((c (copy-list op)))
                 (setf (nth 4 c) (round (* (nth 4 op) op1000) 1000)) c)))))

(defun %clip-device-rect (rect xf)
  "Map a local (:rect x y w h) to a device (x y w h) under XF (translate
exact; general xf transforms the origin and keeps extent)."
  (destructuring-bind (x y w h) (if (eq (car rect) :rect) (cdr rect) rect)
    (multiple-value-bind (dx dy) (%translate-delta xf)
      (if dx (list (+ x dx) (+ y dy) w h)
          (multiple-value-bind (nx ny) (xform-apply xf x y)
            (list (round nx) (round ny) w h))))))

;;; ---- the layer sub-frame (shared by DIRECT + LAYERED so they agree) ----

(defun %layer-subframe (layer-node &key surfaces)
  "Render a (:layer NAME (:size W H) child*) into its OWN fresh W x H opaque
buffer (fully flattened, opacity reset).  Returns the frame."
  (destructuring-bind (name size &rest children) (cdr layer-node)
    (declare (ignore name))
    (destructuring-bind (w h) (cdr size)
      (let ((sub (make-frame w h)))
        (dolist (c children)
          (render-node c sub :xf (xform-identity) :op1000 1000
                             :clip (list 0 0 w h) :surfaces surfaces))
        sub))))

;;; ---- DIRECT executor ----

(defun render-node (node frame &key (xf (xform-identity)) (op1000 1000)
                                    clip surfaces)
  "Execute NODE onto FRAME under accumulated affine XF, opacity OP1000
(0..1000) and CLIP rect (device (x y w h) or NIL).  Pre-order = painter's
algorithm (later siblings SRC_OVER over earlier)."
  (ecase (car node)
    (:draw
     (%draw-op frame (%scale-alpha (%xf-op (cadr node) xf) op1000)
               :clip clip :surfaces surfaces))
    (:group
     (dolist (c (cdr node))
       (render-node c frame :xf xf :op1000 op1000 :clip clip :surfaces surfaces)))
    (:transform
     (let ((xf2 (xform-compose (cadr node) xf)))
       (dolist (c (cddr node))
         (render-node c frame :xf xf2 :op1000 op1000 :clip clip
                             :surfaces surfaces))))
    (:clip
     (let* ((dev (%clip-device-rect (cadr node) xf))
            (clip2 (if clip (clip-intersect clip dev) dev)))
       (dolist (c (cddr node))
         (render-node c frame :xf xf :op1000 op1000 :clip clip2
                             :surfaces surfaces))))
    (:opacity
     (let ((op2 (round (* op1000 (cadr node)) 1000)))
       (dolist (c (cddr node))
         (render-node c frame :xf xf :op1000 op2 :clip clip
                             :surfaces surfaces))))
    (:scroll
     ;; (:scroll (:offset dx dy) rect child*) ==
     ;;   (:clip rect (:transform (:translate -dx -dy) child*))
     (destructuring-bind (off rect &rest children) (cdr node)
       (destructuring-bind (dx dy) (cdr off)
         (render-node
          (list* :clip rect
                 (list (list* :transform (xform-translate (- dx) (- dy))
                              children)))
          frame :xf xf :op1000 op1000 :clip clip :surfaces surfaces))))
    (:layer
     (let ((sub (%layer-subframe node :surfaces surfaces)))
       (multiple-value-bind (dx dy) (%translate-delta xf)
         (multiple-value-bind (ox oy)
             (if dx (values dx dy)
                 (multiple-value-bind (x y) (xform-apply xf 0 0)
                   (values (round x) (round y))))
           ;; fb-blit honours CLIP by drawing into a copy then copying back
           (if clip
               (let ((scratch (copy-frame frame)))
                 (fb-blit scratch sub ox oy op1000)
                 (%copy-region frame scratch clip))
               (fb-blit frame sub ox oy op1000))))))
    )
  frame)

(defun %copy-region (dst src clip)
  "Copy the pixels of SRC inside CLIP (x y w h) into DST."
  (destructuring-bind (cx cy cw ch) clip
    (let* ((w (frame-width dst)) (h (frame-height dst))
           (dp (frame-pixels dst)) (sp (frame-pixels src)))
      (loop for y from (max 0 cy) below (min h (+ cy ch)) do
        (loop for x from (max 0 cx) below (min w (+ cx cw)) do
          (let ((idx (* 3 (+ x (* y w)))))
            (setf (aref dp idx) (aref sp idx)
                  (aref dp (+ idx 1)) (aref sp (+ idx 1))
                  (aref dp (+ idx 2)) (aref sp (+ idx 2)))))))))

(defun render-scene (scene &key surfaces)
  "Render a (:scene (:size W H) node) to a fresh frame via the DIRECT
executor."
  (let ((s (if (scene-p scene) scene (parse-scene scene))))
    (multiple-value-bind (w h) (scene-size s)
      (let ((frame (make-frame w h)))
        (render-node (scene-root s) frame :surfaces surfaces)
        frame))))

;;; ---- FLAT executor + flatten (immediate-mode subset) ----

(defun flatten-scene (scene-or-node &key (xf (xform-identity)) (op1000 1000))
  "Fold a group/transform/clip/draw/opacity tree into a FLAT op stream:
translate baked into coords, opacity folded into alpha, clips emitted as
(:clip-push rect)/(:clip-pop) control ops.  Errors on :layer (use the
compositor path).  This is the Face-B -> Face-A projection."
  (let ((node (if (scene-p scene-or-node) (scene-root scene-or-node)
                  scene-or-node)))
    (ecase (car node)
      (:draw (list (%scale-alpha (%xf-op (cadr node) xf) op1000)))
      (:group (mapcan (lambda (c) (flatten-scene c :xf xf :op1000 op1000))
                      (cdr node)))
      (:transform (let ((xf2 (xform-compose (cadr node) xf)))
                    (mapcan (lambda (c) (flatten-scene c :xf xf2 :op1000 op1000))
                            (cddr node))))
      (:opacity (let ((op2 (round (* op1000 (cadr node)) 1000)))
                  (mapcan (lambda (c) (flatten-scene c :xf xf :op1000 op2))
                          (cddr node))))
      (:clip (let ((dev (%clip-device-rect (cadr node) xf)))
               (append (list (list :clip-push dev))
                       (mapcan (lambda (c) (flatten-scene c :xf xf :op1000 op1000))
                               (cddr node))
                       (list (list :clip-pop)))))
      (:scroll (destructuring-bind (off rect &rest children) (cdr node)
                 (destructuring-bind (dx dy) (cdr off)
                   (flatten-scene
                    (list* :clip rect
                           (list (list* :transform
                                        (xform-translate (- dx) (- dy))
                                        children)))
                    :xf xf :op1000 op1000)))))))

(defun render-display-list (ops frame &key surfaces)
  "The FLAT executor: run a flat op stream (paint ops + :clip-push/:clip-pop
control ops) onto FRAME, maintaining a clip stack.  Equal to
render-node . flatten-scene on the immediate-mode subset."
  (let ((clip nil) (stack '()))
    (dolist (op ops frame)
      (case (car op)
        (:clip-push (push clip stack)
         (setf clip (if clip (clip-intersect clip (cadr op)) (cadr op))))
        (:clip-pop (setf clip (pop stack)))
        (t (%draw-op frame op :clip clip :surfaces surfaces))))))

;;; ---- LAYERED executor (the promoted seL4 compositor) ----

(defun flatten-to-layers (scene &key surfaces)
  "Walk a scene's top-level nodes and, for each (:layer ...), collect
(NAME sub-frame OX OY A1000) by descending through the (:transform
(:translate ...)) / (:opacity ...) wrappers.  The sub-frame is rendered by
the SAME %layer-subframe render-node uses, so compose-layers reproduces
render-scene byte-for-byte."
  (let* ((s (if (scene-p scene) scene (parse-scene scene)))
         (root (scene-root s))
         (tops (if (eq (car root) :group) (cdr root) (list root)))
         (out '()))
    (labels ((walk (node ox oy a1000)
               (ecase (car node)
                 (:layer
                  (push (list (cadr node) (%layer-subframe node :surfaces surfaces)
                              ox oy a1000)
                        out))
                 (:transform
                  (multiple-value-bind (dx dy) (%translate-delta (cadr node))
                    (unless dx (error "flatten-to-layers: non-translate xform"))
                    (dolist (c (cddr node)) (walk c (+ ox dx) (+ oy dy) a1000))))
                 (:opacity
                  (let ((a2 (round (* a1000 (cadr node)) 1000)))
                    (dolist (c (cddr node)) (walk c ox oy a2))))
                 (:group
                  (dolist (c (cdr node)) (walk c ox oy a1000))))))
      (dolist (n tops) (walk n 0 0 1000)))
    (nreverse out)))

(defun compose-layers (layers w h)
  "Composite a list of (NAME sub-frame OX OY A1000) surfaces onto a fresh
W x H black scanout, in z-order (list order = painter's algorithm), via
fb-blit == repeated rosette-raster-core:set-pixel-blend."
  (let ((scan (make-frame w h)))
    (dolist (L layers scan)
      (destructuring-bind (name sub ox oy a1000) L
        (declare (ignore name))
        (fb-blit scan sub ox oy a1000)))))
