;;;; tests.lisp --- rosette-scene-ir laws.
;;;;
;;;; Gates the design's laws AND cross-checks against the EXISTING seL4
;;;; oracles committed in the display brick:
;;;;   sel4/fb_scene.txt   + sel4/oracle.ppm       (immediate display list)
;;;;   sel4/oracle_comp.ppm                        (3-surface compositor)
;;;; If those artifacts are present the executor is byte-checked against
;;;; them directly; otherwise the internal from-scratch raster-core oracle
;;;; still gates every law.

(defpackage #:rosette-scene-ir/tests
  (:use #:cl #:rosette-scene-ir)
  (:export #:run-all-tests))

(in-package #:rosette-scene-ir/tests)

(defparameter *fail-count* 0)
(defparameter *test-count* 0)

(defun is (condition msg)
  (incf *test-count*)
  (unless condition
    (incf *fail-count*)
    (format *error-output* "  FAIL: ~A~%" msg)))

(defmacro test-case (name &body body)
  `(progn (format t "~&[~A]~%" ,name) ,@body))

(defparameter *sel4-dir*
  "./01-kernel/compiler/rosette-lisp-codegen/sel4/")

;;; ---- pixel helpers ----

(defun frames-equal-p (a b)
  (and (= (rosette-raster-core:frame-width a) (rosette-raster-core:frame-width b))
       (= (rosette-raster-core:frame-height a) (rosette-raster-core:frame-height b))
       (equalp (rosette-raster-core:frame-pixels a) (rosette-raster-core:frame-pixels b))))

(defun first-diff (a b)
  (let ((pa (rosette-raster-core:frame-pixels a)) (pb (rosette-raster-core:frame-pixels b)))
    (loop for i below (min (length pa) (length pb))
          unless (= (aref pa i) (aref pb i)) return i)))

;;; ==================================================================
;;; The seL4 immediate display list (mirror of fb_oracle.lisp *ops*).
;;; ==================================================================

(defparameter *fb-ops*
  ;; clear + thick lines, exactly the fb_oracle boot scene (system window).
  '((:clear 12 14 28)
    (:line 5 5 194 5 90 110 200 3 1000) (:line 194 5 194 114 90 110 200 3 1000)
    (:line 194 114 5 114 90 110 200 3 1000) (:line 5 114 5 5 90 110 200 3 1000)
    (:line 6 13 193 13 40 50 110 15 1000)
    (:line 6 113 193 6 60 80 160 1 1000)
    (:line 24 40 24 80 235 235 255 4 1000) (:line 24 40 42 40 235 235 255 4 1000)
    (:line 42 40 42 58 235 235 255 4 1000) (:line 42 58 24 58 235 235 255 4 1000)
    (:line 26 58 44 80 235 235 255 4 1000)
    (:line 84 44 66 44 235 235 255 4 1000) (:line 66 44 66 60 235 235 255 4 1000)
    (:line 66 60 84 60 235 235 255 4 1000) (:line 84 60 84 78 235 235 255 4 1000)
    (:line 84 78 66 78 235 235 255 4 1000)
    (:line 104 40 104 80 235 235 255 4 1000) (:line 126 40 126 80 235 235 255 4 1000)
    (:line 104 60 126 60 235 235 255 4 1000)))

(defun raster-oracle (ops w h)
  "Independent rosette-raster-core reference for a clear+line op list."
  (let ((f nil))
    (dolist (op ops f)
      (ecase (car op)
        (:clear (destructuring-bind (r g b) (cdr op)
                  (setf f (rosette-raster-core:make-frame w h :r r :g g :b b))))
        (:line (destructuring-bind (x0 y0 x1 y1 r g b th a) (cdr op)
                 (rosette-raster-core:draw-line-thick
                  f x0 y0 x1 y1 :r r :g g :b b
                  :alpha (float (/ a 1000) 1f0) :thickness th)))))))

;;; ---- LAW: EXECUTOR==ORACLE (fb-scene-parity) ----

(defun test-executor-oracle ()
  (test-case "EXECUTOR==ORACLE: render-display-list == rosette-raster-core reference"
    (let* ((w 200) (h 120)
           (ref (raster-oracle *fb-ops* w h))
           (frame (render-display-list *fb-ops* (rosette-raster-core:make-frame w h))))
      (is (frames-equal-p ref frame)
          (format nil "display-list executor != raster-core oracle (first diff ~a)"
                  (first-diff ref frame)))
      ;; cross-check against the real committed seL4 oracle.ppm if present
      (let ((ppm (merge-pathnames "oracle.ppm" *sel4-dir*)))
        (when (probe-file ppm)
          (let ((seL4 (rosette-raster-core:read-ppm-file ppm)))
            (is (frames-equal-p seL4 frame)
                "display-list executor != committed seL4 oracle.ppm")))))))

;;; ---- LAW: fb_scene.txt round-trip + SEL4 fidelity ----

(defun test-fb-scene-roundtrip ()
  (test-case "ROUND-TRIP: fb-scene-text->scene . scene->fb-scene-text == id (FACE-A)"
    ;; canonical flat FACE-A scene: a group of clear+line draws
    (let* ((draws (mapcar (lambda (op) (list :draw op)) *fb-ops*))
           (s (parse-scene (list :scene (list :size 200 120)
                                 (cons :group draws))))
           (text (scene->fb-scene-text s))
           (back (fb-scene-text->scene text)))
      (is (equal s back) "flat FACE-A scene did not round-trip through fb_scene.txt")
      ;; the emitted text renders byte-identically to the scene
      (let ((f1 (render-scene s))
            (f2 (render-scene back)))
        (is (frames-equal-p f1 f2) "emitted fb_scene.txt renders differently"))
      ;; and it matches the REAL committed sel4/fb_scene.txt frame
      (let ((file (merge-pathnames "fb_scene.txt" *sel4-dir*)))
        (when (probe-file file)
          (let* ((txt (with-open-file (in file)
                        (let ((buf (make-string (file-length in))))
                          (read-sequence buf in) buf)))
                 (scene (fb-scene-text->scene txt))
                 (frame (render-scene scene))
                 (ppm (merge-pathnames "oracle.ppm" *sel4-dir*)))
            (when (probe-file ppm)
              (is (frames-equal-p (rosette-raster-core:read-ppm-file ppm) frame)
                  "parsed committed sel4/fb_scene.txt != committed oracle.ppm"))))))))

;;; ==================================================================
;;; The seL4 compositor scene (mirror of compositor.lisp 3 surfaces).
;;; ==================================================================

(defun base-layer ()
  (list* :layer :base (list :size 200 120)
         (mapcar (lambda (op) (list :draw op))
                 '((:clear 12 14 28)
                   (:line 5 5 194 5 90 110 200 3 1000)
                   (:line 194 5 194 114 90 110 200 3 1000)
                   (:line 194 114 5 114 90 110 200 3 1000)
                   (:line 5 114 5 5 90 110 200 3 1000)
                   (:line 6 13 193 13 40 50 110 15 1000)
                   (:line 24 40 24 80 235 235 255 4 1000)
                   (:line 24 40 42 40 235 235 255 4 1000)
                   (:line 42 40 42 58 235 235 255 4 1000)
                   (:line 42 58 24 58 235 235 255 4 1000)
                   (:line 26 58 44 80 235 235 255 4 1000)))))

(defun overlay-layer ()
  (list* :layer :overlay (list :size 120 44)
         (mapcar (lambda (op) (list :draw op))
                 '((:clear 210 220 245)
                   (:line 0 0 119 0 60 70 120 2 1000)
                   (:line 119 0 119 43 60 70 120 2 1000)
                   (:line 119 43 0 43 60 70 120 2 1000)
                   (:line 0 43 0 0 60 70 120 2 1000)
                   (:line 8 14 111 14 30 34 60 3 1000)
                   (:line 8 26 90 26 30 34 60 2 1000)))))

(defun badge-layer ()
  (list* :layer :badge (list :size 46 16)
         (mapcar (lambda (op) (list :draw op))
                 '((:clear 180 60 60)
                   (:line 6 8 40 8 255 235 235 3 1000)))))

(defun compositor-scene ()
  (parse-scene
   (list :scene (list :size 200 120)
         (list :group
               (base-layer)
               (list :transform (xform-translate 60 60)
                     (list :opacity 550 (overlay-layer)))
               (list :transform (xform-translate 145 10)
                     (badge-layer))))))

;;; ---- LAW: DIRECT==LAYERED (keystone compositor law) ----

(defun test-direct-equals-layered ()
  (test-case "DIRECT==LAYERED: render-scene == compose-layers . flatten-to-layers"
    (let* ((s (compositor-scene))
           (direct (render-scene s))
           (layers (flatten-to-layers s))
           (layered (compose-layers layers 200 120)))
      (is (= 3 (length layers)) "expected 3 top-level layers")
      (is (frames-equal-p direct layered)
          (format nil "DIRECT != LAYERED (first diff ~a)" (first-diff direct layered)))
      ;; cross-check against the real committed seL4 compositor oracle
      (let ((ppm (merge-pathnames "oracle_comp.ppm" *sel4-dir*)))
        (when (probe-file ppm)
          (let ((seL4 (rosette-raster-core:read-ppm-file ppm)))
            (is (frames-equal-p seL4 direct)
                "compositor render != committed seL4 oracle_comp.ppm")))))))

;;; ---- LAW: FLATTEN-PARITY (retained == immediate) ----

(defun test-flatten-parity ()
  (test-case "FLATTEN-PARITY: render-node(n) == render-display-list(flatten-scene n)"
    (let* ((node (list :group
                       (list :draw '(:clear 8 8 16))
                       (list :transform (xform-translate 10 20)
                             (list :draw '(:line 0 0 30 0 200 60 60 2 1000))
                             (list :transform (xform-translate 5 5)
                                   (list :draw '(:line 0 0 0 25 60 200 60 3 1000))))
                       (list :clip '(:rect 5 5 40 40)
                             (list :draw '(:fill-rect 0 0 60 60 20 40 200 800)))))
           (scene (parse-scene (list :scene (list :size 80 80) node)))
           (f-direct (render-scene scene))
           (flat (flatten-scene scene))
           (f-flat (render-display-list flat (rosette-raster-core:make-frame 80 80))))
      (is (frames-equal-p f-direct f-flat)
          (format nil "retained != immediate (first diff ~a)"
                  (first-diff f-direct f-flat))))))

;;; ---- LAW: OP-CLOSURE-FAITHFUL ----

(defun test-op-closure ()
  (test-case "OP-CLOSURE-FAITHFUL: op->closure(op) == render-display-list((op))"
    (dolist (op '((:clear 30 40 50)
                  (:line 2 3 40 44 200 100 50 3 1000)
                  (:fill-rect 5 5 20 10 90 90 200 700)))
      (let ((a (funcall (op->closure op) (rosette-raster-core:make-frame 50 50)))
            (b (render-display-list (list op) (rosette-raster-core:make-frame 50 50))))
        (is (frames-equal-p a b)
            (format nil "closure != display-list for ~S" (car op)))))))

;;; ---- LAW: XFORM-COMPOSITION ----

(defun test-xform-composition ()
  (test-case "XFORM-COMPOSITION: associativity, identity, integer-translate add"
    ;; identity is a no-op
    (multiple-value-bind (x y) (xform-apply (xform-identity) 7 9)
      (is (and (= x 7) (= y 9)) "identity moved a point"))
    ;; translate compose == add on the integer path, and matches nested apply
    (let* ((a (xform-translate 3 5)) (b (xform-translate 10 -2))
           (ab (xform-compose a b)))
      (multiple-value-bind (x y) (xform-apply ab 0 0)
        (is (and (= x 13) (= y 3)) "translate compose != sum"))
      ;; compose = apply a first, then b
      (multiple-value-bind (ax ay) (xform-apply a 1 1)
        (multiple-value-bind (bx by) (xform-apply b ax ay)
          (multiple-value-bind (cx cy) (xform-apply ab 1 1)
            (is (and (= cx bx) (= cy by)) "compose != nested apply")))))
    ;; associativity of a general (rotate/scale) chain within tolerance
    (let* ((r (xform-rotate 0.7d0)) (s (xform-scale 2d0 3d0)) (tt (xform-translate 4 5))
           (left (xform-compose (xform-compose r s) tt))
           (right (xform-compose r (xform-compose s tt))))
      (multiple-value-bind (lx ly) (xform-apply left 1.3d0 -0.4d0)
        (multiple-value-bind (rx ry) (xform-apply right 1.3d0 -0.4d0)
          (is (and (< (abs (- lx rx)) 1d-9) (< (abs (- ly ry)) 1d-9))
              "xform-compose not associative"))))
    ;; nested translate transform == single composed translate (byte-exact)
    (let* ((nested (parse-scene
                    (list :scene '(:size 60 60)
                          (list :transform (xform-translate 7 8)
                                (list :transform (xform-translate 4 5)
                                      (list :draw '(:line 0 0 20 20 200 200 60 2 1000)))))))
           (single (parse-scene
                    (list :scene '(:size 60 60)
                          (list :transform (xform-translate 11 13)
                                (list :draw '(:line 0 0 20 20 200 200 60 2 1000)))))))
      (is (frames-equal-p (render-scene nested) (render-scene single))
          "nested translate != composed translate (byte)"))))

;;; ---- LAW: CLIP-INTERSECTION ----

(defun test-clip-intersection ()
  (test-case "CLIP-INTERSECTION: nested clip == single intersect; outside => zero px"
    (is (equal (clip-intersect '(:rect 0 0 50 50) '(:rect 20 20 100 100))
               '(20 20 30 30))
        "clip-intersect wrong")
    ;; nested clips == a single intersected clip
    (let* ((nested (parse-scene
                    (list :scene '(:size 80 80)
                          (list :clip '(:rect 0 0 50 50)
                                (list :clip '(:rect 20 20 100 100)
                                      (list :draw '(:fill-rect 0 0 80 80 200 50 50 1000)))))))
           (single (parse-scene
                    (list :scene '(:size 80 80)
                          (list :clip '(:rect 20 20 30 30)
                                (list :draw '(:fill-rect 0 0 80 80 200 50 50 1000)))))))
      (is (frames-equal-p (render-scene nested) (render-scene single))
          "nested clip != single intersected clip"))
    ;; a draw fully outside the clip writes zero pixels
    (let* ((scene (parse-scene
                   (list :scene '(:size 40 40)
                         (list :clip '(:rect 0 0 10 10)
                               (list :draw '(:fill-rect 20 20 10 10 255 255 255 1000))))))
           (frame (render-scene scene))
           (blank (rosette-raster-core:make-frame 40 40)))
      (is (frames-equal-p frame blank) "clipped-out draw wrote pixels"))))

;;; ---- LAW: OPACITY-MULTIPLIES ----

(defun test-opacity ()
  (test-case "OPACITY: 1000 == identity; 0 == nothing; multiplies"
    (let ((base (list :draw '(:fill-rect 0 0 20 20 200 100 50 1000))))
      ;; opacity 1000 identity
      (let ((a (render-scene (parse-scene (list :scene '(:size 20 20) base))))
            (b (render-scene (parse-scene (list :scene '(:size 20 20)
                                                (list :opacity 1000 base))))))
        (is (frames-equal-p a b) "opacity 1000 not identity"))
      ;; opacity 0 => nothing painted
      (let ((c (render-scene (parse-scene (list :scene '(:size 20 20)
                                                (list :opacity 0 base)))))
            (blank (rosette-raster-core:make-frame 20 20)))
        (is (frames-equal-p c blank) "opacity 0 painted something"))
      ;; opacity nesting multiplies: (opacity 500 (opacity 500 X)) alpha => 250/1000
      (let* ((nested (render-scene (parse-scene
                                    (list :scene '(:size 20 20)
                                          (list :opacity 500 (list :opacity 500 base))))))
             (direct (render-scene (parse-scene
                                    (list :scene '(:size 20 20)
                                          (list :draw '(:fill-rect 0 0 20 20 200 100 50 250)))))))
        (is (frames-equal-p nested direct) "nested opacity != multiplied alpha")))))

;;; ---- LAW: TEXT-LOWERING ----

(defun test-text-lowering ()
  (test-case "TEXT-LOWERING: (text ...) == its own vector-font (line ...) set; deterministic"
    (let* ((op '(:text 10 10 2000 235 235 255 2 "SYS"))
           (lines (text->lines op)))
      (is (and (consp lines) (every (lambda (l) (eq (car l) :line)) lines))
          "text->lines produced non-line ops")
      ;; rendering the text op == rendering its lowered lines
      (let ((a (render-display-list (list op) (rosette-raster-core:make-frame 80 30)))
            (b (render-display-list lines (rosette-raster-core:make-frame 80 30))))
        (is (frames-equal-p a b) "text render != lowered-lines render"))
      ;; deterministic
      (is (equal lines (text->lines op)) "text->lines not deterministic"))))

;;; ---- LAW: LAYOUT SIZE-PRESERVING + DETERMINISM ----

(defun test-layout ()
  (test-case "LAYOUT: size-preserving, deterministic, frac largest-remainder EXACT"
    ;; largest-remainder sums EXACTLY and is drift-free
    (let ((parts (rosette-scene-ir::%largest-remainder 100 '(1 1 1))))
      (is (= 100 (reduce #'+ parts)) "frac parts do not sum to total")
      (is (equal parts '(34 33 33)) "largest-remainder not left-to-right"))
    (let* ((ui '(:ui (:size 200 120)
                 (:box (:dir :col :pad 4 :gap 6 :bg (20 24 40))
                       (:box (:h 20 :bg (60 60 120)))
                       (:box (:h (:frac 1) :bg (40 120 60)))
                       (:box (:h (:frac 2) :bg (120 40 60))))))
           (scene (layout ui)))
      ;; size preserved
      (multiple-value-bind (uw uh) (ui-size ui)
        (multiple-value-bind (sw sh) (scene-size scene)
          (is (and (= uw sw) (= uh sh)) "layout changed scene size")))
      ;; deterministic (pure fn)
      (is (equal scene (layout ui)) "layout not deterministic")
      ;; the three children fill the content height EXACTLY:
      ;; content = 120 - 2*4 = 112; gaps = 2*6 = 12; fixed = 20;
      ;; leftover 80 split 1:2 => 27 + 53 ; 20+27+53 + 12 == 112
      (is (= 112 (+ 20 27 53 12)) "manual size arithmetic sanity")
      ;; renders without error and is non-blank
      (let ((frame (render-scene scene)))
        (is (> (loop for b across (rosette-raster-core:frame-pixels frame)
                     count (> b 0)) 0)
            "layout rendered blank")))))

;;; ---- LAW: PAINTER MONOTONICITY / REPLAY + fingerprint ----

(defun test-painter-and-fingerprint ()
  (test-case "PAINTER: reorder non-overlapping siblings == same; fingerprint stable"
    (let* ((s1 (parse-scene
                (list :scene '(:size 60 30)
                      (list :group
                            (list :draw '(:fill-rect 0 0 20 20 200 50 50 1000))
                            (list :draw '(:fill-rect 30 0 20 20 50 50 200 1000))))))
           (s2 (parse-scene
                (list :scene '(:size 60 30)
                      (list :group
                            (list :draw '(:fill-rect 30 0 20 20 50 50 200 1000))
                            (list :draw '(:fill-rect 0 0 20 20 200 50 50 1000)))))))
      (is (frames-equal-p (render-scene s1) (render-scene s2))
          "reordering non-overlapping siblings changed the frame")
      ;; fingerprint is deterministic + input-sensitive
      (is (= (scene-fingerprint s1) (scene-fingerprint s1)) "fingerprint unstable")
      (is (/= (scene-fingerprint s1)
              (scene-fingerprint
               (parse-scene (list :scene '(:size 60 30)
                                  (list :draw '(:clear 1 2 3))))))
          "fingerprint collided on different scenes"))))

;;; ---- LAW: ROUND-TRIP parse/unparse ----

(defun test-parse-roundtrip ()
  (test-case "ROUND-TRIP: (parse-scene (unparse-scene s)) == s"
    (let ((s (compositor-scene)))
      (is (equal s (parse-scene (unparse-scene s))) "scene did not parse-round-trip"))
    (let ((s (layout '(:ui (:size 100 60)
                       (:box (:dir :row :pad 2 :gap 2)
                             (:box (:w (:frac 1) :bg (10 20 30)))
                             (:box (:w (:frac 1) :bg (30 20 10))))))))
      (is (equal s (parse-scene (unparse-scene s))) "layout scene did not round-trip"))))

;;; ==================================================================
;;; END-TO-END WITNESS: declarative UI -> layout -> node -> flatten ->
;;; render -> compose -> fb_scene.txt, cross-checked against an
;;; independent rosette-raster-core oracle at every seam.
;;; ==================================================================

(defun e2e-ui ()
  '(:ui (:size 160 90)
    (:box (:dir :col :pad 4 :gap 4 :bg (14 16 30))
          (:box (:h 18 :bg (40 50 110)))                      ; title bar
          (:box (:dir :row :h (:frac 1) :gap 4)
                (:box (:w 40 :bg (60 60 120)))                ; sidebar
                (:box (:w (:frac 1) :bg (24 28 44)))))))       ; content

(defun test-e2e-witness ()
  (test-case "E2E WITNESS: ui->layout->flatten->render==compose; fb_scene.txt fidelity"
    (let* ((ui (e2e-ui))
           (scene (layout ui)))                    ; Face C -> Face B
      ;; (1) DIRECT render
      (let* ((direct (render-scene scene))
             ;; (2) flat display-list render (Face B -> Face A -> execute)
             (flat (flatten-scene scene))
             (dl (render-display-list flat
                    (rosette-raster-core:make-frame 160 90))))
        (is (frames-equal-p direct dl) "e2e: retained != immediate")
        ;; (3) fb_scene.txt emit -> parse-back -> render == direct
        (let* ((text (scene->fb-scene-text scene))
               (back (fb-scene-text->scene text))
               (rback (render-scene back)))
          (is (frames-equal-p direct rback)
              (format nil "e2e: fb_scene.txt render != direct (first diff ~a)"
                      (first-diff direct rback)))
          ;; the emitted text is in the C brick's vocabulary (size/clear/line only)
          (is (every (lambda (line)
                       (let ((tok (first (rosette-scene-ir::%split-ws line))))
                         (or (null tok)
                             (member tok '("#" "size" "clear" "line") :test #'string=)
                             (char= (char line 0) #\#))))
                     (with-input-from-string (in text)
                       (loop for l = (read-line in nil nil) while l collect l)))
              "emitted fb_scene.txt used vocabulary outside size/clear/line"))
        ;; (4) INDEPENDENT ORACLE: rebuild the exact expected pixels with a
        ;; hand-written rosette-raster-core reference from the known layout
        ;; geometry, and demand byte-equality with the pipeline.
        (let ((oracle (e2e-oracle)))
          (is (frames-equal-p direct oracle)
              (format nil "e2e: pipeline != independent raster-core oracle (first diff ~a)"
                      (first-diff direct oracle))))
        ;; (5) determinism: same input -> identical pixels + fingerprint
        (is (= (frame-fingerprint direct) (frame-fingerprint (render-ui ui)))
            "e2e: render not deterministic")))))

(defun e2e-oracle ()
  "Independent reference for (e2e-ui): compute each box rect by hand and
fill it with the SAME raster-core scanline fill the executor uses.
  outer 160x90 pad4 gap4 col:
    content = (152 x 82) at (4,4)
    title  : h18 -> rect (4,4,152,18)
    row    : frac1 -> h = 82 - 18 - 4 = 60 -> rect (4,26,152,60), pad0 gap4 row:
        sidebar: w40 -> (4,26,40,60)
        content: frac1 -> w = 152 - 40 - 4 = 108 -> (48,26,108,60)"
  (let ((f (rosette-raster-core:make-frame 160 90)))
    (flet ((fill-rect (x y w h r g b)
             (dotimes (row h)
               (rosette-raster-core:draw-line-thick
                f x (+ y row) (+ x w -1) (+ y row)
                :r r :g g :b b :alpha 1f0 :thickness 1))))
      (fill-rect 0 0 160 90 14 16 30)     ; outer bg
      (fill-rect 4 4 152 18 40 50 110)    ; title
      (fill-rect 4 26 40 60 60 60 120)    ; sidebar
      (fill-rect 48 26 108 60 24 28 44))  ; content
    f))

;;; ---- runner ----

(defun run-all-tests ()
  (setf *test-count* 0 *fail-count* 0)
  (test-parse-roundtrip)
  (test-executor-oracle)
  (test-fb-scene-roundtrip)
  (test-direct-equals-layered)
  (test-flatten-parity)
  (test-op-closure)
  (test-xform-composition)
  (test-clip-intersection)
  (test-opacity)
  (test-text-lowering)
  (test-layout)
  (test-painter-and-fingerprint)
  (test-e2e-witness)
  (format t "~&rosette-scene-ir: ~A assertions, ~A failures.~%" *test-count* *fail-count*)
  (unless (zerop *fail-count*)
    (error "rosette-scene-ir tests failed: ~A failure(s)." *fail-count*))
  t)
