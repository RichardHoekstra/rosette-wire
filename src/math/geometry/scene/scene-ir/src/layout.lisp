;;;; layout.lisp --- Face C -> Face B: declarative UI -> property tree.
;;;;
;;;;   layout : (:ui (:size W H) box) -> (:scene (:size W H) node)
;;;;
;;;; A single-pass block/flow lowering.  Each :id'd/box lowers 1:1 to a
;;;; (:transform (:translate x y) ...) carrying an optional (:clip (:rect
;;;; 0 0 w h)) and a background (:fill-rect ...), so Face C -> Face B is
;;;; literal and flatten then yields Face A.
;;;;
;;;; SIZE-PRESERVING + DETERMINISM law: pure fn; (scene-size (layout ui)) ==
;;;; (ui-size ui); on each axis Sum(resolved lens)+Sum(gaps)+2*pad ==
;;;; container; (:frac K) leftover distributed largest-remainder
;;;; left-to-right (EXACT, no floats on the place path); siblings never
;;;; overlap on the main axis; every child rect is contained in its parent.

(in-package #:rosette-scene-ir)

(defun ui-size (ui)
  "Return (values W H) of a (:ui (:size W H) box) form."
  (destructuring-bind (w h) (cdr (cadr ui)) (values w h)))

(defun %pl (plist key &optional default)
  (getf plist key default))

;;; ---- intrinsic text metrics (certified vfont advance) ----

(defun %label-scale (plist)
  (/ (float (%pl plist :scale1000 1000) 1d0) 1000d0))

(defun %label-ascent (plist)
  "Cap-height in px for a label (rounded)."
  (round (* (%label-scale plist) rosette-vector-font:+cap-height+)))

(defun %label-width (str plist)
  (max 1 (ceiling (rosette-vector-font:string-width str :size (%label-scale plist)))))

;;; ---- axis helpers (dir :col -> main = vertical; :row -> main = horizontal) ----

(defun %col-p (dir) (eq dir :col))

(defun %intrinsic-main (box dir)
  "Intrinsic main-axis size of BOX along parent axis DIR (:col -> height,
:row -> width).  Used for :auto and for :frac-free packing."
  (ecase (car box)
    (:label (destructuring-bind (plist str) (cdr box)
              (if (%col-p dir) (%label-ascent plist) (%label-width str plist))))
    (:box (destructuring-bind (plist &rest _) (cdr box)
            (declare (ignore _))
            (let ((explicit (if (%col-p dir) (%pl plist :h) (%pl plist :w))))
              (cond ((integerp explicit) explicit)
                    (t (%measure-box box dir))))))))

(defun %measure-box (box parent-dir)
  "Best-effort intrinsic main size of a :box from its own children."
  (destructuring-bind (plist &rest items) (cdr box)
    (let* ((dir (%pl plist :dir :col))
           (pad (%pl plist :pad 0))
           (gap (%pl plist :gap 0))
           (children (remove-if-not (lambda (x) (member (car x) '(:box :label)))
                                    items))
           (n (length children)))
      (if (eq dir parent-dir)
          (+ (* 2 pad) (* gap (max 0 (1- n)))
             (reduce #'+ children :key (lambda (c) (%intrinsic-main c dir))
                                  :initial-value 0))
          (+ (* 2 pad)
             (reduce #'max children
                     :key (lambda (c) (%intrinsic-main c parent-dir))
                     :initial-value 0))))))

;;; ---- largest-remainder allocator (EXACT, integer-only) ----

(defun %largest-remainder (total weights)
  "Distribute integer TOTAL over positive integer WEIGHTS so the parts sum
EXACTLY to TOTAL; remainder added left-to-right to the largest fractional
parts (ties -> earlier index)."
  (let* ((sw (reduce #'+ weights :initial-value 0)))
    (if (or (<= sw 0) (null weights))
        (mapcar (constantly 0) weights)
        (let* ((base (mapcar (lambda (k) (floor (* total k) sw)) weights))
               (used (reduce #'+ base :initial-value 0))
               (rem (- total used))
               ;; fractional remainder for each, with index for stable order
               (fr (loop for k in weights for i from 0
                         collect (list (- (* total k) (* sw (floor (* total k) sw)))
                                       i)))
               (order (stable-sort (copy-list fr)
                                   (lambda (a b) (> (first a) (first b)))))
               (result (copy-list base)))
          (loop repeat (max 0 rem)
                for pair in order
                do (incf (nth (second pair) result)))
          result))))

;;; ---- the lowering ----

(defun %lower-paint (op pad)
  "A box-local paint op: origin = content top-left = (pad,pad)."
  (list :transform (xform-translate pad pad) (list :draw op)))

(defun %lower-label (box w h)
  (declare (ignore w h))
  (destructuring-bind (plist str) (cdr box)
    (let* ((pad (%pl plist :pad 0))
           (color (%pl plist :color '(230 230 240)))
           (thick (%pl plist :thick 1))
           (ascent (%label-ascent plist)))
      (destructuring-bind (r g b) color
        (list :draw (list :text pad (+ pad ascent) (%pl plist :scale1000 1000)
                          r g b thick str))))))

(defun %lower-box (box w h)
  "Lower BOX (already sized W x H at its own origin) to a node whose content
lives in local coords 0..W, 0..H."
  (ecase (car box)
    (:label (%lower-label box w h))
    (:box
     (destructuring-bind (plist &rest items) (cdr box)
       (let* ((dir (%pl plist :dir :col))
              (pad (%pl plist :pad 0))
              (gap (%pl plist :gap 0))
              (bg (%pl plist :bg))
              (opacity (%pl plist :opacity))
              (clipp (%pl plist :clip?))
              (paints (remove-if-not (lambda (x) (member (car x) *ops-vocabulary*))
                                     items))
              (children (remove-if-not (lambda (x) (member (car x) '(:box :label)))
                                       items))
              (content '()))
         ;; background
         (when bg
           (destructuring-bind (r g b) bg
             (push (list :draw (list :fill-rect 0 0 w h r g b 1000)) content)))
         ;; box-local paints
         (dolist (op paints) (push (%lower-paint op pad) content))
         ;; flow children
         (when children
           (let* ((col (%col-p dir))
                  (cmain (- (if col h w) (* 2 pad)))
                  (ccross (- (if col w h) (* 2 pad)))
                  (n (length children))
                  (gaps (* gap (max 0 (1- n))))
                  ;; resolve each child's main len + whether it is a frac
                  (specs (mapcar
                          (lambda (c)
                            (let* ((cp (if (eq (car c) :box) (cadr c) (cadr c)))
                                   (len (if col (%pl cp :h) (%pl cp :w))))
                              (cond ((and (consp len) (eq (car len) :frac))
                                     (list :frac (cadr len)))
                                    ((integerp len) (list :fixed len))
                                    (t (list :fixed (%intrinsic-main c dir))))))
                          children))
                  (fixed-sum (reduce #'+ specs
                                     :key (lambda (s) (if (eq (car s) :fixed)
                                                          (cadr s) 0))
                                     :initial-value 0))
                  (leftover (max 0 (- cmain gaps fixed-sum)))
                  (frac-weights (loop for s in specs
                                      when (eq (car s) :frac) collect (cadr s)))
                  (frac-parts (%largest-remainder leftover frac-weights))
                  (mains (let ((fp frac-parts))
                           (mapcar (lambda (s)
                                     (if (eq (car s) :frac)
                                         (pop fp) (cadr s)))
                                   specs)))
                  (pos pad))
             (loop for c in children for m in mains
                   for cp = (if (eq (car c) :box) (cadr c) (cadr c))
                   do (let* ((cross-len
                              (let ((cl (if col (%pl cp :w) (%pl cp :h))))
                                (if (integerp cl) cl ccross)))
                             (cx (if col pad pos)) (cy (if col pos pad))
                             (cw (if col cross-len m))
                             (chh (if col m cross-len)))
                        (push (list :transform (xform-translate cx cy)
                                    (%lower-box c cw chh))
                              content)
                        (incf pos (+ m gap))))))
         (let ((node (cons :group (nreverse content))))
           (when clipp
             (setf node (list :clip (list :rect 0 0 w h) node)))
           (when (and opacity (< opacity 1000))
             (setf node (list :opacity opacity node)))
           node))))))

(defun layout (ui)
  "Lower a (:ui (:size W H) box) to a (:scene (:size W H) node).  Pure and
deterministic."
  (unless (and (consp ui) (eq (car ui) :ui))
    (error "rosette-scene-ir: not a (:ui (:size W H) ...) form: ~S" ui))
  (multiple-value-bind (w h) (ui-size ui)
    (let* ((root-box (caddr ui))
           (node (%lower-box root-box w h)))
      (parse-scene (list :scene (list :size w h) node)))))

(defun render-ui (ui &key surfaces)
  "End-to-end convenience: render a declarative UI to a frame
(= render-scene . layout)."
  (render-scene (layout ui) :surfaces surfaces))
