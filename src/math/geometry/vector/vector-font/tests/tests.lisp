;;;; tests.lisp --- laws for rosette-vector-font.
;;;;
;;;; Pure geometry: the font emits the SAME strokes regardless of backend,
;;;; so the laws are exact algebraic identities on the polyline coordinates
;;;; (scale-linearity, translation-equivariance, y-flip, advance-additivity,
;;;; cap-height/baseline anchoring) plus glyph-coverage.

(in-package #:rosette-vector-font/tests)

(defun v1 (string &rest kw)
  "First vertex of the first stroke of STRING under STRING-POLYLINES."
  (first (first (apply #'string-polylines string kw))))

(defun ~= (a b &optional (tol 1d-9))
  "Boolean tolerance compare (CHECK records booleans, not signals)."
  (<= (abs (- a b)) tol))

(defun run-all-tests ()
  (with-test-run (run "rosette-vector-font")

    ;; --- coverage: printable ASCII present -------------------------------
    (check run (>= (hash-table-count *glyph-table*) 95)
           "table holds the full printable-ASCII set (>= 95 glyphs)")
    (check run (loop for c from 33 to 126 always (gethash c *glyph-table*))
           "every printable ASCII code 33..126 has a glyph")
    (check run (and (glyph-present-p #\Space) (null (glyph-strokes #\Space)))
           "space is present but draws no strokes")
    (check run (plusp (glyph-advance #\Space))
           "space still advances the pen")
    (check run (every (lambda (c) (glyph-strokes c)) '(#\A #\H #\g #\7 #\@))
           "representative glyphs carry strokes")

    ;; --- metrics ---------------------------------------------------------
    (check run (plusp +cap-height+)
           "cap height measured positive from capital H")
    (check run (~= (scale-for-cap-height (* 2d0 +cap-height+)) 2d0)
           "scale-for-cap-height inverts cap height (2x cap -> scale 2)")

    ;; --- advance additivity & width linearity ----------------------------
    (check run (~= (string-advance "AB")
                             (+ (glyph-advance #\A) (glyph-advance #\B)))
           "string-advance is the sum of glyph advances")
    (check run (~= (string-width "hello" :size 3d0)
                             (* 3d0 (string-width "hello" :size 1d0)))
           "string-width is linear in size")

    ;; --- scale-linearity about the origin --------------------------------
    (let ((a (v1 "A" :size 1d0))
          (b (v1 "A" :size 2d0)))
      (check run (and (~= (car b) (* 2d0 (car a)))
                      (~= (cdr b) (* 2d0 (cdr a))))
             "doubling SIZE doubles every vertex coordinate"))

    ;; --- translation equivariance ----------------------------------------
    (let ((a (v1 "Q" :size 1d0 :x 0d0 :y 0d0))
          (b (v1 "Q" :size 1d0 :x 17d0 :y -5d0)))
      (check run (and (~= (car b) (+ (car a) 17d0))
                      (~= (cdr b) (- (cdr a) 5d0)))
             "shifting the origin shifts every vertex by the same offset"))

    ;; --- Y-UP is the exact negation of Y-DOWN (origin on baseline) --------
    (let ((up   (v1 "R" :size 1d0 :y-up t))
          (down (v1 "R" :size 1d0 :y-up nil)))
      (check run (~= (car up) (car down))
             "Y orientation leaves X untouched")
      (check run (~= (cdr up) (- (cdr down)))
             "Y-UP coordinate is the negation of Y-DOWN about the baseline"))

    ;; --- cap-height / baseline anchoring on 'H' --------------------------
    (multiple-value-bind (xmin ymin xmax ymax) (string-bounds "H" :y-up t)
      (declare (ignore xmin xmax))
      (check run (~= (- ymax ymin) (float +cap-height+ 1d0))
             "rendered height of capital H equals the cap-height metric")
      (check run (~= ymin 0d0)
             "capital H sits exactly on the baseline (ymin = 0)"))

    ;; --- rotation: full turn is the identity; pi negates about origin -----
    (let ((z  (v1 "Z" :size 1d0 :rotate 0d0))
          (tau (v1 "Z" :size 1d0 :rotate (* 2d0 pi)))
          (hp (v1 "Z" :size 1d0 :rotate (coerce pi 'double-float))))
      (check run (and (~= (car z) (car tau) 1d-9)
                      (~= (cdr z) (cdr tau) 1d-9))
             "rotation by 2*pi is the identity")
      (check run (and (~= (car hp) (- (car z)) 1d-9)
                      (~= (cdr hp) (- (cdr z)) 1d-9))
             "rotation by pi negates the vertex about the origin"))

    ;; --- Greek alphabet present at Unicode code points -------------------
    (check run (>= (hash-table-count *glyph-table*) 140)
           "table holds ASCII + the Greek alphabet (>= 140 glyphs)")
    (check run (every (lambda (cp) (glyph-strokes (code-char cp)))
                      '(945 955 960 963 952 937 916))   ; α λ π σ θ Ω Δ
           "Greek alpha/lambda/pi/sigma/theta/Omega/Delta carry strokes")
    (check run (let ((g (glyph-strokes (code-char 945))))
                 (loop for s in g always (loop for (x . y) in s
                                               always (and (integerp x) (integerp y)))))
           "Greek glyph vertices are exact integers")

    ;; --- integer glyph data (no float drift baked into the table) --------
    (check run (loop for s in (glyph-strokes #\W)
                     always (loop for (x . y) in s
                                  always (and (integerp x) (integerp y))))
           "stored glyph vertices are exact integers")

    run))
