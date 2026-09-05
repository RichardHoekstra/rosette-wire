;;;; tests.lisp --- tests for rosette-hott-core.

(defpackage #:rosette-hott-core/tests
  (:use #:cl #:rosette-hott-core #:rosette-assert-core)
  (:export #:run-all-tests))

(in-package #:rosette-hott-core/tests)

(defun run-all-tests ()
  (with-test-run (run "rosette-hott-core")
    (let ((nat (make-hott-type :nat
                               :predicate (lambda (x) (typep x '(integer 0 *)))
                               :truncation +set+)))
      (check run (in-type-p nat 3) "nat accepts non-negative integers")
      (check run (not (in-type-p nat -1)) "nat rejects negative integers")
      (check run (truncation<= +proposition+ +set+) "propositions are sets")
      (let* ((p (refl nat 2))
             (q (path-inverse p))
             (r (path-compose p q)))
        (check run (path-loop-p p) "refl is a loop")
        (check run (= (hott-path-from q) 2) "inverse preserves endpoint value")
        (check run (path-loop-p r) "path composed with inverse is a loop")
        (let ((ap (path-ap #'1+ nat p)))
          (check run (= (hott-path-from ap) 3) "path-ap maps from endpoint")
          (check run (= (hott-path-to ap) 3) "path-ap maps to endpoint"))
        (check run (= (path-recursion
                       p
                       (lambda (source refl-path)
                         (declare (ignore refl-path))
                         source))
                      2)
               "path-recursion reduces to reflexivity data")
        (check run (eq (path-induction
                        p
                        (lambda (source refl-path original-path)
                          (declare (ignore source refl-path original-path))
                          :ok)
                        :checker (lambda (path result)
                                   (and (typep path 'hott-path)
                                        (eq result :ok))))
                       :ok)
               "path-induction validates with caller checker")
        (check run (= (based-path-induction
                       2 p
                       (lambda (base refl-path original-path)
                         (declare (ignore refl-path original-path))
                         base))
                      2)
               "based-path-induction fixes the source endpoint")
        (check run (path-compose-associative-p p q p)
               "path composition is endpoint-associative")
        (check run (path-left-unit-p p)
               "path left unit law holds at endpoints")
        (check run (path-right-unit-p p)
               "path right unit law holds at endpoints")
        (check run (path-inverse-left-p p)
               "path inverse-left law holds at endpoints")
        (check run (path-inverse-right-p p)
               "path inverse-right law holds at endpoints")
        (check run (path-ap-compose-p #'1+ #'1+ nat p)
               "path-ap composition law holds at endpoints")))
    (let* ((base (make-hott-type :base
                                 :predicate (lambda (x) (member x '(:a :b)))))
           (fiber-a (make-hott-type :fiber-a
                                    :predicate #'integerp))
           (fiber-b (make-hott-type :fiber-b
                                    :predicate #'stringp))
           (family (make-hott-family
                    base
                    (lambda (x)
                      (ecase x
                        (:a fiber-a)
                        (:b fiber-b)))
                    :transport (lambda (path value source target)
                                 (declare (ignore source target))
                                 (if (eq (hott-path-to path) :b)
                                     (write-to-string value)
                                     (parse-integer value)))))
           (p (make-hott-path base :a :b :witness :manual))
           (q (make-hott-path base :b :a :witness :manual)))
      (check run (eq (family-fiber family :a) fiber-a)
             "dependent family selects source fiber")
      (check run (string= "42" (transport family p 42))
             "transport moves value into target fiber")
      (check run (= 42 (transport-compose family p q 42))
             "transport-compose moves along composed reindexing")
      (check run (= 7 (transport-refl
                       (make-hott-family base (constantly fiber-a))
                       :a 7))
             "transport-refl is identity for constant family")
      (let* ((constant-family (make-hott-family base (constantly fiber-a)))
             (pa (refl base :a)))
        (check run (transport-refl-p constant-family :a 7)
               "transport-refl law predicate checks identity")
        (check run (transport-compose-p constant-family pa pa 7)
               "transport-compose law predicate checks sequential transport"))
      (let* ((constant-family (make-hott-family base (constantly fiber-a)))
             (pa (refl base :a))
             (pathover (make-dependent-path constant-family pa 7 7))
             (section (make-pi-function constant-family
                                        (lambda (x)
                                          (declare (ignore x))
                                          11)))
             (apd (dependent-action-path section pa)))
        (check run (dependent-path-valid-p pathover)
               "dependent path validates transported endpoint")
        (check run (= (dependent-path-from pathover) 7)
               "dependent path remembers source fiber value")
        (check run (= (dependent-path-to apd) 11)
               "dependent action path targets section value")))
    (let* ((bool (make-hott-type :bool :predicate (lambda (x) (member x '(nil t)))))
           (bit (make-hott-type :bit :predicate (lambda (x) (member x '(0 1)))))
           (eqv (make-hott-equivalence
                 bool bit
                 (lambda (x) (if x 1 0))
                 (lambda (x) (= x 1))
                 :left-homotopy (lambda (x) (refl bool x))
                 :right-homotopy (lambda (x) (refl bit x)))))
      (check run (= (equiv-forward eqv t) 1) "bool->bit forward map")
      (check run (eq (equiv-backward eqv 0) nil) "bit->bool backward map")
      (check run (path-loop-p (equiv-path eqv :left t)) "left homotopy path")
      (check run (path-loop-p (equiv-path eqv :right 1)) "right homotopy path")
      (let* ((id-bit (identity-equivalence bit))
             (roundtrip (compose-equivalences eqv (inverse-equivalence eqv)))
             (same-bit (compose-equivalences id-bit id-bit))
             (homotopy (make-hott-homotopy
                        bool bit
                        (lambda (x) (if x 1 0))
                        (lambda (x) (if x 1 0))
                        (lambda (x)
                          (refl bit (if x 1 0))))))
        (check run (= (equiv-forward id-bit 1) 1)
               "identity equivalence preserves values")
        (check run (eq (equiv-forward (inverse-equivalence eqv) 0) nil)
               "inverse equivalence swaps direction")
        (check run (eq (equiv-forward roundtrip t) t)
               "equivalence composition returns to source")
        (check run (= (equiv-forward same-bit 1) 1)
               "identity equivalences compose")
        (check run (homotopic-on-p homotopy '(nil t))
               "homotopy validates on finite samples")
        (check run (= (hott-path-from (point-homotopy-path homotopy t)) 1)
               "point homotopy path starts at left function value"))
      (let ((type-path (equiv->path eqv)))
        (check run (eq (hott-path-from type-path) bool)
               "equiv->path starts at source type")
        (check run (eq (hott-path-to type-path) bit)
               "equiv->path ends at target type")
        (check run (eq (path->equiv type-path) eqv)
               "path->equiv recovers stored equivalence")
        (check run (= (univalence-transport type-path t) 1)
               "univalence-transport moves source value forward")
        (check run (eq (univalence-transport type-path 0 :direction :backward)
                       nil)
               "univalence-transport moves target value backward"))
      (let* ((pointed-bool (make-pointed-type bool t))
             (pointed-bit (make-pointed-type bit 1))
             (pointed-eqv (make-pointed-equivalence
                           pointed-bool pointed-bit eqv
                           :forward-basepoint-path (refl bit 1)))
             (inverse-pointed (inverse-pointed-equivalence pointed-eqv))
             (composed-pointed
               (compose-pointed-equivalences pointed-eqv inverse-pointed))
             (bool-loop (loop-refl pointed-bool)))
        (check run (= (equiv-forward
                       (pointed-equivalence-equivalence pointed-eqv)
                       t)
                      1)
               "pointed equivalence carries forward equivalence")
        (check run (eq (pointed-map-target
                        (pointed-equivalence-forward-map pointed-eqv))
                       pointed-bit)
               "pointed equivalence exposes forward pointed map")
        (check run (eq (pointed-equivalence-source inverse-pointed)
                       pointed-bit)
               "inverse pointed equivalence swaps source")
        (check run (eq (pointed-equivalence-target composed-pointed)
                       pointed-bool)
               "composed pointed equivalence returns to source")
        (check run (path-loop-p
                    (pointed-equivalence-loop-map pointed-eqv bool-loop))
               "pointed equivalence maps based loops")))
    (let* ((bool (make-hott-type :bool :predicate (lambda (x) (member x '(nil t)))))
           (bit (make-hott-type :bit :predicate (lambda (x) (member x '(0 1)))))
           (forward (lambda (x) (if x 1 0)))
           (fiber-witness
             (make-fiberwise-equivalence-witness
              bool bit forward
              (lambda (y)
                (let* ((point (= y 1))
                       (center (make-hott-fiber-value
                                bool bit forward y point
                                :image-path (refl bit y)))
                       (fiber-type (homotopy-fiber-type
                                    bool bit forward y
                                    :name (list :bool-bit-fiber y))))
                  (make-contractible-witness
                   fiber-type
                   center
                   (lambda (value)
                     (make-hott-path fiber-type center value
                                     :witness :sampled-fiber-contract)))))))
           (fiber0 (fiberwise-witness-for fiber-witness 0))
           (fiber1 (fiberwise-witness-for fiber-witness 1))
           (eqv (equiv-from-fiberwise-witness fiber-witness)))
      (check run (in-type-p (contractible-witness-type fiber0)
                            (contractible-witness-center fiber0))
             "fiber center inhabits homotopy fiber")
      (check run (contractible-on-p
                  fiber1
                  (list (contractible-witness-center fiber1)))
             "contractible fiber validates on center sample")
      (check run (eq (equiv-backward eqv 0) nil)
             "fiberwise equivalence chooses inverse from fiber center")
      (check run (= (equiv-forward eqv t) 1)
             "fiberwise equivalence keeps forward map")
      (check run (path-loop-p (equiv-path eqv :right 1))
             "fiberwise equivalence supplies right homotopy"))
    (let* ((int (make-hott-type :int :predicate #'integerp))
           (parity (make-hott-type :parity
                                   :predicate (lambda (x) (member x '(0 1)))))
           (left-map (lambda (x) (mod x 2)))
           (right-map #'identity)
           (pullback (homotopy-pullback-type
                      int parity parity left-map right-map
                      :name :int-over-parity))
           (value (make-homotopy-pullback-value
                   int parity parity left-map right-map 7 1)))
      (check run (in-type-p pullback value)
             "homotopy pullback accepts coherent comparison triples")
      (check run (= (homotopy-pullback-left value) 7)
             "homotopy pullback left projection returns source value")
      (check run (= (homotopy-pullback-right value) 1)
             "homotopy pullback right projection returns source value")
      (check run (path-loop-p (homotopy-pullback-path value))
             "homotopy pullback stores target comparison path"))
    (let* ((unit (make-hott-type :unit :predicate (lambda (x) (eq x :star))))
           (bool (make-hott-type :bool :predicate (lambda (x) (member x '(nil t)))))
           (bit (make-hott-type :bit :predicate (lambda (x) (member x '(0 1)))))
           (pushout (make-homotopy-pushout
                     bool bit unit
                     (lambda (x)
                       (declare (ignore x))
                       t)
                     (lambda (x)
                       (declare (ignore x))
                       1)
                     :name :bool-bit-wedge))
           (left (pushout-left pushout nil))
           (right (pushout-right pushout 0))
           (glue (pushout-glue pushout :star))
           (target (make-hott-type :side
                                   :predicate (lambda (x)
                                                (member x '(:left :right)))))
           (recursor (pushout-recursion
                      pushout target
                      (lambda (value)
                        (declare (ignore value))
                        :left)
                      (lambda (value)
                        (declare (ignore value))
                        :right)
                      (lambda (value)
                        (declare (ignore value))
                        (make-hott-path target :left :right
                                        :witness :pushout-image)))))
      (check run (in-type-p (homotopy-pushout-type pushout) left)
             "homotopy pushout accepts left injection")
      (check run (in-type-p (homotopy-pushout-type pushout) right)
             "homotopy pushout accepts right injection")
      (check run (eq (hit-value-constructor (hott-path-from glue)) :left)
             "homotopy pushout glue starts at left image")
      (check run (eq (hit-value-constructor (hott-path-to glue)) :right)
             "homotopy pushout glue ends at right image")
      (check run (eq (hit-eliminate-value recursor left) :left)
             "pushout recursor maps left constructor")
      (check run (eq (hit-eliminate-value recursor right) :right)
             "pushout recursor maps right constructor")
      (check run (eq (hott-path-type
                      (hit-eliminate-path recursor :glue :star))
                     target)
             "pushout recursor maps glue coherently"))
    (let* ((base (make-hott-type :switch
                                 :predicate (lambda (x) (member x '(:count :name)))))
           (nat (make-hott-type :nat
                                :predicate (lambda (x) (typep x '(integer 0 *)))))
           (string-type (make-hott-type :string
                                        :predicate #'stringp))
           (family (make-hott-family
                    base
                    (lambda (x)
                      (ecase x
                        (:count nat)
                        (:name string-type)))))
           (sigma (sigma-type family :name :dependent-pair))
           (pair (make-sigma-value family :count 3 :witness :observed))
           (pi-fn (make-pi-function
                   family
                   (lambda (x)
                     (ecase x
                       (:count 0)
                       (:name "")))
                   :checker (lambda (fn base-type)
                              (and (in-type-p base-type :count)
                                   (integerp (funcall fn :count))))))
           (same-pi (make-pi-function family
                                      (lambda (x)
                                        (ecase x
                                          (:count 0)
                                          (:name ""))))))
      (check run (in-type-p sigma pair)
             "sigma-type accepts well-formed dependent pair")
      (check run (not (in-type-p sigma :not-a-pair))
             "sigma-type rejects non-pairs")
      (check run (= (pi-apply pi-fn :count) 0)
             "pi-apply checks numeric fiber")
      (check run (string= (pi-apply pi-fn :name) "")
             "pi-apply checks string fiber")
      (let* ((pi-type (pi-type family :name :dependent-function))
             (path (pi-extensional-path pi-type pi-fn same-pi '(:count :name))))
        (check run (eq (hott-path-type path) pi-type)
               "pi-extensional-path witnesses sampled equality")))
    (let* ((int (make-hott-type :int :predicate #'integerp :truncation +set+))
           (mod3 (make-hott-quotient
                  int
                  (lambda (a b) (= (mod (- a b) 3) 0))
                  :canonicalize (lambda (x) (mod x 3))
                  :witness :mod-3
                  :name :z/3z))
           (a (make-quotient-value mod3 4 :witness :input))
           (b (make-quotient-value mod3 1 :witness :input))
           (c (make-quotient-value mod3 2 :witness :input))
           (groupoid (make-hott-type :loop-space :truncation +groupoid+))
           (set-view (set-truncation groupoid))
           (maybe-int (propositional-truncation int :name :merely-int))
           (has-four (make-trunc-value int 4 :witness :observed))
           (has-five (make-trunc-value int 5 :witness :observed)))
      (check run (= (quotient-value-canonical a) 1)
             "quotient canonicalizes representatives")
      (check run (in-type-p (hott-quotient-type mod3) a)
             "quotient type accepts well-formed quotient values")
      (check run (quotient-related-p mod3 a b)
             "quotient relation recognizes equal classes")
      (check run (not (quotient-related-p mod3 a c))
             "quotient relation rejects distinct classes")
      (check run (eq (hott-path-type (quotient-path mod3 a b))
                     (hott-quotient-type mod3))
             "quotient-path produces paths in the quotient type")
      (check run (= (hott-type-truncation set-view) +set+)
             "set-truncation lowers higher types to set level")
      (check run (= (hott-type-truncation maybe-int) +proposition+)
             "propositional truncation is proposition-level")
      (check run (in-type-p maybe-int has-four)
             "propositional truncation accepts wrapped witness")
      (check run (eq (trunc-value-base has-four) int)
             "truncation value remembers base type")
      (check run (eq (hott-path-type
                      (trunc-path maybe-int has-four has-five))
                     maybe-int)
             "trunc-path connects any two truncation inhabitants"))
    (let* ((bool (make-hott-type :bool
                                 :predicate (lambda (x) (member x '(nil t)))))
           (unit (make-hott-type :unit
                                 :predicate (lambda (x) (eq x :star))))
           (to-unit (lambda (x)
                      (declare (ignore x))
                      :star))
           (connected
             (make-connected-map-witness
              bool unit to-unit
              (lambda (target-value)
                (make-hott-fiber-value
                 bool unit to-unit target-value t
                 :image-path (refl unit target-value))))))
      (check run (in-type-p
                  (connected-fiber-proposition connected :star)
                  (connected-fiber-witness-for connected :star))
             "connected map supplies merely inhabited fiber")
      (check run (connected-on-p connected '(:star))
             "connected map validates sampled target fibers"))
    (let* ((bool (make-hott-type :bool
                                 :predicate (lambda (x) (member x '(nil t)))))
           (id-bool #'identity)
           (embedding
             (make-embedding-map-witness
              bool bool id-bool
              (lambda (target-value left right fiber-type)
                (declare (ignore target-value))
                (make-hott-path fiber-type left right
                                :witness :sampled-embedding-fiber))))
           (fiber-true
             (make-hott-fiber-value bool bool id-bool t t
                                    :image-path (refl bool t))))
      (check run (path-loop-p
                  (embedding-fiber-path embedding t
                                        fiber-true fiber-true))
             "embedding fiber path connects sampled fiber values")
      (check run (embedding-on-p embedding t (list fiber-true))
             "embedding witness validates sampled fiber pairs"))
    (let* ((bool (make-hott-type :bool
                                 :predicate (lambda (x) (member x '(nil t)))))
           (id-bool #'identity)
           (connected
             (make-connected-map-witness
              bool bool id-bool
              (lambda (target-value)
                (make-hott-fiber-value
                 bool bool id-bool target-value target-value
                 :image-path (refl bool target-value)))))
           (embedding
             (make-embedding-map-witness
              bool bool id-bool
              (lambda (target-value left right fiber-type)
                (declare (ignore target-value))
                (make-hott-path fiber-type left right
                                :witness :identity-fiber-prop))))
           (eqv (equiv-from-connected-embedding connected embedding)))
      (check run (eq (equiv-forward eqv t) t)
             "connected embedding equivalence keeps forward map")
      (check run (eq (equiv-backward eqv nil) nil)
             "connected embedding equivalence chooses fiber center inverse")
      (check run (path-loop-p (equiv-path eqv :right t))
             "connected embedding equivalence supplies right homotopy"))
    (let* ((nat (make-hott-type :nat
                                :predicate (lambda (x)
                                             (typep x '(integer 0 *)))))
           (parity (make-hott-type :parity
                                   :predicate (lambda (x) (member x '(0 1)))))
           (truth (make-hott-type :truth
                                  :predicate (lambda (x)
                                               (member x '(:even :odd)))))
           (top-map (lambda (x) (mod x 2)))
           (bottom-map (lambda (x) (if (evenp x) :even :odd)))
           (left-map #'identity)
           (right-map (lambda (x) (if (zerop x) :even :odd)))
           (square (make-homotopy-square
                    nat parity nat truth
                    top-map bottom-map left-map right-map
                    (lambda (x)
                      (refl truth (if (evenp x) :even :odd)))))
           (fiber (make-hott-fiber-value
                   nat parity top-map 1 7
                   :image-path (refl parity 1)))
           (mapped (homotopy-square-fiber-map square fiber 1)))
      (check run (path-loop-p (homotopy-square-path square 4))
             "homotopy square path checks commuting endpoints")
      (check run (= (hott-fiber-value-point mapped) 7)
             "homotopy square fiber map preserves left image point")
      (check run (eq (hott-path-to
                      (hott-fiber-value-image-path mapped))
                     :odd)
             "homotopy square fiber map targets right image of fiber target"))
    (let* ((circle (circle-type))
           (base (circle-base circle))
           (loop (circle-loop circle))
           (pointed-circle (make-pointed-type
                            (higher-inductive-type-base-type circle)
                            base))
           (omega-circle (loop-space pointed-circle))
           (double-loop (loop-power loop 2))
           (inverse-loop (loop-power loop -1))
           (pi1-circle (fundamental-group pointed-circle))
           (loop-laws (loop-group-law-report
                       pi1-circle
                       (list (loop-group-identity pi1-circle)
                             loop
                             double-loop
                             inverse-loop)))
           (interval (interval-type))
           (left (interval-left interval))
           (right (interval-right interval))
           (segment (interval-segment interval))
           (bit (make-hott-type :bit
                                :predicate (lambda (x) (member x '(0 1)))))
           (circle-rec (circle-recursion circle bit 1 (refl bit 1)))
           (pointed-bit (make-pointed-type bit 1))
           (pointed-rec (make-pointed-map
                         pointed-circle pointed-bit
                         (lambda (value)
                           (hit-eliminate-value circle-rec value))
                         :basepoint-path (refl bit 1)))
           (interval-rec (interval-recursion
                          interval bit 0 1
                          (make-hott-path bit 0 1
                                          :witness :interval-image))))
      (check run (in-type-p (higher-inductive-type-base-type circle) base)
             "circle base inhabits circle HIT")
      (check run (path-loop-p loop)
             "circle loop is an endpoint loop")
      (check run (in-type-p omega-circle loop)
             "loop-space accepts circle generator")
      (check run (in-type-p omega-circle (loop-refl pointed-circle))
             "loop-space accepts reflexivity loop")
      (check run (path-loop-p double-loop)
             "positive loop power is a loop")
      (check run (path-loop-p inverse-loop)
             "negative loop power is a loop")
      (check run (path-loop-p (pointed-map-loop pointed-rec loop))
             "pointed map sends based loops to based loops")
      (check run (in-type-p (loop-group-carrier pi1-circle) loop)
             "fundamental group carrier accepts circle loop")
      (check run (loop-group-equal-p
                  pi1-circle
                  (loop-group-compose pi1-circle
                                      (loop-group-identity pi1-circle)
                                      loop)
                  loop)
             "loop-group identity is a left unit")
      (check run (loop-group-equal-p
                  pi1-circle
                  (loop-group-compose pi1-circle
                                      (loop-group-inverse pi1-circle loop)
                                      loop)
                  (loop-group-identity pi1-circle))
             "loop-group inverse cancels on sampled endpoints")
      (check run (getf loop-laws :associative)
             "loop-group sampled associativity holds")
      (check run (getf loop-laws :closed)
             "loop-group sampled closure holds")
      (check run (eq (hott-path-type loop)
                     (higher-inductive-type-base-type circle))
             "circle loop lives in circle type")
      (check run (in-type-p (higher-inductive-type-base-type interval) left)
             "interval left endpoint inhabits interval HIT")
      (check run (in-type-p (higher-inductive-type-base-type interval) right)
             "interval right endpoint inhabits interval HIT")
      (check run (eq (hit-value-constructor (hott-path-from segment)) :left)
             "interval segment starts at left constructor")
      (check run (eq (hit-value-constructor (hott-path-to segment)) :right)
             "interval segment ends at right constructor")
      (check run (= (hit-eliminate-value circle-rec base) 1)
             "circle recursor maps base constructor")
      (check run (path-loop-p (hit-eliminate-path circle-rec :loop))
             "circle recursor maps loop coherently")
      (check run (= (hit-eliminate-value interval-rec left) 0)
             "interval recursor maps left endpoint")
      (check run (= (hit-eliminate-value interval-rec right) 1)
             "interval recursor maps right endpoint")
      (check run (eq (hott-path-type
                      (hit-eliminate-path interval-rec :segment))
                     bit)
             "interval recursor maps segment into target type"))
    (let* ((bool (make-hott-type :bool
                                 :predicate (lambda (x) (member x '(nil t)))))
           (bit (make-hott-type :bit
                                :predicate (lambda (x) (member x '(0 1)))))
           (to-bit (lambda (x) (if x 1 0)))
           (cofiber (make-homotopy-cofiber bool bit to-bit
                                           :name :bool-to-bit-cofiber))
           (included (cofiber-include cofiber 1))
           (collapsed (cofiber-basepoint cofiber))
           (cofiber-path (cofiber-glue cofiber t))
           (product-type (hott-product-type bool bit :name :bool-times-bit))
           (pair (make-hott-product-value t 1))
           (join (make-homotopy-join bool bit :name :bool-join-bit))
           (join-segment (join-glue join t 1))
           (pointed-bool (make-pointed-type bool nil))
           (pointed-bit (make-pointed-type bit 0))
           (pointed-to-bit
             (make-pointed-map pointed-bool pointed-bit to-bit
                               :basepoint-path (refl bit 0)))
           (pointed-fiber (make-pointed-fiber
                           pointed-to-bit
                           :name :pointed-bool-to-bit-fiber))
           (fiber-sequence (make-pointed-fiber-sequence pointed-to-bit))
           (pointed-cofiber (make-pointed-cofiber
                             pointed-to-bit
                             :name :pointed-bool-to-bit-cofiber))
           (wedge (make-pointed-wedge pointed-bool pointed-bit
                                      :name :bool-wedge-bit))
           (left-base (wedge-basepoint wedge))
           (right-base (wedge-right wedge 0))
           (wedge-path (wedge-glue wedge))
           (smash (make-pointed-smash pointed-bool pointed-bit
                                      :name :bool-smash-bit))
           (smash-value (smash-pair smash t 1))
           (smash-point (smash-basepoint smash))
           (left-collapse (smash-collapse-left smash nil))
           (right-collapse (smash-collapse-right smash 0)))
      (check run (in-type-p (homotopy-cofiber-type cofiber) included)
             "cofiber includes target values")
      (check run (in-type-p (homotopy-cofiber-type cofiber) collapsed)
             "cofiber has collapsed basepoint")
      (check run (eq (hit-value-constructor (hott-path-from cofiber-path))
                     :left)
             "cofiber glue starts at included image")
      (check run (eq (hit-value-constructor (hott-path-to cofiber-path))
                     :right)
             "cofiber glue ends at collapsed point")
      (check run (in-type-p (pointed-fiber-type pointed-fiber)
                            (pointed-fiber-basepoint-value pointed-fiber))
             "pointed fiber basepoint inhabits fiber type")
      (check run (eq (pointed-fiber-include
                      pointed-fiber
                      (pointed-fiber-basepoint-value pointed-fiber))
                     nil)
             "pointed fiber inclusion returns source basepoint")
      (check run (eq (pointed-map-target
                      (pointed-fiber-inclusion-map pointed-fiber))
                     pointed-bool)
             "pointed fiber exposes pointed inclusion into source")
      (check run (eq (pointed-map-target
                      (pointed-fiber-sequence-composite-map
                       fiber-sequence))
                     pointed-bit)
             "pointed fiber sequence composite targets original codomain")
      (check run (path-loop-p
                  (pointed-nullhomotopy-path
                   (pointed-fiber-sequence-nullhomotopy fiber-sequence)
                   (pointed-fiber-basepoint-value pointed-fiber)))
             "pointed fiber sequence composite is null on fiber basepoint")
      (check run (eq (pointed-fiber-sequence-inclusion-map fiber-sequence)
                     (pointed-fiber-inclusion-map
                      (pointed-fiber-sequence-fiber fiber-sequence)))
             "pointed fiber sequence exposes canonical inclusion")
      (check run (in-type-p (pointed-cofiber-type pointed-cofiber)
                            (pointed-cofiber-include pointed-cofiber 1))
             "pointed cofiber includes target values")
      (check run (equalp (hott-path-from
                          (pointed-cofiber-collapse-path pointed-cofiber))
                         (pointed-cofiber-include pointed-cofiber 0))
             "pointed cofiber collapse path starts at included target base")
      (check run (equalp (hott-path-to
                          (pointed-cofiber-collapse-path pointed-cofiber))
                         (pointed-cofiber-basepoint pointed-cofiber))
             "pointed cofiber collapse path targets cofiber basepoint")
      (check run (eq (pointed-map-target
                      (pointed-cofiber-include-map pointed-cofiber))
                     (pointed-cofiber-pointed-type pointed-cofiber))
             "pointed cofiber exposes pointed target inclusion")
      (check run (in-type-p product-type pair)
             "product type accepts checked pairs")
      (check run (eq (hott-product-left pair) t)
             "product left projection returns left component")
      (check run (= (hott-product-right pair) 1)
             "product right projection returns right component")
      (check run (in-type-p (homotopy-join-type join)
                            (join-left join nil))
             "join accepts left injection")
      (check run (in-type-p (homotopy-join-type join)
                            (join-right join 0))
             "join accepts right injection")
      (check run (eq (hit-value-constructor
                      (hott-path-from join-segment))
                     :left)
             "join segment starts at left endpoint")
      (check run (eq (hit-value-constructor
                      (hott-path-to join-segment))
                     :right)
             "join segment ends at right endpoint")
      (check run (in-type-p (pointed-wedge-type wedge) left-base)
             "wedge basepoint inhabits wedge type")
      (check run (in-type-p (pointed-wedge-type wedge) right-base)
             "wedge right injection inhabits wedge type")
      (check run (eq (hit-value-constructor (hott-path-from wedge-path))
                     :left)
             "wedge glue starts at left basepoint")
      (check run (eq (hit-value-constructor (hott-path-to wedge-path))
                     :right)
             "wedge glue ends at right basepoint")
      (check run (in-type-p (pointed-smash-type smash) smash-value)
             "smash product includes non-base product pairs")
      (check run (in-type-p (pointed-smash-type smash) smash-point)
             "smash product has collapsed wedge basepoint")
      (check run (eq (hit-value-constructor
                      (hott-path-to left-collapse))
                     :right)
             "smash collapses left wedge image")
      (check run (eq (hit-value-constructor
                      (hott-path-to right-collapse))
                     :right)
             "smash collapses right wedge image"))
    (let* ((bool (make-hott-type :bool
                                 :predicate (lambda (x) (member x '(nil t)))))
           (susp (suspension-type bool :name :susp-bool))
           (north (suspension-north susp))
           (south (suspension-south susp))
           (meridian (suspension-meridian susp t))
           (target (make-hott-type :bit
                                   :predicate (lambda (x)
                                                (member x '(0 1)))))
           (recursor
             (suspension-recursion
              susp target 0 1
              (lambda (value)
                (declare (ignore value))
                (make-hott-path target 0 1
                                :witness :suspension-image))))
           (sphere0 (sphere-type 0))
           (sphere1 (sphere-type 1 :name :circle-as-suspension)))
      (check run (in-type-p (higher-inductive-type-base-type
                             (suspension-hit susp))
                            north)
             "suspension north inhabits suspension HIT")
      (check run (in-type-p (higher-inductive-type-base-type
                             (suspension-hit susp))
                            south)
             "suspension south inhabits suspension HIT")
      (check run (eq (hit-value-constructor (hott-path-from meridian))
                     :north)
             "suspension meridian starts at north")
      (check run (eq (hit-value-constructor (hott-path-to meridian))
                     :south)
             "suspension meridian ends at south")
      (check run (= (hit-eliminate-value recursor north) 0)
             "suspension recursor maps north")
      (check run (= (hit-eliminate-value recursor south) 1)
             "suspension recursor maps south")
      (check run (eq (hott-path-type
                      (hit-eliminate-path recursor :meridian t))
                     target)
             "suspension recursor maps meridians coherently")
      (check run (eq (hit-value-constructor
                      (make-hit-value sphere0 :minus))
                     :minus)
             "sphere zero exposes a negative point constructor")
      (check run (suspension-type-p sphere1)
             "positive spheres are iterated suspensions"))
    (let* ((bool (make-hott-type :bool
                                 :predicate (lambda (x) (member x '(nil t)))))
           (bit (make-hott-type :bit
                                :predicate (lambda (x) (member x '(0 1)))))
           (pointed-bool (make-pointed-type bool nil))
           (pointed-bit (make-pointed-type bit 0))
           (to-bit
             (make-pointed-map pointed-bool pointed-bit
                               (lambda (x) (if x 1 0))
                               :basepoint-path (refl bit 0)))
           (susp-bool (make-pointed-suspension pointed-bool
                                               :name :pointed-susp-bool))
           (susp-bit (make-pointed-suspension pointed-bit
                                              :name :pointed-susp-bit))
           (susp-map (make-pointed-suspension-map to-bit
                                                  :source susp-bool
                                                  :target susp-bit))
           (north (pointed-suspension-north susp-bool))
           (south (pointed-suspension-south susp-bool))
           (meridian (pointed-suspension-meridian susp-bool t))
           (mapped-north
             (funcall (pointed-map-function
                       (pointed-suspension-map-pointed-map susp-map))
                      north))
           (mapped-south
             (funcall (pointed-map-function
                       (pointed-suspension-map-pointed-map susp-map))
                      south))
           (mapped-meridian
             (pointed-suspension-map-meridian susp-map t)))
      (check run (in-type-p (pointed-suspension-type susp-bool) north)
             "pointed suspension north inhabits carrier")
      (check run (equalp (pointed-suspension-basepoint susp-bool) north)
             "pointed suspension is based at north")
      (check run (eq (hit-value-constructor (hott-path-from meridian))
                     :north)
             "pointed suspension meridian starts at north")
      (check run (eq (hit-value-constructor mapped-north) :north)
             "suspension map preserves north")
      (check run (eq (hit-value-constructor mapped-south) :south)
             "suspension map preserves south")
      (check run (equalp (hott-path-from mapped-meridian)
                         (pointed-suspension-north susp-bit))
             "suspension map meridian starts at target north")
      (check run (equalp (hott-path-to mapped-meridian)
                         (pointed-suspension-south susp-bit))
             "suspension map meridian ends at target south"))
    (let* ((bool (make-hott-type :bool
                                 :predicate (lambda (x) (member x '(nil t)))))
           (bit (make-hott-type :bit
                                :predicate (lambda (x) (member x '(0 1)))))
           (pointed-bool (make-pointed-type bool nil))
           (pointed-bit (make-pointed-type bit 0))
           (recursion
             (make-pointed-suspension-recursion
              pointed-bool
              pointed-bit
              (lambda (value)
                (declare (ignore value))
                (refl bit 0))))
           (susp-map (pointed-suspension-recursion-pointed-map recursion))
           (transpose (pointed-suspension-recursion-transpose-map recursion))
           (susp (pointed-suspension-recursion-suspension recursion))
           (north (pointed-suspension-north susp))
           (south (pointed-suspension-south susp))
           (north-image (funcall (pointed-map-function susp-map) north))
           (south-image (funcall (pointed-map-function susp-map) south))
           (loop-image (funcall (pointed-map-function transpose) t))
           (meridian-image
             (pointed-suspension-recursion-meridian recursion t)))
      (check run (= north-image 0)
             "pointed suspension recursion maps north to target base")
      (check run (= south-image 0)
             "pointed suspension recursion maps south to target base")
      (check run (in-type-p (loop-space pointed-bit) loop-image)
             "pointed suspension transpose maps source into loop space")
      (check run (path-loop-p loop-image)
             "pointed suspension transpose values are loops")
      (check run (path-loop-p meridian-image)
             "pointed suspension recursion meridian is a target loop")
      (check run (equalp (pointed-type-basepoint
                          (pointed-map-target transpose))
                         (loop-refl pointed-bit))
             "pointed suspension transpose target is based loop space"))))
