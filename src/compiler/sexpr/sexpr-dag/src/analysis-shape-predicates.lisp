;;;; analysis-shape-predicates.lisp --- predicate helpers for shape classifiers.

(in-package #:rosette-sexpr-dag)

(defun %declaration-spec-p (sexp)
  (%sexp-head-name-in-p sexp
                        '("type" "ftype" "inline" "notinline" "special"
                          "ignorable" "ignore" "optimize" "dynamic-extent"
                          "declaration")))

(defun %typed-slot-spec-p (sexp)
  (and (consp sexp)
       (%proper-list-p sexp)
       (member :type sexp :test #'eq)))

(defun %slot-option-fragment-p (sexp)
  (and (consp sexp)
       (%proper-list-p sexp)
       (member :read-only sexp :test #'eq)))

(defun %optimize-quality-p (sexp)
  (%sexp-head-name-in-p sexp
                        '("speed" "safety" "debug" "space"
                          "compilation-speed")))

(defun %compiler-policy-p (sexp)
  (cond
    ((%sexp-head-name-in-p sexp '("declare" "declaim" "proclaim"))
     (some #'%compiler-policy-p (rest sexp)))
    ((%sexp-head-name-in-p sexp '("optimize"))
     (every #'%optimize-quality-p (rest sexp)))
    ((and (consp sexp) (%every-proper-list-p #'%optimize-quality-p sexp)) t)
    ((and (consp sexp) (%every-proper-list-p #'%compiler-policy-p sexp)) t)
    (t nil)))

(defun %symbol-name-in-p (symbol names)
  (and (symbolp symbol)
       (member (string-downcase (symbol-name symbol)) names :test #'string=)))

(defun %sexp-head-name-in-p (sexp names)
  "Return true when SEXP is a cons whose head symbol name is in NAMES."
  (and (consp sexp)
       (%symbol-name-in-p (first sexp) names)))

(defun %test-counter-symbol-p (sexp)
  (%symbol-name-in-p sexp '("*test-count*" "*fail-count*" "*failure-count*"
                            "*skip-count*" "*passes*" "*fails*")))

(defun %test-harness-p (sexp)
  (cond
    ((and (%sexp-head-name-in-p sexp '("defun" "defmacro"))
          (%symbol-name-in-p (second sexp) '("is" "ok" "check" "test-case")))
     t)
    ((and (%sexp-head-name-in-p sexp '("is" "ok" "check" "test-case"))
          (%sexp-contains-symbol-p sexp '("*test-count*" "*fail-count*"
                                           "*failure-count*" "*skip-count*")))
     t)
    ((and (%sexp-head-name-in-p sexp '("defvar" "defparameter"))
          (%test-counter-symbol-p (second sexp)))
     t)
    ((and (%sexp-head-name-in-p sexp '("incf" "decf"))
          (%test-counter-symbol-p (second sexp)))
     t)
    ((and (%sexp-head-name-in-p sexp '("setf"))
          (some #'%test-counter-symbol-p (rest sexp)))
     t)
    ((and (consp sexp)
          (%sexp-contains-symbol-p sexp '("condition" "msg"))
          (%sexp-contains-symbol-p sexp '("*test-count*" "*fail-count*"
                                           "*failure-count*" "*skip-count*"
                                           "*passes*" "*fails*")))
     t)
    (t nil)))

(defun %asdf-bootstrap-p (sexp)
  (and (consp sexp)
       (%sexp-contains-symbol-p sexp '("asdf" "require" "find-package"))
       (or (%sexp-contains-symbol-p sexp '("require" "find-package"))
           (%symbol-name-in-p (first sexp) '("eval-when" "unless"))
           (%sexp-contains-symbol-p sexp '("compile-toplevel"
                                           "load-toplevel"
                                           "execute")))))

(defun %asdf-registry-bootstrap-p (sexp)
  (and (consp sexp)
       (or (and (%proper-list-p sexp)
                (= (length sexp) 1)
                (%asdf-registry-bootstrap-p (first sexp)))
           (and (%sexp-contains-symbol-p sexp
                                          '("*load-pathname*"
                                            "*load-truename*"
                                            "make-pathname"))
                (or (%sexp-contains-symbol-p sexp
                                              '("*central-registry*" "pushnew"))
                    (%sexp-contains-symbol-p sexp
                                             '("initialize-source-registry"
                                               "source-registry"
                                               "repo-root"
                                               "test-dir"
                                               "truename"))))
           (and (symbolp (first sexp))
                (consp (second sexp))
                (%symbol-name-in-p (first (second sexp)) '("make-pathname"))
                (%sexp-contains-symbol-p sexp
                                         '("*load-pathname*"
                                           "*load-truename*"
                                           "*compile-file-truename*"
                                           "self-dir"
                                           "parent-dir"
                                           "test-dir"
                                           "repo-root")))
           (and (%symbol-name-in-p (first sexp) '("pushnew"))
                (%sexp-contains-symbol-p sexp
                                         '("*central-registry*"
                                           "asdf:*central-registry*")))
           (and (%sexp-contains-symbol-p sexp
                                          '("*central-registry*"
                                            "asdf:*central-registry*"))
                (%proper-list-p sexp)
                (member :test sexp :test #'eq)
                (%sexp-contains-symbol-p sexp '("equal")))
           (and (%proper-list-p sexp)
                (member :defaults sexp :test #'eq)
                (%sexp-contains-symbol-p sexp
                                         '("butlast"
                                           "pathname-directory"
                                           "test-dir"))))))

(defun %asdf-component-metadata-p (sexp)
  (and (consp sexp)
       (or (%symbol-name-in-p (first sexp) '("defsystem"))
           (and (keywordp (first sexp))
                (member (first sexp) '(:file :static-file :module)
                        :test #'eq))
           (and (%proper-list-p sexp)
                (or (every (lambda (item)
                             (and (consp item)
                                  (keywordp (first item))
                                  (member (first item)
                                          '(:file :static-file :module)
                                          :test #'eq)))
                           sexp)
                    (member :depends-on sexp :test #'eq)
                    (member :components sexp :test #'eq)
                    (member :pathname sexp :test #'eq)
                    (member :in-order-to sexp :test #'eq)
                    (member :perform sexp :test #'eq))))))

(defun %build-metadata-p (sexp)
  (or (%asdf-bootstrap-p sexp)
      (%asdf-registry-bootstrap-p sexp)
      (%asdf-component-metadata-p sexp)))

(defun %timing-scaffold-p (sexp)
  (%sexp-contains-symbol-p sexp
                           '("get-internal-real-time"
                             "internal-time-units-per-second"
                             "elapsed-seconds")))

(defun %bench-scaffold-p (sexp)
  (or (%sexp-contains-symbol-p sexp
                               '("run-bench" "run-benchmark"
                                 "run-domain-kernel-bench"
                                 "define-domain-kernel-bench"))
      (and (%sexp-head-name-in-p sexp '("defun"))
           (%symbol-name-in-p (second sexp) '("run-bench"
                                             "run-benchmark")))))

(defun %vector-reduction-p (sexp)
  (and (%sexp-contains-symbol-p sexp '("dotimes" "loop"))
       (%sexp-contains-symbol-p sexp '("aref" "length"))
       (%sexp-contains-symbol-p sexp '("sum" "acc" "dot"))))

(defun %paired-vector-access-p (sexp)
  (and (consp sexp)
       (%proper-list-p sexp)
       (every (lambda (item)
                (and (consp item)
                     (%symbol-name-in-p (first item) '("aref"))))
              sexp)))

(defun %coerced-vector-access-p (sexp)
  (labels ((float-type-target-p (target)
             (or (%symbol-name-in-p target '("double-float" "single-float"))
                 (and (%quoted-form-p target)
                      (%symbol-name-in-p (second target)
                                         '("double-float"
                                           "single-float")))))
           (aref-value-p (value)
             (and (consp value)
                  (%symbol-name-in-p (first value) '("aref")))))
    (and (consp sexp)
         (if (%sexp-head-name-in-p sexp '("coerce"))
             (and (aref-value-p (second sexp))
                  (float-type-target-p (third sexp)))
             (or (and (%proper-list-p sexp)
                      (= (length sexp) 1)
                      (%coerced-vector-access-p (first sexp)))
                 (and (%proper-list-p sexp)
                      (= (length sexp) 2)
                      (aref-value-p (first sexp))
                      (float-type-target-p (second sexp))))))))

(defun %float-accumulator-finalization-p (sexp)
  (labels ((float-type-target-p (target)
             (or (%symbol-name-in-p target '("double-float" "single-float"))
                 (and (%quoted-form-p target)
                      (%symbol-name-in-p (second target)
                                         '("double-float"
                                           "single-float")))))
           (accumulator-symbol-p (value)
             (%symbol-name-in-p value '("sum" "acc" "total" "dot" "score"))))
    (and (consp sexp)
         (or (and (%symbol-name-in-p (first sexp) '("coerce"))
                  (accumulator-symbol-p (second sexp))
                  (float-type-target-p (third sexp)))
             (and (%proper-list-p sexp)
                  (= (length sexp) 1)
                  (%float-accumulator-finalization-p (first sexp)))))))
(defun %dimension-wildcard-symbol-p (sexp)
  (%symbol-name-in-p sexp '("*")))

(defun %array-dimensions-p (sexp)
  (and (consp sexp)
       (or (%every-proper-list-p #'%dimension-wildcard-symbol-p sexp)
           (and (%every-proper-list-p #'consp sexp)
                (%every-proper-list-p #'%array-dimensions-p sexp)))))

(defun %type-spec-p (sexp)
  (cond
    ((symbolp sexp)
     (%symbol-name-in-p sexp
                        '("fixnum" "integer" "real" "float"
                          "single-float" "double-float"
                          "number" "boolean" "string" "symbol"
                          "function" "cons" "list" "vector"
                          "array" "simple-array" "bit"
                          "unsigned-byte" "signed-byte"
                          "byte-vector" "u8" "rgb-channel"
                          "rgb-buffer" "f-scalar" "f-array"
                          "f-vector" "f64-array" "f32-array"
                          "vec3" "null" "t")))
    ((and (consp sexp)
          (null (rest sexp))
          (%type-spec-p (first sexp)))
     t)
    ((and (consp sexp)
          (%type-spec-p (first sexp))
          (%array-dimensions-p (rest sexp)))
     t)
    ((%sexp-head-name-in-p sexp
                           '("integer" "real" "float" "mod"
                             "unsigned-byte" "signed-byte"
                             "simple-array" "array" "vector"
                             "simple-vector" "or" "and" "not"
                             "member" "eql" "satisfies" "function"
                             "values" "byte-vector" "u8"
                             "rgb-channel" "rgb-buffer"
                             "f-scalar" "f-array" "f-vector"
                             "f64-array" "f32-array" "vec3"))
     t)
    ((and (consp sexp) (%every-proper-list-p #'%type-spec-p sexp)) t)
    (t nil)))

(defun %symbol-name-list= (sexp names)
  (and (%proper-list-p sexp)
       (= (length sexp) (length names))
       (every (lambda (item name)
                (%symbol-name-in-p item (list name)))
              sexp
              names)))

(defun %function-designator-name-p (sexp name)
  (and (%sexp-head-name-in-p sexp '("function"))
       (consp (rest sexp))
       (null (cddr sexp))
       (%symbol-name-in-p (second sexp) (list name))))

(defun %finite-state-accessor-args-p (sexp)
  (and (%proper-list-p sexp)
       (= (length sexp) 6)
       (%function-designator-name-p (first sexp) "state-values")
       (%function-designator-name-p (second sexp) "state-weights")
       (%function-designator-name-p (third sexp) "state-metadata")
       (%symbol-name-list= (subseq sexp 3) '("label" "left" "right"))))

(defun %domain-parameter-list-p (sexp)
  (or (%symbol-name-list= sexp
                          '("spot" "strike" "rate" "dividend"
                            "volatility" "maturity"))
      (%symbol-name-list= sexp
                          '("strike" "rate" "dividend"
                            "volatility" "maturity"))
      (%symbol-name-list= sexp
                          '("rate" "dividend" "volatility" "maturity"))
      (%symbol-name-list= sexp
                          '("dividend" "volatility" "maturity"))
      (%symbol-name-list= sexp
                          '("volatility" "maturity"))
      (%symbol-name-list= sexp
                          '("call" "strike" "forward" "discount"
                            "maturity"))
      (%symbol-name-list= sexp
                          '("strike" "forward" "discount" "maturity"))
      (%symbol-name-list= sexp
                          '("forward" "discount" "maturity"))
      (%symbol-name-list= sexp
                          '("discount" "maturity"))
      (%symbol-name-list= sexp
                          '("put" "strike" "forward" "discount"
                            "maturity"))))

(defun %quoted-form-p (sexp)
  (%sexp-head-name-in-p sexp '("quote")))

(defun %equality-policy-fragment-p (sexp)
  (labels ((test-name-p (x)
             (or (eq x :test)
                 (%symbol-name-in-p x '("test"))))
           (equal-function-p (x)
             (or (%symbol-name-in-p x '("equal"))
                 (and (%quoted-form-p x)
                      (%symbol-name-in-p (second x) '("equal")))
                 (and (%sexp-head-name-in-p x '("function"))
                      (%symbol-name-in-p (second x) '("equal")))))
           (fragment-p (x)
             (and (consp x)
                  (consp (cdr x))
                  (null (cddr x))
                  (test-name-p (first x))
                  (equal-function-p (second x)))))
    (or (fragment-p sexp)
        (and (%proper-list-p sexp)
             (every #'fragment-p sexp)))))

(defun %sexp-contains-symbol-p (sexp names)
  (cond ((symbolp sexp)
         (member (string-downcase (symbol-name sexp)) names :test #'string=))
        ((consp sexp)
         (or (%sexp-contains-symbol-p (car sexp) names)
             (%sexp-contains-symbol-p (cdr sexp) names)))
        (t nil)))
