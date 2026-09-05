;;;; analysis-shape-domain-predicates.lisp --- domain classifier predicates.

(in-package #:rosette-sexpr-dag)

(defun %vector-element-difference-p (sexp)
  (labels ((aref-form-p (form)
             (%sexp-head-name-in-p form '("aref")))
           (difference-form-p (form)
             (and (%sexp-head-name-in-p form '("-"))
                  (aref-form-p (second form))
                  (aref-form-p (third form)))))
    (and (consp sexp)
         (or (difference-form-p sexp)
             (and (%symbol-name-in-p (first sexp) '("abs"))
                  (difference-form-p (second sexp)))
             (and (%proper-list-p sexp)
                  (= (length sexp) 1)
                  (%vector-element-difference-p (first sexp)))))))

(defun %serializer-scaffold-p (sexp)
  (%sexp-contains-symbol-p sexp
                           '("encode" "decode" "serialize"
                             "deserialize" "write" "read"
                             "read-line" "with-open-file"
                             "write-string" "write-char"
                             "split-tabs" "fields" "line-number"
                             "escape" "escaped")))

(defun %config-scaffold-p (sexp)
  (or (%sexp-contains-symbol-p sexp
                                '("getenv" "uiop:getenv" "uiop/os:getenv"
                                  "env-int" "env-float" "env-bool"
                                  "parse-integer" "environment"))
      (and (%sexp-contains-symbol-p sexp '("value" "length" "plusp"))
           (or (and (consp sexp)
                    (%proper-list-p sexp)
                    (= (length sexp) 2)
                    (%sexp-head-name-in-p sexp '("value"))
                    (consp (second sexp))
                    (%symbol-name-in-p (first (second sexp)) '("plusp"))
                    (%sexp-contains-symbol-p (second sexp)
                                             '("length" "value")))
               (and (%sexp-head-name-in-p sexp '("and"))
                    (%sexp-contains-symbol-p sexp
                                             '("value" "length" "plusp")))
               (and (%sexp-contains-symbol-p sexp
                                              '("read-from-string"
                                                "coerce"))
                    (%sexp-contains-symbol-p sexp
                                             '("value" "length" "plusp")))))))

(defun %gpu-pipeline-p (sexp)
  (%sexp-contains-symbol-p sexp
                           '("gpu-synchronize"
                             "gpu-to-host"
                             "gpu-rope"
                             "gpu-store-kv-cache"
                             "gpu-gqa-decode-attention"
                             "gguf-gpu-norm-qkv-proj-apply"
                             "gguf-gpu-iq4-linear-apply"
                             "query-out"
                             "key-out"
                             "value-out")))

(defun %row-getf-access-p (sexp)
  (and (consp sexp)
       (or (and (%sexp-head-name-in-p sexp '("getf"))
                (%symbol-name-in-p (second sexp) '("row"))
                (keywordp (third sexp)))
           (and (%sexp-head-name-in-p sexp '("lambda"))
                (consp (second sexp))
                (null (rest (second sexp)))
                (%symbol-name-in-p (first (second sexp)) '("row"))
                (%row-getf-access-p (third sexp)))
           (and (%proper-list-p sexp)
                (= (length sexp) 2)
                (consp (first sexp))
                (null (rest (first sexp)))
                (%sexp-head-name-in-p (first sexp) '("row"))
                (%row-getf-access-p (second sexp)))
           (and (%proper-list-p sexp)
                (= (length sexp) 1)
                (%row-getf-access-p (first sexp))))))

(defun %analysis-scaffold-p (sexp)
  (or (and (%sexp-contains-symbol-p sexp '("form" "header"))
           (%sexp-contains-symbol-p sexp '("length" "second" "consp")))
      (%row-getf-access-p sexp)
      (and (%sexp-contains-symbol-p sexp '("row" "getf"))
           (%sexp-contains-symbol-p sexp
                                    '("raw-f64-allocs"
                                      "canonical-primitive-provider-p")))))

(defun %reporting-scaffold-p (sexp)
  (and (%sexp-contains-symbol-p sexp '("format" "error" "errors"))
       (%sexp-contains-symbol-p sexp '(":file" ":error" "getf"))))

(defun %work-scheduler-p (sexp)
  (%sexp-contains-symbol-p sexp
                           '("make-work-item"
                             "work-item-name"
                             "work-item-depends-on"
                             "work-item-cost"
                             "plan-work-schedule"
                             "backend-step"
                             "backend-steps")))

(defun %bit-packing-p (sexp)
  (and (%sexp-contains-symbol-p sexp '("logand" "ldb" "byte"))
       (%sexp-contains-symbol-p sexp '("ash" "logior" "code"
                                       "low" "high" "byte"))))

(defun %byte-buffer-allocator-p (sexp)
  (and (%sexp-contains-symbol-p sexp '("unsigned-byte"
                                       "u8"
                                       "byte-vector"
                                       "row-bytes"))
       (or (%sexp-contains-symbol-p sexp '("make-array"))
           (and (consp sexp)
                (%proper-list-p sexp)
                (member :element-type sexp :test #'eq)))))

(defun %representation-table-p (sexp)
  (and (%sexp-contains-symbol-p sexp '("elements" "lambda"))
       (%sexp-contains-symbol-p sexp '("e"
                                       "representation"
                                       "make-representation"
                                       "permutation-representation"
                                       "character"
                                       "matrix"
                                       "identity"
                                       "dimension"))))

(defun %geometry-metric-p (sexp)
  (and (%sexp-contains-symbol-p sexp '("sqrt" "dx" "dy"))
       (%sexp-contains-symbol-p sexp '("x" "y" "cx" "cy"))))

(defun %symbolic-expression-p (sexp)
  (%sexp-contains-symbol-p sexp
                           '("cf-add" "cf-sub" "cf-mul" "cf-div"
                             "cf-var" "cf-const" "cf-log" "cf-exp"
                             "cf-sqrt" "cf-node")))

(defun %finite-difference-p (sexp)
  (or (%sexp-contains-symbol-p sexp
                               '("finite-difference"
                                 "central-difference"
                                 "gradient"
                                 "hessian"))
      (%sexp-contains-symbol-p sexp '("fp2" "fp1" "f0" "fm1" "fm2"))))
