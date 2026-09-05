;;;; rosette-browser-artifact/tests/tests.lisp --- Test suite.

(defpackage #:rosette-browser-artifact/tests
  (:use #:cl #:rosette-browser-artifact)
  (:import-from #:rosette-assert-core #:assert-true)
  (:export #:run-all-tests))

(in-package #:rosette-browser-artifact/tests)

(defun signals-artifact-error-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (browser-artifact-error () t)))

(defun run-all-tests ()
  (let* ((artifact (make-browser-artifact
                    :title "A&B"
                    :body "<main id=\"app\">ready</main>"
                    :styles '("body{color:#123}")
                    :scripts (list (cons "runtime" "document.body.dataset.ready='yes';"))
                    :data (list (cons "model" "{\"n\":4}"))
                    :capabilities '(:dom)))
         (html (render-browser-artifact artifact))
         (receipt (audit-browser-artifact artifact)))
    (assert-true (search "<title>A&amp;B</title>" html) "title is HTML-escaped")
    (assert-true
     (search (format nil "<meta charset=\"utf-8\">~%<meta name=\"viewport\"") html)
     "assembled document uses real newlines between head elements")
    (assert-true
     (search (format nil "</head>~%<body>~%") html)
     "assembled document uses real newlines at the body boundary")
    (assert-true (search "data-rosette-resource=\"runtime\"" html) "named script is inline")
    (assert-true (search "type=\"application/json\"" html) "JSON data is inert")
    (assert-true (equal '(:dom) (getf receipt :capabilities)) "capabilities reach receipt")
    (assert-true
     (string= "<!doctype html><p>ok</p>"
              (render-browser-artifact
               (browser-artifact-from-template
                "<!doctype html><p>__BODY__</p>" '(("__BODY__" . "ok")))))
     "template replacement is exact and deterministic")
    (assert-true
     (signals-artifact-error-p
      (lambda () (browser-artifact-from-template
                  "<!doctype html>__X____X__" '(("__X__" . "x")))))
     "a repeated template marker fails loudly")
    (assert-true
     (signals-artifact-error-p
      (lambda () (browser-artifact-from-template
                  "<!doctype html><!--__UNDECLARED__-->" '())))
     "an undeclared template marker fails loudly")
    (assert-true
     (signals-artifact-error-p
      (lambda () (make-browser-artifact :scripts '("fetch('/state')"))))
     "offline network access fails audit")
    (assert-true
     (browser-artifact-p
      (make-browser-artifact :mode :local-service :scripts '("fetch('/state')")))
     "local-service mode admits relative service actions")
    (assert-true
     (signals-artifact-error-p
      (lambda () (make-browser-artifact :scripts '("const x='</script>';"))))
     "script element termination fails at construction")
    (let ((literal (js-string-literal "`\n</script>")))
      (assert-true (not (search "</script>" literal)) "JS string cannot close script element")
      (assert-true (not (position #\Newline literal :test #'char=))
                   "JS string contains no raw newline")))
  (format t "~&rosette-browser-artifact: policy and assembly obligations passed.~%")
  t)
