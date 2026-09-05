;;;; tests.lisp --- Test suite.

(defpackage #:rosette-sexpr-dag/tests
  (:use #:cl #:rosette-sexpr-dag)
  (:export #:run-all-tests))

(in-package #:rosette-sexpr-dag/tests)

(defvar *passes* 0)
(defvar *fails* 0)

(defmacro check (name expression)
  `(if ,expression
       (progn (incf *passes*) (format t "~&  PASS  ~A~%" ',name))
       (progn (incf *fails*)
              (format t "~&  FAIL  ~A~%    expression: ~S~%" ',name ',expression))))

(defun write-test-file (pathname contents)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defun check-recursive-package-bootstrap ()
  (let* ((root (merge-pathnames
                (format nil "sexpr-dag-bootstrap-~A/"
                        (get-universal-time))
                (uiop:temporary-directory)))
         (package-file (merge-pathnames
                        "00-primitive/compiler/rosette-nested-provider/src/package.lisp"
                        root))
         (example-file (merge-pathnames
                        "04-witness/compiler/rosette-nested-consumer/examples/01-qualified.lisp"
                        root)))
    (write-test-file
     package-file
     "(in-package #:cl-user)
(defpackage #:rosette-nested-provider
  (:use #:cl)
  (:export #:answer))
")
    (write-test-file
     example-file
     "(list rosette-nested-provider:answer)
")
    (multiple-value-bind (_ errors)
        (index-directory root)
      (declare (ignore _))
      (check recursive-package-bootstrap (null errors)))))

(defun check-qualified-subpackage-bootstrap ()
  (let* ((root (merge-pathnames
                (format nil "sexpr-dag-qualified-~A/"
                        (get-universal-time))
                (uiop:temporary-directory)))
         (example-file (merge-pathnames
                        "03-tower/compiler/rosette-cold-reader/examples/01-subpackage.lisp"
                        root)))
    (write-test-file
     example-file
     "(list rosette-cold-reader/model:score rosette-cold-reader/model::state)
")
    (multiple-value-bind (_ errors)
        (index-directory root)
      (declare (ignore _))
      (check qualified-subpackage-bootstrap (null errors)))))

(defun check-local-nickname-bootstrap ()
  (let* ((root (merge-pathnames
                (format nil "sexpr-dag-local-nickname-~A/"
                        (get-universal-time))
                (uiop:temporary-directory)))
         (provider-package (merge-pathnames
                            "01-kernel/compiler/rosette-local-provider/src/package.lisp"
                            root))
         (consumer-file (merge-pathnames
                         "04-witness/compiler/rosette-local-consumer/examples/01-local-nickname.lisp"
                         root)))
    (write-test-file
     provider-package
     "(in-package #:cl-user)
(defpackage #:rosette-local-provider
  (:use #:cl)
  (:export #:answer))
")
    (write-test-file
     consumer-file
     "(in-package #:cl-user)
(defpackage #:rosette-local-consumer
  (:use #:cl)
  (:local-nicknames (#:lp #:rosette-local-provider)))
(in-package #:rosette-local-consumer)
(defun read-answer () lp:answer)
")
    (multiple-value-bind (_ errors)
        (index-directory root)
      (declare (ignore _))
      (check local-nickname-bootstrap (null errors)))))

(defun check-subdirectory-repo-root-bootstrap ()
  (let* ((root (merge-pathnames
                (format nil "sexpr-dag-subdir-root-~A/"
                        (get-universal-time))
                (uiop:temporary-directory)))
         (index-file (merge-pathnames "INDEX.md" root))
         (capabilities-file (merge-pathnames "CAPABILITIES.json" root))
         (provider-package (merge-pathnames
                            "02-construction/compiler/rosette-cross-provider/src/package.lisp"
                            root))
         (consumer-dir (merge-pathnames
                        "01-kernel/compiler/rosette-cross-consumer/experiments/"
                        root))
         (consumer-file (merge-pathnames "01-local-nickname.lisp" consumer-dir)))
    (write-test-file index-file "")
    (write-test-file capabilities-file "[]")
    (write-test-file
     provider-package
     "(in-package #:cl-user)
(defpackage #:rosette-cross-provider
  (:use #:cl)
  (:export #:answer))
")
    (write-test-file
     consumer-file
     "(in-package #:cl-user)
(defpackage #:rosette-cross-consumer
  (:use #:cl)
  (:local-nicknames (#:x #:rosette-cross-provider)))
(in-package #:rosette-cross-consumer)
(defun read-answer () x:answer)
")
    (multiple-value-bind (_ errors)
        (index-directory consumer-dir)
      (declare (ignore _))
      (check subdirectory-repo-root-bootstrap (null errors)))))

(defun run-all-tests ()
  "Run rosette-sexpr-dag tests."
  (setf *passes* 0 *fails* 0)
  (format t "~&;; rosette-sexpr-dag test run --------------------------------~%")
  (check-recursive-package-bootstrap)
  (check-qualified-subpackage-bootstrap)
  (check-local-nickname-bootstrap)
  (check-subdirectory-repo-root-bootstrap)
  (let* ((sexp '(+ (* x x) (* x x)))
         (index (make-dag-index))
         (root (sexp->dag sexp :index index :root t
                          :provenance '(:file "memory" :form-index 0)))
	         (shared (largest-shared-subdags index :min-size 3 :min-occurrences 2)))
    (check roundtrip (equal sexp (dag->sexp index root)))
    (check tree-count (= 19 (tree-node-count sexp)))
    (check dag-count-less-than-tree (< (dag-index-node-count index)
                                       (dag-index-tree-node-count index)))
    (check beta1-positive (> (beta1 index) 0))
    (check dot-output
           (search "digraph rosette_sexpr_dag"
                   (with-output-to-string (out)
                     (print-dag-dot index :stream out :limit 10))))
    (check shared-subdag-found (not (null shared)))
	    (check repeated-mul-present
	           (member '(* x x)
	                   (mapcar (lambda (row)
	                             (dag->sexp index (shared-subdag-node row)))
	                           shared)
	                   :test #'equal))
	    (let ((row (first shared)))
	      (check classification-kinds (equal '(:other) (shared-subdag-kinds row)))
	      (check classification-scope (eq :unknown (shared-subdag-scope row)))
	      (check shape-classification (eq :iteration (shared-subdag-shape '(loop repeat 3 collect x))))
	      (check binding-list-shape (eq :binding-list (shared-subdag-shape '((x 1) (y 2)))))
	      (check declaration-shape (eq :declaration (shared-subdag-shape '(declare (type double-float x)))))
	      (check lambda-list-marker-shape
	             (eq :lambda-list
	                 (shared-subdag-shape
	                  '(form &optional (message "assertion failed")))))
	      (check top-level-declaim-shape
	             (eq :declaration
	                 (shared-subdag-shape '(declaim (optimize (speed 3) (safety 1))))))
	      (check typed-slot-spec-shape
	             (eq :declaration
	                 (shared-subdag-shape '(nil :type (or null (simple-array double-float (* * *)))))))
	      (check typed-slot-spec-list-shape
	             (eq :declaration
	                 (shared-subdag-shape '((metadata nil :type list)))))
	      (check slot-option-fragment-shape
	             (eq :declaration
	                 (shared-subdag-shape '((or null function) :read-only t))))
	      (check array-dimensions-shape
	             (eq :array-dimensions
	                 (shared-subdag-shape '(* * *))))
	      (check nested-array-dimensions-shape
	             (eq :array-dimensions
	                 (shared-subdag-shape '((* * *)))))
	      (check array-dimensions-role
	             (eq :metadata
	                 (shared-subdag-role '((* * *)))))
	      (check type-spec-shape
	             (eq :type-spec
	                 (shared-subdag-shape '(integer 1 *))))
	      (check nested-type-spec-shape
	             (eq :type-spec
	                 (shared-subdag-shape '((integer 1 *)))))
	      (check type-spec-role
	             (eq :metadata
	                 (shared-subdag-role '((integer 1 *)))))
	      (check element-type-dimensions-shape
	             (eq :type-spec
	                 (shared-subdag-shape '(double-float (* * *)))))
	      (check element-type-dimensions-role
	             (eq :metadata
	                 (shared-subdag-role '(double-float (* * *)))))
	      (check domain-parameter-list-shape
	             (eq :domain-parameter-list
	                 (shared-subdag-shape
	                  '(spot strike rate dividend volatility maturity))))
	      (check domain-parameter-list-role
	             (eq :metadata
	                 (shared-subdag-role
	                  '(spot strike rate dividend volatility maturity))))
	      (check domain-parameter-list-suffix-shape
	             (eq :domain-parameter-list
	                 (shared-subdag-shape
	                  '(strike rate dividend volatility maturity))))
	      (check keyword-default-binding-list-shape
	             (eq :lambda-list-fragment
	                 (shared-subdag-shape
	                  '((compute-weight 1.0d0)
	                    (critical-path-weight 1.0d0)
	                    (communication-weight 1.0d0)))))
	      (check macro-binding-spec-shape
	             (eq :macro-binding-spec
	                 (shared-subdag-shape '(nx ny nz dx dy dz))))
	      (check macro-binding-spec-suffix-shape
	             (eq :macro-binding-spec
	                 (shared-subdag-shape '(ny nz dx dy dz))))
	      (check field-component-binding-shape
	             (eq :macro-binding-spec
	                 (shared-subdag-shape '(bx by bz mesh))))
	      (check finite-state-accessor-args-shape
	             (eq :macro-binding-spec
	                 (shared-subdag-shape
	                  '(#'state-values #'state-weights #'state-metadata
	                    label left right))))
	      (check macro-binding-spec-role
	             (eq :metadata
	                 (shared-subdag-role '(nx ny nz dx dy dz))))
	      (check rosette-type-spec-shape
	             (eq :type-spec
	                 (shared-subdag-shape '(f64-array (* * *)))))
	      (check rosette-type-spec-role
	             (eq :metadata
	                 (shared-subdag-role '(f64-array (* * *)))))
	      (check declaration-spec-shape (eq :declaration (shared-subdag-shape '(type fixnum x y))))
	      (check declaration-spec-list-shape
	             (eq :declaration (shared-subdag-shape '((type fixnum x y)
	                                                     (type (unsigned-byte 8) r g b)))))
	      (check optimize-quality-list-shape
	             (eq :declaration
	                 (shared-subdag-shape '((speed 3) (safety 1) (debug 1)))))
	      (check compiler-policy-motif
	             (eq :compiler-policy
	                 (shared-subdag-motif
	                  '(declaim (optimize (speed 3) (safety 1) (debug 1))))))
	      (check compiler-policy-role
	             (eq :compiler-policy
	                 (shared-subdag-role
	                  '(declaim (optimize (speed 3) (safety 1) (debug 1))))))
	      (check compiler-policy-inner-role
	             (eq :compiler-policy
	                 (shared-subdag-role
	                  '((optimize (speed 3) (safety 1) (debug 1))))))
	      (check compiler-policy-gravity
	             (string= "keep as local compiler policy"
	                      (shared-subdag-gravity
	                       row
	                       '(declaim (optimize (speed 3) (safety 1) (debug 1))))))
	      (check test-harness-shape
	             (eq :test-harness
	                 (shared-subdag-shape
	                  '(defmacro is (form &optional (message "assertion failed"))
	                    `(progn
	                       (incf *test-count*)
	                       (unless ,form
	                         (incf *failure-count*)
	                         (format t "FAIL: ~A~%" ,message)))))))
	      (check test-counter-shape
	             (eq :test-harness
	                 (shared-subdag-shape '(incf *test-count*))))
	      (check test-harness-motif
	             (eq :test-harness
	                 (shared-subdag-motif '(defun is (condition msg)
	                                        (incf *test-count*)
	                                        (unless condition
	                                          (incf *fail-count*)
	                                          (format t "FAIL: ~A~%" msg))))))
	      (check test-harness-role
	             (eq :test-harness
	                 (shared-subdag-role '(incf *test-count*))))
	      (check test-harness-tail-role
	             (eq :test-harness
	                 (shared-subdag-role
	                  '(is (condition msg)
	                    (incf *test-count*)
	                    (unless condition
	                      (incf *fail-count*)
	                      (format t "FAIL: ~A~%" msg))))))
	      (check test-helper-body-role
	             (eq :test-harness
	                 (shared-subdag-role
	                  '((condition msg)
	                    (incf *test-count*)
	                    (unless condition
	                      (incf *fail-count*)
	                      (format t "FAIL: ~A~%" msg))))))
	      (check test-harness-gravity
	             (string= "test harness / rosette-assert-core"
	                      (shared-subdag-gravity row '(incf *test-count*))))
	      (check build-metadata-shape
	             (eq :build-metadata
	                 (shared-subdag-shape
	                  '(eval-when (:compile-toplevel :load-toplevel :execute)
	                    (unless (find-package :asdf)
	                      (require :asdf))))))
	      (check build-metadata-inner-bootstrap-shape
	             (eq :build-metadata
	                 (shared-subdag-shape
	                  '((:compile-toplevel :load-toplevel :execute)
	                    (unless (find-package :asdf)
	                      (require :asdf))))))
	      (check build-metadata-component-fragment-shape
	             (eq :build-metadata
	                 (shared-subdag-shape
	                  '((:file "package") (:file "core")))))
	      (check build-metadata-single-component-shape
	             (eq :build-metadata
	                 (shared-subdag-shape
	                  '(:file "package"))))
	      (check build-metadata-module-component-shape
	             (eq :build-metadata
	                 (shared-subdag-shape
	                  '(:module "src"
	                    :components ((:file "core"))))))
	      (check build-metadata-motif
	             (eq :build-metadata
	                 (shared-subdag-motif
	                  '(:pathname "bench/" :components ((:file "microbench"))))))
	      (check build-metadata-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '(defsystem #:rosette-example
	                    :depends-on (#:rosette-core)
	                    :components ((:file "package") (:file "core"))))))
	      (check asdf-registry-bootstrap-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '(let* ((self-dir
	                           (make-pathname :defaults *load-pathname*
	                                          :name nil
	                                          :type nil))
	                          (parent-dir
	                           (make-pathname :defaults self-dir
	                                          :directory '(:relative :up))))
	                    (dolist (dir (list self-dir parent-dir))
	                      (pushnew dir asdf:*central-registry*
	                               :test #'equal))))))
	      (check asdf-pathname-binding-fragment-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '(self-dir
	                    (make-pathname :defaults *load-pathname*
	                                   :name nil
	                                   :type nil)))))
	      (check asdf-parent-pathname-binding-fragment-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '(parent-dir
	                    (make-pathname :defaults self-dir
	                                   :directory '(:relative :up))))))
	      (check asdf-registry-pushnew-fragment-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '(pushnew dir asdf:*central-registry*
	                    :test #'equal))))
	      (check asdf-registry-pushnew-wrapper-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '((pushnew dir asdf:*central-registry*
	                     :test #'equal)))))
	      (check asdf-registry-test-fragment-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '(asdf:*central-registry* :test #'equal))))
	      (check asdf-source-registry-pathname-tail-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '((butlast (pathname-directory test-dir) 2)
	                    :defaults test-dir))))
	      (check asdf-source-registry-bootstrap-role
	             (eq :build-metadata
	                 (shared-subdag-role
	                  '((test-dir
	                     (make-pathname :defaults *load-truename*
	                                    :name nil
	                                    :type nil))
	                    (repo-root
	                     (make-pathname
	                      :directory (butlast (pathname-directory test-dir)
	                                          2)
	                      :defaults test-dir))
	                    (initialize-source-registry
	                     (:source-registry
	                      (:tree (truename repo-root))
	                      :inherit-configuration))))))
	      (check build-metadata-gravity
	             (string= "keep as ASDF/build metadata"
	                      (shared-subdag-gravity
	                       row
	                       '(:pathname "bench/" :components ((:file "microbench"))))))
	      (check timing-scaffold-role
	             (eq :timing-scaffold
	                 (shared-subdag-role
	                  '(/ (- (get-internal-real-time) start)
	                      internal-time-units-per-second))))
	      (check benchmark-scaffold-role
	             (eq :timing-scaffold
	                 (shared-subdag-role
	                  '(defun run-bench (&key (iterations 1000))
	                    (run-domain-kernel-bench #'run-domain-kernel
	                                             :iterations iterations)))))
	      (check serializer-loop-role
	             (eq :serializer
	                 (shared-subdag-role
	                  '(with-open-file (in path)
	                    (loop for line = (read-line in nil nil)
	                          while line
	                          collect line)))))
	      (check serializer-tsv-body-role
	             (eq :serializer
	                 (shared-subdag-role
	                  '(while line do
	                    (incf line-number)
	                    (let* ((fields (split-tabs line))
	                           (n (length fields)))
	                      n)))))
	      (check serializer-string-escape-role
		     (eq :serializer
			 (shared-subdag-role
			  '(case ch
			    (#\" (write-string "\\\"" out))
			    (#\\ (write-string "\\\\" out))
			    (otherwise (write-char ch out))))))
	      (let ((config-scaffold
		      '(defun env-int (name default)
			(let ((value (uiop/os:getenv name)))
			  (if value
			      (parse-integer value)
			      default)))))
		(check config-scaffold-role
		       (eq :config-scaffold
			   (shared-subdag-role config-scaffold)))
		(check config-scaffold-gravity
		       (string= "local config/env helper"
				(shared-subdag-gravity row config-scaffold))))
	      (check config-value-present-fragment-role
		     (eq :config-scaffold
			 (shared-subdag-role
			  '(value (plusp (length value))))))
	      (check config-value-present-and-role
		     (eq :config-scaffold
			 (shared-subdag-role
			  '(and value (plusp (length value))))))
	      (check config-read-from-string-role
		     (eq :config-scaffold
			 (shared-subdag-role
			  '(if (and value (plusp (length value)))
			    (coerce (read-from-string value) 'single-float)
			    default))))
	      (let ((reporting-scaffold
		      '((error errors)
			(format t "  ~A: ~A~%"
				(getf error :file)
				(getf error :error)))))
		(check reporting-scaffold-role
		       (eq :reporting-scaffold
			   (shared-subdag-role reporting-scaffold)))
		(check reporting-scaffold-gravity
		       (string= "local reporting scaffold"
				(shared-subdag-gravity row
						       reporting-scaffold))))
	      (check analysis-row-getf-role
		     (eq :analysis-scaffold
			 (shared-subdag-role '(getf row :name))))
	      (check analysis-row-getf-wrapper-role
		     (eq :analysis-scaffold
			 (shared-subdag-role '((getf row :name)))))
	      (check analysis-row-getf-lambda-role
		     (eq :analysis-scaffold
			 (shared-subdag-role
			  '(lambda (row) (getf row :name)))))
	      (check analysis-row-getf-lambda-parts-role
		     (eq :analysis-scaffold
			 (shared-subdag-role
			  '((row) (getf row :name)))))
	      (check gpu-pipeline-role
	             (eq :gpu-pipeline
	                 (shared-subdag-role
	                  '(progn
	                    (gguf-gpu-norm-qkv-proj-apply block x
	                                                   :query-out q
	                                                   :key-out k
	                                                   :value-out v)
	                    (gpu-rope q positions theta)
	                    (gpu-store-kv-cache k cache 1)
	                    (gpu-gqa-decode-attention q cache values scale)
	                    (gguf-gpu-iq4-linear-apply output ctx)))))
	      (let ((analysis-scaffold
		      '((>= (length form) 3)
			(let ((header (second form)))
			  (and (consp header)
			       (= 2 (length header)))))))
		(check analysis-scaffold-role
		       (eq :analysis-scaffold
			   (shared-subdag-role analysis-scaffold)))
			(check analysis-scaffold-gravity
			       (string= "sexpr DAG analyzer scaffold"
					(shared-subdag-gravity row
								analysis-scaffold))))
		      (check analysis-report-filter-role
			     (eq :analysis-scaffold
				 (shared-subdag-role
				  '((lambda (row)
				      (and (plusp (getf row :raw-f64-allocs))
					   (not (canonical-primitive-provider-p
						 (getf row :name)))))))))
		      (let ((work-scheduler-fixture
			      '((slow-b-mutated
				 (make-work-item :name :slow-b
						 :depends-on '(:slow-a)
						 :cost 30d0))
				(manifest
				 (plan-work-schedule
				  :items (list slow-a slow-b-mutated))))))
			(check work-scheduler-role
			       (eq :work-scheduler
				   (shared-subdag-role work-scheduler-fixture)))
			(check work-scheduler-gravity
			       (string= "rosette-work-scheduler"
					(shared-subdag-gravity row
								work-scheduler-fixture))))
		      (let ((bit-packing
			      '((low (logand code 15))
				(high (ldb (byte 2 4) code))
				(low-byte (if (< j 8) low high)))))
			(check bit-packing-role
			       (eq :bit-packing
				   (shared-subdag-role bit-packing)))
				(check bit-packing-gravity
				       (string= "rosette-byte-core"
						(shared-subdag-gravity row bit-packing))))
		      (let ((byte-buffer
			      '(make-array row-bytes
				:element-type '(unsigned-byte 8))))
			(check byte-buffer-role
			       (eq :byte-buffer
				   (shared-subdag-role byte-buffer)))
			(check byte-buffer-gravity
			       (string= "rosette-byte-core"
					(shared-subdag-gravity row byte-buffer))))
		      (let ((byte-buffer-fragment
			      '(row-bytes :element-type '(unsigned-byte 8))))
			(check byte-buffer-fragment-role
			       (eq :byte-buffer
				   (shared-subdag-role byte-buffer-fragment)))
			(check byte-buffer-fragment-gravity
			       (string= "rosette-byte-core"
					(shared-subdag-gravity
					 row byte-buffer-fragment))))
		      (let ((representation-table
			      '(elements 1
				(lambda (g)
				  (if (eq g :e)
				      '((1.0d0))
				      '((-1.0d0)))))))
			(check representation-table-role
			       (eq :representation-table
				   (shared-subdag-role representation-table)))
			(check representation-table-gravity
			       (string= "rosette-representation-core"
					(shared-subdag-gravity
					 row representation-table))))
		      (let ((spectrum-table
			      '((spectrum units)
				(eigenvalue multiplicity zeta))))
			(check spectrum-table-role
			       (eq :spectrum-table
				   (shared-subdag-role spectrum-table)))
				(check spectrum-table-gravity
				       (string= "rosette-multiplicity-spectral"
						(shared-subdag-gravity
						 row spectrum-table))))
		      (let ((geometry-metric
			      '((dx (- x cx))
				(dy (- y cy))
				(r (sqrt (+ (* dx dx) (* dy dy)))))))
			(check geometry-metric-role
			       (eq :geometry-metric
				   (shared-subdag-role geometry-metric)))
			(check geometry-metric-gravity
			       (string= "rosette-vec3-core"
					(shared-subdag-gravity
					 row geometry-metric))))
		      (let ((symbolic-expression
			      '((cf-add (cf-mul (cf-var 'x) (cf-var 'x))
				 (cf-log
				  (cf-add (cf-var 'y) (cf-const 2d0)))))))
			(check symbolic-expression-role
			       (eq :symbolic-expression
				   (shared-subdag-role symbolic-expression)))
				(check symbolic-expression-gravity
				       (string= "rosette-expression-core"
						(shared-subdag-gravity
						 row symbolic-expression))))
		      (let ((finite-difference
			      '(/ (+ (- fp2)
				     (* 16.0d0 fp1)
				     (* -30.0d0 f0)
				     (* 16.0d0 fm1)
				     (- fm2))
				  (* 12.0d0 h h))))
			(check finite-difference-role
			       (eq :finite-difference
				   (shared-subdag-role finite-difference)))
			(check finite-difference-gravity
			       (string= "rosette-diff-ops"
					(shared-subdag-gravity
					 row finite-difference))))
	      (check vector-reduction-role
	             (eq :vector-reduction
	                 (shared-subdag-role
	                  '(dotimes (i (length a) (coerce sum 'single-float))
	                    (incf sum (* (aref a i) (aref b i)))))))
	      (check vector-reduction-gravity
	             (string= "rosette-linear-algebra"
	                      (shared-subdag-gravity
	                       row
	                       '(dotimes (i (length a) (coerce sum 'single-float))
	                         (incf sum (* (aref a i) (aref b i)))))))
	      (check vector-access-pair-role
	             (eq :vector-reduction
	                 (shared-subdag-role '((aref a i) (aref b i)))))
	      (check coerced-vector-access-role
	             (eq :vector-reduction
	                 (shared-subdag-role
	                  '(coerce (aref x i) 'double-float))))
	      (check coerced-vector-access-args-role
	             (eq :vector-reduction
	                 (shared-subdag-role
	                  '((aref x i) 'double-float))))
	      (check coerced-vector-access-list-role
	             (eq :vector-reduction
	                 (shared-subdag-role
	                  '((coerce (aref x i) 'double-float)))))
	      (check coerced-vector-access-gravity
	             (string= "rosette-linear-algebra"
	                      (shared-subdag-gravity
	                       row
	                       '(coerce (aref x i) 'double-float))))
	      (check float-accumulator-finalization-role
	             (eq :vector-reduction
	                 (shared-subdag-role
	                  '(coerce sum 'single-float))))
	      (check float-accumulator-finalization-gravity
	             (string= "rosette-linear-algebra"
	                      (shared-subdag-gravity
	                       row
	                       '(coerce sum 'single-float))))
	      (check vector-element-difference-role
	             (eq :vector-reduction
	                 (shared-subdag-role
	                  '(- (aref a i) (aref b i)))))
	      (check vector-element-abs-difference-role
	             (eq :vector-reduction
	                 (shared-subdag-role
	                  '(abs (- (aref a i) (aref b i))))))
	      (check quoted-literal-shape
	             (eq :literal-data (shared-subdag-shape ''(1))))
	      (check quoted-literal-list-shape
	             (eq :literal-data (shared-subdag-shape '('(1)))))
	      (check nested-literal-table-role
		     (eq :literal-data
			 (shared-subdag-role
			  '('(((0 0) (0 0) (0 0))
			     ((0 0) (1 0) (2 0))
			     ((0 0) (2 0) (1 0)))))))
	      (check wrapped-nested-literal-table-role
		     (eq :literal-data
			 (shared-subdag-role
			  (list '('(((0 0) (0 0) (0 0))
				    ((0 0) (1 0) (2 0))
				    ((0 0) (2 0) (1 0))))))))
	      (check nested-literal-table-gravity
		     (string= "keep as literal data / named constant"
			      (shared-subdag-gravity
			       row
			       '('(((0 0) (0 0) (0 0))
				  ((0 0) (1 0) (2 0))
				  ((0 0) (2 0) (1 0)))))))
	      (check allocation-literal-list-shape
	             (eq :literal-data
	                 (shared-subdag-shape '('single-float :initial-element 0.0))))
	      (check glyph-loader-shape
	             (eq :literal-data
	                 (shared-subdag-shape
	                  '(%glyph (char-code #\A) 1 2 3 4 5))))
	      (check glyph-loader-role
	             (eq :literal-data
	                 (shared-subdag-role
	                  '(%glyph (char-code #\A) 1 2 3 4 5))))
	      (check initial-contents-loader-role
	             (eq :literal-data
	                 (shared-subdag-role
	                  '(defparameter *lut*
	                    (make-array 2
	                     :element-type 'double-float
	                     :initial-contents '(1.0d0 2.0d0))))))
	      (check equality-policy-fragment-shape
	             (eq :equality-policy
	                 (shared-subdag-shape '(test #'equal))))
	      (check equality-policy-fragment-role
	             (eq :equality-policy
	                 (shared-subdag-role '((test #'equal)))))
	      (check dotted-list-classification-does-not-error
	             (shared-subdag-role
	              '((action . weight)
	                in pairs
	                do (check-type action action-functional)
	                (check-type weight real))))
	      (check expression-shape (eq :expression (shared-subdag-shape '(+ x y))))
	      (check motif-classification
	             (eq :typed-vector-result
	                 (shared-subdag-motif '(make-double-float-array 3))))
	      (check role-classification-allocator
	             (eq :allocator
	                 (shared-subdag-role '(make-double-float-array 3))))
	      (check role-classification-grid-allocator
	             (eq :allocator
	                 (shared-subdag-role '(zeros3 n n n))))
	      (check gravity-classification-discrete-grid
	             (string= "rosette-discrete-grid"
	                      (shared-subdag-gravity row '(zeros3 n n n))))
	      (check role-classification-predicate
	             (eq :predicate
	                 (shared-subdag-role '(approx= a b))))
	      (check gravity-classification-array-core
	             (string= "rosette-array-core"
	                      (shared-subdag-gravity
	                       row
	                       '(make-double-float-array 3))))
	      (check normalized-shape-equivalence
	             (equal (normalize-sexpr-shape '(+ x 1))
	                    (normalize-sexpr-shape '(+ y 2))))
	      (check extraction-hint-string
	             (stringp (shared-subdag-extraction-hint row)))
		      (let ((summary (shared-structure-summary index :min-size 3)))
		        (check summary-present (not (null summary)))
		        (check summary-has-beta1
		               (some (lambda (bucket)
		                       (plusp (getf bucket :beta1 0)))
		                     summary))
		        (check summary-has-sample
		               (every (lambda (bucket)
		                        (typep (getf bucket :sample) 'shared-subdag))
		                      summary)))
		      (let ((profile (sexpr-type-profile '(+ x 1))))
		        (check sexpr-type-profile-expression
		               (eq (getf profile :shape) :expression))
		        (check sexpr-type-profile-normalizes
		               (equal (getf profile :normalized-shape)
		                      (normalize-sexpr-shape '(+ y 2)))))
		      (let ((lattice (concept-lattice index :limit 100 :min-size 3)))
	        (check concept-lattice-present (not (null lattice)))
	        (check concept-lattice-has-scope
	               (some (lambda (node)
	                       (eq (getf node :level) :scope))
	                     lattice))
	        (check concept-lattice-has-sample
	               (every (lambda (node)
	                        (typep (getf node :sample) 'shared-subdag))
	                      lattice)))
	      (let ((dot (with-output-to-string (out)
	                   (print-concept-lattice-dot index
	                                              :stream out
	                                              :limit 100
	                                              :min-size 3))))
	        (check concept-lattice-dot-present
	               (search "digraph rosette_sexpr_concept_lattice" dot))
        (check concept-lattice-dot-has-edges
               (search " -> " dot))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (* x x) (* x x))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-hotspot-a/src/core.lisp"
                              :form-index 0))
    (let ((hotspots (library-hotspots index :limit 100 :min-size 3)))
      (check library-hotspots-present (not (null hotspots)))
      (check library-hotspots-have-library
             (every (lambda (row)
                      (stringp (getf row :library)))
                    hotspots))
      (check library-hotspots-have-gravity
             (every (lambda (row)
                      (getf row :top-gravity))
                    hotspots))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (make-double-float-array n)
                      (make-double-float-array n))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                              :form-index 0))
    (check concept-lattice-has-role
           (some (lambda (node)
                   (and (eq (getf node :level) :role)
                        (eq (getf node :key) :allocator)))
                 (concept-lattice index :limit 100 :min-size 3)))
    (check role-summary-present
           (not (null (role-summary index :min-size 3)))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (declaim (optimize (speed 3) (safety 1)))
                      (declaim (optimize (speed 3) (safety 1)))
                      (+ x x)
                      (+ x x))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                              :form-index 0))
    (let ((roles (role-summary index :limit 100 :min-size 3 :actionable-only t))
          (hotspots (library-hotspots index :limit 100 :min-size 3 :actionable-only t)))
      (check actionable-roles-hide-transparent
             (not (some (lambda (node)
                          (member (getf node :key)
                                  '(:compiler-policy :test-harness
                                    :build-metadata :metadata)
                                  :test #'eq))
                        roles)))
      (check actionable-hotspots-present
             (not (null hotspots)))
      (check actionable-hotspots-attribute-source-libraries-only
             (let ((mixed-index (make-dag-index)))
               (sexp->dag '(list (* x x) (* x x))
                          :index mixed-index
                          :root t
                          :provenance '(:file "/tmp/rosette-source/src/core.lisp"
                                        :form-index 0))
               (sexp->dag '(list (* x x) (* x x))
                          :index mixed-index
                          :root t
                          :provenance '(:file "/tmp/rosette-test-only/tests/core.lisp"
                                        :form-index 0))
               (let ((mixed-hotspots
                       (library-hotspots mixed-index
                                         :limit 100
                                         :min-size 3
                                         :actionable-only t)))
                 (and (some (lambda (row)
                              (string= (getf row :library) "rosette-source"))
                            mixed-hotspots)
                      (not (some (lambda (row)
                                   (string= (getf row :library)
                                            "rosette-test-only"))
                                 mixed-hotspots))))))
      (check actionable-hotspots-hide-transparent-sample
             (every (lambda (row)
                      (let* ((sample (getf row :sample))
                             (sexp (and sample
                                        (dag->sexp index
                                                   (shared-subdag-node sample)))))
                        (not (member (and sexp (shared-subdag-role sexp))
                                     '(:compiler-policy :test-harness
                                       :build-metadata :metadata)
                                     :test #'eq))))
                    hotspots))
      (check actionable-roles-hide-metadata
             (let ((metadata-index (make-dag-index)))
               (sexp->dag '(list (simple-array double-float (*))
                                  (simple-array double-float (*))
                                  (+ x x)
                                  (+ x x))
                          :index metadata-index
                          :root t
                          :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                                        :form-index 0))
               (not (some (lambda (node)
                            (eq (getf node :key) :metadata))
                          (role-summary metadata-index
                                        :limit 100
                                        :min-size 3
                                        :actionable-only t)))))
      (check actionable-roles-use-actionable-filter
             (let ((byte-index (make-dag-index)))
               (sexp->dag '(list (make-array n :element-type '(unsigned-byte 8))
                                  (make-array n :element-type '(unsigned-byte 8))
                                  (+ x x)
                                  (+ x x))
                          :index byte-index
                          :root t
                          :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                                        :form-index 0))
               (not (some (lambda (node)
                            (eq (getf node :key) :byte-buffer))
                          (role-summary byte-index
                                        :limit 100
                                        :min-size 3
                                        :actionable-only t)))))
      (check role-summary-print-empty-state
             (search "none at current thresholds"
                     (with-output-to-string (out)
                       (print-role-summary (make-dag-index)
                                           :stream out
                                           :limit 100
                                           :min-size 3
                                           :actionable-only t))))
      (check actionable-hotspots-hide-known-allocators
             (let ((allocator-index (make-dag-index)))
               (sexp->dag '(list ((zeros3 n n n))
                                ((zeros3 n n n))
                                ((out (make-double-float-array n)))
                                ((out (make-double-float-array n)))
                                (+ x x)
                                (+ x x))
                          :index allocator-index
                          :root t
                          :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                                        :form-index 0))
               (not (some (lambda (row)
                            (let* ((sample (getf row :sample))
                                   (sexp (and sample
                                              (dag->sexp allocator-index
                                                         (shared-subdag-node sample)))))
                              (and sexp
                                   (eq (shared-subdag-role sexp) :allocator)
                                   (let ((text (string-upcase
                                                (prin1-to-string sexp))))
                                     (or (search "ZEROS" text)
                                         (search "MAKE-DOUBLE-FLOAT-ARRAY"
                                                 text))))))
                          (library-hotspots allocator-index
                                            :limit 100
                                            :min-size 3
                                            :actionable-only t)))))
      (check actionable-hotspots-hide-foundation-normalizer-only
             (let ((foundation-index (make-dag-index)))
               (sexp->dag (let ((pair (list (list 'coerce 'a (list 'quote 'double-float))
                                             (list 'coerce 'b (list 'quote 'double-float)))))
                            (list 'list pair pair))
                          :index foundation-index
                          :root t
                          :provenance '(:file "/tmp/rosette-foundation-categorical/src/util.lisp"
                                        :form-index 0))
               (not (some (lambda (row)
                            (let* ((sample (getf row :sample))
                                   (sexp (and sample
                                              (dag->sexp foundation-index
                                                         (shared-subdag-node sample)))))
                              (and (string= (getf row :library)
                                            "rosette-foundation-categorical")
                                   (eq (and sexp (shared-subdag-role sexp))
                                       :normalizer))))
                          (library-hotspots foundation-index
                                            :limit 100
                                            :min-size 3
                                            :actionable-only t)))))
      (check actionable-hotspots-keep-nonfoundation-normalizer
             (let ((normalizer-index (make-dag-index)))
               (sexp->dag (let ((pair (list (list 'coerce 'a (list 'quote 'double-float))
                                             (list 'coerce 'b (list 'quote 'double-float)))))
                            (list 'list pair pair))
                          :index normalizer-index
                          :root t
                          :provenance '(:file "/tmp/rosette-other/src/core.lisp"
                                        :form-index 0))
               (some (lambda (row)
                       (string= (getf row :library) "rosette-other"))
                     (library-hotspots normalizer-index
                                       :limit 100
                                       :min-size 3
                                       :actionable-only t)))))
      (check actionable-hotspots-hide-finite-state-scaffold
             (let ((finite-index (make-dag-index)))
               (sexp->dag
                (let ((merge '(apply #'make-state
                                (finite-state-merge-plist-for-states
                                 #'state-values
                                 #'state-weights
                                 #'state-metadata
                                 label
                                 left
                                 right))))
                  (list 'list merge merge))
                :index finite-index
                :root t
                :provenance '(:file "/tmp/rosette-worldmodel-game/src/engine.lisp"
                              :form-index 0))
               (not (some (lambda (row)
                             (let* ((sample (getf row :sample))
                                    (sexp (and sample
                                               (dag->sexp finite-index
                                                          (shared-subdag-node sample)))))
                               (and sexp
                                    (search "FINITE-STATE-MERGE-PLIST-FOR-STATES"
                                            (string-upcase
                                             (prin1-to-string sexp))))))
                           (library-hotspots finite-index
                                             :limit 100
                                             :min-size 3
                                             :actionable-only t))))))
      (check actionable-hotspots-hide-equality-policy
             (let ((eq-index (make-dag-index)))
               (sexp->dag '(list (member a b :test #'equal)
                                  (member c d :test #'equal))
                          :index eq-index
                          :root t
                          :provenance '(:file "/tmp/rosette-eq/src/core.lisp"
                                        :form-index 0))
               (not (some (lambda (row)
                             (let* ((sample (getf row :sample))
                                    (sexp (and sample
                                               (dag->sexp eq-index
                                                          (shared-subdag-node sample)))))
                               (eq (and sexp (shared-subdag-role sexp))
                                   :equality-policy)))
                           (library-hotspots eq-index
                                             :limit 100
                                             :min-size 2
                                             :actionable-only t)))))
      (check actionable-hotspots-hide-coerced-vector-access
             (let ((coerce-index (make-dag-index)))
               (sexp->dag '(list (coerce (aref x i) 'double-float)
                                  (coerce (aref x i) 'double-float))
                          :index coerce-index
                          :root t
                          :provenance '(:file "/tmp/rosette-vectors/src/core.lisp"
                                        :form-index 0))
               (not (some (lambda (row)
                             (let* ((sample (getf row :sample))
                                    (sexp (and sample
                                               (dag->sexp
                                                coerce-index
                                                (shared-subdag-node sample)))))
                               (eq (and sexp (shared-subdag-role sexp))
                                   :vector-reduction)))
                           (library-hotspots coerce-index
                                             :limit 100
                                             :min-size 3
                                             :actionable-only t)))))
      (check actionable-hotspots-hide-float-accumulator-finalization
             (let ((coerce-index (make-dag-index)))
               (sexp->dag '(list (coerce sum 'single-float)
                                  (coerce sum 'single-float))
                          :index coerce-index
                          :root t
                          :provenance '(:file "/tmp/rosette-vectors/src/core.lisp"
                                        :form-index 0))
               (not (some (lambda (row)
                             (let* ((sample (getf row :sample))
                                    (sexp (and sample
                                               (dag->sexp
                                                coerce-index
                                                (shared-subdag-node sample)))))
                               (eq (and sexp (shared-subdag-role sexp))
                                   :vector-reduction)))
                           (library-hotspots coerce-index
                                             :limit 100
                                             :min-size 3
                                             :actionable-only t)))))
      (check actionable-hotspots-hide-vector-element-difference
             (let ((diff-index (make-dag-index)))
               (sexp->dag '(list (abs (- (aref a i) (aref b i)))
                                  (abs (- (aref a i) (aref b i))))
                          :index diff-index
                          :root t
                          :provenance '(:file "/tmp/rosette-vectors/src/core.lisp"
                                        :form-index 0))
               (not (some (lambda (row)
                             (let* ((sample (getf row :sample))
                                    (sexp (and sample
                                               (dag->sexp
                                                diff-index
                                                (shared-subdag-node sample)))))
                               (eq (and sexp (shared-subdag-role sexp))
                                   :vector-reduction)))
                           (library-hotspots diff-index
                                             :limit 100
                                             :min-size 3
                                             :actionable-only t)))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (make-double-float-array n)
                      (make-double-float-array n)
                      (make-double-float-array n))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                              :form-index 0))
    (let ((rules (concept-implications index :limit 100 :min-size 3)))
      (check concept-implications-present (not (null rules)))
      (check concept-implications-have-confidence
             (every (lambda (rule)
                      (>= (getf rule :confidence) 1))
                    rules)))
    (check concept-implications-print-actionable-empty-state
           (search "none at current thresholds"
                   (with-output-to-string (out)
                     (print-concept-implications index
                                                 :stream out
                                                 :limit 100
                                                 :min-size 3
                                                 :actionable-only t)))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (* x x) (* x x)
                      (make-double-float-array n)
                      (make-double-float-array n))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-profile/src/core.lisp"
                              :form-index 0))
    (let ((profile (concept-profile index :limit 100 :min-size 3)))
      (check concept-profile-has-summary
             (plusp (getf (getf profile :summary) :beta1)))
      (check concept-profile-has-concepts
             (not (null (getf profile :concepts))))
	      (check concept-profile-has-hotspot-sample
	             (some (lambda (row)
	                     (getf row :sample))
	                   (getf profile :hotspots))))
	    (let ((certificate (layer-collapse-certificate index :limit 100
	                                                   :min-size 3)))
	      (check layer-collapse-certificate-kind
	             (eq (getf certificate :kind) :layer-collapse-certificate))
	      (check layer-collapse-certificate-has-hotspots
	             (not (null (getf certificate :hotspots))))
	      (check layer-collapse-certificate-role-samples
	             (every (lambda (row)
	                      (getf row :sample))
	                    (getf certificate :primitive-roles)))
	      (check layer-collapse-certificate-has-layer-targets
	             (not (null (getf certificate :layer-targets))))
	      (check layer-collapse-certificate-layer-target-samples
	             (every (lambda (row)
	                      (getf row :sample))
	                    (getf certificate :layer-targets)))
	      (check layer-collapse-certificate-actionable-implications
	             (not (some (lambda (rule)
	                          (let* ((sample (getf rule :sample))
	                                 (role (and sample
	                                            (getf sample :role))))
	                            (member role '(:compiler-policy
	                                           :test-harness
	                                           :build-metadata)
	                                    :test #'eq)))
	                        (getf certificate :implications)))))
	    (check layer-collapse-certificate-print
	           (search "Layer collapse certificate"
	                   (with-output-to-string (out)
	                     (print-layer-collapse-certificate index
	                                                       :stream out
	                                                       :limit 100
	                                                       :min-size 3))))
	    (check layer-target-pressure-print
	           (search "Layer target pressure"
	                   (with-output-to-string (out)
	                     (print-layer-target-pressure index
	                                                  :stream out
	                                                  :limit 100
	                                                  :min-size 3)))))
	    (let ((test-index (make-dag-index)))
	      (sexp->dag '(list (periodic-index i n)
	                        (periodic-index i n))
	                 :index test-index
	                 :root t
	                 :provenance '(:file "/tmp/rosette-test/tests/tests.lisp"
	                               :form-index 0))
	      (check layer-target-pressure-hides-test-only-rows
		     (null (layer-target-pressure test-index
						  :limit 100
						  :min-size 3))))
	    (let ((empty-index (make-dag-index)))
	      (check library-hotspots-print-empty-state
		     (search "none at current thresholds"
			     (with-output-to-string (out)
			       (print-library-hotspots empty-index
						       :stream out
						       :limit 100
						       :min-size 3
						       :actionable-only t)))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (dot a b) (dot a b))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-alpha/src/core.lisp"
                              :form-index 0))
    (sexp->dag '(list (dot a b) (dot a b))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-beta/src/core.lisp"
                              :form-index 0))
    (let ((cones (dependency-cones index
                                   :limit 100
                                   :min-size 3)))
      (check dependency-cones-cross-library
             (some (lambda (row)
                     (and (= (getf row :width) 2)
                          (equal (getf row :libraries)
                                 '("rosette-alpha" "rosette-beta"))))
                   cones))
      (check dependency-cones-print
             (search "Dependency cones"
                     (with-output-to-string (out)
                       (print-dependency-cones index
                                               :stream out
                                               :limit 100
                                               :min-size 3))))))
  (let ((profile (sexpr-type-profile
                  '(let ((y (* x x)))
                    (+ y (make-double-float-array n))))))
    (check sexpr-type-profile-has-shape
           (keywordp (getf profile :shape)))
    (check sexpr-type-profile-has-operator
           (eq (getf profile :operator) 'let)))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (* x x) (* x x)
                      (make-double-float-array n)
                      (make-double-float-array n))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-collapse/src/core.lisp"
                              :form-index 0))
    (let ((certificate (layer-collapse-certificate index
                                                   :limit 100
                                                   :min-size 3)))
      (check layer-collapse-certificate-kind
             (eq (getf certificate :kind) :layer-collapse-certificate))
      (check layer-collapse-certificate-has-summary
             (plusp (getf (getf certificate :summary) :beta1)))
      (check layer-collapse-certificate-has-hotspots
             (not (null (getf certificate :hotspots))))))
  (let ((index (make-dag-index)))
    (sexp->dag '(+ (* x x) 1)
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                              :form-index 0))
    (sexp->dag '(+ (* y y) 2)
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-b/src/core.lisp"
                              :form-index 0))
    (check structural-isomorphism-groups-present
           (not (null (structural-isomorphism-groups index
                                                      :min-size 3
                                                      :min-occurrences 2)))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (12 . :float64) (12 . :float64))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-dotted/src/core.lisp"
                              :form-index 0))
    (check actionable-isomorphisms-dotted-pair-does-not-error
           (not (eq :error
                    (handler-case
                        (progn
                          (structural-isomorphism-groups
                           index
                           :min-size 3
                           :min-occurrences 2
                           :actionable-only t)
                          :ok)
                      (error () :error))))))
  (let ((index (make-dag-index)))
    (sexp->dag '(declare (type double-float x))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-iso-a/src/core.lisp"
                              :form-index 0))
    (sexp->dag '(declare (type single-float y))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-iso-b/src/core.lisp"
                              :form-index 0))
    (sexp->dag '(+ (* x x) 1)
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-iso-a/src/core.lisp"
                              :form-index 1))
    (sexp->dag '(+ (* y y) 2)
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-iso-b/src/core.lisp"
                              :form-index 1))
    (let ((groups (structural-isomorphism-groups
                   index
                   :min-size 3
                   :min-occurrences 2
                   :actionable-only t)))
      (check actionable-isomorphisms-present
             (not (null groups)))
      (check actionable-isomorphisms-have-scope
             (eq (getf (first groups) :scope) :cross-library))
      (check actionable-isomorphisms-have-libraries
             (equal (getf (first groups) :libraries)
                    '("rosette-iso-a" "rosette-iso-b")))
      (check actionable-isomorphisms-cross-scope-filter
             (not (null (structural-isomorphism-groups
                         index
                         :min-size 3
                         :min-occurrences 2
                         :actionable-only t
                         :scope :cross-library))))
      (check actionable-isomorphisms-intra-scope-filter
             (null (structural-isomorphism-groups
                    index
                    :min-size 3
                    :min-occurrences 2
                    :actionable-only t
                    :scope :intra-library)))
      (check actionable-isomorphisms-hide-metadata
             (not (some (lambda (group)
                          (some (lambda (row)
                                  (eq (shared-subdag-role
                                       (shared-subdag-sexp index row))
                                      :metadata))
                                (getf group :rows)))
                        groups)))
      (check actionable-isomorphisms-print
             (let ((report (with-output-to-string (out)
                             (print-structural-isomorphism-groups
                              index
                              :stream out
                              :min-size 3
                              :actionable-only t
                              :scope :cross-library))))
               (and (search "filter:     actionable only" report)
                    (search "scope:      :CROSS-LIBRARY" report)
                    (search "scope=:CROSS-LIBRARY" report)
                    (search "libs=rosette-iso-a,rosette-iso-b" report))))))
  (let ((index (make-dag-index)))
    (sexp->dag '(%glyph (char-code #\A) 1 2 3 4 5)
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-glyph/src/font.lisp"
                              :form-index 0))
    (sexp->dag '(%glyph (char-code #\B) 6 7 8 9 10)
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-glyph/src/font.lisp"
                              :form-index 1))
    (check actionable-isomorphisms-hide-literal-loaders
           (null (structural-isomorphism-groups
                  index
                  :min-size 3
                  :min-occurrences 2
                  :actionable-only t))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (* x x) (* x x)
                      (+ y 1) (+ z 2))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-leaps-a/src/core.lisp"
                              :form-index 0))
    (sexp->dag '(list (* a a) (* a a)
                      (+ b 3) (+ c 4))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-leaps-b/src/core.lisp"
                              :form-index 0))
    (let ((leaps (simplification-leaps index
                                       :limit 5
                                       :min-size 3)))
      (check simplification-leaps-kind
             (eq (getf leaps :kind) :simplification-leaps))
      (check simplification-leaps-has-extractions
             (not (null (getf leaps :extractions))))
      (check simplification-leaps-extraction-sample-profiled
             (let ((sample (getf (first (getf leaps :extractions)) :sample)))
               (and (getf sample :role)
                    (getf sample :gravity)
                    (getf sample :sexp))))
      (check simplification-leaps-has-isomorphisms
             (not (null (getf leaps :isomorphisms))))
      (check simplification-leaps-isomorphisms-have-scope
             (eq (getf (first (getf leaps :isomorphisms)) :scope)
                 :cross-library))
      (check simplification-leaps-print
             (search "Five sexpr simplification leaps"
                     (with-output-to-string (out)
                       (print-simplification-leaps index
                                                   :stream out
                                                   :limit 5
                                                   :min-size 3))))))
  (let ((index (make-dag-index)))
    (sexp->dag '(defun alpha (x)
                  (let ((y (+ x 1)))
                    (* y y)))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-knot-a/src/core.lisp"
                              :form-index 0))
    (sexp->dag '(defun beta (z)
                  (let ((y (+ z 1)))
                    (* y y)))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-knot-b/tests/tests.lisp"
                              :form-index 0))
    (let ((knots (dag-knots index
                            :limit 5
                            :min-size 3
                            :min-occurrences 2)))
      (check dag-knots-present
             (not (null knots)))
      (check dag-knots-have-pressure-metadata
             (let ((row (first knots)))
               (and (plusp (getf row :score))
                    (getf row :scope)
                    (getf row :role)
                    (getf row :gravity)
                    (getf row :sample))))
      (check dag-knots-print
             (search "DAG knots"
                     (with-output-to-string (out)
                       (print-dag-knots index
                                        :stream out
                                        :limit 5
                                        :min-size 3))))))
  (let ((index (make-dag-index)))
    (sexp->dag '(+ (* x x) (* x x))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                              :form-index 0))
    (check extraction-candidates-present
           (not (null (extraction-candidates index
                                             :min-size 3
                                             :min-occurrences 2))))
    (check extraction-patches-print
           (search "Extraction patch drafts"
                   (with-output-to-string (out)
                     (print-extraction-patches index
                                               :stream out
                                               :min-size 3
                                               :min-occurrences 2)))))
  (check extraction-patches-print-empty-state
         (search "none at current thresholds"
                 (with-output-to-string (out)
                   (print-extraction-patches (make-dag-index)
                                             :stream out
                                             :min-size 3
                                             :min-occurrences 2))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second)
                      (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second)
                      (* x x)
                      (* x x))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                              :form-index 0))
    (let ((candidates (extraction-candidates index
                                             :min-size 3
                                             :min-occurrences 2
                                             :limit 10)))
      (check extraction-candidates-hide-timing-scaffolds
             (not (some (lambda (candidate)
                          (let ((sexp (shared-subdag-sexp
                                       index
                                       (extraction-candidate-row candidate))))
                            (eq (shared-subdag-role sexp)
                                :timing-scaffold)))
                        candidates)))
      (check extraction-candidates-keep-actionable-expression
             (some (lambda (candidate)
                     (equal (shared-subdag-sexp
                             index
                             (extraction-candidate-row candidate))
                            '(* x x)))
                   candidates))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (* x y)
                      (* x y)
                      (+ a b)
                      (+ a b))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/hot.lisp"
                              :form-index 0))
    (let* ((costs '((:match "hot.lisp" :weight 7.5d0 :label "hot trace")))
           (candidates (extraction-candidates index
                                              :min-size 3
                                              :min-occurrences 2
                                              :runtime-costs costs
                                              :limit 10))
           (weighted (find 7.5d0 candidates
                           :key #'extraction-candidate-runtime-weight
                           :test #'=)))
      (check extraction-candidates-runtime-cost-weight
             weighted)
      (check extraction-candidates-runtime-cost-score
             (and weighted
                  (= (extraction-candidate-score weighted)
                     (* (shared-subdag-beta1
                         (extraction-candidate-row weighted))
                        7.5d0))))
      (check extraction-candidates-runtime-cost-print
             (search "runtime-weight=7.50"
                     (with-output-to-string (out)
                       (print-extraction-candidates index
                                                    :stream out
                                                    :min-size 3
                                                    :min-occurrences 2
                                                    :runtime-costs costs))))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (* hot-x y)
                      (* hot-x y)
                      (+ cold-x y)
                      (+ cold-x y))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                              :form-index 0))
    (let* ((costs '((:symbol "hot-x" :weight 9d0 :label "traced hot term")))
           (candidates (extraction-candidates index
                                              :min-size 3
                                              :min-occurrences 2
                                              :runtime-costs costs
                                              :limit 10))
           (weighted (find 9d0 candidates
                           :key #'extraction-candidate-runtime-weight
                           :test #'=)))
      (check extraction-candidates-runtime-symbol-weight
             weighted)
      (check extraction-candidates-runtime-symbol-label
             (and weighted
                  (string= (extraction-candidate-runtime-label weighted)
                           "traced hot term")))))
  (let ((index (make-dag-index)))
    (sexp->dag '(list (* hot-x y)
                      (* hot-x y)
                      (+ cold-x y)
                      (+ cold-x y))
                :index index
                :root t
                :provenance '(:file "/tmp/rosette-a/src/core.lisp"
                              :form-index 0))
    (let* ((costs '((:symbol "hot-x" :weight 9d0 :label "traced hot knot")))
           (weighted (first (dag-knots index
                                       :min-size 3
                                       :min-occurrences 2
                                       :runtime-costs costs
                                       :limit 10))))
      (check dag-knots-runtime-symbol-weight
             (and weighted
                  (= 9d0 (getf weighted :runtime-weight))))
      (check dag-knots-runtime-symbol-label
             (and weighted
                  (string= "traced hot knot"
                           (getf weighted :runtime-label))))
      (check dag-knots-runtime-print
             (search "runtime-weight=9.00"
                     (with-output-to-string (out)
                       (print-dag-knots index
                                        :stream out
                                        :min-size 3
                                        :min-occurrences 2
                                        :runtime-costs costs))))))
  (format t "~&;; ----------------------------------------------------------~%")
  (format t "~&;; ~D pass, ~D fail~%" *passes* *fails*)
  (unless (zerop *fails*)
    (error "rosette-sexpr-dag tests failed: ~D failures." *fails*))
  (values *passes* *fails*)))
