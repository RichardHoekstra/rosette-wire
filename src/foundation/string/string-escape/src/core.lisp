;;;; core.lisp --- string escaping primitives.

(in-package #:rosette-string-escape)

(defun dot-escape (thing &key readably)
  "Return THING escaped for use inside a Graphviz DOT string label.

When READABLY is true, render THING with PRIN1 first; otherwise use PRINC."
  (let ((text (if readably
                  (prin1-to-string thing)
                  (princ-to-string thing))))
    (declare (type string text))
    (let ((extra 0))
      (declare (type fixnum extra))
      (loop for ch across text
            do (case ch
                 ((#\" #\\ #\Newline) (incf extra))))
      (if (zerop extra)
          text
          (let* ((n (length text))
                 (out (make-string (+ n extra)))
                 (j 0))
            (declare (type fixnum n j))
            (dotimes (i n out)
              (let ((ch (schar text i)))
                (case ch
                  (#\" (setf (schar out j) #\\
                             (schar out (1+ j)) #\")
                       (incf j 2))
                  (#\\ (setf (schar out j) #\\
                             (schar out (1+ j)) #\\)
                       (incf j 2))
                  (#\Newline (setf (schar out j) #\\
                                    (schar out (1+ j)) #\n)
                              (incf j 2))
                  (t (setf (schar out j) ch)
                     (incf j))))))))))
