;;;; ppm.lisp --- binary PPM (P6) header + pixel writer + tiny reader.
;;;;
;;;; The Portable Pixmap (PPM) "P6" binary format is a 4-line ASCII
;;;; header followed by a raw RGB byte buffer.  Reference: Jef
;;;; Poskanzer's NetPBM specification (http://netpbm.sourceforge.net/
;;;; doc/ppm.html).
;;;;
;;;;   P6\n
;;;;   <width> <height>\n
;;;;   <maxval>\n
;;;;   <width * height * 3 bytes of raw RGB>
;;;;
;;;; We hard-code MAXVAL = 255 because the framebuffer is
;;;; U8.
;;;;
;;;; The writer takes any U8 STREAM (file, in-memory,
;;;; or a Unix pipe to ffmpeg).  WRITE-PPM-HEADER emits the header in
;;;; a single WRITE-SEQUENCE so a partial flush can't corrupt the
;;;; maxval line halfway across processes.

(in-package #:rosette-raster-core)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defun write-ppm-header (stream width height)
  "Write the binary PPM (P6) ASCII header to STREAM in a single
WRITE-SEQUENCE.  STREAM must be a U8 output stream."
  (declare (type fixnum width height))
  (let* ((s (format nil "P6~%~D ~D~%255~%" width height))
         (buf (the u8-vector (make-byte-vector (length s)))))
    (loop for i of-type fixnum below (length s)
          do (setf (aref buf i) (char-code (char s i))))
    (write-sequence buf stream)))

(defun write-ppm-pixels (stream frame)
  "Write the raw RGB byte buffer of FRAME to STREAM.  STREAM must be an
U8 output stream."
  (declare (type rosette-frame frame))
  (write-sequence (rosette-frame-pixels frame) stream))

(defun write-ppm-file (path frame &key (if-exists :supersede))
  "Write FRAME to PATH as a binary PPM (P6) file."
  (with-open-file (out path
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist :create
                       :element-type 'u8)
    (write-ppm-header out (rosette-frame-width frame) (rosette-frame-height frame))
    (write-ppm-pixels out frame))
  path)

;;; --- tiny reader (used in tests for round-trip verification) ---------

(defun %read-ascii-line (stream)
  "Read a single LF-terminated ASCII line from a U8 stream."
  (with-output-to-string (out)
    (loop for byte = (read-byte stream nil nil)
          while (and byte (/= byte (char-code #\Newline)))
          do (write-char (code-char byte) out))))

(defun %skip-ppm-comments (stream)
  "Peek at STREAM and consume any PPM '#' comment lines."
  (loop
    (let ((b (peek-char nil stream nil nil)))
      (declare (ignore b)))                     ; we don't actually peek-char on byte streams
    (return)))

(defun read-ppm-file (path)
  "Read a P6 PPM from PATH, returning a fresh ROSETTE-FRAME.  The reader is
intentionally minimal: it accepts P6, MAXVAL=255, no comments — i.e.
exactly what WRITE-PPM-FILE produces.  Used for round-trip tests."
  (with-open-file (in path
                      :direction :input
                      :element-type 'u8)
    (let* ((magic (%read-ascii-line in))
           (dims  (%read-ascii-line in))
           (mv    (%read-ascii-line in)))
      (unless (string= magic "P6")
        (error 'raster-error
               :format-control "PPM magic mismatch (~S, expected \"P6\") in ~A"
               :format-arguments (list magic path)))
      (unless (string= mv "255")
        (error 'raster-error
               :format-control "PPM maxval mismatch (~S, expected \"255\") in ~A"
               :format-arguments (list mv path)))
      (let* ((space (position #\Space dims))
             (w (parse-integer dims :start 0 :end space))
             (h (parse-integer dims :start (1+ space)))
             (n (* 3 w h))
             (frame (make-frame w h)))
        (let ((px (rosette-frame-pixels frame)))
          (declare (type rgb-buffer px))
          (let ((nread (read-sequence px in :end n)))
            (unless (= nread n)
              (error 'raster-error
                     :format-control "PPM pixel buffer short read: ~D / ~D bytes"
                     :format-arguments (list nread n)))))
        frame))))
