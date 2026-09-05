;;;; core.lisp --- dependency-free metadata plist hygiene.

(in-package #:rosette-metadata-core)

(defun plist-p (value)
  "Return true when VALUE is a plist with symbol keys."
  (and (listp value)
       (evenp (length value))
       (loop for (key nil) on value by #'cddr
             always (symbolp key))))

(defun require-plist (value &key (label "METADATA"))
  "Return VALUE when it is a metadata plist; otherwise signal an error."
  (unless (plist-p value)
    (error "~A must be a plist with symbol keys, got ~S." label value))
  value)

(defun copy-plist (plist &key (label "METADATA"))
  "Validate and copy PLIST so records do not retain caller-owned conses."
  (copy-list (require-plist plist :label label)))

(defun merge-plist-right (&rest plists)
  "Return a fresh plist with later PLISTS overriding earlier values."
  (let ((result nil))
    (dolist (plist plists (copy-list result))
      (require-plist plist)
      (loop for (key value) on plist by #'cddr
            do (setf (getf result key) value)))))

(defun keyword-plist-p (value)
  "Return true when VALUE is a plist with keyword keys."
  (and (listp value)
       (evenp (length value))
       (loop for (key nil) on value by #'cddr
             always (keywordp key))))

(defun require-keyword-plist (value &key (label "KEYWORD-METADATA"))
  "Return VALUE when it is a keyword-keyed plist; otherwise signal an error."
  (unless (keyword-plist-p value)
    (error "~A must be a plist with keyword keys, got ~S." label value))
  value)

(defun copy-keyword-plist (plist &key (label "KEYWORD-METADATA"))
  "Validate and copy a keyword-keyed PLIST."
  (copy-list (require-keyword-plist plist :label label)))

(defun tagged-term-p (value tag)
  "Return true when VALUE is a cons of TAG and a keyword-keyed plist."
  (and (consp value)
       (eq tag (first value))
       (keyword-plist-p (rest value))))

(defun required-key-p (plist key)
  "Return true when PLIST is a keyword-keyed plist containing KEY."
  (and (keyword-plist-p plist)
       (member key plist :test #'eq)))

(defun tagged-term-key-p (value tag key)
  "Return true when VALUE is a tagged term with TAG and body contains KEY."
  (and (tagged-term-p value tag)
       (required-key-p (rest value) key)))

(defun copy-plist-tree (value)
  "Copy VALUE's tree structure, safe for non-consp atoms."
  (if (consp value) (copy-tree value) value))
