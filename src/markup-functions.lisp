(in-package :cl-user)

(defpackage :markup-functions
  (:use :cl)
  (:export))

(in-package :markup-functions)

(defun pprint-attributes (stream args &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream args)
    (do ((key (pprint-pop) (pprint-pop))
         (*print-escape* nil))
        (nil)
      (write key :stream stream)
      (pprint-exit-if-list-exhausted)
      (let ((v (pprint-pop)))
        (format stream "='~W'"
                (if (eq t v)
                    key
                    v))
        (pprint-exit-if-list-exhausted)
        (write-char #\Space stream)
        (pprint-newline :miser stream)))))

(defun tag (attributes tag body)
  (let ((tag (princ-to-string tag)))
    (concatenate 'string "~<<" tag
                 (let ((attributes
                        (with-output-to-string (s)
                          (pprint-attributes s attributes))))
                   (if (string= "" attributes)
                       attributes
                       (concatenate 'string " " attributes)))
                 ">~VI~_" body "~VI~_</" tag ">~:>")))

(defvar *inside-of* nil)

(defvar *depth* 0)

(defparameter *indent* 2)

(defun indent (&optional de-indent-p)
  (*
    (if de-indent-p
        (1- *depth*)
        *depth*)
    *indent*))

(defgeneric pprint-put
    (stream thing &rest noise)
  (:method (stream (o null) &rest noise) (declare (ignore noise))
   ;; do nothing
   nil)
  (:method (stream (o string) &rest noise) (declare (ignore noise))
   (write-string o stream))
  (:method (stream (o function) &rest noise) (declare (ignore noise))
   (write-string (funcall o) stream))
  (:method (stream (o rational) &rest noise) (declare (ignore noise))
   (write o :stream stream))
  (:method (stream (o float) &rest noise) (declare (ignore noise))
   (write o :stream stream)))
