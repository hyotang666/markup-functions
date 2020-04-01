(in-package :cl-user)

(defpackage :markup-functions
  (:use :cl)
  (:export))

(in-package :markup-functions)

(defparameter *strict* 'error)

(declaim (type (member error warn nil) *strict*))

(defparameter *global-attributes*
  '(:accesskey ; Specifies a shortcut key to activate/focus an element
    :class ; Specifies one or more classnames for an element (refers to a class
           ; in a style sheet)
    :contenteditable ; Specifies whether the content of an element is editable
                     ; or not
    ;; :data-* ; Used to store custom data private to the page or application
    :dir ; Specifies the text direction for the content in an element
    :draggable ; Specifies whether an element is draggable or not
    :dropzone ; Specifies whether the dragged data is copied, moved, or linked,
              ; when dropped
    :hidden ; Specifies that an element is not yet, or is no longer, relevant
    :id ; Specifies a unique id for an element
    :lang ; Specifies the language of the element's content
    :spellcheck ; Specifies whether the element is to have its spelling and
                ; grammar checked or not
    :style ; Specifies an inline CSS style for an element
    :tabindex ; Specifies the tabbing order of an element
    :title ; Specifies extra information about an element
    :translate ; Specifies whether the content of an element should be
               ; translated or not
    ))

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

(define-compiler-macro tag (&whole whole attributes tag body)
  (if (and (constantp attributes) (constantp tag) (constantp body))
      `(formatter (eval whole))
      whole))

(defun empty-tag (tag)
  (let ((tag (princ-to-string tag)))
    (concatenate 'string "~:<<" tag
                 " ~;~/markup-functions:pprint-attributes/~;>~:>")))

(define-compiler-macro empty-tag (&whole whole tag)
  (if (constantp tag)
      `(formatter ,(eval whole))
      whole))

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

(defmacro standard-attributed-tag-lambda
          (tag-name attributes args &optional invalids)
  (let ((gtag (gensym "TAG-NAME")) (ginvalids (gensym "INVALIDS")))
    `(lambda ()
       (let ((,gtag ,tag-name))
         ,@(when invalids
             `((let ((,ginvalids (intersection *inside-of* ,invalids)))
                 (when ,ginvalids
                   (error "~S is invalid inside of ~S" ,gtag ,ginvalids)))))
         (let ((*inside-of* (cons ,gtag *inside-of*)) (*depth* (1+ *depth*)))
           (format nil
                   (tag ,attributes ,gtag
                        "~{~/markup-function:pprint-put~^ ~_~}")
                   (list (indent) ,args (indent t))))))))

(defmacro define-empty-element (tag-name &key attributes)
  (check-type tag-name symbol)
  `(progn
    (defun ,tag-name (&rest args)
      (lambda () (format nil (formatter ,(empty-tag tag-name)) (list args))))
    (define-compiler-macro ,tag-name (&whole whole &rest args)
      (when *strict*
        (do* ((args args (cddr args))
              (key (car args) (car args)))
             ((null args))
          (when (and (keywordp key)
                     (or (not (find key ,attributes))
                         (not (uiop:string-prefix-p "DATA-" key))))
            (funcall *strict*
                     ,(concatenate 'string "Unknown attributes for "
                                   (princ-to-string tag-name) " tag: ~S")
                     key))))
      whole)))

(set-pprint-dispatch '(cons (member define-empty-element))
                     (pprint-dispatch '(block) nil))
