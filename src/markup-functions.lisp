(in-package :cl-user)

(defpackage :markup-functions
  (:use :cl)
  (:export))

(in-package :markup-functions)

(defparameter *strict* 'error)

(declaim (type (member error warn nil) *strict*))

(defun table (list)
  (let ((ht (make-hash-table :test #'eq)))
    (mapc (lambda (elt) (setf (gethash elt ht) elt)) list)
    ht))

(defparameter *global-attributes*
  (table
    '(:accesskey :class :contenteditable :dir :draggable :dropzone :hidden :id
      :lang :spellcheck :style :tabindex :title :translate
      ;; :data-*
      )))

(defparameter *event-attributes*
  (table
    '(
      ;; Window events.
      :onafterprint :onbeforeprint :onbeforeunload :onerror :onhashchange
      :onload :onmessage :onoffline :ononline :onpagehide :onpageshow
      :onpopstate :onresize :onstorage :onunload
      ;; Form events
      :onblur :onchange :oncontextmenu :onfocus :oninput :oninvalid :onreset
      :onsearch :onselect :onsubmit
      ;; Keyboard events
      :onkeydown :onkeypress :onkeyup
      ;; Mouse events
      :onclick :ondblclick :onmousedown :onmousemove :onmouseout :onmouseover
      :onmouseup :onmousewheel :onwheel
      ;; Drag events
      :ondrag :ondragend :ondragenter :ondragleave :ondragover :ondragstart
      :ondrop :onscroll
      ;; Clipboard events
      :oncopy :oncut :onpaste
      ;; Media events
      :onabort :oncanplay :oncanplaythroug :oncuechange :ondurationchang
      :onemptied :onended :onerror :onloadeddata :onloadedmetadat :onloadstart
      :onpause :onplay :onplaying :onprogress :onratechange :onseeked
      :onseeking :onstalled :onsuspend :ontimeupdate :onvolumechange :onwaiting
      ;; Misc event
      :ontoggle)))

(defun supportedp (key list) (some (lambda (ht) (gethash key ht)) list))

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
                     (or (not (supportedp key ,attributes))
                         (not (uiop:string-prefix-p "DATA-" key))))
            (funcall *strict*
                     ,(concatenate 'string "Unknown attributes for "
                                   (princ-to-string tag-name) " tag: ~S")
                     key))))
      whole)))

(set-pprint-dispatch '(cons (member define-empty-element))
                     (pprint-dispatch '(block) nil))

(define-empty-element meta
  :attributes
  (list (table '(:charset :content :http-equiv :default-style :refresh :name))
        *global-attributes*))

(define-empty-element link
  :attributes
  (list *global-attributes* *event-attributes*
        (table
          '(:crossorigin :href :hreflang :media :referrerpolicy :rel :sizes
            :title :type))))
