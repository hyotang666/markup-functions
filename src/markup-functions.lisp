(in-package :cl-user)

(defpackage :markup-functions
  (:use :cl)
  (:export))

(in-package :markup-functions)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *strict* 'error)
  (declaim (type (member error warn nil) *strict*))
  (defun table (list)
    (let ((ht (make-hash-table :test #'eq)))
      (mapc (lambda (elt) (setf (gethash elt ht) elt)) list)
      ht))
  (defparameter *global-attributes*
    (table
      '(:accesskey :class :contenteditable :dir :draggable :dropzone :hidden
        :id :lang :spellcheck :style :tabindex :title :translate
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
        :onblur :onchange :oncontextmenu :onfocus :oninput :oninvalid
        :onreset :onsearch :onselect :onsubmit
        ;; Keyboard events
        :onkeydown :onkeypress :onkeyup
        ;; Mouse events
        :onclick :ondblclick :onmousedown :onmousemove :onmouseout
        :onmouseover :onmouseup :onmousewheel :onwheel
        ;; Drag events
        :ondrag :ondragend :ondragenter :ondragleave :ondragover :ondragstart
        :ondrop :onscroll
        ;; Clipboard events
        :oncopy :oncut :onpaste
        ;; Media events
        :onabort :oncanplay :oncanplaythroug :oncuechange :ondurationchang
        :onemptied :onended :onerror :onloadeddata :onloadedmetadat
        :onloadstart :onpause :onplay :onplaying :onprogress :onratechange
        :onseeked :onseeking :onstalled :onsuspend :ontimeupdate
        :onvolumechange :onwaiting
        ;; Misc event
        :ontoggle))))

(defgeneric list-all-attributes
    (thing))

(defun supportedp (key list) (some (lambda (ht) (gethash key ht)) list))

(defun pprint-attributes (stream args &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream args)
    (do ((key)
         (*print-escape* nil))
        (nil)
      (pprint-exit-if-list-exhausted)
      (write (setf key (pprint-pop)) :stream stream)
      (pprint-exit-if-list-exhausted)
      (let ((v (pprint-pop)))
        (format stream "='~W'"
                (if (eq t v)
                    key
                    v))
        (pprint-exit-if-list-exhausted)
        (write-char #\Space stream)
        (pprint-newline :miser stream)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun empty-tag (tag)
    (let ((tag (princ-to-string tag)))
      (concatenate 'string "~:<<" tag
                   " ~;~/markup-functions:pprint-attributes/~;>~:>"))))

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

(defmacro define-empty-element (tag-name &body clauses)
  (check-type tag-name symbol)
  (dolist (clause clauses)
    (assert (find (car clause) '(:attributes :valid-parents :satisfies))))
  (let ((var (intern (format nil "*~A-ATTRIBUTES*" tag-name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,var ,(cadr (assoc :attributes clauses)))
       (defun ,tag-name (&rest args)
         (lambda ()
           ,@(let ((valid-parents (find :valid-parents clauses :key #'car)))
               (when valid-parents
                 `((when (and *strict*
                              *inside-of*
                              (not
                                (intersection ,(cadr valid-parents)
                                              *inside-of*)))
                     (funcall *strict*
                              ,(or (getf valid-parents :report)
                                   "~A tag is invalid be inside of ~S")
                              ',tag-name *inside-of*)))))
           ,@(let ((satisfies (find :satisfies clauses :key #'car)))
               (when satisfies
                 `((when *strict*
                     (unless (funcall ,(cadr satisfies) args)
                       (funcall *strict*
                                ,(or (getf satisfies :report)
                                     "Not satisfies constraint. ~S ~S")
                                ',(cadr satisfies) args))))))
           (format nil (formatter ,(empty-tag tag-name)) (list args))))
       (define-compiler-macro ,tag-name (&whole whole &rest args)
         (when *strict*
           (do* ((args args (cddr args))
                 (key (car args) (car args)))
                ((null args))
             (when (and (keywordp key)
                        ,var
                        (not (supportedp key ,var))
                        (not (uiop:string-prefix-p "DATA-" key)))
               (funcall *strict*
                        ,(concatenate 'string "Unknown attributes for tag "
                                      (princ-to-string tag-name) ": ~S")
                        key))))
         whole)
       (defmethod list-all-attributes ((s (eql ',tag-name)))
         (mapcan
           (lambda (table)
             (loop :for key :being :each :hash-key :of table
                   :collect key))
           ,var))
       ',tag-name)))

(defun pprint-clause (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (format stream "~:<~@{~W~^~2I ~:_~W~^~I ~_~}~:>" exp))

(defun pprint-define-empty-element (stream exp)
  (setf stream (or stream *standard-output*))
  (format stream
          "~:<~W~^~3I ~@_~1I~W~^ ~:_~@{~/markup-functions:pprint-clause/~^ ~:_~}~:>"
          exp))

(set-pprint-dispatch '(cons (member define-empty-element))
                     'pprint-define-empty-element)

(define-empty-element !doctype)

(define-empty-element meta
  (:attributes
     (list
       (table '(:charset :content :http-equiv :default-style :refresh :name))
       *global-attributes*))
  (:valid-parents '(head)
   :report "<meta> tags always go inside the <head> element.")
  (:satisfies
     (lambda (attributes)
       (flet ((must-pair (elt)
                (find elt '(:name :http-equiv))))
         (if (getf attributes :content)
             (some #'must-pair attributes)
             (notany #'must-pair attributes))))
   :report
     "The content attribute MUST be defined if the name or the http-equiv attribute is defined.~:@_~
     If none of these are defined, the content attribute CANNOT be defined."))

(define-empty-element link
  (:attributes
     (list *global-attributes* *event-attributes*
           (table
             '(:crossorigin :href :hreflang :media :referrerpolicy :rel :sizes
               :title :type))))
  (:valid-parents '(head)
   :report
     "The <link> element goes only in the head section, but it can appear any number of times."))

(define-empty-element input
  (:attributes
     (list *global-attributes* *event-attributes*
           (table
             '(:accept :alt :autocomplete :autofocus :checked :dirname
               :disabled :form :formaction :formenctype :formmethod
               :formnovalidate :formtarget :height :list :max :maxlength :min
               :minlength :multiple :name :pattern :placeholder :readonly
               :required :size :src :step :type :value :width)))))

(defmacro define-element (name &key attributes)
  (check-type name symbol)
  (let ((attr (gensym "ATTRIBUTES")))
    `(let ((,attr ,attributes))
       (defun ,name (attributes &rest args)
         (lambda ()
           (let ((*inside-of* (cons ',name *inside-of*)))
             (format nil
                     (formatter
                      ,(concatenate 'string "~<<" (princ-to-string name)
                                    "~@[ ~/markup-functions:pprint-attributes/~]>"
                                    "~VI~_~{~/markup-functions:pprint-put/~}~VI~_</"
                                    (princ-to-string name) ">~:>"))
                     (list attributes (indent) args (indent t))))))
       (define-compiler-macro ,name (&whole whole attributes &rest args)
         (when (and *strict* (constantp attributes))
           (do* ((rest (eval attributes) (cddr rest))
                 (key (car rest) (car rest)))
                ((null rest))
             (when (and (keywordp key)
                        (or (not (supportedp key ,attr))
                            (not (uiop:string-prefix-p "DATA-" key))))
               (funcall *strict*
                        ,(concatenate 'string "Unknown attributes for tag "
                                      (princ-to-string name) ": ~S")
                        key))))
         (if (constantp attributes)
             `(lambda ()
                (let ((*inside-of* (cons ',',name *inside-of*)))
                  (format nil
                          (formatter
                           ,(concatenate 'string "~<<" (princ-to-string ',name)
                                         (if (null attributes)
                                             ""
                                             (with-output-to-string (s)
                                               (write-char #\Space s)
                                               (pprint-attributes s
                                                                  (eval
                                                                    attributes))))
                                         ">~VI~_~{~/markup-functions:pprint-put/~}~VI~_</"
                                         (princ-to-string ',name) ">~:>"))
                          (list (indent) (list ,@args) (indent t)))))
             whole))
       (defmethod list-all-attributes ((o (eql ',name)))
         (mapcan
           (lambda (table)
             (loop :for key :being :each :hash-key :of table
                   :collect key))
           ,attr))
       ',name)))

(set-pprint-dispatch '(cons (member define-element))
                     (pprint-dispatch '(block) nil))

(define-element html :attributes (list *global-attributes* (table '(:xmlns))))

(define-element title :attributes (list *global-attributes*))

(define-element head :attributes (list *global-attributes*))

(define-element body :attributes (list *global-attributes* *event-attributes*))

(define-element footer
  :attributes
  (list *global-attributes* *event-attributes*))

(define-element h1 :attributes (list *global-attributes* *event-attributes*))

(define-element h2 :attributes (list *global-attributes* *event-attributes*))

(define-element h3 :attributes (list *global-attributes* *event-attributes*))

(define-element h4 :attributes (list *global-attributes* *event-attributes*))

(define-element h5 :attributes (list *global-attributes* *event-attributes*))

(define-element h6 :attributes (list *global-attributes* *event-attributes*))

(define-element p :attributes (list *global-attributes* *event-attributes*))

(define-element a
  :attributes
  (list *global-attributes* *event-attributes*
        (table
          '(:type :target :rel :referrerpolicy :ping :media :hreflang :href
            :download))))

(define-element div :attributes (list *global-attributes* *event-attributes*))

(define-element nav :attributes (list *global-attributes* *event-attributes*))

(define-element header
  :attributes
  (list *global-attributes* *event-attributes*))

(define-element main :attributes (list *global-attributes* *event-attributes*))

(define-element form
  :attributes
  (list *global-attributes* *event-attributes*
        (table
          '(:accept-charset :action :autocomplete :enctype :method :name
            :novalidate :target))))

(define-element label
  :attributes
  (list *global-attributes* *event-attributes* (table '(:for :form))))

(defun html5 (attributes &rest args)
  (concatenate 'string (funcall (!doctype :html)) #.(string #\Newline)
               (funcall (apply #'html attributes args))))

#++
(defun retrieve (url)
  (dev:put-expand
    (mapcar (lambda (key) (intern (string-upcase key) :keyword))
            (map 'list #'plump:text
                 (clss:select "#table1 td:first-child"
                              (plump:parse (dex:get url)))))))
