(in-package :cl-user)

(defpackage :markup-functions
  (:use :cl)
  (:export))

(in-package :markup-functions)

(let* ((main-functions '(html5))
       (standard-elements
        '(#:dummy html title head body footer h1 h2 h3 h4 h5 h6 p a div nav
          header main form label b table tr td))
       (empty-elements '(!doctype meta link input br))
       (config '(*indent* *strict* *print-case* *print-pretty*))
       (dev-tools '(list-all-attributes))
       (all
        (append main-functions (cdr standard-elements) empty-elements config
                dev-tools)))
  (unless (find-package :htmf)
    (make-package :htmf :use nil))
  (import all :htmf)
  (export all :htmf)
  (defun pprint-element (stream exp)
    (setf stream (or stream *standard-output*))
    (format stream "~:<~W~^ ~:S~^~1I ~_~@{~W~^ ~_~}~:>" exp))
  (set-pprint-dispatch `(cons (member ,@(cdr standard-elements)))
                       'pprint-element))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *strict* 'error)
  (declaim (type (member error warn nil) *strict*))
  (defun table<-list (list)
    (let ((ht (make-hash-table :test #'eq)))
      (mapc (lambda (elt) (setf (gethash elt ht) elt)) list)
      ht))
  (defparameter *global-attributes*
    (table<-list
      '(:accesskey :class :contenteditable :dir :draggable :dropzone :hidden
        :id :lang :spellcheck :style :tabindex :title :translate
        ;; :data-*
        )))
  (defparameter *event-attributes*
    (table<-list
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
        (pprint-newline :fill stream)))))

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
  (:method (stream (o null) &rest noise) (declare (ignore stream noise))
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

(defun <inside-check> (clause name not)
  (when clause
    `((when (and *strict*
                 *inside-of*
                 ,(if not
                      `(not (intersection ,(cadr clause) *inside-of*))
                      `(intersection ,(cadr clause) *inside-of*)))
        (funcall *strict*
                 ,(or (getf clause :report)
                      "~A tag is invalid be inside of ~S.")
                 ',name *inside-of*)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun <satisfies-check> (clause attributes)
    (let ((satisfies (getf clause :satisfies)))
      (when satisfies
        `((when (and *strict* (not (funcall ,satisfies ,attributes)))
            (funcall *strict*
                     ,(or (getf clause :report)
                          "Not satisfies constraint. ~S ~S")
                     ',satisfies ,attributes)))))))

(defun <attributes-checker> (fun-name supported-attributes tag-name)
  `((defun ,fun-name (attributes)
      (when *strict*
        (do* ((rest attributes (cddr rest))
              (key (car rest) (car rest)))
             ((null rest))
          (when (and (keywordp key)
                     (not (supportedp key ,supported-attributes))
                     (not (uiop:string-prefix-p "DATA-" key)))
            (funcall *strict* "Unknown attributes for tag ~A: ~S" ',tag-name
                     key)))))))

#| BNF
(define-empty-element tag-name &body clause+)

tag-name := (and symbol (not (or boolean keyword)))
clause := [ attributes-clause | valid-prents-clause | invalid-parents-clause ]

attributes-clause := (:attributes attributes-form &rest attributes-option*)
attributes-form := S-expression which generate list of hash tables.

attributes-option := [ satisfies-option | report-option ]

satisfies-option := :satisfies satisfies-function
satisfies-function := S-expression which generates function-designator which
                      as (function (attributes) generalized-boolean)
                      attributes := key value pair.

report-option := :report string

valid-parents-clause := (:valid-parents valid-parents-form report-option?)
valid-parents-form := S-expression which generates list which have tag symbols.

invalid-parents-clause := (:invalid-parents invalid-parents-form report-option?)
invalid-parents-form := S-expression which generates list which have tag symbols.
|#

(defmacro define-empty-element (tag-name &body clauses)
  ;; Trivial syntax check.
  (check-type tag-name symbol)
  (dolist (clause clauses)
    (assert (find (car clause)
                  '(:attributes :valid-parents :invalid-parents))))
  ;; Bindings
  (let ((supported-attributes (intern (format nil "*~A-ATTRIBUTES*" tag-name)))
        (checker (intern (format nil "CHECK-~A-ATTRIBUTES" tag-name)))
        (attributes-specified (cadr (assoc :attributes clauses))))
    ;; Body
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; Special var.
       ,@(when attributes-specified
           `((defparameter ,supported-attributes ,attributes-specified)))
       ;; Attributes checker.
       ,@(when attributes-specified
           (<attributes-checker> checker supported-attributes tag-name))
       ;; Main function.
       (defun ,tag-name (&rest args)
         ,@(when attributes-specified
             `((,checker args))) ; Runtime attributes check.
         ,@(<satisfies-check> (assoc :attributes clauses) 'args)
         ;; Return value closure.
         (lambda ()
           (signal 'element-existance :tag ',tag-name)
           ,@(<inside-check> (assoc :valid-parants clauses) tag-name t)
           ,@(<inside-check> (assoc :invalid-parents clauses) tag-name nil)
           (format nil
                   (formatter
                    "~<<~A~@[ ~:I~@_~/markup-functions:pprint-attributes/~]>~:>")
                   (list ',tag-name args))))
       ;; Compile time attribute checker.
       ,@(when attributes-specified
           `((define-compiler-macro ,tag-name (&whole whole &rest args)
               (,checker args)
               whole)))
       ;; Describe.
       (defmethod list-all-attributes ((s (eql ',tag-name)))
         ,(when attributes-specified
            `(mapcan
               (lambda (table)
                 (loop :for key :being :each :hash-key :of table
                       :collect key))
               ,supported-attributes)))
       ',tag-name)))

(defun pprint-clause (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (funcall (formatter "~:<~@{~W~^~2I ~:_~W~^~I ~_~}~:>") stream exp))

(defun pprint-define-empty-element (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     "~:<~W~^~3I ~@_~1I~W~^ ~_~@{~/markup-functions:pprint-clause/~^ ~:_~}~:>")
    stream exp))

(set-pprint-dispatch '(cons (member define-empty-element define-element))
                     'pprint-define-empty-element)

(define-empty-element !doctype
  (:invalid-parents '(html)
   :report
     "The <html> tag is the container for all other HTML elements (except for the <!DOCTYPE> tag)."))

(define-empty-element meta
  (:attributes
     (list
       (table<-list
         '(:charset :content :http-equiv :default-style :refresh :name))
       *global-attributes*)
   :satisfies
     (lambda (attributes)
       (flet ((must-pair (elt)
                (find elt '(:name :http-equiv))))
         (if (getf attributes :content)
             (some #'must-pair attributes)
             (notany #'must-pair attributes))))
   :report
     "The content attribute MUST be defined if the name or the http-equiv attribute is defined.~:@_~
     If none of these are defined, the content attribute CANNOT be defined.")
  (:valid-parents '(head)
   :report "<meta> tags always go inside the <head> element."))

(define-empty-element link
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:crossorigin :href :hreflang :media :referrerpolicy :rel :sizes
               :title :type))))
  (:valid-parents '(head)
   :report
     "The <link> element goes only in the head section, but it can appear any number of times."))

(define-empty-element input
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:accept :alt :autocomplete :autofocus :checked :dirname
               :disabled :form :formaction :formenctype :formmethod
               :formnovalidate :formtarget :height :list :max :maxlength :min
               :minlength :multiple :name :pattern :placeholder :readonly
               :required :size :src :step :type :value :width)))))

(define-empty-element br
  (:attributes (list *global-attributes* *event-attributes*)))

(define-condition element-existance ()
  ((tag :initarg :tag :reader existance-tag)))

(defun <require-check> (body clause)
  `(let (elements)
     (handler-bind ((element-existance
                     (lambda (condition)
                       (push (existance-tag condition) elements))))
       (let ((result ,body))
         (when (and *strict* (not (intersection elements ,(cadr clause))))
           (funcall *strict*
                    ,(or (getf clause :report) "Missing required elements. ~S")
                    ',(cadr clause)))
         result))))

(defun <tag-formatter> (attributes)
  `(formatter
    ,(concatenate 'string "~<<~A"
                  (if (null attributes)
                      "~@[ ~/markup-functions:pprint-attributes/~]"
                      (with-output-to-string (s)
                        (write-char #\Space s)
                        (pprint-attributes s attributes)
                        (write-string "~*" s)))
                  ">~VI~_~{~/markup-functions:pprint-put/~^ ~:_~}~VI~_</~A>~:>")))

#| BNF
(define-element tag-name &body clause+)

tag-name := (and symbol (not (or boolean keyword)))
clause := [ attributes-clause | require-clause | invalid-parents-clause ]

attributes-clause := (:attributes attributes-form &rest attributes-option*)
attributes-form := S-expression which generate list of hash tables.

attributes-option := [ satisfies-option | report-option ]

satisfies-option := :satisfies satisfies-function
satisfies-function := S-expression which generates function-designator which
                      as (function (attributes) generalized-boolean)
                      attributes := key value pair.

report-option := :report string

require-clause := (:require require-form report-option?)
require-form := S-expression which generates list which have tag symbols.

invalid-parents-clause := (:invalid-parents invalid-parents-form report-option?)
invalid-parents-form := S-expression which generates list which have tag symbols.
|#

(defmacro define-element (name &body clauses)
  ;; Trivial syntax check.
  (check-type name symbol)
  (dolist (clause clauses)
    (assert (find (car clause) '(:attributes :require :invalid-parents))))
  ;; Bind
  (let ((supported-attributes (gensym "ATTRIBUTES"))
        (checker (intern (format nil "CHECK-~A-ATTRIBUTES" name))))
    ;; Body
    `(let ((,supported-attributes ,(cadr (assoc :attributes clauses))))
       ;; Attributes checker
       (when ,supported-attributes
         ,(car (<attributes-checker> checker supported-attributes name)))
       ;; Main function.
       (defun ,name (attributes &rest args)
         (when ,supported-attributes
           (,checker attributes))
         ,@(<satisfies-check> (assoc :attributes clauses) 'attributes)
         ;; Return value closure
         (lambda ()
           (signal 'element-existance :tag ',name)
           ,@(<inside-check> (assoc :invalid-parents clauses) name nil)
           (let ((*inside-of* (cons ',name *inside-of*)) (*depth* (1+ *depth*)))
             ,(let ((require (assoc :require clauses))
                    (body
                     `(format nil ,(<tag-formatter> nil)
                              (list ',name attributes (indent) args (indent t)
                                    ',name))))
                (if require
                    (<require-check> body require)
                    body)))))
       ;; Compile time attributes check.
       (define-compiler-macro ,name (&whole whole attributes &rest args)
         (when (constantp attributes)
           (,checker (eval attributes)))
         (if (constantp attributes)
             `(lambda ()
                (signal 'element-existance :tag ',',name)
                (let ((*inside-of* (cons ',',name *inside-of*))
                      (*depth* (1+ *depth*)))
                  (format nil ,(<tag-formatter> (eval attributes))
                          (list ',',name nil (indent) (list ,@args) (indent t)
                                ',',name))))
             whole))
       ;; Describe
       (defmethod list-all-attributes ((o (eql ',name)))
         (mapcan
           (lambda (table)
             (loop :for key :being :each :hash-key :of table
                   :collect key))
           ,supported-attributes))
       ',name)))

(define-element html
  (:attributes (list *global-attributes* (table<-list '(:xmlns))))
  (:require '(title)
   :report "The <title> tag is required in all HTML documents"))

(define-element title (:attributes (list *global-attributes*)))

(define-element head (:attributes (list *global-attributes*)))

(define-element body
  (:attributes (list *global-attributes* *event-attributes*)))

(define-element footer
  (:attributes (list *global-attributes* *event-attributes*)))

(define-element h1 (:attributes (list *global-attributes* *event-attributes*)))

(define-element h2 (:attributes (list *global-attributes* *event-attributes*)))

(define-element h3 (:attributes (list *global-attributes* *event-attributes*)))

(define-element h4 (:attributes (list *global-attributes* *event-attributes*)))

(define-element h5 (:attributes (list *global-attributes* *event-attributes*)))

(define-element h6 (:attributes (list *global-attributes* *event-attributes*)))

(define-element p (:attributes (list *global-attributes* *event-attributes*)))

(define-element a
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:type :target :rel :referrerpolicy :ping :media :hreflang :href
               :download)))
   :satisfies
     (lambda (attributes)
       (if (intersection '(:download :hreflang :media :rel :target :type)
                         attributes)
           (getf attributes :href)
           t))
   :report "href attribute is not present."))

(define-element div (:attributes (list *global-attributes* *event-attributes*)))

(define-element nav (:attributes (list *global-attributes* *event-attributes*)))

(define-element header
  (:attributes (list *global-attributes* *event-attributes*))
  (:invalid-parents '(footer adress header)
   :report
     "A <header> tag cannot be placed within a <footer>, <address> or another <header> element."))

(define-element main
  (:attributes (list *global-attributes* *event-attributes*))
  (:invalid-parents '(article aside footer header nav)
   :report
     "The <main> element must NOT be a descendant of an <article>, <aside>, <footer>, <header>, or <nav> element."))

(define-element form
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:accept-charset :action :autocomplete :enctype :method :name
               :novalidate :target)))))

(define-element label
  (:attributes
     (list *global-attributes* *event-attributes* (table<-list '(:for :form)))))

(define-element b (:attributes (list *global-attributes* *event-attributes*)))

(define-element table
  (:attributes (list *global-attributes* *event-attributes*)))

(define-element tr (:attributes (list *global-attributes* *event-attributes*)))

(define-element td
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list '(:colspan :headers :rowspan)))))

(defun html5 (attributes &rest args)
  (concatenate 'string (funcall (!doctype :html)) (format nil "~<~:@_~:>" nil)
               (funcall (apply #'html attributes args))))

#++
(defun retrieve (url)
  (dev:put-expand
    (mapcar (lambda (key) (intern (string-upcase key) :keyword))
            (map 'list #'plump:text
                 (clss:select "#table1 td:first-child"
                              (plump:parse (dex:get url)))))))

#++
(defun cme (form) (funcall (compiler-macro-function (car form)) form nil))
