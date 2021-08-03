(in-package :cl-user)

(defpackage :markup-functions
  (:use :cl)
  (:export))

(in-package :markup-functions)

(declaim (optimize speed))

(defun compile-time-check (&optional (do? nil suppliedp))
  (let ((tag :htmf-compile-time-check))
    (cond (do? (pushnew tag *features*))
          (suppliedp (setf *features* (remove tag *features*)))
          (t (uiop:featurep tag)))))

(compile-time-check t)

(let* ((main-functions '(html5))
       (standard-elements
        '(#:dummy a abbr b body button div figcaption figure footer form h1 h2
          h3 h4 h5 h6 head header html iframe label li main map* mark nav ol
          option p script select span style table td textarea title tr ul
          blockquote article section strong))
       (empty-elements '(!doctype meta link input br img area))
       (config
        '(*indent* *strict* *print-case* *print-pretty* *optional-attributes*))
       (dev-tools '(list-all-attributes pprint-put compile-time-check))
       (all
        (append main-functions (cdr standard-elements) empty-elements config
                dev-tools)))
  (unless (find-package :htmf)
    (make-package :htmf :use nil))
  (import all :htmf)
  (export all :htmf)
  (defun pprint-element (stream exp)
    (setf stream (or stream *standard-output*))
    (funcall (formatter "~:<~W~^ ~:S~^~1I ~_~@{~W~^ ~_~}~:>") stream exp))
  (set-pprint-dispatch `(cons (member html5 ,@(cdr standard-elements)))
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
      '(:accesskey :autocapitalize :autofocus :contenteditable :dir :draggable
        :enterkeyhint :hidden :inputmode :is :itemid :itemprop :itemref
        :itemscope :itemtype :lang :nonce :spellcheck :style :tabindex :title
        :translate
        ;; :data-*
	:class :id :slot ; from DOM
        )))
  (defparameter *event-attributes*
    (table<-list
      '(;; Window events.
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

(declaim (type list *optional-attributes*))

(defparameter *optional-attributes* nil)

(defgeneric list-all-attributes (thing))

(declaim (ftype (function (keyword list) (values t &optional)) supportedp))

(defun supportedp (key list)
  (or (find key *optional-attributes*)
      (eq :role key)
      (uiop:string-prefix-p "ARIA-" key)
      (some (lambda (ht) (gethash key ht)) list)))

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
        (funcall (formatter "~@[='~W'~]") stream
                 (if (eq t v)
                     key
                     v))
        (pprint-exit-if-list-exhausted)
        (write-char #\Space stream)))))

(defvar *inside-of* nil)

(defparameter *indent* 2)

(defparameter *escape*
  (let ((ht (make-hash-table)))
    (setf (gethash #\' ht) "&quot;"
          (gethash #\& ht) "&amp;"
          (gethash #\< ht) "&lt;"
          (gethash #\> ht) "&gt;")
    ht))

(defun escape (char) (values (gethash char *escape* char)))

(defgeneric pprint-put (stream thing &rest noise)
  (:method (stream (o list) &rest noise)
    (declare (ignore noise))
    (funcall (formatter "~{~/markup-functions:pprint-put/~^~_~}") stream o))
  (:method (stream (o string) &rest noise)
    (declare (ignore noise)
             (type simple-string o))
    (loop :for c :across o
          :do (write (escape c) :stream stream :escape nil)))
  (:method (stream (o function) &rest noise)
    (declare (ignore noise))
    (let ((*standard-output* stream))
      (funcall o)))
  (:method (stream (o rational) &rest noise)
    (declare (ignore noise))
    (write o :stream stream))
  (:method (stream (o float) &rest noise)
    (declare (ignore noise))
    (write o :stream stream)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun <inside-check> (clause name not)
    (when clause
      `((when (and *strict*
                   *inside-of*
                   ,(if not
                        `(not (intersection ,(cadr clause) *inside-of*))
                        `(intersection ,(cadr clause) *inside-of*)))
          (funcall (coerce *strict* 'function)
                   ,(or (getf clause :report)
                        "~A tag is invalid be inside of ~S.")
                   ',name *inside-of*)))))
  (defun <satisfies-check> (clause attributes)
    (let ((satisfies (getf clause :satisfies)))
      (when satisfies
        `((when (and *strict* (not (funcall ,satisfies ,attributes)))
            (funcall (coerce *strict* 'function)
                     ,(or (getf clause :report)
                          "Not satisfies constraint. ~S ~S")
                     ',satisfies ,attributes))))))
  (defun <attributes-checker> (fun-name supported-attributes tag-name)
    `((defun ,fun-name (attributes)
        (if (not (listp attributes))
            (cerror "Ignore." "~A require attributes but ~S." ',tag-name
                    attributes)
            (when *strict*
              (do* ((rest attributes (cddr rest))
                    (key (car rest) (car rest)))
                   ((null rest))
                (when (and (keywordp key)
                           (not (supportedp key ,supported-attributes))
                           (not (uiop:string-prefix-p "DATA-" key)))
                  (restart-case (funcall (coerce *strict* 'function)
                                         "Unknown attributes for tag ~A: ~S"
                                         ',tag-name key)
                    (continue ()
                        :report "Ignore."
                      nil))))))))))

#| BNF
 | (define-empty-element tag-name &body clause+)
 | 
 | tag-name := (and symbol (not (or boolean keyword)))
 | clause := [ attributes-clause | valid-prents-clause | invalid-parents-clause ]
 | 
 | attributes-clause := (:attributes attributes-form attributes-option*)
 | attributes-form := S-expression which generate list of hash tables.
 | 
 | attributes-option := [ satisfies-option | report-option ]
 | 
 | satisfies-option := :satisfies satisfies-function
 | satisfies-function := S-expression which generates function-designator which
 |                       as (function (attributes) generalized-boolean)
 |                       attributes := key value pair.
 | 
 | report-option := :report string
 | 
 | valid-parents-clause := (:valid-parents valid-parents-form report-option?)
 | valid-parents-form := S-expression which generates list which have tag symbols.
 | 
 | invalid-parents-clause := (:invalid-parents invalid-parents-form report-option?)
 | invalid-parents-form := S-expression which generates list which have tag symbols.
 |#

(defmacro define-empty-element (tag-name &body clauses)
  ;; Trivial syntax check.
  (check-type tag-name symbol)
  (dolist (clause clauses)
    (assert (find (car clause)
                  '(:attributes :valid-parents :invalid-parents
                    :documentation))))
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
       (defun ,tag-name (args)
         ,@(let ((documentation (cadr (assoc :documentation clauses))))
             (when documentation
               (list documentation)))
         ,@(when attributes-specified
             `((,checker args))) ; Runtime attributes check.
         ,@(<satisfies-check> (assoc :attributes clauses) 'args)
         ;; Return value closure.
         (lambda ()
           (signal 'element-existance :tag ',tag-name)
           ,@(<inside-check> (assoc :valid-parants clauses) tag-name t)
           ,@(<inside-check> (assoc :invalid-parents clauses) tag-name nil)
           (funcall
             (formatter
              #.(concatenate 'string "~<" ; pprint-logical-block.
                             "<~A" ; open-tag
                             "~@[ ~:I~@_~/markup-functions:pprint-attributes/~]" ; attributes
                             ">" ; close-tag
                             "~:>"))
             *standard-output* (list ',tag-name args))))
       ;; Compile time attribute checker.
       ,@(when attributes-specified
           `((define-compiler-macro ,tag-name (&whole whole args)
               (when (and (compile-time-check) (constantp args))
                 (let ((args (eval args)))
                   ,@(<satisfies-check> (assoc :attributes clauses) 'args)
                   (,checker args)))
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

#++
((lambda (string)
   ;; Easy attributes scraper.
   ;; https://html.spec.whatwg.org/multipage/embedded-content.html
   (sort
     (loop :for line
                :in (uiop:split-string string :separator #.(string #\Newline))
           :with *package* = (find-package :keyword)
           :collect (read-from-string (subseq line 0 (search " — " line))))
     #'string<))
 "alt — Replacement text for use when images are not available
    src — Address of the resource
    srcset — Images to use in different situations, e.g., high-resolution displays, small monitors, etc.
    sizes — Image sizes for different page layouts
    crossorigin — How the element handles crossorigin requests
    usemap — Name of image map to use
    ismap — Whether the image is a server-side image map
    width — Horizontal dimension
    height — Vertical dimension
    referrerpolicy — Referrer policy for fetches initiated by the element
    decoding — Decoding hint to use when processing this image for presentation
    loading — Used when determining loading deferral")

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
             '(:as :color :crossorigin :disabled :href :hreflang :imagesizes
               :imagesrcset :integrity :media :referrerpolicy :rel :sizes
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

(define-empty-element img
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:alt :crossorigin :decoding :height :ismap :loading
               :referrerpolicy :sizes :src :srcset :usemap :width)))
   :satisfies (lambda (attributes) (getf attributes :src))
   :report "Missing src attribute for <img> tag."))

(define-empty-element area
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:alt :coords :download :href :hreflang :media :rel :shape
               :target :type))))
  (:valid-parents '(map*)
   :report "The <area> element is always nested inside a <map> tag."))

(define-condition element-existance ()
  ((tag :initarg :tag :reader existance-tag)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun <require-check> (body clause)
    `(let (elements)
       (handler-bind ((element-existance
                       (lambda (condition)
                         (push (existance-tag condition) elements))))
         (let ((result ,body))
           (when (and *strict* (not (intersection elements ,(cadr clause))))
             (funcall (coerce *strict* 'function)
                      ,(or (getf clause :report)
                           "Missing required elements. ~S")
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
                    ">~VI~_~{~/markup-functions:pprint-put/~^~_~}~I~_</~A>~:>"))))

#| BNF
 | (define-element tag-name &body clause+)
 | 
 | tag-name := (and symbol (not (or boolean keyword)))
 | clause := [ attributes-clause | require-clause | invalid-parents-clause
 |           | documentation-clause | valid-parents-clause | pretty-clause ]
 | 
 | attributes-clause := (:attributes attributes-form attributes-option*)
 | attributes-form := S-expression which generate list of hash tables.
 | 
 | attributes-option := [ satisfies-option | report-option ]
 | 
 | satisfies-option := :satisfies satisfies-function
 | satisfies-function := S-expression which generates function-designator which
 |                       as (function (attributes) generalized-boolean)
 |                       attributes := key value pair.
 | 
 | report-option := :report string
 | 
 | require-clause := (:require require-form report-option?)
 | require-form := S-expression which generates list which have tag symbols.
 | 
 | invalid-parents-clause := (:invalid-parents parents-form report-option?)
 | valid-parents-clause := (:valid-parents parents-form report-option?)
 | parents-form := S-expression which generates list which have tag symbols.
 | 
 | documentation-clause := (:documentation string)
 | 
 | pretty-clause := (:pretty boolean)
|#

(defmacro define-element (name &body clauses)
  ;; Trivial syntax check.
  (check-type name symbol)
  (dolist (clause clauses)
    (assert (find (car clause)
                  '(:attributes :require :invalid-parents :documentation
                    :valid-parents :pretty))))
  ;; Bind
  (let ((supported-attributes (gensym "ATTRIBUTES"))
        (checker (intern (format nil "CHECK-~A-ATTRIBUTES" name))))
    ;; Body
    `(let ((,supported-attributes ,(cadr (assoc :attributes clauses))))
       ;; Attributes checker
       (when ,supported-attributes
         ,(car (<attributes-checker> checker supported-attributes name)))
       ;; Main function.
       (declaim
        (ftype (function (list &rest t) (values function &optional)) ,name))
       (defun ,name (attributes &rest args)
         ,@(let ((documentation (cadr (assoc :documentation clauses))))
             (when documentation
               (list documentation)))
         (when ,supported-attributes
           (,checker attributes))
         ,@(<satisfies-check> (assoc :attributes clauses) 'attributes)
         ;; Return value closure
         (lambda ()
           (signal 'element-existance :tag ',name)
           ,@(<inside-check> (assoc :valid-parents clauses) name t)
           ,@(<inside-check> (assoc :invalid-parents clauses) name nil)
           (let ((*inside-of* (cons ',name *inside-of*))
                 ,@(when (assoc :pretty clauses)
                     `(*print-pretty*)))
             ,(let ((require (assoc :require clauses))
                    (body
                     `(funcall ,(<tag-formatter> nil) *standard-output*
                               (list ',name attributes *indent* args ',name))))
                (if require
                    (<require-check> body require)
                    body)))))
       ;; Compile time attributes check.
       (define-compiler-macro ,name (&whole whole attributes &rest args)
         (declare (ignore args))
         (when (and (compile-time-check) (constantp attributes))
           (let ((attributes (eval attributes)))
             ,@(<satisfies-check> (assoc :attributes clauses) 'attributes)
             (,checker attributes)))
         whole)
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

(define-element button
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:autofocus :disabled :form :formaction :formenctype :formmethod
               :formnovalidate :formtarget :name :type :value)))
   :satisfies (lambda (attributes) (getf attributes :type))
   :report "Always specify the type attribute for a <button> element.~:@_~
   Different browsers use different default types for the <button> element."))

(define-element ul (:attributes (list *global-attributes* *event-attributes*)))

(define-element ol
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list '(:reversed :start :type)))))

(define-element li
  (:attributes (list *global-attributes* *event-attributes*))
  (:valid-parents '(ol ul menu)
   :report
     "The <li> tag is used in ordered lists(<ol>), unordered lists (<ul>), and in menu lists (<menu>)."))

(define-element script
  (:attributes
     (list *global-attributes*
           (table<-list
             '(:src :type :nomodule :async :defer :crossorigin :integrity
               :referrerpolicy)))))

(define-element mark
  (:attributes (list *global-attributes* *event-attributes*)))

(define-element textarea
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:autofocus :cols :dirname :disabled :form :maxlength :name
               :placeholder :readonly :required :rows :wrap))))
  (:pretty nil))

(define-element span
  (:attributes (list *global-attributes* *event-attributes*)))

(define-element abbr
  (:attributes (list *global-attributes* *event-attributes*)))

(define-element map*
  (:attributes
     (list *global-attributes* *event-attributes* (table<-list '(:name)))
   :satisfies (lambda (attributes) (getf attributes :name))
   :report "Missing required attribute :NAME."))

(define-element style
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list '(:media :type))))
  (:pretty nil))

(define-element select
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:autofocus :disabled :form :multiple :name :required :size))))
  (:require '(option) :report "No option inside select tag."))

(define-element option
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list '(:disabled :label :selected :value))))
  (:valid-parents '(select optgroup datalist)
   :report
     "<option> elements go inside a <select>, <optgroup>, or <datalist> element."))

(define-element iframe
  (:attributes
     (list *global-attributes* *event-attributes*
           (table<-list
             '(:allow :allowfullscreen :height :loading :name :referrerpolicy
               :sandbox :src :srcdoc :width)))))

(define-element figure
  (:attributes (list *global-attributes* *event-attributes*)))

(define-element figcaption
  (:attributes (list *global-attributes* *event-attributes*))
  (:valid-parents '(figure)
   :report
     "The <figcaption> element can be placed as the first or last child of the <figure> element."))

(define-element blockquote
  (:attributes (list *global-attributes* (table<-list '(:cite)))))

(define-element article
  (:attributes (list *global-attributes* *event-attributes*))
  (:documentation
     "The <article> tag specifies independent, self-contained content."))

(define-element section
  (:attributes (list *global-attributes* *event-attributes*))
  (:documentation "The <section> tag defines a section in a document."))

(define-element strong
  (:attributes (list *global-attributes* *event-attributes*))
  (:documentation
     "The <strong> tag is used to define text with strong importance.
The content inside is typically displayed in bold."))

(defun html5 (attributes &rest args)
  (with-output-to-string (s)
    (funcall
      (formatter
       "~<~/markup-functions:pprint-put/~:@_~/markup-functions:pprint-put/~:>")
      s (list (!doctype :html) (apply #'html attributes args)))))

#++
(defun retrieve (url)
  (dev:put-expand
    (mapcar (lambda (key) (intern (string-upcase key) :keyword))
            (map 'list #'plump:text
                 (clss:select ".w3-table-all td:first-child"
                              (plump:parse (dex:get url)))))))

#++
(defun cme (form) (funcall (compiler-macro-function (car form)) form nil))