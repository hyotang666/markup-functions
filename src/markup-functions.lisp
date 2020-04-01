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

(defparameter *event-attributes*
  '(
    ;; Window events.
    :onafterprint ; script Script to be run after the document is printed
    :onbeforeprint ; script Script to be run before the document is printed
    :onbeforeunload ; script Script to be run when the document is about to be
                    ; unloaded
    :onerror ; script Script to be run when an error occurs
    :onhashchange ; script Script to be run when there has been changes to the
                  ; anchor part of the a URL
    :onload ; script Fires after the page is finished loading
    :onmessage ; script Script to be run when the message is triggered
    :onoffline ; script Script to be run when the browser starts to work offline
    :ononline ; script Script to be run when the browser starts to work online
    :onpagehide ; script Script to be run when a user navigates away from a page
    :onpageshow ; script Script to be run when a user navigates to a page
    :onpopstate ; script Script to be run when the window's history changes
    :onresize ; script Fires when the browser window is resized
    :onstorage ; script Script to be run when a Web Storage area is updated
    :onunload ; script Fires once a page has unloaded (or the browser window
              ; has been closed)
    ;; Form events
    :onblur ; Fires the moment that the element loses focus
    :onchange ; Fires the moment when the value of the element is changed
    :oncontextmenu ; Script to be run when a context menu is triggered
    :onfocus ; Fires the moment when the element gets focus
    :oninput ; Script to be run when an element gets user input
    :oninvalid ; Script to be run when an element is invalid
    :onreset ; Fires when the Reset button in a form is clicked
    :onsearch ; Fires when the user writes something in a search field (for
              ; <input="search">)
    :onselect ; Fires after some text has been selected in an element
    :onsubmit ; Fires when a form is submitted
    ;; Keyboard events
    :onkeydown ; Fires when a user is pressing a key
    :onkeypress ; Fires when a user presses a key
    :onkeyup ; Fires when a user releases a key
    ;; Mouse events
    :onclick ; Fires on a mouse click on the element
    :ondblclick ; Fires on a mouse double-click on the element
    :onmousedown ; Fires when a mouse button is pressed down on an element
    :onmousemove ; Fires when the mouse pointer is moving while it is over an
                 ; element
    :onmouseout ; Fires when the mouse pointer moves out of an element
    :onmouseover ; Fires when the mouse pointer moves over an element
    :onmouseup ; Fires when a mouse button is released over an element
    :onmousewheel ; Deprecated. Use the onwheel attribute instead
    :onwheel ; Fires when the mouse wheel rolls up or down over an element
    ;; Drag events
    :ondrag ; Script to be run when an element is dragged
    :ondragend ; Script to be run at the end of a drag operation
    :ondragenter ; Script to be run when an element has been dragged to a valid
                 ; drop target
    :ondragleave ; Script to be run when an element leaves a valid drop target
    :ondragover ; Script to be run when an element is being dragged over a
                ; valid drop target
    :ondragstart ; Script to be run at the start of a drag operation
    :ondrop ; Script to be run when dragged element is being dropped
    :onscroll ; Script to be run when an element's scrollbar is being scrolled
    ;; Clipboard events
    :oncopy ; Fires when the user copies the content of an element
    :oncut ; Fires when the user cuts the content of an element
    :onpaste ; Fires when the user pastes some content in an element
    ;; Media events
    :onabort ; Script to be run on abort
    :oncanplay ; Script to be run when a file is ready to start playing (when
               ; it has buffered enough to begin)
    :oncanplaythroug ; Script to be run when a file can be played all the way
                     ; to the end without pausing for buffering
    :oncuechange ; Script to be run when the cue changes in a <track> element
    :ondurationchang ; Script to be run when the length of the media changes
    :onemptied ; Script to be run when something bad happens and the file is
               ; suddenly unavailable (like unexpectedly disconnects)
    :onended ; Script to be run when the media has reach the end (a useful
             ; event for messages like "thanks for listening")
    :onerror ; Script to be run when an error occurs when the file is being
             ; loaded
    :onloadeddata ; Script to be run when media data is loaded
    :onloadedmetadat ; Script to be run when meta data (like dimensions and
                     ; duration) are loaded
    :onloadstart ; Script to be run just as the file begins to load before
                 ; anything is actually loaded
    :onpause ; Script to be run when the media is paused either by the user or
             ; programmatically
    :onplay ; Script to be run when the media is ready to start playing
    :onplaying ; Script to be run when the media actually has started playing
    :onprogress ; Script to be run when the browser is in the process of
                ; getting the media data
    :onratechange ; Script to be run each time the playback rate changes (like
                  ; when a user switches to a slow motion or fast forward mode)
    :onseeked ; Script to be run when the seeking attribute is set to false
              ; indicating that seeking has ended
    :onseeking ; Script to be run when the seeking attribute is set to true
               ; indicating that seeking is active
    :onstalled ; Script to be run when the browser is unable to fetch the media
               ; data for whatever reason
    :onsuspend ; Script to be run when fetching the media data is stopped
               ; before it is completely loaded for whatever reason
    :ontimeupdate ; Script to be run when the playing position has changed
                  ; (like when the user fast forwards to a different point in
                  ; the media)
    :onvolumechange ; Script to be run each time the volume is changed which
                    ; (includes setting the volume to "mute")
    :onwaiting ; Script to be run when the media has paused but is expected to
               ; resume (like when the media pauses to buffer more data)
    ;; Misc event
    :ontoggle ; Fires when the user opens or closes the <details> element
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

(define-empty-element meta
  :attributes
  (list* :charset
         :content :http-equiv
         :default-style :refresh
         :name *global-attributes*))
