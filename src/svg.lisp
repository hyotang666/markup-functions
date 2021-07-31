(in-package :markup-functions)

(let* ((main-functions 'nil)
       (standard-elements '(svg))
       (empty-elements '(path))
       (all (append main-functions (cdr standard-elements) empty-elements)))
  (unless (find-package :htmf.svg)
    (make-package :htmf.svg :use nil))
  (import all :htmf.svg)
  (export all :htmf.svg)
  #++
  (set-pprint-dispatch `(cons (member ,@(cdr standard-elements)))
                       'pprint-element))

(defparameter *svg-aria-attributes*
  (table<-list
    '(:aria-activedescendant :aria-atomic :aria-autocomplete :aria-busy
      :aria-checked :aria-colcount :aria-colindex :aria-colspan :aria-controls
      :aria-current :aria-describedby :aria-details :aria-disabled
      :aria-dropeffect :aria-errormessage :aria-expanded :aria-flowto
      :aria-grabbed :aria-haspopup :aria-hidden :aria-invalid
      :aria-keyshortcuts :aria-label :aria-labelledby :aria-level :aria-live
      :aria-modal :aria-multiline :aria-multiselectable :aria-orientation
      :aria-owns :aria-placeholder :aria-posinset :aria-pressed :aria-readonly
      :aria-relevant :aria-required :aria-roledescription :aria-rowcount
      :aria-rowindex :aria-rowspan :aria-selected :aria-setsize :aria-sort
      :aria-valuemax :aria-valuemin :aria-valuenow :aria-valuetext :role)))

(defparameter *svg-conditional-processing-attributes*
  (table<-list '(:requiredextensions :systemlanguage)))

(defparameter *svg-core-attributes*
  (table<-list '(:id :tabindex :autofocus :lang :xml :space :class :style)))

(defparameter *svg-document-event-attributes*
  (table<-list '(:onunload :onabort :onerror :onresize :onscroll)))

(defparameter *svg-global-event-attributes*
  (table<-list
    '(:oncancel :oncanplay :oncanplaythrough :onchange :onclick :onclose
      :oncuechange :ondblclick :ondrag :ondragend :ondragenter :ondragexit
      :ondragleave :ondragover :ondragstart :ondrop :ondurationchange
      :onemptied :onended :onerror :onfocus :oninput :oninvalid :onkeydown
      :onkeypress :onkeyup :onload :onloadeddata :onloadedmetadata :onloadstart
      :onmousedown :onmouseenter :onmouseleave :onmousemove :onmouseout
      :onmouseover :onmouseup :onpause :onplay :onplaying :onprogress
      :onratechange :onreset :onresize :onscroll :onseeked :onseeking :onselect
      :onshow :onstalled :onsubmit :onsuspend :ontimeupdate :ontoggle
      :onvolumechange :onwaiting :onwheel)))

(defparameter *svg-document-element-event-attributes*
  (table<-list '(:oncopy :oncut :onpaste)))

#++
((lambda (string)
   ;; https://svgwg.org/svg2-draft/struct.html#SVGElement
   (multiple-value-bind (pre post)
       (ppcre:scan " — " string)
     `'(defparameter
           ,(intern
              (format nil "*SVG-~:@(~A~)*"
                      (nsubstitute #\- #\Space (subseq string 0 pre))))
         (table<-list
           ',(loop :for word
                        :in (ppcre:all-matches-as-strings "\\w+" string
                                                          :start post)
                   :with *package* := (find-package :keyword)
                   :collect (read-from-string word))))))
 "document element event attributes — ‘oncopy’, ‘oncut’, ‘onpaste’")

(define-empty-element path
  (:attributes
     (list *svg-aria-attributes* *svg-conditional-processing-attributes*
           *svg-core-attributes* *svg-global-event-attributes*
           *svg-document-element-event-attributes*
           (table<-list '(:pathlength :d)))))

(define-element svg
  (:attributes
     (list *svg-aria-attributes* *svg-conditional-processing-attributes*
           *svg-core-attributes* *svg-document-event-attributes*
           *svg-global-event-attributes*
           *svg-document-element-event-attributes*
           (table<-list
             '(:viewbox :preserveaspectratio :transform :x :y :width
               :height)))))
