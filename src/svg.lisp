(in-package :cl-user)

(defpackage :markup-functions.svg
  (:use :cl :markup-functions)
  (:documentation "To develop HTMF.SVG package."))

(in-package :markup-functions.svg)

(let* ((main-functions 'nil)
       (standard-elements '(:dummy svg text))
       (empty-elements '(path rect))
       (all (append main-functions (cdr standard-elements) empty-elements)))
  (unless (find-package :htmf.svg)
    (make-package :htmf.svg :use nil))
  (import all :htmf.svg)
  (export all :htmf.svg)
  (set-pprint-dispatch `(cons (member ,@(cdr standard-elements)))
                       'pprint-element))

(defparameter *standard-color-names*
  '(:aliceblue :antiquewhite :aqua :aquamarine :azure :beige :bisque :black
    :blanchedalmond :blue :blueviolet :brown :burlywood :cadetblue :chartreuse
    :chocolate :coral :cornflowerblue :cornsilk :crimson :cyan :darkblue
    :darkcyan :darkgoldenrod :darkgray :darkgreen :darkgrey :darkkhaki
    :darkmagenta :darkolivegreen :darkorange :darkorchid :darkred :darksalmon
    :darkseagreen :darkslateblue :darkslategray :darkslategrey :darkturquoise
    :darkviolet :deeppink :deepskyblue :dimgray :dodgerblue :firebrick
    :floralwhite :forestgreen :fuchsia :gainsboro :ghostwhite :gold :goldenrod
    :gray :green :greenyellow :grey :honeydew :hotpink :indianred :indigo
    :ivory :khaki :lavender :lavenderblush :lawngreen :lemonchiffon :lightblue
    :lightcoral :lightcyan :lightgoldenrodyellow :lightgray :lightgreen
    :lightgrey :lightpink :lightsalmon :lightseagreen :lightskyblue
    :lightslategray :lightslategrey :lightsteelblue :lightyellow :lime
    :limegreen :linen :magenta :maroon :mediumaquamarine :mediumblue
    :mediumorchid :mediumpurple :mediumseagreen :mediumslateblue
    :mediumspringgreen :mediumturquoise :mediumvioletred :midnightblue
    :mintcream :mistyrose :moccasin :navajowhite :navy :oldlace :olive
    :olivedrab :orange :orangered :orchid :palegoldenrod :palegreen
    :paleturquoise :palevioletred :papayawhip :peachpuff :peru :pink :plum
    :powderblue :purple :rebeccapurple :red :rosybrown :royalblue :saddlebrown
    :salmon :sandybrown :seagreen :seashell :sienna :silver :skyblue :slateblue
    :slategray :slategrey :snow :springgreen :steelblue :tan :teal :thistle
    :tomato :turquoise :violet :wheat :white :whitesmoke :yellow :yellowgreen))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; DEFINE-ELEMENT and DEFINE-EMPTY-ELEMENT needs this eval-when.
  (defparameter *svg-aria-attributes*
    (table<-list
      '(:aria-activedescendant :aria-atomic :aria-autocomplete :aria-busy
        :aria-checked :aria-colcount :aria-colindex :aria-colspan
        :aria-controls :aria-current :aria-describedby :aria-details
        :aria-disabled :aria-dropeffect :aria-errormessage :aria-expanded
        :aria-flowto :aria-grabbed :aria-haspopup :aria-hidden :aria-invalid
        :aria-keyshortcuts :aria-label :aria-labelledby :aria-level :aria-live
        :aria-modal :aria-multiline :aria-multiselectable :aria-orientation
        :aria-owns :aria-placeholder :aria-posinset :aria-pressed
        :aria-readonly :aria-relevant :aria-required :aria-roledescription
        :aria-rowcount :aria-rowindex :aria-rowspan :aria-selected
        :aria-setsize :aria-sort :aria-valuemax :aria-valuemin :aria-valuenow
        :aria-valuetext :role)))
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
        :onkeypress :onkeyup :onload :onloadeddata :onloadedmetadata
        :onloadstart :onmousedown :onmouseenter :onmouseleave :onmousemove
        :onmouseout :onmouseover :onmouseup :onpause :onplay :onplaying
        :onprogress :onratechange :onreset :onresize :onscroll :onseeked
        :onseeking :onselect :onshow :onstalled :onsubmit :onsuspend
        :ontimeupdate :ontoggle :onvolumechange :onwaiting :onwheel)))
  (defparameter *svg-document-element-event-attributes*
    (table<-list '(:oncopy :oncut :onpaste)))
  (defparameter *svg-presentation-attributes*
    (table<-list
      '(:alignment-baseline :baseline-shift :clip :clip-path :clip-rule :color
        :color-interpolation :color-interpolation-filters :color-profile
        :color-rendering :cursor :direction :display :dominant-baseline
        :enable-background :fill :fill-opacity :fill-rule :filter :flood-color
        :flood-opacity :font-family :font-size :font-size-adjust :font-stretch
        :font-style :font-variant :font-weight :glyph-orientation-horizontal
        :glyph-orientation-vertical :image-rendering :kerning :letter-spacing
        :lighting-color :marker-end :marker-mid :marker-start :mask :opacity
        :overflow :pointer-events :shape-rendering :solid-color :solid-opacity
        :stop-color :stop-opacity :stroke :stroke-dasharray :stroke-dashoffset
        :stroke-linecap :stroke-linejoin :stroke-miterlimit :stroke-opacity
        :stroke-width :text-anchor :text-decoration :text-rendering :transform
        :unicode-bidi :vector-effect :visibility :word-spacing :writing-mode))
    ;; https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/Presentation
    "SVG presentation attributes are CSS properties that can be used as attributes on SVG elements.")
  (defparameter *svg-styling-attributes* (table<-list '(:class :style))))

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
           *svg-presentation-attributes* (table<-list '(:pathlength :d)))))

(define-element svg
  (:attributes
     (list *svg-aria-attributes* *svg-conditional-processing-attributes*
           *svg-core-attributes* *svg-document-event-attributes*
           *svg-global-event-attributes*
           *svg-document-element-event-attributes*
           (table<-list
             '(:viewbox :preserveaspectratio :transform :x :y :width
               :height)))))

(define-empty-element rect
  (:attributes
     (list *svg-aria-attributes* *svg-conditional-processing-attributes*
           *svg-core-attributes* *svg-global-event-attributes*
           *svg-document-element-event-attributes*
           *svg-presentation-attributes*
           (table<-list '(:pathlength :x :y :width :height :rx :ry))))
  (:documentation
     "The ‘rect’ element defines a rectangle which is axis-aligned with the current user coordinate system.
Rounded rectangles can be achieved by setting non-zero values for the rx and ry geometric properties."))

(define-element text
  (:attributes
     (list *svg-aria-attributes* *svg-conditional-processing-attributes*
           *svg-core-attributes* *svg-global-event-attributes*
           *svg-document-element-event-attributes*
           *svg-presentation-attributes*
           (table<-list '(:lengthadjust :x :y :dx :dy :rotate :textlength))))
  (:documentation
     "The ‘text’ element defines a graphics element consisting of text."))