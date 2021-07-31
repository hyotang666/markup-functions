# MARKUP-FUNCTIONS 0.0.0
## What is this?
HTML5 generator.

### Current lisp world
There is [cl-who](https://edicl.github.io/cl-who/)
or [cl-markup](https://github.com/arielnetworks/cl-markup).

### Issues and Proposals

#### Heavily used macros.
I am stupid.
For me, it is too hard to learn how to use above libraries since it is macros.
You need to know about its [syntax and semantics](https://edicl.github.io/cl-who/#syntax).

Markup-functions provides (as name shows) functions rather than macros.
You never need to know about its syntax and semantics since you already knows about function's one.

#### Too much flexible.
I am stupid.
I always typo markup tags, but above libraries silently generate invalid html.
I will find problem when I browse it.

```lisp
* (cl-who:with-html-output(*standard-output*)
    (:titl "<--- Last 'e' missing."))

<titl><--- Last 'e' missing.</titl>
```

With markup-functions, when you typo markup tags imediately an error is signaled because it is implemented as function.

```lisp
* (htmf:titl () "Error is signaled in compile time.")

debugger invoked on a SB-INT:SIMPLE-READER-PACKAGE-ERROR in thread
#<THREAD "main thread" RUNNING {....}>:
  Symbol "TITL" not found in the HTMF package.
  ...
```

Of course I typo attributes too.
With markup-functions, when you typo attributes keys an error is signaled.

```lisp
* (htmf:meta :char-set :utf-8)

debugger invoked on a SIMPLE-ERROR in thread
#<THREAD: "main thread" RUNNING {....}>:
  Unknown attributes for tag META: :CHAR-SET
  ...
```

I am stupid.
I always missing correct attributes spells.
You can ask supported attributes to lisp.

```lisp
* (htmf:list-all-attributes 'meta)

(:CHARSET :CONTENT :HTTP-EQUIV :DEFAULT-STYLE :REFRESH :NAME :ACCESSKEY :CLASS
 :CONTENTEDITABLE :DIR :DRAGGABLE :DROPZONE :HIDDEN :ID :LANG :SPELLCHECK
 :STYLE :TABINDEX :TITLE :TRANSLATE)
```

#### Not pretty
I am stupid.
I always making html which is not what I expeced.
As last resort, sometimes I need to check raw html.
For such eye grepping, I need pretty printed html.

Cl-markup does not provide indentation.

```lisp
(cl-markup:markup
 (:table :border 0 :cellpadding 4
         (loop for i below 25 by 5
               collect
               (cl-markup:markup
                 (:tr :align "right"
                      (loop for j from i below (+ i 5)
                            collect
                            (cl-markup:markup
                              (:td :bgcolor
                                   (if (oddp j)
                                       "pink"
                                       "green")
                                   (format nil "~@R" (1+ j))))))))))

"<table border=\"0\" cellpadding=\"4\"><tr align=\"right\"><td bgcolor=\"green\">I</td><td bgcolor=\"pink\">II</td><td bgcolor=\"green\">III</td><td bgcolor=\"pink\">IV</td><td bgcolor=\"green\">V</td></tr><tr align=\"right\"><td bgcolor=\"pink\">VI</td><td bgcolor=\"green\">VII</td><td bgcolor=\"pink\">VIII</td><td bgcolor=\"green\">IX</td><td bgcolor=\"pink\">X</td></tr><tr align=\"right\"><td bgcolor=\"green\">XI</td><td bgcolor=\"pink\">XII</td><td bgcolor=\"green\">XIII</td><td bgcolor=\"pink\">XIV</td><td bgcolor=\"green\">XV</td></tr><tr align=\"right\"><td bgcolor=\"pink\">XVI</td><td bgcolor=\"green\">XVII</td><td bgcolor=\"pink\">XVIII</td><td bgcolor=\"green\">XIX</td><td bgcolor=\"pink\">XX</td></tr><tr align=\"right\"><td bgcolor=\"green\">XXI</td><td bgcolor=\"pink\">XXII</td><td bgcolor=\"green\">XXIII</td><td bgcolor=\"pink\">XXIV</td><td bgcolor=\"green\">XXV</td></tr></table>" 
```

Cl-who provides indentation but not pretty.

```lisp
(cl-who:with-html-output-to-string (*standard-output* nil :indent 2)
  (:table :border 0 :cellpadding 4
   (loop for i below 25 by 5
         do (cl-who:htm
             (:tr :align "right"
              (loop for j from i below (+ i 5)
                    do (cl-who:htm
                        (:td :bgcolor (if (oddp j)
                                        "pink"
                                        "green")
                             (cl-who:fmt "~@R" (1+ j))))))))))

"
  <table border='0' cellpadding='4'>
  <tr align='right'>
  <td bgcolor='green'>I
  </td>
  <td bgcolor='pink'>II
  </td>
  <td bgcolor='green'>III
  </td>
  <td bgcolor='pink'>IV
  </td>
  <td bgcolor='green'>V
  </td>
  </tr>
  <tr align='right'>
  <td bgcolor='pink'>VI
  </td>
  <td bgcolor='green'>VII
  </td>
  <td bgcolor='pink'>VIII
  </td>
  <td bgcolor='green'>IX
  </td>
  <td bgcolor='pink'>X
  </td>
  </tr>
  <tr align='right'>
  <td bgcolor='green'>XI
  </td>
  <td bgcolor='pink'>XII
  </td>
  <td bgcolor='green'>XIII
  </td>
  <td bgcolor='pink'>XIV
  </td>
  <td bgcolor='green'>XV
  </td>
  </tr>
  <tr align='right'>
  <td bgcolor='pink'>XVI
  </td>
  <td bgcolor='green'>XVII
  </td>
  <td bgcolor='pink'>XVIII
  </td>
  <td bgcolor='green'>XIX
  </td>
  <td bgcolor='pink'>XX
  </td>
  </tr>
  <tr align='right'>
  <td bgcolor='green'>XXI
  </td>
  <td bgcolor='pink'>XXII
  </td>
  <td bgcolor='green'>XXIII
  </td>
  <td bgcolor='pink'>XXIV
  </td>
  <td bgcolor='green'>XXV
  </td>
  </tr>
  </table>" 
```

Markup-functions generate pretty printing html.

```lisp
(let ((htmf:*strict* nil)) ; Attributes :BGCOLOR is not supported in html5.
  (htmf:html5 nil
              (htmf:table '(:border 0 :cellpadding 4)
                (loop :for i :below 25 :by 5
                      :collect (htmf:tr '(:align "right")
                                 (loop :for j :from i :below (+ i 5)
                                       :collect (htmf:td (list :bgcolor (if (oddp j)
                                                                            "pink"
                                                                            "green"))
                                                   (format nil "~@R" (1+ j)))))))))

"<!DOCTYPE HTML>
<HTML>
  <TABLE BORDER='0' CELLPADDING='4'>
    <TR ALIGN='right'>
      <TD BGCOLOR='green'>I</TD>
      <TD BGCOLOR='pink'>II</TD>
      <TD BGCOLOR='green'>III</TD>
      <TD BGCOLOR='pink'>IV</TD>
      <TD BGCOLOR='green'>V</TD>
    </TR>
    <TR ALIGN='right'>
      <TD BGCOLOR='pink'>VI</TD>
      <TD BGCOLOR='green'>VII</TD>
      <TD BGCOLOR='pink'>VIII</TD>
      <TD BGCOLOR='green'>IX</TD>
      <TD BGCOLOR='pink'>X</TD>
    </TR>
    <TR ALIGN='right'>
      <TD BGCOLOR='green'>XI</TD>
      <TD BGCOLOR='pink'>XII</TD>
      <TD BGCOLOR='green'>XIII</TD>
      <TD BGCOLOR='pink'>XIV</TD>
      <TD BGCOLOR='green'>XV</TD>
    </TR>
    <TR ALIGN='right'>
      <TD BGCOLOR='pink'>XVI</TD>
      <TD BGCOLOR='green'>XVII</TD>
      <TD BGCOLOR='pink'>XVIII</TD>
      <TD BGCOLOR='green'>XIX</TD>
      <TD BGCOLOR='pink'>XX</TD>
    </TR>
    <TR ALIGN='right'>
      <TD BGCOLOR='green'>XXI</TD>
      <TD BGCOLOR='pink'>XXII</TD>
      <TD BGCOLOR='green'>XXIII</TD>
      <TD BGCOLOR='pink'>XXIV</TD>
      <TD BGCOLOR='green'>XXV</TD>
    </TR>
  </TABLE>
</HTML>" 
```

## Usage

Every element functions returns function as `(FUNCTION () NULL)`.

```lisp
* (htmf:br ())
#<CLOSURE (LAMBDA () :IN BR) {...}>

* (funcall *)
<BR> ; <--- side effect.
NIL
```

Every empty element functions accepts key value pair as attributes.

```lisp
* (funcall (htmf:meta '(:charset :utf-8)))
<META CHARSET='UTF-8'>
NIL
```

When attribute value is `T`, value becomes key itself.

```lisp
* (funcall (htmf:meta '(:charset t)))
<META CHARSET='CHARSET'>
NIL
```

Every standard element functions accepts key value pair (i.e. plist) as its first argument.

```lisp
* (funcall (htmf:a '(:href "/url") "label"))
<A HREF='/url'>label</A>
NIL
```

Top level function `HTML5` has same API of standard element functions,
but returns string which has doctype declaration.

```lisp
* (htmf:html5 () (htmf:title () "hoge"))
"<!DOCTYPE HTML>
<HTML><TITLE>hoge</TITLE></HTML>"
```

### Extend.

You can write `PPRINT-PUT` method.
For example, to support [`PLUMP:NODE`](https://github.com/Shinmera/plump), you can write method like below.

```lisp
(defmethod htmf:pprint-put (stream (node plump:node) &rest noise)
  (declare (ignore noise))
  (plump:serialize node stream))
```

### Relaxing errors.

Relaxing to `WARN`, bind `HTMF:*STRICT*` with `WARN`.

```lisp
* (let ((htmf:*strict* 'warn))
    (htmf:meta '(:char-set :utf-8)))

WARNING: Unknown attributes for tag META: :CHAR-SET
#<CLOSURE (LAMBDA () :IN META) {...}>
```

Disable any checking, bind `HTMF:*STRICT*` with `NIL`.

```lisp
* (let ((htmf:*strict* nil))
    (htmf:meta '(:char-set :utf-8)))

#<CLOSURE (LAMBDA () :IN META) {...}>
```

To accept not supported attributes (e.g. aria-label) temporarily, bind `HTMF:*OPTIONAL-ATTRIBUTES*` with list.

```lisp
* (let ((htmf:*optional-attributes* '(:char-set)))
    (htmf:meta '(:char-set :utf-8)))

#<CLOSURE (LAMBDA () :IN META) {...}>
```

### Control pretty printings.

Bind `CL:*PRINT-PRETTY*`.

```lisp
(let ((*print-pretty* nil) (htmf:*strict* nil))
  (htmf:html5 nil
              (htmf:table '(:border 0 :cellpadding 4)
                (loop :for i :below 25 :by 5
                      :collect (htmf:tr '(:align "right")
                                 (loop :for j :from i :below (+ i 5)
                                       :collect (htmf:td (list :bgcolor (if (oddp j)
                                                                            "pink"
                                                                            "green"))
                                                   (format nil "~@R" (1+ j)))))))))

"<!DOCTYPE HTML><HTML><TABLE BORDER='0' CELLPADDING='4'><TR ALIGN='right'><TD BGCOLOR='green'>I</TD> <TD BGCOLOR='pink'>II</TD> <TD BGCOLOR='green'>III</TD> <TD BGCOLOR='pink'>IV</TD> <TD BGCOLOR='green'>V</TD></TR> <TR ALIGN='right'><TD BGCOLOR='pink'>VI</TD> <TD BGCOLOR='green'>VII</TD> <TD BGCOLOR='pink'>VIII</TD> <TD BGCOLOR='green'>IX</TD> <TD BGCOLOR='pink'>X</TD></TR> <TR ALIGN='right'><TD BGCOLOR='green'>XI</TD> <TD BGCOLOR='pink'>XII</TD> <TD BGCOLOR='green'>XIII</TD> <TD BGCOLOR='pink'>XIV</TD> <TD BGCOLOR='green'>XV</TD></TR> <TR ALIGN='right'><TD BGCOLOR='pink'>XVI</TD> <TD BGCOLOR='green'>XVII</TD> <TD BGCOLOR='pink'>XVIII</TD> <TD BGCOLOR='green'>XIX</TD> <TD BGCOLOR='pink'>XX</TD></TR> <TR ALIGN='right'><TD BGCOLOR='green'>XXI</TD> <TD BGCOLOR='pink'>XXII</TD> <TD BGCOLOR='green'>XXIII</TD> <TD BGCOLOR='pink'>XXIV</TD> <TD BGCOLOR='green'>XXV</TD></TR></TABLE></HTML>"
```

### Control printing case.

Bind `CL:*PRINT-CASE*`.

```lisp
(let ((*print-case* :downcase) htmf:*strict*)
  (htmf:html5 nil
              (htmf:table '(:border 0 :cellpadding 4)
                (loop :for i :below 25 :by 5
                      :collect (htmf:tr '(:align "right")
                                 (loop :for j :from i :below (+ i 5)
                                       :collect (htmf:td (list :bgcolor (if (oddp j)
                                                                            "pink"
                                                                            "green"))
                                                   (format nil "~@R" (1+ j)))))))))

"<!doctype html>
<html>
  <table border='0' cellpadding='4'>
    <tr align='right'>
      <td bgcolor='green'>I</td>
      <td bgcolor='pink'>II</td>
      <td bgcolor='green'>III</td>
      <td bgcolor='pink'>IV</td>
      <td bgcolor='green'>V</td>
    </tr>
    <tr align='right'>
      <td bgcolor='pink'>VI</td>
      <td bgcolor='green'>VII</td>
      <td bgcolor='pink'>VIII</td>
      <td bgcolor='green'>IX</td>
      <td bgcolor='pink'>X</td>
    </tr>
    <tr align='right'>
      <td bgcolor='green'>XI</td>
      <td bgcolor='pink'>XII</td>
      <td bgcolor='green'>XIII</td>
      <td bgcolor='pink'>XIV</td>
      <td bgcolor='green'>XV</td>
    </tr>
    <tr align='right'>
      <td bgcolor='pink'>XVI</td>
      <td bgcolor='green'>XVII</td>
      <td bgcolor='pink'>XVIII</td>
      <td bgcolor='green'>XIX</td>
      <td bgcolor='pink'>XX</td>
    </tr>
    <tr align='right'>
      <td bgcolor='green'>XXI</td>
      <td bgcolor='pink'>XXII</td>
      <td bgcolor='green'>XXIII</td>
      <td bgcolor='pink'>XXIV</td>
      <td bgcolor='green'>XXV</td>
    </tr>
  </table>
</html>" 
```

### NOTE
Markup-functions developed as YAGNI style.
So many tags are not implemented yet.

## From developer

### Product's goal

### License

### Developed with
SBCL

### Tested with

## Installation

