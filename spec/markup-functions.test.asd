; vim: ft=lisp et
(in-package :asdf)
(defsystem "markup-functions.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "markup-functions")
  :components
  ((:file "markup-functions"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :markup-functions args)))