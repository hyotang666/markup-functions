; vim: ft=lisp et
(in-package :asdf)
(defsystem "markup-functions"
  :version
  "4.0.4"
  :depends-on
  (
   "uiop" ; Utilities.
   "fuzzy-match"        ; String fuzzy matcih especially for better error messages.
   )
  :pathname
  "src/"
  :components
  ((:file "markup-functions")
   (:file "svg" :depends-on ("markup-functions")))
  :author "SATO Shinichi"
  :description "Pretty HTML generator which have compile time syntax checking."
  :source-control (:git "git@github.com:hyotang666/markup-functions")
  :bug-tracker "https://github.com/hyotang666/markup-functions/issues"
  :license "MIT")

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "markup-functions").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "markup-functions"))))
  (append (call-next-method) '((test-op "markup-functions.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "markup-functions")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "markup-functions"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (symbol-call :jingoh.documentizer :import c)))))
