(common-lisp:in-package :common-lisp-user)

#+mk-defsystem (use-package "MK")

(defsystem :sm-lib #-mk-defsystem ()
  #+mk-defsystem :source-pathname (directory-namestring *load-truename*)
  #+mk-defsystem :source-extension "lisp"
  #+mk-defsystem :depends-on (:ice-lib)
  :components
  (:serial
   #-mk-defsystem :ice-lib
   "package.lisp"
   "sm.lisp"))
