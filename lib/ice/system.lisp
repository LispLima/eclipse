(common-lisp:in-package :common-lisp-user)

#+mk-defsystem (use-package "MK")

(defsystem :ice-lib #-mk-defsystem ()
  #+mk-defsystem :source-pathname (directory-namestring *load-truename*)
  #+mk-defsystem :source-extension "lisp"
  :components
  (:serial
   "package.lisp"
   "dependent.lisp"
   "ICE.lisp"
   "ICE-buffer.lisp"
   "ICE-request.lisp"
   "ICE-auth.lisp"
   "ICE-lib.lisp"))


