(common-lisp:in-package :common-lisp-user)

(defvar *sm-lib-src-directory* (directory-namestring *load-truename*))

(defpackage :sm-lib-system (:use :common-lisp #+:asdf :asdf))
(in-package :sm-lib-system)

#+:asdf (defclass sm-lib-source-file (ice-lib-system::ice-lib-source-file) ())

(macrolet
    ((sm-lib-defsystem ((module &key depends-on) &rest components)
       `(progn
	 #+mk-defsystem
	 (mk:defsystem ,module
	     :source-pathname cl-user::*sm-lib-src-directory*
	     :source-extension "lisp"
	     ,@(and depends-on `(:depends-on ,depends-on))
	     :components (:serial ,@components))
	 #+asdf
	 (asdf:defsystem ,module
	     ,@(and depends-on `(:depends-on ,depends-on))
	     :serial t
	     :default-component-class sm-lib-source-file
	     :components 
	     (,@(loop for c in components
		      for p = (merge-pathnames
			       (parse-namestring c)
			       (make-pathname 
				:type "lisp"
				:defaults cl-user::*sm-lib-src-directory*))
		      collect `(:file ,(pathname-name p) :pathname ,p)))))))

  (sm-lib-defsystem (:sm-lib :depends-on (:ice-lib))
    "package.lisp"
    "sm.lisp"))
