(common-lisp:in-package :common-lisp-user)

(defvar *sm-lib-src-directory* (directory-namestring *load-truename*))

(macrolet
    ((sm-lib-defsystem ((module &key depends-on) &rest components)
       `(progn
	 #+mk-defsystem
	 (mk:defsystem ,module
	     :source-pathname *sm-lib-src-directory*
	     :source-extension "lisp"
	     ,@(and depends-on `(:depends-on ,depends-on))
	     :components (:serial ,@components))
	 #+asdf
	 (asdf:defsystem ,module
	     ,@(and depends-on `(:depends-on ,depends-on))
	     :serial t
	     :components 
	     (,@(loop for c in components
		      for p = (merge-pathnames
			       (parse-namestring c)
			       (make-pathname 
				:type "lisp"
				:defaults *sm-lib-src-directory*))
		      collect `(:file ,(pathname-name p) :pathname ,p)))))))
  (sm-lib-defsystem (:sm-lib :depends-on (:ice-lib))
    "package.lisp" "sm.lisp"))
