(common-lisp:in-package :common-lisp-user)

(defvar *ice-lib-src-directory* (directory-namestring *load-truename*))

(macrolet
    ((ice-lib-defsystem ((module &key depends-on) &rest components)
       `(progn
	 #+mk-defsystem
	 (mk:defsystem ,module
	     :source-pathname *ice-lib-src-directory*
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
				:defaults *ice-lib-src-directory*))
		      collect `(:file ,(pathname-name p) :pathname ,p)))))))
  (ice-lib-defsystem (:ice-lib)
    "package.lisp"
    "dependent.lisp"
    "ICE.lisp"
    "ICE-buffer.lisp"
    "ICE-request.lisp"
    "ICE-auth.lisp"
    "ICE-lib.lisp"))


