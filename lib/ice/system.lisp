(common-lisp:in-package :common-lisp-user)

(defvar *ice-lib-src-directory* (directory-namestring *load-truename*))

(defpackage :ice-lib-system (:use :common-lisp #+:asdf :asdf))
(in-package :ice-lib-system)  

#+sbcl (progn (require :asdf) (require :sb-bsd-sockets))
#+:asdf (defclass ice-lib-source-file (cl-source-file) ())

(macrolet
    ((ice-lib-defsystem ((module &key depends-on) &rest components)
       `(progn
	 #+mk-defsystem
	 (mk:defsystem ,module
	     :source-pathname cl-user::*ice-lib-src-directory*
	     :source-extension "lisp"
	     ,@(and depends-on `(:depends-on ,depends-on))
	     :components (:serial ,@components))
	 #+asdf
	 (asdf:defsystem ,module
	     ,@(and depends-on `(:depends-on ,depends-on))
	     :serial t
	     :default-component-class ice-lib-source-file
	     :components 
	     (,@(loop for c in components
		      for p = (merge-pathnames
			       (parse-namestring c)
			       (make-pathname 
				:type "lisp"
				:defaults cl-user::*ice-lib-src-directory*))
		      collect `(:file ,(pathname-name p) :pathname ,p)))))))

  (ice-lib-defsystem (:ice-lib)
    "package.lisp"
    "dependent.lisp"
    "ICE.lisp"
    "ICE-buffer.lisp"
    "ICE-macros.lisp"
    "ICE-request.lisp"
    "ICE-auth.lisp"
    "ICE-lib.lisp"))

#+:sbcl
(defmethod perform :around (o (f ice-lib-source-file))
  ;; SBCL signals an error if DEFCONSTANT is asked to redefine a
  ;; constant unEQLly. For ICE lib's purposes, however, we are defining
  ;; structured constants (lists and arrays) not for EQLity, but for
  ;; the purposes of constant-folding operations such as (MEMBER FOO
  ;; +BAR+), so it is safe to abort the redefinition provided the
  ;; structured data is sufficiently equal.
  (handler-bind
      ((sb-ext:defconstant-uneql
	   (lambda (c)
	     ;; KLUDGE: this really means "don't warn me about
	     ;; efficiency of generic array access, please"
	     (declare (optimize (sb-ext:inhibit-warnings 3)))
	     (let ((old (sb-ext:defconstant-uneql-old-value c))
		   (new (sb-ext:defconstant-uneql-new-value c)))
	       (typecase old
		 (list (when (equal old new) (abort c)))
		 (string (when (and (typep new 'string)
				    (string= old new))
			   (abort c)))
		 (simple-vector
		  (when (and (typep new 'simple-vector)
			     (= (length old) (length new))
			     (every #'eql old new))
		    (abort c)))
		 (array
		  (when (and (typep new 'array)
			     (equal (array-dimensions old)
				    (array-dimensions new))
			     (equal (array-element-type old)
				    (array-element-type new))
			     (dotimes (i (array-total-size old) t)
			       (unless (eql (row-major-aref old i)
					    (row-major-aref new i))
				 (return nil))))
		    (abort c))))))))
    (call-next-method)))
