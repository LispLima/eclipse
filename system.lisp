;;; -*- Mode: Lisp; Package: User -*-
;;; $Id: system.lisp,v 1.13 2004/03/16 18:20:35 ihatchondo Exp $
;;;
;;; This file is part of Eclipse.
;;; Copyright (C) 2000, 2001, 2002 Iban HATCHONDO
;;; Copyright (C) 2000 Julien BONINFANTE, Aymeric LACORTE, Jocelyn FRECHOT
;;; contact : hatchond@yahoo.fr
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(common-lisp:in-package :common-lisp-user)

(defparameter *eclipse-src-directory* (directory-namestring *load-truename*))

#+:cmu
(progn
  #-:clx (require :clx)
  #-(or :mk-defsystem :asdf) (load "library:subsystems/defsystem"))

#+:sbcl
(progn
   #-:asdf (require :asdf)
   #-:clx (require :clx))
#+:excl
(progn
  #-:clx (require :clx)
  #-:loop (require :loop))

;; For session management & connection.

(load "lib/ice/system.lisp")
(load "lib/sm/system.lisp")

;;;; ECLIPSE SYSTEM.

(defpackage :eclipse-system 
  (:use :common-lisp #+:asdf :asdf #+(and (not :asdf) :mk-defsystem) :mk)
  (:export #:compile-theme #:compile-themes #:compile-eclipse-system
	   #:load-eclipse-system #:clean-eclipse-system))

(in-package :eclipse-system)  

#+:asdf (defclass eclipse-source-file (cl-source-file) ())

(defmacro eclipse-defsystem ((module &key depends-on) &rest components)
  `(progn
    #+mk-defsystem
    (mk:defsystem ,module
	:source-pathname cl-user::*eclipse-src-directory*
	:source-extension "lisp"
	,@(and depends-on `(:depends-on ,depends-on))
	:components (:serial ,@components))
    #+asdf
    (asdf:defsystem ,module
	,@(and depends-on `(:depends-on ,depends-on))
	:serial t
	:default-component-class eclipse-source-file
	:components 
	(,@(loop for c in components
		 for p = (merge-pathnames
			     (parse-namestring c)
			     (make-pathname 
			         :type "lisp"
				 :defaults cl-user::*eclipse-src-directory*))
		 collect `(:file ,(pathname-name p) :pathname ,p))))))

(defun load-eclipse-system ()
  #+:asdf (operate 'load-op :eclipse)
  #+(and (not :asdf) :mk-defsystem) (mk:operate-on-system :eclipse :load))

(defun compile-eclipse-system ()
  #+:asdf (operate 'compile-op :eclipse)
  #+(and (not :asdf) :mk-defsystem) (mk:operate-on-system :eclipse :compile))

(defun clean-eclipse-system ()
  #+:mk-defsystem (mk:operate-on-system :eclipse :clean))

(defun compile-themes (&rest directory-theme-names)
  "Compiles a list of themes. Each item in the list is a theme directory path."
  (load-eclipse-system)
  (loop for directory-name in directory-theme-names
        for i-filespec = (merge-pathnames "theme.lisp" directory-name)
	for directory-truename = (directory-namestring (truename i-filespec))
	for o-filespec = (merge-pathnames "theme.o" directory-truename)
	do (load i-filespec)
	   (compile-file i-filespec :output-file o-filespec)))

(defun compile-theme (directory-name)
  "Compiles a theme located in the indicated directory."
  (let* ((i-filespec (merge-pathnames "theme.lisp" directory-name))
	 (directory-truename (directory-namestring (truename i-filespec)))
	 (o-filespec (merge-pathnames "theme.o" directory-truename)))
    (load-eclipse-system)
    (load i-filespec)
    (compile-file i-filespec :output-file o-filespec)))

(eclipse-defsystem (:clx-ext)
  "lib/clx-ext/clx-patch.lisp"
  "lib/clx-ext/xvidmode.lisp"
  "lib/clx-ext/package.lisp"
  "lib/clx-ext/clx-extensions"
  "lib/clx-ext/cursordef"
  "lib/clx-ext/cursor"
  "lib/clx-ext/keysyms"
  "lib/clx-ext/keysymdef"
  "lib/clx-ext/event"
  )

(eclipse-defsystem (:eclipse-lib)
  "lib/image-reader"
  "lib/manager-commons"
  "lib/netwm-manager"
  "lib/gnome-manager"
  )

(eclipse-defsystem (:eclipse :depends-on (:clx-ext :eclipse-lib :sm-lib))
  "config.lisp"
  "programmed-tasks"
  "package"
  "global"
  "misc"
  "themer"
  "menu"
  "gestures"
  "widgets"
  "virtual-screen"
  "rectangles"
  "wm"
  "input"
  "move-resize"
  "eclipse"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; SBCL hack copied from sbcl clx.asd

#+:sbcl
(defmethod perform :around (o (f eclipse-source-file))
  ;; SBCL signals an error if DEFCONSTANT is asked to redefine a
  ;; constant unEQLly. For Eclipse's purposes, however, we are defining
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
