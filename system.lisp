;;; -*- Mode: Lisp; Package: User -*-
;;; $Id: system.lisp,v 1.5 2003/04/07 13:35:32 hatchond Exp $
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

#+cmu
(progn
  #-CLX (require :cmucl-clx)		;works under Debian Linux
  #-MK-DEFSYSTEM (load "library:subsystems/defsystem"))

#+:excl(require :clx)
#+:excl(require :loop)
#+mk-defsystem (use-package "MK")

(defmacro eclipse-defsystem ((module &key depends-on) &rest components)
  `(defsystem ,module #-mk-defsystem ()
       #+mk-defsystem :source-pathname *eclipse-src-directory*
       #+mk-defsystem :source-extension "lisp"
       #+mk-defsystem ,@(and depends-on `(:depends-on ,depends-on))
        :components
	(:serial
	 #-mk-defsystem ,@depends-on
	 ,@components)))

(eclipse-defsystem (:clx-ext)
  "lib/clx-ext/clx-patch.lisp"
  "lib/clx-ext/xvidmode.lisp"
  "lib/clx-ext/package.lisp"
  "lib/clx-ext/clx-extensions"
  "lib/clx-ext/cursor"
  "lib/clx-ext/cursordef"
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

(eclipse-defsystem (:eclipse :depends-on (:clx-ext :eclipse-lib))
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
   "wm"
   "input"
   "move-resize"
   "eclipse"
   )

(defun compile-theme (directory-name)
  (let ((i-filespec (merge-pathnames "theme.lisp" directory-name))
	(o-filespec (merge-pathnames "theme.o")))
    (operate-on-system :eclipse :load)
    (load i-filespec)
    (compile-file i-filespec :output-file o-filespec)))

