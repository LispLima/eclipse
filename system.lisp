;;; -*- Mode: Lisp; Package: User -*-
;;; $Id$
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

(defparameter *eclipse-directory* (directory-namestring *load-truename*))

(mk:defsystem :eclipse #-mk-defsystem ()
  #+mk-defsystem :source-pathname #+mk-defsystem *eclipse-directory*
  #+mk-defsystem :source-extension #+mk-defsystem "lisp"
  #+mk-defsystem :components
  (:serial
   "gnome-manager"      
   "event"
   "image-reader"
   "programmed-tasks"
   "keysyms"
   "keysymdef"
   "cursor"
   "cursordef"
   "virtual-screen"
   "global"
   "menu"
   "wm"
   ))


