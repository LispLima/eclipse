;;; -*- Mode: Lisp; Package: Keyboard -*-
;;; $Id: keysyms.lisp,v 1.2 2002/06/24 07:33:44 james Exp $
;;;
;;; This is a CLX extension for managing keyboard.
;;;
;;; Copyright (C) 2002 Iban HATCHONDO, Gilbert BAUMANN
;;; contact : hatchond@yahoo.fr
;;;           unk6@rz.uni-karlsruhe.de
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

(defpackage keyboard
  (:nicknames kb)
  (:use common-lisp)
  (:size 50)
  (:export
   define-keysym
   keysym->keyname
   keyname->keysym
   keyname->keycodes
   keycode->keyname
   modifier->modifier-mask
   init-keyboard))

(in-package :keyboard)

(defvar *keyname->keysyms* (make-hash-table :test #'eq))
(defvar *keysym->keyname* (make-hash-table :test #'eql))
(defvar *modifier->modifier-mask* '(:any #x8000))

(defun define-keysym (name value)
  (pushnew name (gethash value *keysym->keyname* nil))
  (setf (gethash name *keyname->keysyms*) value))

(defun keysym->keyname (keysym)
  (car (last (gethash keysym *keysym->keyname*))))

(defun keyname->keysym (keyname)
  (gethash keyname *keyname->keysyms*))

(defun keyname->keycodes (disp keyname)
  (xlib:keysym->keycodes disp (keyname->keysym keyname)))

(defun keycode->keyname (disp keycode)
  (keysym->keyname (xlib:keycode->keysym disp keycode 0)))

(defun modifier->modifier-mask (modifier-name)
  (getf *modifier->modifier-mask* modifier-name))

(defsetf modifier->modifier-mask (disp mod) (value)
  `(setf (getf *modifier->modifier-mask* (keycode->keyname ,disp ,mod)) ,value))

(defun init-keyboard (disp)
  (loop for mods in (multiple-value-list (xlib:modifier-mapping disp))
	for i = 1 then (* 2 i) do
	(loop for mod in mods do (setf (modifier->modifier-mask disp mod) i))))

