;;; -*- Mode: Lisp; Package: Keyboard -*-
;;; $Id: keysyms.lisp,v 1.3 2003/09/16 21:32:53 hatchond Exp $
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
   modifiers
   modifier-map-changed-p))

(in-package :keyboard)

(defvar *keyname->keysyms* (make-hash-table :test #'eq))
(defvar *keysym->keyname* (make-hash-table :test #'eql))
(defvar *modifier->modifier-mask* nil)

(defun make-modifier-mask-table (disp)
  (loop with map = '(:any #x8000)
	for mods in (multiple-value-list (xlib:modifier-mapping disp))
	for i = 1 then (* 2 i) do
	(loop for mod in mods
	      for key-name = (keycode->keyname disp mod)
	      for prec = (getf map key-name) do
	      (setf (getf map key-name) (+ (if (numberp prec) prec 0) i)))
	finally (return map)))

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

(defun modifier->modifier-mask (disp modifier-name)
  (unless *modifier->modifier-mask*
    (setf *modifier->modifier-mask* (make-modifier-mask-table disp)))
  (getf *modifier->modifier-mask* modifier-name))

(defun modifiers ()
  (loop for (key nil) on *modifier->modifier-mask* by #'cddr collect key))

(defsetf modifier->modifier-mask (disp mod) (value)
  `(setf (getf *modifier->modifier-mask* (keycode->keyname ,disp ,mod)) ,value))

(defun modifier-map-changed-p (display)
  (unless (eql (make-modifier-mask-table display) *modifier->modifier-mask*)
    (setf *modifier->modifier-mask* nil)
    t))
