;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: $
;;;
;;; ECLIPSE. The Common Lisp Window Manager.
;;; Copyright (C) 2002 Iban HATCHONDO
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA

;;;; This file contain the Key gestion and the cursor gestion via the keyboard.

(in-package :ECLIPSE-INTERNALS)

(defvar *keystrock-table* (make-hash-table :test #'equal))

(defmacro action ((&rest f1) (&rest f2))
  (when (or (eq (car f1) :release) (eq (car f2) :press)) (rotatef f1 f2))
  `(lambda (event)
     (typecase event
       (key-press ,@(cdr f1))
       (key-release ,@(cdr f2)))))

(defun action-key->lambda (action-keyword)
  (with-slots (vscreens) *root*
    (case action-keyword
      (:switch-win-up
       (action () (:press (circulate-window vscreens :direction :above))))
      (:switch-win-down
       (action () (:press (circulate-window vscreens :direction :below))))
      (:switch-screen-left (action (:press (change-vscreen vscreens #'-)) ()))
      (:switch-screen-right (action (:press (change-vscreen vscreens #'+)) ()))
      (:move-right (action (:press (move-cursor-right)) ()))
      (:move-left (action (:press (move-cursor-left)) ()))
      (:move-up (action (:press (move-cursor-up)) ()))
      (:move-down (action (:press (move-cursor-down)) ()))
      (:left-click #'(lambda (event) (perform-click 1 event)))
      (:middle-click #'(lambda (event) (perform-click 2 event)))
      (:right-click #'(lambda (event) (perform-click 3 event)))
      (:scroll-up #'(lambda (event) (perform-click 4 event)))
      (:scroll-down #'(lambda (event) (perform-click 5 event)))
      )))

(defmacro realize-keystroke (key mask action-keyword &optional lambda)
  `(progn
     (setf (gethash (cons ,key ,mask) *keystrock-table*)
	   (or ,lambda (action-key->lambda ,action-keyword)))
     (xlib:grab-key *root-window* ,key :modifiers ,mask :owner-p nil)))

;;;  - composition of modifiers as '(:and :ALT-LEFT :CONTROL-RIGHT)
;;;  - a simple modifier as :ALT-LEFT or 18 (a modifier mask)
;;;  - a list of possible modifiers as '(:ALT-LEFT :CONTOL-RIGHT)
(defun define-key-combo (name &key keys
			          (default-modifiers-p t)
				  (modifiers :any)
				  fun)
  (catch 'keystroke-definition
    (handler-bind
        ((error #'(lambda (condition)
		    (declare (ignorable condition))
		    (format t "Can't realize key-combo ~A~%" name)
		    (format t " modifiers : ~A~% key : ~A~%" modifiers keys)
		    (throw 'keystroke-definition nil))))
      (define-combo-internal name keys default-modifiers-p modifiers fun))))

(defun define-combo-internal (name keys default-modifiers-p modifiers fun)
  (setf keys (loop for k in keys append
		   (multiple-value-list (kb:keyname->keycodes *display* k))))
  (cond ((keywordp modifiers)
	 (setf modifiers (list (kb:modifier->modifier-mask modifiers))))
	((numberp modifiers) (setf modifiers (list modifiers)))
	((eq (car modifiers) :and)
	 (loop for mod in (cdr modifiers)
	       sum (kb:modifier->modifier-mask mod) into mod-mask
	       finally (setf modifiers (list mod-mask))))
	(t (map-into modifiers #'kb:modifier->modifier-mask modifiers)))
  (loop with num-l = (kb:modifier->modifier-mask :NUM-LOCK)
	with caps-l = (kb:modifier->modifier-mask :CAPS-LOCK)
	for mask in modifiers do
	(loop for key in keys do
	      (realize-keystroke key mask name fun)
	      (when (and default-modifiers-p (not (eql mask :any)))
		(and caps-l (realize-keystroke key (+ mask caps-l) name fun))
		(and num-l (realize-keystroke key (+ mask num-l) name fun))
		(when (and num-l caps-l)
		  (realize-keystroke key (+ mask num-l caps-l) name fun))))))

;;; Cursor movements, and click.

(defun move-cursor-right () (xlib:warp-pointer-relative *display* 10 0))
(defun move-cursor-left () (xlib:warp-pointer-relative *display* -10 0))
(defun move-cursor-up () (xlib:warp-pointer-relative *display* 0 -10))
(defun move-cursor-down () (xlib:warp-pointer-relative *display* 0 10))

(defun perform-click (buton-number ev)
  (flet ((my-query (win) (multiple-value-list (xlib:query-pointer win))))
    (loop with window = *root-window*
	  with type = (if (typep ev 'key-press) :button-press :button-release)
	  for (x y ssp child nil root-x root-y root) = (my-query window)
	  while child do (setf window child)
	  finally
	    (xlib:send-event window type nil
			     :x x :y y :root-x root-x :root-y root-y
			     :window window :event-window window
			     :root root :child child
			     :state nil :code buton-number
			     :same-screen-p ssp :time (event-time ev)))))
