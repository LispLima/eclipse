;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: gestures.lisp,v 1.7 2003/09/16 21:56:12 hatchond Exp $
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

(defvar *keystroke-map* (make-hash-table :test #'equal))
(defvar *mouse-stroke-map* (make-hash-table :test #'equal))
(defvar *registered-keycodes* (make-array 256 :element-type 'bit))
(defvar *keystrokes* (make-hash-table :test #'eq))
(defvar *mousestrokes* (make-hash-table :test #'eq))

(defun lookup-keystroke (code state)
  "Find the associated callback if any for this pair code modifier state."
  (gethash (cons code state) *keystroke-map*))

(defun lookup-mouse-stroke (button state)
  "Find the associated callback if any for this pair button modifier state."
  (gethash (cons button state) *mouse-stroke-map*))

(defun keycode-registered-p (keycode &optional (count 1))
  "Returns t if this keycode is used for any keystroke."
  (loop for i from keycode below (+ keycode count)
	when (aref *registered-keycodes* keycode) do (return t)))

(defun unregister-all-keystrokes ()
  "Unregister all keystroke at the X server level."
  (xlib:ungrab-key *root-window* :any :modifiers #x8000)
  (setf *registered-keycodes* (make-array 256 :element-type 'bit))
  (clrhash *keystroke-map*))

(defun register-all-keystrokes ()
  "Register, at the X server level, all declared keystroke."
  (loop for keystroke being each hash-value in *keystrokes*
	do (define-combo-internal keystroke *root-window*)))

(defun unregister-all-mouse-strokes ()
  "Unregister all keystroke at the X server level."
  (xlib:ungrab-button *root-window* :any :modifiers #x8000)
  (clrhash *mouse-stroke-map*))

(defun register-all-mouse-strokes ()
  "Register, at the X server level, all declared mouse strokes."
  (loop for mouse-stroke being each hash-value in *mousestrokes*
	do (define-combo-internal mouse-stroke *root-window* :mouse-p t)))
    
;;;; stroke

(defclass stroke ()
  ((name 
     :initarg :name 
     :reader stroke-name)
   (default-modifiers-p 
     :initarg :default-modifiers-p
     :reader default-modifiers-p)
   (modifiers
     :initarg :modifiers 
     :reader stroke-modifiers)
   (action
     :initarg :action
     :reader stroke-action)))

(defgeneric stroke-keys (stroke))
(defgeneric stroke-equal (s1 s2))

(defmethod stroke-equal (s1 s2)
  (declare (ignorable s1 s2))
  nil)

(defmethod stroke-equal :around ((s1 stroke) (s2 stroke))
  (with-slots (name1 default-modifiers-p1 modifiers1) s1
    (with-slots (name2 default-modifiers-p2 modifiers2) s2
      (and (eq name1 name2)
	   (and default-modifiers-p1 default-modifiers-p2)
	   (equal modifiers1 modifiers2)
	   (if (next-method-p) (call-next-method) t)))))

;;;; keystroke

(defclass keystroke (stroke)
  ((keysyms :initarg :keysyms :reader keystroke-keysyms)))

(defun make-keystroke (name key-name-set modifiers default-modifiers-p action)
  (make-instance 'keystroke
    :name name
    :keysyms (mapcar #'kb:keyname->keysym key-name-set)
    :default-modifiers-p default-modifiers-p
    :modifiers modifiers
    :action action))

(defun keystroke-p (stroke)
  (typep stroke 'keystroke))

(defmethod stroke-keys ((stroke keystroke))
  (loop for k in (keystroke-keysyms stroke)
	append (multiple-value-list (xlib:keysym->keycodes *display* k))))

(defmethod stroke-equal ((s1 keystroke) (s2 keystroke))
  (equal (slot-value s1 'keysyms)  (slot-value s2 'keysyms)))
  
;;;; mouse stroke

(defclass mouse-stroke (stroke)
  ((button :initarg :button :reader mouse-stroke-button)))

(defun make-mouse-stroke (name button modifiers default-modifiers-p action)
  (unless (or (numberp button) (eq :any button)) 
    (error (format nil "wrong button type: ~A~%" (type-of button))))
  (make-instance 'mouse-stroke
    :name name
    :button (list button)
    :default-modifiers-p default-modifiers-p
    :modifiers modifiers
    :action action))

(defun mouse-stroke-p (stroke)
  (typep stroke 'mouse-stroke))

(defmethod stroke-keys ((stroke mouse-stroke))
  (mouse-stroke-button stroke))

(defmethod stroke-equal ((s1 mouse-stroke) (s2 mouse-stroke))
  (= (car (slot-value s1 'button)) (car (slot-value s2 'button))))

;;;;

(defun translate-modifiers (dpy modifiers)
  (cond ((keywordp modifiers) 
	 (list (kb:modifier->modifier-mask dpy modifiers)))
	((numberp modifiers) 
	 (list modifiers))
	((eq (car modifiers) :and)
	 (list (loop for mod in (cdr modifiers)
		     sum (kb:modifier->modifier-mask dpy mod))))
	(t 
	 (mapcar #'(lambda (m) (kb:modifier->modifier-mask dpy m)) modifiers))))

(defun action-key->lambda (action-keyword)
  "Returns the associated predefined callback for the given action keyword."
  (case action-keyword
    (:switch-win-up
     (action () (:press (circulate-window *root* :direction :above))))
    (:switch-win-down
     (action () (:press (circulate-window *root* :direction :below))))
    (:switch-screen-left
     (action (:press (change-vscreen *root* :direction #'-)) ()))
    (:switch-screen-right
     (action (:press (change-vscreen *root* :direction #'+)) ()))
    (:move-right (action (:press (move-cursor-right)) ()))
    (:move-left (action (:press (move-cursor-left)) ()))
    (:move-up (action (:press (move-cursor-up)) ()))
    (:move-down (action (:press (move-cursor-down)) ()))
    (:left-click #'(lambda (event) (perform-click 1 event)))
    (:middle-click #'(lambda (event) (perform-click 2 event)))
    (:right-click #'(lambda (event) (perform-click 3 event)))
    (:scroll-up #'(lambda (event) (perform-click 4 event)))
    (:scroll-down #'(lambda (event) (perform-click 5 event)))
    (:move-window 
     #'(lambda (event)
	 (mouse-stroke-for-move-and-resize event :action :move)))
    (:resize-window 
     #'(lambda (event)
	 (mouse-stroke-for-move-and-resize event :action :resize)))
    ))

(defmacro action ((&rest f1) (&rest f2))
  (when (or (eq (car f1) :release) (eq (car f2) :press)) (rotatef f1 f2))
  `(lambda (event)
     (typecase event
       (button-press ,@(cdr f1))
       (key-press ,@(cdr f1))
       (key-release ,@(cdr f2)))))

(defmacro unrealize ((window &key mouse-p) code mask)
  `(progn
     ,@(if mouse-p
	   `((remhash (cons ,code ,mask) *mouse-stroke-map*)
	     (xlib:ungrab-button ,window ,code :modifiers ,mask))
	   `((remhash (cons ,code ,mask) *keystroke-map*)
	     (setf (aref *registered-keycodes* ,code) 0)
	     (xlib:ungrab-key ,window ,code :modifiers ,mask)))))

(defmacro undefine-combo-internal (stroke dest-window &key mouse-p)
  `(with-slots (name modifiers default-modifiers-p action) ,stroke
     (remhash name ,(if mouse-p '*mousestrokes* '*keystrokes*))
     (loop with dpy = (xlib:drawable-display ,dest-window)
	   with num-l = (kb:modifier->modifier-mask dpy :NUM-LOCK)
	   with caps-l = (kb:modifier->modifier-mask dpy :CAPS-LOCK)
	   for mask in (translate-modifiers dpy modifiers) do
	   (loop for key in (stroke-keys ,stroke) do
		 (unrealize (,dest-window :mouse-p ,mouse-p) key mask)
		 (when (and default-modifiers-p (not (eql mask :any)))
		   (when caps-l 
		     (unrealize (,dest-window :mouse-p ,mouse-p)
		       key (+ mask caps-l)))
		   (when num-l 
		     (unrealize (,dest-window :mouse-p ,mouse-p)
		       key (+ mask num-l)))
		   (when (and num-l caps-l)
		     (unrealize (,dest-window :mouse-p ,mouse-p)
		       key (+ mask num-l caps-l))))))))

(defmacro realize ((window &key mouse-p) code mask action-keyword action)
  `(progn
     ,@(if mouse-p
	   `((setf (gethash (cons ,code ,mask) *mouse-stroke-map*)
		 (or ,action (action-key->lambda ,action-keyword)))
	     (xlib:grab-button ,window
			       ,code 
			       '(:button-press) 
			       :modifiers ,mask 
			       :sync-pointer-p t))
	   `((setf (gethash (cons ,code ,mask) *keystroke-map*)
		   (or ,action (action-key->lambda ,action-keyword)))
	     (setf (aref *registered-keycodes* ,code) 1)
	     (xlib:grab-key ,window ,code :modifiers ,mask :owner-p nil)))))

(defmacro define-combo-internal (stroke dest-window &key mouse-p)
  `(with-slots (name modifiers default-modifiers-p action) ,stroke  
     (loop with dpy = (xlib:drawable-display ,dest-window)
           with num-l = (kb:modifier->modifier-mask dpy :NUM-LOCK)
	   with caps-l = (kb:modifier->modifier-mask dpy :CAPS-LOCK)
	   for mask in (translate-modifiers dpy modifiers) do
	   (loop for key in (stroke-keys ,stroke) do
		 (realize (,dest-window :mouse-p ,mouse-p)
		   key mask name action)
		 (when (and default-modifiers-p (not (eql mask :any)))
		   (when caps-l 
		     (realize (,dest-window :mouse-p ,mouse-p)
		       key (+ mask caps-l) name action ))
		   (when num-l 
		     (realize (,dest-window :mouse-p ,mouse-p)
		       key (+ mask num-l) name action))
		   (when (and num-l caps-l)
		     (realize (,dest-window :mouse-p ,mouse-p)
		       key (+ mask num-l caps-l) name action)))))))

(defun define-key-combo (name &key keys
			          (default-modifiers-p t)
				  (modifiers :any)
				  fun)
" modifiers can be:
  - composition of modifiers as '(:and :ALT-LEFT :CONTROL-RIGHT)
  - a simple modifier as :ALT-LEFT or 18 (a modifier mask)
  - a list of possible modifiers as '(:ALT-LEFT :CONTOL-RIGHT)"
  (catch 'keystroke-definition
    (handler-bind
        ((error #'(lambda (condition)
		    (declare (ignorable condition))
		    (format *stderr* "Can't realize key-combo ~A~%" name)
		    (format *stderr* " mods : ~A~% key : ~A~%" modifiers keys)
		    (throw 'keystroke-definition nil))))
      (let ((ks (make-keystroke name keys modifiers default-modifiers-p fun)))
	(when (stroke-equal ks (gethash name *keystrokes*))
	  (undefine-combo-internal ks *root-window*))
	(define-combo-internal ks *root-window*)
	(setf (gethash name *keystrokes*) ks)))))

(defun define-mouse-combo (name &key button
				     (default-modifiers-p t)
				     (modifiers :any)
				     fun)
" modifiers can be:
  - composition of modifiers as '(:and :ALT-LEFT :CONTROL-RIGHT)
  - a simple modifier as :ALT-LEFT or 18 (a modifier mask)
  - a list of possible modifiers as '(:ALT-LEFT :CONTOL-RIGHT)"
  (catch 'mouse-stroke-definition
    (handler-bind
        ((error #'(lambda (condition)
		    (declare (ignorable condition))
		    (format *stderr* "Can't realize mouse-combo ~A~%" name)
		    (format *stderr* " mods : ~A~% key : ~A~%" modifiers button)
		    (throw 'mouse-stroke-definition nil))))
      (let ((ms (make-mouse-stroke 
		    name button modifiers default-modifiers-p fun)))
	(when (stroke-equal ms (gethash name *mousestrokes*))
	  (undefine-combo-internal ms *root-window* :mouse-p t))
	(define-combo-internal ms *root-window* :mouse-p t)
	(setf (gethash name *mousestrokes*) ms)))))

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

(defun mouse-stroke-for-move-and-resize (event &key action)
  (let ((widget (lookup-widget (event-child event))))
    (unless (decoration-p widget)
      (return-from mouse-stroke-for-move-and-resize nil))
    (when (eq *focus-type* :on-click)
      (focus-widget widget 0))
    (xlib:grab-pointer (event-child event) +pointer-event-mask+)
    (menu-3-process event widget :key action)
    (funcall (define-menu-3 action))))
