;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: gestures.lisp,v 1.20 2005/01/18 23:22:44 ihatchondo Exp $
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

;;;; Internal machinery for stroke management.

(defvar *keystroke-map* (make-hash-table :test #'equal))
(defvar *mouse-stroke-map* (make-hash-table :test #'equal))
(defvar *registered-keycodes* (make-array 256 :element-type 'bit))
(defvar *keystrokes* (make-hash-table :test #'eq))
(defvar *mousestrokes* (make-hash-table :test #'eq))

(declaim (type (simple-array bit (256)) *registered-keycodes*))

(defun realize-keystroke (window code mask action)
  (setf (gethash (cons code mask) *keystroke-map*) action)
  (setf (aref *registered-keycodes* code) 1)
  (xlib:grab-key window code :modifiers mask :owner-p nil))

(defun unrealize-keystroke (window code mask action)
  (declare (ignore action))
  (remhash (cons code mask) *keystroke-map*)
  (setf (aref *registered-keycodes* code) 0)
  (xlib:ungrab-key window code :modifiers mask))

(defun realize-mousestroke (window code mask action)
  (setf (gethash (cons code mask) *mouse-stroke-map*) action)
  (xlib:grab-button 
      window code '(:button-press) :modifiers mask :sync-pointer-p t))

(defun unrealize-mousestroke (window code mask action)
  (declare (ignore action))
  (remhash (cons code mask) *mouse-stroke-map*)
  (xlib:ungrab-button window code :modifiers mask))

(defmacro with-combo-realizer((mode) stroke &key destination type)
  (let* ((prefix (ecase mode (:undefine 'unrealize) (:define 'realize)))
	 (macro (intern (format nil "~a-~a" prefix type))))
    `(with-slots (name modifiers default-modifiers-p action) ,stroke
       (remhash name ,(if (eq type :keystroke) '*keystrokes* '*mousestrokes*))
       (loop with dpy = (xlib:drawable-display ,destination)
	     with num-l = (kb:modifier->modifier-mask dpy :NUM-LOCK)
	     with caps-l = (kb:modifier->modifier-mask dpy :CAPS-LOCK)
	     for mask in (translate-modifiers dpy modifiers) do
	     (loop for key in (stroke-keys ,stroke) do
		   (,macro ,destination key mask action)
	           (when (and default-modifiers-p (not (eql mask #x8000)))
		     (when caps-l 
		       (,macro ,destination key (+ mask caps-l) action))
		     (when num-l 
		       (,macro ,destination key (+ mask num-l) action))
		     (when (and num-l caps-l)
		       (,macro ,destination key
			       (+ mask num-l caps-l) action))))))))

(defmacro undefine-combo-internal (stroke dest-window &key mouse-p)
  `(with-combo-realizer (:undefine) ,stroke
     :type ,(if mouse-p :mousestroke :keystroke)
     :destination ,dest-window))

(defmacro define-combo-internal (stroke dest-window &key mouse-p)
  `(with-combo-realizer (:define) ,stroke
     :type ,(if mouse-p :mousestroke :keystroke)
     :destination ,dest-window))

;; Public.

(defun register-callback (action-keyword callback)
  "Associates a particular callback function with a callback name.
   action-keyword (keyword): the name of the stroke.
   callback (function): a function designator of one argument of type event."
  (setf (get action-keyword 'callback) callback))

(defun action-key->lambda (action-keyword)
  "Returns the associated callback for the given action keyword."
  (get action-keyword 'callback))

(defun lookup-keystroke (code state)
  "Find the associated callback if any for this pair (code, modifier state)."
  (or (gethash (cons code state) *keystroke-map*)
      (gethash (cons code #x8000) *keystroke-map*)))

(defun lookup-mouse-stroke (button state)
  "Find the associated callback if any for this pair (button, modifier state)."
  (or (gethash (cons button state) *mouse-stroke-map*)
      (gethash (cons button #x8000) *keystroke-map*)))

(defun keycode-registered-p (keycode &optional (count 1))
  "Returns T if this keycode is used for any keystroke."
  (loop for i from keycode below (+ keycode count)
	when (= 1 (aref *registered-keycodes* keycode)) do (return t)))

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

;;;; Stroke protocol class.

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

(defgeneric stroke-keys (stroke)
  (:documentation "Returns the list of keycodes that activate this stroke."))

(defgeneric stroke-equal (s1 s2)
  (:documentation "Returns T if the two stroks are equal.")
  (:method (s1 s2) (declare (ignorable s1 s2))))

(defmethod stroke-equal :around ((s1 stroke) (s2 stroke))
  (with-slots ((name1 name) (dmp1 default-modifiers-p) (mods1 modifiers)) s1
    (with-slots ((name2 name) (dmp2 default-modifiers-p) (mods2 modifiers)) s2
      (and (eq name1 name2) (and dmp1 dmp2) (equal mods1 mods2)
	   (if (next-method-p) (call-next-method) t)))))

;;;; Keystroke

(defclass keystroke (stroke)
  ((keysyms :initarg :keysyms :reader keystroke-keysyms)))

(defun make-keystroke (name key-name-set modifiers default-modifiers-p action)
  (make-instance 'keystroke
    :name name
    :keysyms (mapcar #'kb:keyname->keysym key-name-set)
    :default-modifiers-p default-modifiers-p
    :modifiers modifiers
    :action (or action (action-key->lambda name))))

(defun keystroke-p (stroke)
  (typep stroke 'keystroke))

(defmethod stroke-keys ((stroke keystroke))
  (loop for k in (keystroke-keysyms stroke)
	append (multiple-value-list (xlib:keysym->keycodes *display* k))))

(defmethod stroke-equal ((s1 keystroke) (s2 keystroke))
  (equal (slot-value s1 'keysyms)  (slot-value s2 'keysyms)))

;;;; Mouse stroke

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
    :action (or action (action-key->lambda name))))

(defun mouse-stroke-p (stroke)
  (typep stroke 'mouse-stroke))

(defmethod stroke-keys ((stroke mouse-stroke))
  (mouse-stroke-button stroke))

(defmethod stroke-equal ((s1 mouse-stroke) (s2 mouse-stroke))
  (= (car (slot-value s1 'button)) (car (slot-value s2 'button))))

;;;;

(defun translate-modifiers (dpy modifiers)
  "Returns a list of modifier mask (list xlib:mask16)."
  (cond ((keywordp modifiers) 
	 (list (kb:modifier->modifier-mask dpy modifiers)))
	((numberp modifiers) 
	 (list modifiers))
	((eq (car modifiers) :and)
	 (list (loop for mod in (cdr modifiers)
		     sum (kb:modifier->modifier-mask dpy mod))))
	(t 
	 (mapcar #'(lambda (m) (kb:modifier->modifier-mask dpy m)) modifiers))))

;;;; End user functions for keystrokes and mousestrokes definition.

(defmacro action ((&rest f1) (&rest f2))
  (when (or (eq (car f1) :release) (eq (car f2) :press)) (rotatef f1 f2))
  `(lambda (event)
     (typecase event
       (button-press ,@(cdr f1)) (button-release ,@(cdr f2))
       (key-press ,@(cdr f1)) (key-release ,@(cdr f2)))))

(defun define-key-combo (name &key keys
			          (default-modifiers-p t)
				  (modifiers '(:any))
				  fun)
  "Defines a keystroke (if already defined then it will be undefined first):
    name (string): name of the defined key stroke.
    :keys (unsigned-byte 8): the keyboard keys used for this keystroke.
    :default-modifiers-p (boolean): If T then :CAPS-LOCK and :NUM-LOCK won't 
     affect the keystroke invokation. (default to T).
    :modifiers (or (member :any) mask16 (list modifier-key)):
     modifiers will be interpreted as follows:
      - composition of modifiers as '(:and :ALT-LEFT :CONTROL-RIGHT)
      - a simple modifier as :ALT-LEFT or 18 (a modifier mask)
      - a list of possible modifiers as '(:ALT-LEFT :CONTOL-RIGHT)"
  (handler-case
      (let ((ks (make-keystroke name keys modifiers default-modifiers-p fun)))
	(when (stroke-equal ks (gethash name *keystrokes*))
	  (undefine-combo-internal ks *root-window*))
	(define-combo-internal ks *root-window*)
	(setf (gethash name *keystrokes*) ks))
    (error ()
      (format *stderr* "Can't realize key-combo ~A~%" name)
      (format *stderr* " mods : ~A~% key : ~A~%" modifiers keys))))

(defun define-mouse-combo (name &key button
				     (default-modifiers-p t)
				     (modifiers '(:any))
				     fun)
  "Defines a mouse stroke (if already defined then it will be undefined first):
    name (string): name of the defined mouse stroke.
    :button (unsigned-byte 8): the mouse button used for this mouse stroke.
    :default-modifiers-p (boolean): If T then :CAPS-LOCK and :NUM-LOCK won't 
     affect the mouse stroke invokation. (default to T).
    :modifiers (or (member :any) mask16 (list modifier-key)):
     modifiers will be interpreted as follows:
      - composition of modifiers as '(:and :ALT-LEFT :CONTROL-RIGHT)
      - a simple modifier as :ALT-LEFT or 18 (a modifier mask)
      - a list of possible modifiers as '(:ALT-LEFT :CONTOL-RIGHT)"
  (handler-case
      (let ((ms (make-mouse-stroke 
		    name button modifiers default-modifiers-p fun)))
	(when (stroke-equal ms (gethash name *mousestrokes*))
	  (undefine-combo-internal ms *root-window* :mouse-p t))
	(define-combo-internal ms *root-window* :mouse-p t)
	(setf (gethash name *mousestrokes*) ms))
    (error ()
      (format *stderr* "Can't realize mouse-combo ~A~%" name)
      (format *stderr* " mods : ~A~% key : ~A~%" modifiers button))))

;;;; Cursor movements, and clicks.

(defun move-cursor-right ()
  "Moves the mouse pointer of ten pixels to the right."
  (xlib:warp-pointer-relative *display* 10 0))

(defun move-cursor-left ()
  "Moves the mouse pointer of ten pixels to the left."
  (xlib:warp-pointer-relative *display* -10 0))

(defun move-cursor-up ()
  "Moves the mouse pointer of ten pixels up."
  (xlib:warp-pointer-relative *display* 0 -10))

(defun move-cursor-down ()
  "Moves the mouse pointer of ten pixels down."
  (xlib:warp-pointer-relative *display* 0 10))

(defun perform-click (buton-number ev)
  "Send a button-{press, release} event for button-number. The type of the
   sent event will be determined according to the type of the ev event
   argument: if type key-press then send button-press, if key-release then
   button-release is sent. The destination window will be retreived in the
   ev event argument."
  (flet ((my-query (win) (multiple-value-list (xlib:query-pointer win))))
    (loop with window = *root-window*
	  with type = (if (typep ev 'key-press) :button-press :button-release)
	  for (x y ssp child nil root-x root-y root) = (my-query window)
	  while child do (setf window child)
	  finally
	    (xlib:send-event window type nil
	      :x x :y y :root-x root-x :root-y root-y
	      :state nil :code buton-number
	      :window window :event-window window :root root :child child
	      :same-screen-p ssp :time (event-time ev)))))

(defun mouse-stroke-for-move-and-resize (event &key action)
  "Initiates the move or resize processas if initiated by the menu-3 machinery.
   The process that will be activated is specified by the action key argument.
   If action is :resize then initiate resize, if :move initiate the move."
  (let ((widget (lookup-widget (event-child event))))
    (unless (or (decoration-p widget) (application-p widget))
      (return-from mouse-stroke-for-move-and-resize nil))
    (when (eq *focus-type* :on-click)
      (focus-widget widget 0))
    (xlib:grab-pointer (event-child event) +pointer-event-mask+)
    (menu-3-process event widget :key action)
    (unless (menu-3-process (make-event :motion-notify) widget :key action)
      (xlib:ungrab-pointer *display*))))

;;;; Hook and Callbacks for :switch-win-{up, down} keystrokes.

(defvar *depth* nil)
(defvar *current-widget-info* nil)
(defvar *windows* nil)

(defun initialize-circulate-window (root-window dpy)
  "Initialize gestures internal hooks before circulating windows."
  (loop with map = *keystroke-map*
	for mod in (stroke-modifiers (gethash :switch-win-up *keystrokes*))
	for code = (unless (eq mod :and) (kb:keyname->keycodes dpy mod))
	when code
	do (setf (gethash (cons (if (listp code) (car code) code) #x8000) map)
		 #'circulate-window-modifier-callback))
  (xlib:grab-keyboard root-window)
  (unless *current-widget-info*
    (setf *current-widget-info* (create-message-box nil :parent root-window)))
  (let ((i (current-vscreen root-window)))
    (setf *windows* (reverse (screen-content i :iconify-p *cycle-icons-p*))
	  *depth* 0)))

(defun circulate-window-modifier-callback (event)
  "Callback to handle the key-release on the modifier keys used for the
   window circulation keystrokes."
  (when (typep event 'key-release)
    (xlib:ungrab-keyboard *display*)
    (loop with map = *keystroke-map*
	  for mod in (stroke-modifiers (gethash :switch-win-up *keystrokes*))
	  for code = (unless (eq mod :and) (kb:keyname->keycodes *display* mod))
	  when code
	  do (remhash (cons (if (listp code) (car code) code) #x8000) map))
    (let ((widget (lookup-widget (car *windows*))))
      (when widget (setf (application-wants-iconic-p widget) nil)))
    (xlib:unmap-window (widget-window *current-widget-info*))
    (setf *depth* nil *windows* nil)))

(defun circulate-window-up-and-down (event direction)
  "Circulate windows according to the `direction' argument (or :above :below)."
  (when (typep event 'key-press)
    (with-slots ((root-win root)) event
      (unless *depth*
	(initialize-circulate-window root-win (xlib:drawable-display root-win)))
      (unless *windows* (return-from circulate-window-up-and-down nil))
      (setf *windows* (loop for w in *windows* if (lookup-widget w) collect w))
      (circulate-window
          (lookup-widget root-win)
	  :direction direction
	  :nth (if (eq direction :above) (incf *depth*) (decf *depth*))
	  :windows *windows*
	  :icon-p *cycle-icons-p*))
    (let* ((length (length *windows*))
	   (depth-aux (mod *depth* length)))
      (cond
	((<= length 1) nil)
	((and (eq direction :above) (= depth-aux 0))
	 (setf (cdr (last *windows*)) (list (pop *windows*))))
	((and (eq direction :below) (= depth-aux (1- length)))
	 (let ((penultimate-cons (last *windows* 2)))
	   (push (cadr penultimate-cons) *windows*)
	   (setf (cdr penultimate-cons) nil)))
	(t
	 (when (eq direction :below) (incf depth-aux))
	 (rotatef (nth 0 *windows*) (nth depth-aux *windows*)))))
    (when (and *verbose-window-cycling* (car *windows*))
      (with-slots (window icon) (lookup-widget (car *windows*))
	(setf (message-pixmap *current-widget-info*)
	      (or (icon-pixmap-to-free icon)
		  (clx-ext::wm-hints-icon-pixmap window)))
	(setf (button-item-to-draw *current-widget-info*) (wm-name window)))
      (with-slots (window) *current-widget-info*	  
	(xlib:map-window window)
	(setf (xlib:window-priority window) :above)
	(repaint *current-widget-info* nil nil)))))

;;;; register predefined callbacks.

(register-callback :move-right (action (:press (move-cursor-right)) ()))
(register-callback :move-left (action (:press (move-cursor-left)) ()))
(register-callback :move-up (action (:press (move-cursor-up)) ()))
(register-callback :move-down (action (:press (move-cursor-down)) ()))
(register-callback :left-click #'(lambda (event) (perform-click 1 event)))
(register-callback :middle-click #'(lambda (event) (perform-click 2 event)))
(register-callback :right-click #'(lambda (event) (perform-click 3 event)))
(register-callback :scroll-up #'(lambda (event) (perform-click 4 event)))
(register-callback :scroll-down #'(lambda (event) (perform-click 5 event)))
(register-callback :move-window 
  #'(lambda (event) (mouse-stroke-for-move-and-resize event :action :move)))
(register-callback :resize-window 
  #'(lambda (event) (mouse-stroke-for-move-and-resize event :action :resize)))
(register-callback :switch-win-up
  #'(lambda (e) (circulate-window-up-and-down e :above)))
(register-callback :switch-win-down
  #'(lambda (e) (circulate-window-up-and-down e :below)))
(register-callback :switch-screen-left
  (action (:press (change-vscreen *root* :direction #'-)) ()))
(register-callback :switch-screen-right
  (action (:press (change-vscreen *root* :direction #'+)) ()))
