;;; -*- Mode: Lisp; Package: User -*-

;;; ECLIPSE. The Common Lisp Window Manager.
;;; Copyright (C) 2000, 2001, 2002 Iban HATCHONDO
;;; Copyright (C) 2000
;;;          Julien BONINFANTE,
;;;          Aymeric LACORTE,
;;;          Jocelyn FRECHOT
;;; contact : hatchond@yahoo.fr

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; usefull for having a quick and short garbadge collection.
#+:cmu (setf extensions:*bytes-consed-between-gcs* 400000
	     extensions:*gc-verbose* nil)

;; protocol for treating events
(defgeneric menu-3-handle-event (event object &rest rest &allow-other-keys))
(defgeneric close-window (object))

(make-error-handler (wrong-name :verbose nil)
  (message-box *root-window* "Wrong application name"))

(defun run-application (program &rest arguments)
  (lambda (&rest)
    (catch 'wrong-name
      (handler-bind ((error #'handle-wrong-name-condition))
	(%run-program% program arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General comment

;;;; We use the term "master" to designate the container of the decoration
;;;; Every were you'll find master it will represent the decoration object
;;;; and each time you'll see master-win, it represent the window of the
;;;; master object.

;;;; To represent the virtual-screens we use the term VSCREEN...
;;;; It will consist in an array of list call vscreens in which each list
;;;; (a vscreen) will represent the virtual screen contents.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Key gestion

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

;;; Eclipse Virtual screens

;;; The size of the array is the number of virtual desktop the user ask for.
;;; Each cell of the array will be a list that contain the windows of each
;;; virtual desktop. It will, only, contain application client window.

(defclass eclipse-screens (virtual-screen:virtual-screens) ())

(defun create-eclipse-virtual-screens ()
  (make-instance 'eclipse-screens :number-of-virtual-screen 4))

(defmacro current-desk () `(vs:current-screen (root-vscreens *root*)))

(defsetf number-of-virtual-screens () (n)
  `(with-slots (vscreens) *root*
     (unless (or (zerop ,n) (= ,n (vs:number-of-virtual-screen vscreens)))
       (flet ((adjust-desk-num (window)
		(setf (window-desktop-num window) (1- ,n))))
	 (vs:adjust-vscreens vscreens ,n :map-when-reduce #'adjust-desk-num))
       (when (> (vs:current-screen vscreens) (1- ,n))
	 (change-vscreen vscreens nil (1- ,n)))
       (setf *nb-vscreen* ,n
	     (gnome:win-workspace-count *root-window*) ,n
	     (gnome:net-number-of-desktops *root-window*) ,n
	     (gnome:net-desktop-viewport *root-window*)
	     (make-view-port-property)))))

(defmethod add-to-vscreen ((vscreens eclipse-screens) window
			  &key (n (vs:current-screen vscreens)))
  (unless (member :WIN_HINTS_SKIP_TASKBAR (gnome:win-hints window))
    (if (= n +any-desktop+)
	(vs:add-to-all vscreens window)
        (vs:add-to-vscreen vscreens window :n n))))

(defmethod remove-from-vscreen ((vscreens eclipse-screens) window
				&key (n (vs:current-screen vscreens)))
  (if (= n +any-desktop+)
      (vs:remove-from-all vscreens window)
      (vs:remove-from-vscreen vscreens window :n n)))
  
(defmethod map-or-unmap-screen ((vscreens eclipse-screens)
				&key (fun #'xlib:unmap-window)
				     (n (vs:current-screen vscreens)))
  (loop for window in (vs:nth-vscreen vscreens n)
	for object = (gethash window *object-table*)
	when (and object (application-master object)) do
	  (setf object (application-master object))
	when (and object (= 1 (first (wm-state window)))) do
	  (funcall fun (object-window object))
	when object collect window into windows
	finally (vs:set-vscreen-content vscreens windows :n n)))

(defmethod change-vscreen ((vscreens eclipse-screens) direction &optional n)
  (let* ((current-screen (vs:current-screen vscreens))
	 (new-screen
	  (or n (mod (funcall direction current-screen 1) *nb-vscreen*))))
    (unless (= new-screen +any-desktop+)
      (with-event-mask (*root-window*)
        ;; first : revert the focus to the root-window.
	(xlib:set-input-focus *display* :pointer-root :pointer-root)
	(map-or-unmap-screen vscreens)
	(map-or-unmap-screen vscreens :fun #'xlib:map-window :n new-screen))
      (setf (vs:current-screen vscreens) new-screen
	    (gnome:win-workspace *root-window*) new-screen
	    (gnome:net-current-desktop *root-window*) new-screen)
      (when *change-desktop-message-active-p*
	(message-box *root-window*
		     (or (nth new-screen (workspace-names *root-window*))
			 (format nil "WORKSPACE ~a" new-screen)))))))

(defun get-visible-windows (vscreen)
  (loop for window in vscreen
	when (= 1 (or (car (wm-state window)) 0))
	collect (with-slots (master) (gethash window *object-table*)
		  (if master (object-window master) window))))

(defmethod circulate-window ((vscreens eclipse-screens) &key direction)
  (loop	initially (unless (cdr (vs:nth-vscreen vscreens))
		    (setf direction :above))
	with windows = (xlib:query-tree *root-window*)
	with screen-windows = (get-visible-windows (vs:nth-vscreen vscreens))
	with dest-win = nil
	with sibling = (and (eql direction :below) (get-root-desktop *root* t))
	for window in (if (eq direction :above) windows (reverse windows))
	when (member window screen-windows :test #'xlib:window-equal)
	do (if (or (eq direction :above) dest-win)
	       (loop-finish)
	       (setf dest-win window))
	finally
	  (when screen-windows
	    (when (and sibling (eql direction :below)) (setf direction :above))
	    (setf (xlib:window-priority (or dest-win window) sibling) direction)
	    (xlib:warp-pointer window 8 5))))

;;;; Our base object

(defclass object ()
  ((window :initarg :window :reader object-window)
   (gcontext :initarg :gcontext :reader object-gcontext :allocation :class)))

(defmethod initialize-instance :after ((object object) &rest)
  (setf (gethash (object-window object) *object-table*) object))

(defgeneric remove-object (object))

(defmethod remove-object ((object object))
  (remhash (object-window object) *object-table*))

(defmethod remove-object (object)
  (declare (ignorable object))
  (values))

(defmethod close-window (object)
  (declare (ignorable object))
  (values))

;;;; The ROOT
(defclass root (object)
  ((resize-status :initform :OFF :accessor root-resize-status)
   (move-status :initform :OFF :accessor root-move-status)
   (current-active-decoration :initform nil)
   (menu1 :initform nil)
   (menu2 :initform nil)
   (menu3 :initform nil)
   (client-list :initform (make-hash-table))
   (desktop :initform nil :accessor root-desktop)
   (vscreens :initform (create-eclipse-virtual-screens) :reader root-vscreens)))

(defun object-p (object)
  (typep object 'object))

(defmethod get-root-desktop ((root root) &optional window-p)
  (with-slots (desktop) root
    (when (first desktop)
      (if window-p (object-window (first desktop)) (first desktop)))))
(defmethod add-desktop-application ((root root) (desktop object))
  (push desktop (root-desktop root)))
(defmethod remove-desktop-application ((root root) (desktop object))
  (setf (root-desktop root) (delete desktop (root-desktop root))))

;;;; Application

(defconstant +unfocusable-mask+
  '(:property-change :enter-window :leave-window :visibility-change))
(defconstant +focusable-mask+ (cons :focus-change +unfocusable-mask+))

(defclass application (object)
  ((master :initarg :master :reader application-master)
   (desktop-number :initform 0 :accessor application-desktop-number)
   (input-model :initform nil :reader application-input-model)
   (icon :initform nil :initarg :icon :reader application-icon)
   (focus-p :initform nil :reader focused-p :accessor application-focus-p)
   (iconic-p :initform nil :accessor application-iconic-p)
   (unobscured-p :initform nil :accessor application-unobscured-p)))

(defmethod initialize-instance :after ((application application) &rest)
  (with-slots (window input-model icon master) application
    (let* ((hint (xlib:get-property window :WM_HINTS))
	   (input (and hint (logbitp 0 (first hint)) (= 1 (second hint))))
	   (wm-take-focus (member :wm_take_focus (xlib:wm-protocols window))))
      (setf icon (create-icon application (or master *root*))
	    input-model (cond ((and (not input) (not wm-take-focus)) :no-input)
			      ((and (not input) wm-take-focus) :globally-active)
			      ((and input (not wm-take-focus)) :passive)
			      ((and input wm-take-focus) :locally-active))))
    (setf (xlib:window-event-mask window)
	  (if (eq input-model :no-input) +unfocusable-mask+ +focusable-mask+))
    (xlib:grab-button
        window :any '(:button-press) :sync-pointer-p t :modifiers #x8000)))

(defmethod close-window ((application application))
  (with-slots (window) application
    (if (member :wm_delete_window (xlib:wm-protocols window))
	(xlib:send-event window
			 :client-message
			 nil
			 :window window
			 :type :WM_PROTOCOLS
			 :format 32
			 :data (list (atom-name->id :wm_delete_window)))
        (destroy-application window))))

(defun undecore-application (application &key state)
  (with-slots (window master icon) application
    (if master
        (multiple-value-bind (x y) (get-window-geometry (object-window master))
	  (incf x *margin*) (incf y top-height)
	  (xlib:reparent-window window *root-window* x y))
        (event-process (make-event :destroy-notify :window window) *root*))
    (when state (setf (wm-state window) state))))

(defun create-application (application master)
  (make-instance 'application :window application :master master))

(defun destroy-application (window)
  (xlib:kill-client (xlib:drawable-display window) (xlib:window-id window)))

(defun application-p (object)
  (typep object 'application))

;;;; Button

(defconstant +std-button-mask+
  '(:button-press :button-release :pointer-motion :owner-grab-button :exposure))

(defclass button (object)
  ((master :initarg :master :reader button-master)
   (item-to-draw :initarg :item-to-draw :accessor button-item-to-draw)))

(defmethod draw-on-focus-in ((button button))
  (with-slots (master item-to-draw window gcontext) button
    (let ((w (xlib:drawable-width item-to-draw))
	  (h (xlib:drawable-height item-to-draw)))
      (xlib:copy-area item-to-draw gcontext 0 0 w h window 0 0))))

(defmethod draw-on-focus-out ((button button))
  (xlib:clear-area (object-window button)))

(defclass box-button (button) ())

(defclass push-button (button)
  ((armed :initform nil :accessor button-armed)
   (active-p :initform nil :accessor button-active-p)))

(defconstant +push-button-mask+ (cons :exposure +pointer-event-mask+))

(defmethod event-process ((event enter-notify) (b push-button))
  (when (button-armed b)
    (setf (button-active-p b) t)))
(defmethod event-process ((event leave-notify) (b push-button))
  (when (button-armed b)
    (setf (button-active-p b) nil)))

(defmethod event-process :around ((event button-press) (b push-button))
  (when (next-method-p)
    (setf (button-armed b) t
	  (button-active-p b) t)
    (call-next-method)))

(defmethod event-process :around ((event button-release) (b push-button))
  (with-slots (armed active-p) b
    (if (and armed active-p)
	(when (next-method-p) (call-next-method))
        (event-process event *root*))
    (setf armed nil
	  active-p nil)))

(defclass title-bar (push-button) ())
(defclass close-button (push-button) ())
(defclass iconify-button (push-button) ())
(defclass maximize-button (push-button) ())

;; Those are master edges and master corners
(defclass edge (button) ())
(defmethod initialize-instance :after ((edge edge) &rest)
  (setf (xlib:window-event-mask (object-window edge)) nil))

(defclass top-left (edge) ())
(defclass top-right (edge) ())
(defclass right (edge) ())
(defclass left (edge) ())
(defclass bottom (edge) ())
(defclass bottom-right (edge) ())
(defclass bottom-left (edge) ())

(defvar +corner-cursor+ nil)
(defvar +side-cursor+ nil)

(defun init-edges-cursors ()
  (setf +corner-cursor+
	(list (get-x-cursor *display* :xc_top_left_corner)
	      (get-x-cursor *display* :xc_top_right_corner)
	      (get-x-cursor *display* :xc_bottom_left_corner)
	      (get-x-cursor *display* :xc_bottom_right_corner))
	+side-cursor+
	(list (get-x-cursor *display* :xc_right_side)
	      (get-x-cursor *display* :xc_left_side)
	      *default-root-cursor*
	      (get-x-cursor *display* :xc_bottom_side))))

;; When calling this function arguments :parent :x :y are required
;; the others are optional.
(defun create-button (button-type &key parent x y gcontext
				  item background master (border 0)
				  (gravity :north-west)
				  (width *button-width*)
				  (height *button-height*)
				  (cursor *default-root-cursor*)
				  (event-mask +std-button-mask+))
  (make-instance
      button-type
      :window (xlib:create-window
	          :parent parent
		  :x x :y y
		  :width width :height height
		  :background background
		  :border border
		  :gravity gravity
		  :cursor cursor
		  :event-mask event-mask)
      :gcontext gcontext
      :item-to-draw item
      :master master))

(defun button-p (object)
  (typep object 'button))

;;;; Icon

(defclass icon (push-button)
  ((desiconify-p :initform nil :accessor icon-desiconify-p)
   (application :initarg :application :reader icon-application)))

(defun icon-p (object)
  (typep object 'icon))

(defun make-background-pix (pixmap gcontext drawable)
  (if (= 1 (xlib:drawable-depth pixmap))
      (let* ((w (xlib:drawable-width pixmap))
	     (h (xlib:drawable-height pixmap))
	     (pix (xlib:create-pixmap :drawable drawable
				      :width w :height h
				      :depth (xlib:drawable-depth drawable))))
	(xlib:copy-plane pixmap gcontext 1 0 0 w h pix 0 0)
	pix)
      pixmap))

(defun create-icon (application master
		    &optional (gcontext *gcontext*)
		              (bg-color (xlib:gcontext-background gcontext)))
  (with-slots (window) application
    (let* ((wm-hints (handle-wm-property (xlib:wm-hints window)))
	   (icon-pixmap (when wm-hints (xlib:wm-hints-icon-pixmap wm-hints)))
	   (width (if icon-pixmap (xlib:drawable-width icon-pixmap) 45))
	   (height (if icon-pixmap (xlib:drawable-height icon-pixmap) 60))
	   (pixmap (when (typep icon-pixmap 'xlib:pixmap)
		     (make-background-pix icon-pixmap gcontext window)))
	   (icon (create-button
		     'icon
		     :parent *root-window* :master master
		     :x 750 :y 50 :width width :height height
		     :gcontext gcontext
		     :item (unless pixmap (wm-name window))
		     :background
		     (if (typep pixmap 'xlib:pixmap) pixmap bg-color))))
      (setf (slot-value icon 'application) application)
      icon)))

(defmethod iconify ((application application))
  (setf (application-iconic-p application) t)
  (xlib:unmap-window (object-window application))
  (xlib:map-window (object-window (application-icon application))))

(defmethod uniconify ((icon icon))
  (with-slots (application desiconify-p) icon
    (setf desiconify-p nil)
    (with-slots (window desktop-number) application
      (unless (= desktop-number (current-desk))
	(remove-from-vscreen (root-vscreens *root*) window :n desktop-number)))
    (unmap-icon-window icon)
    (xlib:map-window (object-window application))))

(defmethod unmap-icon-window ((icon icon))
  (with-slots (window master application) icon
    (xlib:unmap-window window)
    (setf (application-iconic-p application) nil)
    (unless (decoration-p master)
      (with-slots (window) application
        (setf (window-desktop-num window) (current-desk)
	      (wm-state window) 1)))))

;;;; Decoration

(defclass decoration (object)
  ((children :initarg :children :accessor decoration-children)
   (active-p :initform nil :accessor decoration-active-p)
   (maximized :initform nil :accessor decoration-maximized)
   (time :initform 0 :accessor decoration-precedent-time :allocation :class)
   (wm-size-hints :initarg :wm-size-hints :reader decoration-wm-size-hints)
   (initial-geometry :initform (make-array 4))))

(defsetf initial-geometry (master) (x y width height)
  `(with-slots (initial-geometry) ,master
     (setf (aref initial-geometry 0) ,x
	   (aref initial-geometry 1) ,y
	   (aref initial-geometry 2) ,width
	   (aref initial-geometry 3) ,height)))

(defconstant +decoration-event-mask+
  (append '(:substructure-notify :visibility-change) +pointer-event-mask+))

(defmethod get-child ((master decoration) label &key window)
  (if window
      (object-window (getf (decoration-children master) label))
      (getf (decoration-children master) label)))

(defmethod initial-x ((master decoration))
  (aref (slot-value master 'initial-geometry) 0))
(defmethod initial-y ((master decoration))
  (aref (slot-value master 'initial-geometry) 1))
(defmethod initial-width ((master decoration))
  (aref (slot-value master 'initial-geometry) 2))
(defmethod initial-height ((master decoration))
  (aref (slot-value master 'initial-geometry) 3))

(defmethod decoration-min-width ((master decoration))
  (+ (* 2 *margin*) (aref (slot-value master 'wm-size-hints) 0)))
(defmethod decoration-min-height ((master decoration))
  (+ top-height bottom-height (aref (slot-value master 'wm-size-hints) 1)))
(defmethod decoration-max-width ((master decoration))
  (+ (* 2 *margin*) (aref (slot-value master 'wm-size-hints) 2)))
(defmethod decoration-max-height ((master decoration))
  (+ top-height bottom-height (aref (slot-value master 'wm-size-hints) 3)))
(defmethod decoration-inc-sizes ((master decoration))
  (with-slots (wm-size-hints) master
    (values (aref wm-size-hints 4) (aref wm-size-hints 5))))
(defmethod decoration-wm-hints ((master decoration))
  (with-slots (wm-size-hints) master
    (values (decoration-min-width master) (decoration-min-height master)
	    (decoration-max-width master) (decoration-max-height master)
	    (aref wm-size-hints 4) (aref wm-size-hints 5))))

(defmethod focused-p ((master decoration))
  (focused-p (get-child master :application)))

(defun decoration-p (object)
  (typep object 'decoration))

(defun parts-to-redraw-on-focus (master)
  (values (get-child master :icon-b)
	  (get-child master :maximize)
	  (get-child master :close)))
	
(defun draw-focused-decoration (master)
  (setf (application-focus-p (get-child master :application)) t)
  (mapc #'draw-on-focus-in
	(multiple-value-list (parts-to-redraw-on-focus master))))

(defun draw-unfocused-decoration (master)
  (setf (application-focus-p (get-child master :application)) nil)
  (mapc #'draw-on-focus-out
	(multiple-value-list (parts-to-redraw-on-focus master))))

(defun make-buttons-bar (container master gcontext)
  (with-slots (children) master
    (loop for type in (list 'iconify-button 'maximize-button 'close-button)
	  for child in (list :icon-b :maximize :close)
	  for item in (list :active-icon-b :active-maximize :active-close)
	  for x = 0 then (incf x *button-width*)
	  do (setf (getf children child)
		   (create-button type
				  :parent container
				  :master master
				  :background (get-pixmap child)
				  :item (get-pixmap item)
				  :x x :y 0
				  :event-mask +push-button-mask+
				  :gcontext gcontext)))))

(defun make-edges (master gcontext)
  (with-slots (children window) master
    (loop for type in (list 'right 'left 'title-bar 'bottom)
	  for child in (list :right :left :center :bottom)
	  for cursor in +side-cursor+
	  for pixmap = (get-pixmap child)
	  do (setf (getf children child)
		   (create-button type
				  :parent window
				  :master master
				  :background pixmap
				  :x 0 :y *button-height*
				  :width (get-pixmap-width child)
				  :height (get-pixmap-height child)
				  :gcontext gcontext :cursor cursor))))
  (update-edges-geometry master))

(defun make-corner (master master-sizes gcontext)
  (with-slots (children window) master
    (loop with (width height) = master-sizes
	  for type in (list 'top-left 'top-right 'bottom-left 'bottom-right)
	  for gravity in (list :north-west :north-east :south-west :south-east)
	  for child in (list :top-left :top-right :bottom-left :bottom-right)
	  for cursor in +corner-cursor+
	  for pixmap = (get-pixmap child)
	  for pix-w = (get-pixmap-width child)
	  for pix-h = (get-pixmap-height child)
	  for (x . y) in '((0 . 0) (nil . 0) (0 . nil) (nil . nil))
	  do (setf (getf children child)
		   (create-button type
				  :parent window
				  :master master
				  :background pixmap
				  :gravity gravity
				  :x (or x (- width pix-w))
				  :y (or y (- height pix-h))
				  :width pix-w :height pix-h
				  :gcontext gcontext :cursor cursor)))))

(defun update-edges-geometry (master)
  (let* ((title-bar (get-child master :center :window t))
	 (left (get-child master :left :window t))
	 (right (get-child master :right :window t))
	 (bottom (get-child master :bottom :window t))
	 (width (xlib:drawable-width (object-window master)))
	 (height (xlib:drawable-height (object-window master)))
	 (side-height (max 0 (- height top-height bottom-height))))
    (setf (xlib:drawable-height left) side-height)
    (xlib:with-state (right)
      (setf (xlib:drawable-x right) (- width (get-pixmap-width :right))
	    (xlib:drawable-y right) top-height
	    (xlib:drawable-height right) side-height))
    (xlib:with-state (title-bar)
      (setf (xlib:drawable-x title-bar) (get-pixmap-width :top-left)
	    (xlib:drawable-y title-bar) 0
	    (xlib:drawable-width title-bar)
	    (max 0 (- width (+ (get-pixmap-width :top-left)
			       (get-pixmap-width :top-right))))))	
    (xlib:with-state (bottom)
      (setf (xlib:drawable-x bottom) (get-pixmap-width :bottom-left)
	    (xlib:drawable-y bottom) (- height bottom-height)
	    (xlib:drawable-width bottom)
	    (max 0 (- width (+ (get-pixmap-width :bottom-left)
			       (get-pixmap-width :bottom-right))))))))

(defmacro compute-size-decoration (window)
  `(values (+ (* 2 *margin*) (xlib:drawable-width ,window))
           (+ (xlib:drawable-height ,window) top-height bottom-height)))

(defmacro minimize (i &optional (inf 1)) `(max (or ,i ,inf) ,inf))

(defun compute-wm-normal-hints (window)
  (let* ((hints (xlib:wm-normal-hints window))
	 (max-w (- (screen-width) (* 2 *margin*)))
	 (max-h (- (screen-height) top-height bottom-height)))
    (if hints
	(let* ((min-width (minimize (xlib:wm-size-hints-min-width hints)))
	       (min-height (minimize (xlib:wm-size-hints-min-height hints)))
	       (width-inc (minimize (xlib:wm-size-hints-width-inc hints)))
	       (height-inc (minimize (xlib:wm-size-hints-height-inc hints)))
	       (max-w (- max-w (mod (- max-w min-width) width-inc)))
	       (max-h (- max-h (mod (- max-h min-height) height-inc)))
	       (max-width (or (xlib:wm-size-hints-max-width hints) max-w))
	       (max-height (or (xlib:wm-size-hints-max-height hints) max-h)))
	  (unless (< 1 max-width max-w) (setf max-width max-w))
	  (unless (< 1 max-height max-h) (setf max-height max-h))
	  (when (< (xlib:drawable-width window) min-width)
	    (setf (xlib:drawable-width window) min-width))
	  (when (< (xlib:drawable-height window) min-height)
	    (setf (xlib:drawable-height window) min-height))
	  (vector
	      min-width min-height max-width max-height width-inc height-inc))
        (vector 1 1 max-w max-h 1 1))))

(defun create-decoration (window application &key (map t))
  (let* ((button-container)
	 (wm-sizes (compute-wm-normal-hints window))
	 (sizes (multiple-value-list (compute-size-decoration window)))
	 (master-window (xlib:create-window
			    :parent *root-window*
			    :width (first sizes)
			    :height (second sizes)
			    :background :parent-relative
			    :event-mask +decoration-event-mask+
			    :x (max 0 (- (xlib:drawable-x window) *margin*))
			    :y (max 0 (- (xlib:drawable-y window) top-height))))
	 (master (make-instance 'decoration
				:window master-window
				:gcontext *gcontext*
				:children (list :application application)
				:wm-size-hints wm-sizes)))
    (make-edges master *gcontext*)
    (setf button-container (xlib:create-window
			       :parent (get-child master :center :window t)
			       :x (- (car sizes)
				     (get-pixmap-width :top-left)
				     (get-pixmap-width :top-right)
				     (* 3 *button-width*))
			       :y (floor (- top-height *button-height*) 2)
			       :width (* 3 *button-width*)
			       :height *button-height*
			       :background :parent-relative
			       :gravity :north-east))
    (make-buttons-bar button-container master *gcontext*)
    (make-corner master sizes *gcontext*)
    (with-slots (icon) application
      (setf (getf (decoration-children master) :icon) icon
	    (slot-value icon 'master) master
	    (slot-value application 'master) master
	    (button-item-to-draw (get-child master :center)) (wm-name window)
	    (xlib:drawable-border-width window) 0))
    (with-event-mask (master-window)
      (xlib:map-subwindows button-container)
      (xlib:map-subwindows (get-child master :center :window t))
      (xlib:map-subwindows master-window))
    (with-event-mask (master-window (when map +decoration-event-mask+))
      (xlib:reparent-window window master-window *margin* top-height))
    (when map (xlib:map-window window))))

;;;; Focus management. According to ICCCM

(defgeneric set-focus (input-model window timestamp))

(defmethod set-focus ((input-model (eql :globally-active)) window timestamp)
  (xlib:send-event window
		   :client-message
		   nil
		   :window window
		   :type :WM_PROTOCOLS
		   :format 32
		   :data (list (atom-name->id :wm_take_focus) timestamp)))

(defmethod set-focus ((input-model (eql :locally-active)) window timestamp)
  (set-focus :globally-active window timestamp)
  (xlib:set-input-focus *display* window :pointer-root))

(defmethod set-focus ((input-model (eql :passive)) window timestamp)
  (declare (ignorable timestamp))
  (xlib:set-input-focus *display* window :pointer-root))

(defmethod set-focus ((input-model (eql :no-input)) window timestamp)
  (declare (ignorable window timestamp))
  nil)

;;;; Eclipse

(let ((pixmaps (make-array 14 :initial-element nil)))
  (defun get-pixmap (pixmap-key)
    (aref pixmaps (ecase pixmap-key
		    (:icon-b 0)
		    (:active-icon-b 1)
		    (:maximize 2)
		    (:active-maximize 3)
		    (:close 4)
		    (:active-close 5)
		    (:top-left 6)
		    (:center 7)
		    (:top-right 8)
		    (:right 9)
		    (:left 10)
		    (:bottom-right 11)
		    (:bottom 12)
		    (:bottom-left 13))))
  (defun get-pixmap-width (pixmap-key)
    (let ((pixmap (get-pixmap pixmap-key)))
      (if pixmap (xlib:drawable-width pixmap) 0)))
  (defun get-pixmap-height (pixmap-key)
    (let ((pixmap (get-pixmap pixmap-key)))
      (if pixmap (xlib:drawable-height pixmap) 0)))
  (defun create-button-pixmaps (theme window)
    (setf theme (ensure-theme-directory-exists theme))
    (ppm:initialize (xlib:window-colormap window))
    (loop for name in '("iconify_normal" "iconify_active"
			"maximize_normal" "maximize_active"
			"close_normal" "close_active"
			"top_left" "center" "top_right" "right" "left"
			"bottom_right" "bottom" "bottom_left")
	  for index from 0
	  for full-name = (concatenate 'string theme name ".pnm")
	  when (probe-file full-name) do
	    (let ((image (ppm:load-ppm-into-clx-image full-name window)))
	      (setf (aref pixmaps index) (xlib:image-pixmap window image))))))
		
(defun put-on-top (window)
  (let ((object (gethash window *object-table*)))
    (when (application-p object)
      (unless (eq *focus-type* :none)
	(set-focus (application-input-model object) window nil))
      (when (application-master object)
	(setf window (object-window (application-master object)))))
    (setf (xlib:window-priority window) :above)))

(defun raise-window (window vs)
  (lambda (&rest)
    (case (first (wm-state window))
      (1 (unless (eq (vs:current-screen vs) (gnome:win-workspace window))
	   (change-vscreen vs nil (gnome:win-workspace window))))
      (3 (with-slots (icon) (gethash window *object-table*)
	   (uniconify icon))))
    (put-on-top window)))

(defun procede-decoration (window vscreens)
  (let ((application (create-application window nil))
	(win-workspace (or (gnome-desktop-num window) +any-desktop+))
	(stick-p (stick-p window)))
    (xlib:add-to-save-set window)
    (unless (or stick-p (< -1 win-workspace *nb-vscreen*))
      (setf win-workspace (vs:current-screen vscreens)))
    (setf (window-desktop-num window) win-workspace)
    (cond ((eql (motif-wm-decoration window) :OFF)
	   (setf (wm-state window) 1)
	   (if (or (= win-workspace (vs:current-screen vscreens)) stick-p)
	       (xlib:map-window window)
	       (with-event-mask (*root-window*)
		 (xlib:unmap-window window))))
	  ((or (= win-workspace (vs:current-screen vscreens)) stick-p)
	   (create-decoration window application))
	  (t (with-event-mask (*root-window*)
	       (create-decoration window application :map nil)
	       (update-lists application 1 *root*))))
    (if (member :_net_wm_window_type_desktop (gnome:net-wm-window-type window))
	(let* ((prec-desk (get-root-desktop *root* t))
	       (stack-mode (if prec-desk :above :below)))
	  (add-desktop-application *root* application)
	  (xlib:ungrab-button window :any)
	  (setf (xlib:window-priority window prec-desk) stack-mode)))))

(defun message-box (window &rest message)
  (setf message (apply #'concatenate 'string message))
  (multiple-value-bind (width height)
      (xlib:text-extents (xlib:gcontext-font *gcontext*) message)
    (let ((message-box
	      (create-button
	          'box-button
		  :parent window :event-mask '(:exposure)
		  :x (round (- (xlib:drawable-width window) (+ width 40)) 2)
		  :y (round (- (xlib:drawable-height window) (+ height 40)) 2)
		  :width (+ 40 width) :height (+ 40 height)
		  :border 1 :background *white*
		  :item message :gcontext *gcontext*)))
      (setf (xlib:window-override-redirect (object-window message-box)) :on)
      (xlib:map-window (object-window message-box))
      (pt:arm-timer 2 (lambda ()
			(with-slots (window) message-box
			  (xlib:display-finish-output *display*)
			  (remhash window *object-table*)
			  (xlib:destroy-window window)))))))

(defun make-desk-entries (vscreens index)
  (loop for win in (aref (vs:vscreens vscreens) index)
	for state = (= 1 (first (wm-state win)))
	collect (cons (format nil "~:[[ ~A ]~;~A~]" state (wm-name win))
		      (raise-window win vscreens))))

(defun make-running-menu (vscreens)
  (loop with names = (workspace-names *root-window*)
	for i from 0 below (vs:number-of-virtual-screen vscreens)
	for name = (or (pop names) (format nil "workspace ~A" i))
	collect (cons name (make-desk-entries vscreens i)) into entries
	finally (return (apply #'make-pop-up *root* entries))))

(defun initialize-root (display root-window)
  (flet ((handle-redirect-error (condition)
	   (declare (ignorable condition))
	   (format *error-output* "Redirect error - another WM is running~%")
	   (xlib:close-display display)
	   (%quit%)))
    (handler-bind ((error #'handle-redirect-error)) ; xlib:access-error
      (setf (xlib:window-event-mask root-window)
	    '(:substructure-redirect :button-press :button-release :focus-change
	      :key-release :substructure-notify :owner-grab-button :key-press))
      (xlib:display-finish-output display))))

(defun initialize (display-specification)
  (multiple-value-bind (display screen)
      (open-clx-display display-specification)
    (let ((colormap (xlib:screen-default-colormap screen))
	  (root-window (xlib:screen-root screen)))
      (initialize-root display root-window)
      (setf *display* display)
      ;; Specific for X display
      (setf (xlib:display-error-handler display) #'default-handler
	    (xlib:display-after-function display) #'xlib:display-force-output)
      (setf *root* (make-instance 'root :window root-window)
	    *root-window* root-window)
      ;; init all gnome properties on root.
      (init-gnome-compliance display)
      (keyboard:init-keyboard display)
      (unless (load (home-subdirectory ".eclipse") :if-does-not-exist nil)
	(format *error-output* "No file: $HOME/.eclipse~%")
	(unless (load (eclipse-path ".eclipse") :if-does-not-exist nil)
	  (format *error-output* "No file: ~A~%" (eclipse-path ".eclipse"))))
      ;; Eclipse globals vars.
      (setf *font-color* (xlib:alloc-color colormap *font-color*)
	    *black* (xlib:screen-black-pixel screen)
	    *white* (xlib:screen-white-pixel screen)
	    *background1* (xlib:alloc-color colormap *background1*)
	    *background2* (xlib:alloc-color colormap *background2*)
	    *default-root-cursor* (get-x-cursor *display* *default-cursor*)
	    *title-bar-cursor* *default-root-cursor*
	    *cursor-2* (get-x-cursor *display* :xc_fleur))
      (setf (xlib:window-cursor root-window) *default-root-cursor*)
      (init-edges-cursors)
      (init-buttons display root-window))))

(defun init-gnome-compliance (display)
  (gnome:intern-gnome-atom display)
  (let ((win (xlib:create-window :parent *root-window*
				 :override-redirect :on
				 :width 1 :height 1 :x -5 :y -5))
	(first-desknum (or (gnome:net-current-desktop *root-window*)
			   (gnome:win-workspace *root-window*) 0)))
    (delete-root-properties)
    (unless (< -1 first-desknum *nb-vscreen*) (setf first-desknum 0))
    (setf (vs:current-screen (root-vscreens *root*)) first-desknum
	  (gnome:win-protocols *root-window*) +gnome-protocols+
	  (gnome:win-supporting-wm-check win) win
	  (gnome:win-supporting-wm-check *root-window*) win
	  (gnome:win-workspace-count *root-window*) *nb-vscreen*
	  (gnome:win-workspace *root-window*) first-desknum

	  (gnome:net-supported *root-window*) +extended-gnome-protocol+
	  (gnome:net-supporting-wm-check *root-window*) win
	  (gnome:net-supporting-wm-check win) win
	  (gnome:net-number-of-desktops *root-window*) *nb-vscreen*
	  (gnome:net-current-desktop *root-window*) first-desknum
	  (gnome:net-desktop-viewport *root-window*) (make-view-port-property)
	  (gnome:net-desktop-geometry *root-window*)
	  (list (screen-width) (screen-height))
	  )))

(defun init-buttons (display root-window)
  (create-button-pixmaps *theme* root-window)
  (setf *margin* (get-pixmap-width :left)
	top-height (get-pixmap-height :center)
	bottom-height (get-pixmap-height :bottom)
	*button-width* (get-pixmap-width :close)
	*button-height* (get-pixmap-height :close)
	*gctxt* (xlib:create-gcontext :drawable root-window
				      :font (xlib:open-font display "fixed"))
	*max-char-width* (xlib:max-char-width (xlib:gcontext-font *gctxt*))
	*gcontext* (xlib:create-gcontext
		       :drawable root-window
		       :foreground *white*
		       :background *black*
		       :fill-style :solid
		       :line-style :solid
		       :line-width 1
		       :tile (get-pixmap :center)
		       :font (xlib:gcontext-font *gctxt*))))

(defun eclipse (&optional display-specification)
  (initialize display-specification)
  ;(init-log-file)

  ;; Create a socket connection to communicate with the window manager.
  ;; Works only for CMUCL -x86- (unless you compile this mp package).
  #+:mp (progn
	  (setf mp::*idle-process* mp::*initial-process*)
	  (mp::start-lisp-connection-listener :port 6789 :password "clara"))

  (unwind-protect
    (catch 'end
      (handler-bind ((end-of-file #'handle-end-of-file-condition))
	(eclipse-internal-loop)
	(xlib:close-display *display*)))
    (format t "Eclipse exited. Bye.~%")
    (%QUIT%)
    ))
#|
;; should be part of the handler-bind form ; use it for "halting" the WM.
    (loop for val being each hash-value in *object-table*
	  when (application-p val) do
	  (destroy-application (object-window val)))
|#

(defun eclipse-internal-loop ()
  (let ((exit 0)
	(time))

    ;; Sets the root window pop-up menu
    (nconc *menu-1-items* (acons "Exit" (lambda (&rest) (setf exit 1)) '()))
    (with-slots (menu1 menu3) *root*
      (setf menu1 (apply #'make-pop-up *root* *menu-1-items*)
	    menu3 (make-pop-up *root*
			       (cons "Move" (define-menu-3 :move))
			       (cons "Resize" (define-menu-3 :resize))
			       (cons "Close" (define-menu-3 :close))
			       (cons "Kill" (define-menu-3 :kill)))))

    ;; Queue events for dressing windows already displayed at start time.
    (flet ((ignorable-window-p (window)
	     (let ((hints (xlib:get-property window :WM_HINTS)))
	       (or
		(eql (xlib:window-override-redirect window) :ON)
		(eql (xlib:window-map-state window) :UNMAPPED)
		(and (= 0 (or (car (wm-state window)) 1))
		     ;; state = WITHDRAWN but the window is mapped
		     (progn (xlib:unmap-window window) t))
		(and hints (logbitp 1 (car hints)) (= 0 (third hints)))))))

      (xlib:with-event-queue (*display*)
	(mapc #'(lambda (window)
		  (unless (ignorable-window-p window)
		    (xlib:queue-event *display*
				      :map-request
				      :event-window *root-window*
				      :window window)))
	      (xlib:query-tree *root-window*))))

    ;; Main loop
    (loop
      (catch 'general-error
	(handler-bind ((end-of-file #'handle-end-of-file-condition)
		       (error #'handle-general-error-condition))
	  (let ((event (get-next-event *display* :discard-p t :timeout 2)))
	    (when event
	      (with-slots (event-window) event
		(event-process event (gethash event-window *object-table*)))))
	  (when pt:preprogrammed-tasks (pt:execute-preprogrammed-tasks))
	  (case exit
	    (1 (loop for val being each hash-value in *object-table*
		     do (close-window val))
	       (setf time 10 exit 2))
	    (2 (return))))))
    (format t "Main loop exited~%")))

;; This is for updating :
;;  - the root-properties win_client_list, net_client_list(_stacking).
;;  - the virtual screens contents
(defun update-lists (app state root)
  (macrolet ((skip (window property-getter key)
	       `(cond ((= state 0) :remove)
		      ((member ,key (,property-getter ,window)) :remove)
		      (t :append))))
    (with-slots (window iconic-p) app
      (when (and (= state 3) (not iconic-p)) (setf state 0))
      (let ((mode (skip window gnome:net-wm-state :_NET_WM_STATE_SKIP_TASKBAR))
	    (wcl-mode (skip window gnome:win-hints :WIN_HINTS_SKIP_WINLIST))
	    (n (application-desktop-number app)))
	(unless (eq mode :remove)
	  (setf mode (skip window gnome:win-hints :WIN_HINTS_SKIP_TASKBAR)))
	(if (eq mode :remove)
	    (vs:remove-from-all (root-vscreens root) window)
	    (add-to-vscreen (root-vscreens root) window :n n))
	(with-slots (client-list) root
	  (unless (and (eql wcl-mode :append) (gethash window client-list))
	    (setf (gnome:net-client-list *root-window* :mode wcl-mode) window
		  (gnome:win-client-list *root-window* :mode wcl-mode) window))
	  (if (= state 0) 
	      (remhash window client-list)
	      (setf (gethash window client-list) window))
	  (loop with root = *root-window*
		for win in (query-application-tree)
		when (gethash win client-list) collect win into wins
		finally	(setf (gnome:net-client-list-stacking root) wins)))))))

;;;; Methods for traiting events.

; Most general ones

(defmethod event-process (event object)
  (declare (ignorable event object))
  (values))

(defmethod event-process ((event configure-request) (obj object))
  (declare (ignorable obj)) ;; should only be root
  (with-slots (window x y width height stack-mode value-mask above-sibling) event
    (xlib:with-state (window)
      (when (logbitp 0 value-mask) (setf (xlib:drawable-x window) x))
      (when (logbitp 1 value-mask) (setf (xlib:drawable-y window) y))
      (when (logbitp 2 value-mask) (setf (xlib:drawable-width window) width))
      (when (logbitp 3 value-mask) (setf (xlib:drawable-height window) height))
      (when (logbitp 6 value-mask)
	(setf (xlib:window-priority window above-sibling) stack-mode)))))

; Specialized

;;; Events for the root window

(defmethod event-process ((event map-request) (root root))
  (if (gethash (event-window event) *object-table*)
      (with-slots (icon) (gethash (event-window event) *object-table*)
	(when icon (uniconify icon))
	(xlib:map-window (event-window event)))
      (procede-decoration (event-window event) (root-vscreens root))))

(defmethod event-process ((event unmap-notify) (root root))
  (declare (ignorable root))
  (with-slots (window send-event-p) event
    (when (application-p (gethash window *object-table*))
      (if (and send-event-p (eql (xlib:window-map-state window) :unmapped))
	  ;; client withdraws its top-level (ICCCM 4.2.1)
	  (undecore-application (gethash window *object-table*) :state 0)
	  (setf (wm-state window) 3)))))

(defmethod event-process ((event destroy-notify) (root root))
  (unwind-protect
       (xlib:display-finish-output *display*)
    (let ((app (gethash (event-window event) *object-table*)))
      (when (and (application-p app) (not (application-master app)))
	(update-lists app 0 root)
	(mapc #'remove-object (list app (application-icon app)))
	(xlib:destroy-window (object-window (application-icon app)))
	(when (eql app (get-root-desktop root))
	  (remove-desktop-application root app))))))

(defmethod event-process ((event client-message) (root root))
  (with-slots (type data event-window) event
    (case type
      ((or :_WIN_WORKSPACE :_NET_CURRENT_DESKTOP)
       (with-slots (vscreens) root
	 (unless (= (vs:current-screen vscreens) (aref data 0))
	   (change-vscreen vscreens nil (aref data 0)))))
      (:_NET_NUMBER_OF_DESKTOPS (setf *nb-vscreen* (aref data 0)))
      (:_NET_CLOSE_WINDOW (close-window (gethash event-window *object-table*)))
      (:WM_PROTOCOLS
       (when (eq :wm_delete_window (id->atom-name (aref data 0)))
	 (close-window (gethash event-window *object-table*)))))))

(defmethod event-process ((event focus-in) (root root))
  (when (or (eql (event-kind event) :pointer))
    (if (eql *focus-type* :on-click)
	(with-slots (vscreens) root
          (let ((w (find-if
		       #'(lambda (w) (eql :viewable (xlib:window-map-state w)))
		       (virtual-screen:nth-vscreen vscreens))))
	    (when (gethash w *object-table*)
	      (with-slots (window input-model) (gethash w *object-table*)
		(set-focus input-model window 0)))))
        (setf (gnome:net-active-window *root-window*) :none))))

(defmethod event-process ((event keyboard-pointer-event) (root root))
  (with-slots (code state) event
    (let ((callback (gethash (cons code state) *keystrock-table*)))
      (when callback (funcall callback event)))))

(defmethod event-process ((event button-press) (root root))
  (with-slots (menu1 menu2 menu3 vscreens resize-status move-status) root
    (with-slots (code x y) event
      (when (and (eql resize-status move-status) (< 0 code 4)) ; OFF OFF 1|2|3
	(when (= 2 (event-code event))
	  (when menu2 (destroy-substructure menu2))
	  (setf menu2 (make-running-menu vscreens)))
	(realize-pop-up (case code (1 menu1) (2 menu2) (3 menu3)) x  y)))))

(defmethod event-process ((event button-release) (root root))
  (with-slots (move-status resize-status current-active-decoration
	       menu1 menu2 menu3) root
    (cond ((eql move-status :ON)
	   (finalize-move current-active-decoration event))
	  ((eql resize-status :ON)
	   (finish-resize current-active-decoration))
	  (t
	   (with-slots (code) event
	     (when (< 0 code 4)
	       (destroy-substructure (case code (1 menu1) (2 menu2) (3 menu3)))
	       (when (= code 2) (setf menu2 nil))))))))

(defmethod event-process :after ((event button-release) (root root))
  (with-slots (move-status resize-status current-active-decoration) root
    (unless (eql move-status resize-status)
      (xlib:ungrab-server *display*)
      (xlib:ungrab-pointer *display*)
      (setf (decoration-active-p current-active-decoration) nil
	    current-active-decoration nil
	    move-status :OFF
	    resize-status :OFF))))

(defmethod event-process ((event motion-notify) (root root))
  (declare (optimize (speed 3)))
  (with-slots (move-status resize-status) root
    (when (or (eql move-status :ON) (eql resize-status :ON))
      (let* ((master (slot-value root 'current-active-decoration))
	     (timestamp (event-time event))
	     (precedent-timestamp (decoration-precedent-time master)))
	(declare (type (unsigned-byte 32) timestamp precedent-timestamp))
	(if (decoration-active-p master)
	    (when (or (< timestamp precedent-timestamp)
		      (> (- timestamp precedent-timestamp) 50))
	      (setf (decoration-precedent-time master) timestamp)
	      (cond ((eql move-status :ON) (move-object master event))
		    ((eql resize-status :ON) (resize master event))))
	    (with-slots (root-x root-y) event
	      (format t "The pointer has been frozen !!~%")
	      (setf (decoration-active-p master) t)
	      (event-process
	          (make-event :button-release :root-x root-x :root-y root-y)
		  root)))))))

;;; Events for master (type: decoration)

(defmethod event-process ((event configure-notify) (master decoration))
  (flet ((get-gravity (window)
	   (let ((hint (xlib:wm-normal-hints window)))
	     (and hint (xlib:wm-size-hints-win-gravity hint)))))
    (when (application-p (gethash (event-window event) *object-table*))
      (with-slots (event-window window x y) event
	(multiple-value-bind (old-x old-y) (get-window-geometry window)
	  (when (eql (get-gravity window) :static)
	    (decf x *margin*) (decf y top-height))
	  (unless (= old-y top-height) (setf (xlib:drawable-y event-window) y))
	  (unless (= old-x *margin*) (setf (xlib:drawable-x event-window) x)))
	(with-event-mask (event-window)
	  (setf (window-size event-window) (compute-size-decoration window))
	  (update-edges-geometry master)
	  (setf (window-position window) (values *margin* top-height))
	  (send-configuration-notify window))))))

(defmethod event-process ((event reparent-notify) (master decoration))
  (unless (xlib:window-equal (event-event-window event) (event-parent event))
    (event-process (make-event :destroy-notify) master)))

(defmethod event-process ((event unmap-notify) (master decoration))
  (with-slots (event-window window) event
    (xlib:unmap-window event-window)
    (setf (wm-state window) 3)))

(defmethod event-process ((event map-notify) (master decoration))
  (with-slots (window event-window) event
    (when (application-p (gethash window *object-table*))
      (unmap-icon-window (get-child master :icon))
      (xlib:map-window event-window)
      (setf (xlib:window-priority event-window) :above
	    (wm-state window) 1)
      (unless (stick-p window)
	(setf (window-desktop-num window) (current-desk))))))

(defmethod event-process ((event destroy-notify) (master decoration))
  (unwind-protect
       (xlib:display-finish-output *display*)
    (xlib:destroy-window (object-window master))
    (update-lists (get-child master :application) 0 *root*)
    (mapc #'remove-object (cons master (decoration-children master)))
    (xlib:destroy-window (get-child master :icon :window t))
    (dismiss-move *root*)))

(defmethod event-process ((event visibility-notify) (master decoration))
  (setf (application-unobscured-p (get-child master :application))
	(eq (event-state event) :unobscured)))

;; Focus management

(defmethod event-process ((event enter-notify) (master decoration))
  (event-process event (get-child master :application)))

(defmethod event-process ((event leave-notify) (master decoration))
  (when (and (not (eq *focus-type* :on-click)) (eq (event-mode event) :normal))
    (xlib:set-input-focus *display* :pointer-root :pointer-root)))

;; Resize
;; set of methods and functions for resizing.
(defvar *screen-shot* nil)

(defun create-screen-shot (window root-window)
  (multiple-value-bind (x y width height) (get-window-geometry window)
    (when *screen-shot* (xlib:destroy-window *screen-shot*))
    (setf *screen-shot* (xlib:create-window :parent root-window
					    :override-redirect :on
					    :x x :y y
					    :width width :height height
					    :event-mask 0))))

(defun draw-screen-shot (window root-window)
  (xlib:with-gcontext
      (*gcontext* :subwindow-mode :include-inferiors :exposures :OFF)
    (multiple-value-bind (x y w h) (get-window-geometry window)
      (xlib:copy-area root-window *gcontext* x y w h *screen-shot* 0 0))
    (xlib:map-window *screen-shot*)))

(defun destroy-screen-shot () (xlib:unmap-window *screen-shot*))

(defun draw-window-grid (window gctxt dest-window)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (x y width height) (get-window-geometry window)
    (declare (type (signed-byte 16) x y)
	     (type (unsigned-byte 16) width height))
    (xlib:with-gcontext (gctxt :function 8 :subwindow-mode :include-inferiors)
      (xlib:draw-rectangle dest-window gctxt x y width height)
      (let ((w (round width 3))
	    (h (round height 3)))
	(declare (type (unsigned-byte 16) w h))
	(xlib:draw-segments
	    dest-window gctxt
	    (list x (+ h y) (+ x width) (+ h y)
		  x (+ y (* 2 h)) (+ x width) (+ y (* 2 h))
		  (+ x w) y (+ w x) (+ y height)
		  (+ x (* 2 w)) y (+ x (* 2 w)) (+ y height)))))))

(defun where-is-pointer (pointer-x pointer-y master &optional (cp *card-point*))
  (declare (optimize (speed 3) (safety 0))
	   (type xlib:int16 pointer-x pointer-y))
  (flet ((is-pointer-in-window (window-key)
	   (let ((window (get-child master window-key :window t)))
	     (multiple-value-bind (x y w h) (get-window-geometry window)
	       (declare (type xlib:int16 x y) (type xlib:card16 w h))
	       (and (<= x pointer-x (+ x w)) (<= y pointer-y (+ y h)))))))
    (cond ((or (eq cp :ne) (eq cp :nw) (eq cp :se) (eq cp :sw)) cp)
	  ((is-pointer-in-window :top-left) :nw)
	  ((is-pointer-in-window :top-right) :ne)
	  ((is-pointer-in-window :bottom-left) :sw)
	  ((is-pointer-in-window :bottom-right) :se)
	  (cp cp)
	  ((is-pointer-in-window :center) :north)
	  ((is-pointer-in-window :right) :east)
	  ((is-pointer-in-window :bottom) :south)
	  ((is-pointer-in-window :left) :west))))

(defmethod resize ((master decoration) (event motion-notify))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((master-win (object-window master))
	 (root-x (event-root-x event))
	 (root-y (event-root-y event)))
    (declare (type xlib:int16 root-x root-y))
    (multiple-value-bind (x y width height) (get-window-geometry master-win)
      (declare (type xlib:int16 x y)
	       (type xlib:card16 width height))
      (setf *card-point*
	    (where-is-pointer (abs (- x root-x)) (abs (- y root-y)) master))
      (draw-window-grid master-win *gcontext* *root-window*)
      (multiple-value-bind (tmp-width tmp-height new-x new-y)
	  (ecase *card-point*
	    (:north (values width (+ height (- y root-y)) x root-y))
	    (:ne (values (- root-x x) (+ height (- y root-y)) x root-y))
	    (:east (values (- root-x x) height x y))
	    (:se (values (- root-x x) (- root-y y) x y))
	    (:south (values width (- root-y y) x y))	
	    (:sw (values (+ width (- x root-x)) (- root-y y) root-x y))
	    (:west (values (+ width (- x root-x)) height root-x y))
	    (:nw (values (+ width (- x root-x)) (+ height (- y root-y))
			 root-x root-y)))
	(declare (type xlib:int16 new-x new-y)
		 (type xlib:card16 tmp-width tmp-height))
	(multiple-value-bind (new-width new-height)
	    (multiple-value-bind (minw minh maxw maxh incw inch)
		 (decoration-wm-hints master)
	      (declare (type xlib:card16 minw minh maxw maxh incw inch))
	      (macrolet ((check (s min max inc)
			   `(min (max (- ,s (mod (- ,s ,min) ,inc)) ,min) ,max)))
		(values (check tmp-width minw maxw incw)
			(check tmp-height minh maxh inch))))
	  (declare (type xlib:card16 new-width new-height))
	  (case *card-point*
	    ((or :north :ne) (incf new-y (- tmp-height new-height)))
	    ((or :west :sw) (incf new-x (- tmp-width new-width)))
	    (:nw (incf new-y (- tmp-height new-height))
		 (incf new-x (- tmp-width new-width))))
	  (xlib:with-state (master-win)
	    (unless (= width new-width)
	      (setf (xlib:drawable-width master-win) new-width
		    (xlib:drawable-x master-win) new-x))
	    (unless (= height new-height)
	      (setf (xlib:drawable-height master-win) new-height
		    (xlib:drawable-y master-win) new-y))))))
    (draw-window-grid master-win *gcontext* *root-window*)))

;; Finish the resize process :
;; call when button-release on root
;; and when root-resize-status is :ON
(defun finish-resize (master)
  (with-slots (window gcontext) master
    (when (decoration-active-p master)
      (draw-window-grid window gcontext *root-window*)
      (destroy-screen-shot)
      (with-event-mask (window) (xlib:map-subwindows window))
      (xlib:map-window window)
      (multiple-value-bind (x y w h) (get-window-geometry window)
	(declare (ignorable x y))
	(setf (window-size (get-child master :application :window t))
	      (values (- w (* 2 *margin*)) (- h top-height bottom-height))))))
  (setf *card-point* nil))

;;
;; The effective event processing.

;; Initialize the resize process.
(defmethod event-process ((event button-press) (master decoration))
  (setf (xlib:window-priority (object-window master)) :above
	(decoration-active-p master) t
	*card-point* (where-is-pointer (event-x event) (event-y event) master))
  (create-screen-shot (object-window master) *root-window*))

;; Activate the resize process. (finally give the hand to root)
(defmethod event-process ((event motion-notify) (master decoration))
  (when (and (decoration-active-p master) *card-point*
	     (eql (root-resize-status *root*) :OFF))
    (with-slots (window gcontext) master
      (draw-screen-shot window *root-window*)
      (with-event-mask (window) (xlib:unmap-subwindows window))
      (xlib:unmap-window window)
      (grab-root-pointer)
      (xlib:grab-server *display*)
      (setf (root-resize-status *root*) :ON
	    (slot-value *root* 'current-active-decoration) master)
      (draw-window-grid window gcontext *root-window*))))

(defmethod event-process ((event button-release) (master decoration))
  (if (decoration-active-p master)
      (destroy-screen-shot)
      (event-process event *root*))
  (setf (decoration-active-p master) nil))

;;; Events for an application
(defmethod event-process ((event visibility-notify) (application application))
  (unless (application-master application)
    (setf (application-unobscured-p application)
	  (eq (event-state event) :unobscured))))

(defmethod event-process ((event enter-notify) (application application))
  (with-slots (window input-model) application
    (unless (eq *focus-type* :on-click)
      (set-focus input-model window (event-time event)))))

(defmethod event-process ((event leave-notify) (application application))
  (with-slots (master) application
    (when (and (not master)
	       (not (eq *focus-type* :on-click))
	       (eq (event-mode event) :normal))
      (xlib:set-input-focus *display* :pointer-root :pointer-root))))

(defmethod event-process ((event button-press) (application application))
  (with-slots (window unobscured-p) application
    (xlib:allow-events *display* :replay-pointer)
    (unless unobscured-p (put-on-top window))))

(defmethod event-process ((event focus-out) (application application))
  (with-slots (master) application
    (when (and master (not (eql (event-mode event) :while-grabbed)))
      (draw-unfocused-decoration master))))

(defmethod event-process ((event focus-in) (application application))
  (with-slots (master window focus-p) application
    (unless (or (eql (event-mode event) :ungrab) focus-p)
      (when master (draw-focused-decoration master))
      (setf (gnome:net-active-window *root-window*) window))))

(defmethod event-process ((event property-notify) (app application))
  (with-slots (window master) app
    (case (event-atom event)
      (:WM_NORMAL_HINTS
       (when master
	 (setf (slot-value master 'wm-size-hints)
	       (compute-wm-normal-hints window))))
      (:WM_NAME
       (when master
	 (with-slots (window item-to-draw) (get-child master :center)
	   (setf item-to-draw (wm-name (object-window app)))
	   (xlib:queue-event *display* :exposure :window window :count 0))))
      (:WM_STATE
       (update-lists app (car (wm-state window)) *root*)))))

(defmethod event-process ((event client-message) (application application))
  (with-slots (data type) event
    (with-slots (master window) application
      (case type
	(:_WIN_STATE
	 (let* ((vs (root-vscreens *root*))
		(to-change (aref data 0))
		(mask (or (gnome:win-state window :result-type t) 0))
		(new-mask (logior (logandc1 (aref data 0) mask)
				  (logand (aref data 0) (aref data 1)))))
	   (setf (gnome:win-state window) new-mask)
	   ;; win_state_sticky
	   (when (logbitp 0 to-change)
	     (cond ((and (logbitp 0 mask) (not (logbitp 0 new-mask)))
		    (setf (window-desktop-num window) (current-desk))
		    (vs:remove-from-all vs window :except (current-desk)))
		   ((logbitp 0 new-mask)
		    (setf (window-desktop-num window) +any-desktop+)
		    (add-to-vscreen vs window :n +any-desktop+)
		    (xlib:map-window window))))
	   (when master
	     ;; win_state_maximized_vert
	     (when (logbitp 2 to-change) (maximize-window master 2))
	     ;; win_state_maximized_horiz
	     (when (logbitp 3 to-change) (maximize-window master 3)))))
	(:_NET_WM_STATE
         (let ((prop (gnome:net-wm-state window))
	       (prop1 (id->atom-name (aref data 1)))
	       (prop2 (unless (zerop (aref data 2))
			(id->atom-name (aref data 2)))))
	   (case (aref data 0)
	     (0 (setf prop (remove prop1 prop))
		(when prop2 (setf prop (remove prop2 prop))))
	     (1 (push prop1 prop)
		(when prop2 (push prop2 prop))))
	   (setf (gnome:net-wm-state window) prop)
	   (when master
	     (when (or (eql prop1 :_net_wm_state_maximized_vert)
		       (eql prop2 :_net_wm_state_maximized_vert))
	       (maximize-window master 2))
	     (when (or (eql prop1 :_net_wm_state_maximized_horz)
		       (eql prop2 :_net_wm_state_maximized_horz))
	       (maximize-window master 3)))))
	(:_NET_WM_DESKTOP
	 (let* ((cur-desk (gnome:win-workspace window))
		(new-desk (aref data 0))
		(master-window (and master (object-window master))))
	   (unless (= cur-desk new-desk)
	     (setf (window-desktop-num window) new-desk)
	     (remove-from-vscreen (root-vscreens *root*) window :n cur-desk)
	     (add-to-vscreen (root-vscreens *root*) window :n new-desk)
	     (if (or (= new-desk +any-desktop+) (= new-desk (current-desk)))
		 (xlib:map-window window)
		 (with-event-mask (*root-window*)
		   (xlib:unmap-window (or master-window window)))))))
	(:WM_CHANGE_STATE
	 (when (= 3 (aref data 0)) (iconify application)))))))

;;; Events for buttons

(defmethod event-process ((event exposure) (button button))
  (when (and (button-item-to-draw button) (zerop (event-count event)))
    (with-slots (window gcontext item-to-draw master) button
      (xlib:clear-area window)
      (typecase item-to-draw
	(string (draw-centered-text window gcontext item-to-draw))
	(xlib:pixmap (when (focused-p master) (draw-on-focus-in button)))))))

(defmethod event-process ((event button-release) (close close-button))
  (close-window (get-child (button-master close) :application)))

(defmethod event-process ((event button-release) (icon-b iconify-button))
  (iconify (get-child (button-master icon-b) :application)))

;; Maximization

(defun maximize-window (master button-code)
  (with-slots (maximized window initial-geometry wm-size-hints) master
    (let ((app-window (get-child master :application :window t))
	  (max-w (aref wm-size-hints 2))
	  (max-h (aref wm-size-hints 3))
	  new-sizes)
      (multiple-value-bind (x y) (get-window-geometry window)
	(multiple-value-bind (xx yy w h) (get-window-geometry app-window)
	  (declare (ignorable xx yy))
	  (unless maximized (setf (initial-geometry master) (values x y w h)))
	  (case button-code
	    ;; Unmaximize or Maximize in both directions
	    (1 (if maximized
		   (setf new-sizes initial-geometry
			 maximized nil)
		   (setf maximized (list :vertical t :horizontal t)
			 new-sizes (vector 0 0 max-w max-h))))
	    ;; Unmaximize or Maximize Vertically
	    (2 (if (remf maximized :vertical)
		   (setf new-sizes (vector x (initial-y master)
					   w (initial-height master)))
		   (setf (getf maximized :vertical) t
			 new-sizes (vector x 0 w max-h))))
	    ;; Unmaximize or Maximize Horizontally
	    (3 (if (remf maximized :horizontal)
		   (setf new-sizes (vector (initial-x master) y
					   (initial-width master) h))
		   (setf (getf maximized :horizontal) t
			 new-sizes (vector 0 y max-w h)))))))
      (setf (window-position window)
	    (values (aref new-sizes 0) (aref new-sizes 1)))
      (setf (window-size app-window)
	    (values (aref new-sizes 2) (aref new-sizes 3))))))

(defmethod event-process ((event button-release) (max-b maximize-button))
  (maximize-window (button-master max-b) (event-code event)))

;; Move (happen after a click on the title bar)
;; set of methods and functions for moving.

(defun send-configuration-notify (window)
  (multiple-value-bind (x y)
      (xlib:translate-coordinates window 0 0 (xlib:drawable-root window))
    (xlib:send-event window
		     :configure-notify
		     (xlib:make-event-mask :structure-notify)
		     :event-window window :window window
		     :x x :y y
		     :border-width (xlib:drawable-border-width window)
		     :width (xlib:drawable-width window)
		     :height (xlib:drawable-height window))))

(defmethod initialize-move ((object object) (event button-press))
  (with-slots (window active-p) object
    (setf active-p  t
	  *delta-x* (- (event-root-x event) (xlib:drawable-x window))
	  *delta-y* (- (event-root-y event) (xlib:drawable-y window)))))

(defmethod initialize-move :after ((master decoration) (event button-press))
  (let ((app-window (get-child master :application :window t)))
    (when (or (member :WIN_STATE_FIXED_POSITION (gnome:win-state app-window))
	      (member :_NET_WM_STATE_STICKY (gnome:net-wm-state app-window)))
      (setf (decoration-active-p master) nil))))

(defun move-object (object event)
  (with-slots (window active-p gcontext) object
    (when active-p
      (if (and (decoration-p object) (eql *move-type* :box))
	  (progn
	    (draw-window-grid *screen-shot* gcontext *root-window*)
	    (move-window *screen-shot* event)
	    (draw-window-grid *screen-shot* gcontext *root-window*))
	  (move-window window event)))))

(defun move-window (window event)
  (setf (window-position window) (values (- (event-root-x event) *delta-x*)
					 (- (event-root-y event) *delta-y*))))

(defun finalize-move (master event)
  (with-slots (window gcontext active-p) master
    (when (eql *move-type* :box)
      (draw-window-grid *screen-shot* gcontext *root-window*)
      (move-window window event)
      (destroy-screen-shot))
    (setf active-p nil))
  (with-slots (armed active-p) (get-child master :center)
    (setf armed nil active-p nil))
  (send-configuration-notify (get-child master :application :window t)))

;;
;; The effective event processing.

(defmethod event-process ((event button-press) (title title-bar))
  (with-slots (master) title
    (setf (xlib:window-priority (object-window master)) :above)
    (unless (event-send-event-p event)
      (initialize-move master event))))

(defmethod event-process ((event motion-notify) (title title-bar))
  (with-slots (master) title
    (when (decoration-active-p master)
      (with-slots (move-status current-active-decoration) *root*
        (setf move-status :ON
	      current-active-decoration master)
	(grab-root-pointer))
      (with-slots (window gcontext) master
        (when (eql *move-type* :box)
	  (xlib:grab-server *display*)
	  (create-screen-shot window *root-window*)
	  (draw-window-grid window gcontext *root-window*))))))

(defmethod event-process ((event button-release) (title title-bar))
  (with-slots (master) title
    (setf (decoration-active-p master) nil)
    (when (eq *focus-type* :on-click)
      (with-slots (input-model window) (get-child master :application)
	(set-focus input-model window (event-time event))))))

;;; Events for an icon

(defmethod event-process ((event button-press) (icon icon))
  (setf (icon-desiconify-p icon) t
	(xlib:window-priority (object-window icon)) :above)
  (initialize-move icon event))

(defmethod event-process ((event motion-notify) (icon icon))
  (move-object icon event)
  (setf (icon-desiconify-p icon) nil))

(defmethod event-process ((event button-release) (icon icon))
  (when (icon-desiconify-p icon)
    (uniconify icon)))

;;;; End of event processing.
;; Next is methods for menu-3 who permit to manage any window :
;;  choose an action in the menu and click on a window
;;  to perform this action.

(defmethod menu-3-handle-event (event object &rest rest)
  (declare (ignorable event object rest)))

(defmethod menu-3-handle-event ((event button-press) (object object) &rest)
  (xlib:ungrab-pointer *display*))

(defmethod menu-3-handle-event ((event exposure) (object object) &rest)
  (event-process event object)
  nil)

(defmethod menu-3-handle-event ((ev motion-notify) (master decoration) &key key)
  (when (decoration-active-p master)
    (xlib:ungrab-pointer *display*)
    (cond ((eql key :resize)
	   (setf *card-point* :SE)
	   (event-process ev master))
	  ((eql key :move) (event-process ev (get-child master :center))))
    t))

(defmethod menu-3-handle-event ((event enter-notify) (master decoration) &rest)
  (with-slots (window) master
    (xlib:grab-pointer window +pointer-event-mask+ :cursor *cursor-2*))
  nil)

(defmethod menu-3-handle-event ((event leave-notify) (master decoration) &rest)
  (xlib:ungrab-pointer *display*)
  nil)

(defmethod menu-3-handle-event ((ev button-press) (master decoration) &key key)
  (case key
    (:kill (destroy-application (get-child master :application :window t)))
    (:close (close-window (get-child master :application)))
    (:resize (event-process ev master))
    (:move (event-process ev (get-child master :center))))
  (when (or (eq key :close) (eq key :kill)) (xlib:ungrab-pointer *display*)))

(defun define-menu-3 (action)
  (lambda (&rest)
    (with-root-cursor (*cursor-2*)
      (loop for event = (get-next-event *display* :force-output-p t)
	    for object = (gethash (event-event-window event) *object-table*)
	    until (menu-3-handle-event event object :key action)))))
