;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: widgets.lisp,v 1.2 2002/12/18 10:50:49 hatchond Exp $
;;;
;;; ECLIPSE. The Common Lisp Window Manager.
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(in-package :ECLIPSE-INTERNALS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General comment

;;;; We use the term "master" to designate the container of the decoration
;;;; Every were you'll find master it will represent the decoration object
;;;; and each time you'll see master-win, it represent the window of the
;;;; master object. (defined in the file wm.lisp)

;;;; To represent the virtual-screens we use the term VSCREEN...
;;;; It will consist in an array of list call vscreens in which each list
;;;; (a vscreen) will represent the virtual screen contents.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Eclipse Virtual screens

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
	     (netwm:net-number-of-desktops *root-window*) ,n
	     (netwm:net-desktop-viewport *root-window*)
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
  (loop for window across (vs:nth-vscreen vscreens n)
	for widget = (lookup-widget window)
	for i from 0
	when (and widget (application-master widget)) do
	  (setf widget (application-master widget))
	when (and widget (= 1 (first (ignore-errors (wm-state window))))) do
	  (funcall fun (ignore-errors (widget-window widget)))
	unless widget do (setf (aref (vs:nth-vscreen vscreens n) i) nil)
	finally (delete nil (vs:nth-vscreen vscreens n))))

(defmethod change-vscreen ((vscreens eclipse-screens) direction &optional n)
  (let* ((current-screen (vs:current-screen vscreens))
	 (new-screen
	  (or n (mod (funcall direction current-screen 1) *nb-vscreen*))))
    (unless (= new-screen +any-desktop+)
      (with-event-mask (*root-window*)
        ;; Revert the focus to the root-window.
	(xlib:set-input-focus *display* :pointer-root :pointer-root)
	(map-or-unmap-screen vscreens)
	(map-or-unmap-screen vscreens :fun #'xlib:map-window :n new-screen))
      (setf (vs:current-screen vscreens) new-screen
	    (gnome:win-workspace *root-window*) new-screen
	    (netwm:net-current-desktop *root-window*) new-screen)
      (when (eq *focus-type* :on-click)
	(ignore-errors (give-focus-to-next-widget-in-desktop *root*)))
      (when *change-desktop-message-active-p*
	(timed-message-box *root-window*
		     (or (nth new-screen (workspace-names *root-window*))
			 (format nil "WORKSPACE ~a" new-screen)))))))

(defun get-visible-windows (vscreens)
  (loop for window across (vs:nth-vscreen vscreens)
	when (= 1 (or (car (wm-state window)) 0))
	collect (with-slots (master) (lookup-widget window)
		  (if master (widget-window master) window))))

(defmethod circulate-window ((vscreens eclipse-screens) &key direction)
  (loop	initially (unless (> (length (vs:nth-vscreen vscreens)) 1)
		    (setf direction :above))
	with windows = (xlib:query-tree *root-window*)
	with screen-windows = (get-visible-windows vscreens)
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
	    (when *wrap-pointer-when-cycle* (xlib:warp-pointer window 8 5))
	    (and (eq *focus-type* :on-click) *focus-when-window-cycle*
		 (focus-widget (lookup-widget window) 0)))))

(defun give-focus-to-next-widget-in-desktop (root-widget)
  (loop with foo = nil
	for window across (vs:nth-vscreen (root-vscreens root-widget))
	for widget = (lookup-widget window)
	when (and widget (eq :viewable (xlib:window-map-state window)))
	do (with-slots (input-model) widget
	     (unless (eq input-model :no-input)
	       (set-focus input-model window 0)
	       (setf foo t)
	       (loop-finish)))
	finally 
	 (or foo (xlib:set-input-focus *display* :pointer-root :pointer-root))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Base Widget

(defclass base-widget ()
  ((window :initarg :window :reader widget-window)
   (gcontext :initarg :gcontext :reader widget-gcontext :allocation :class)))

(defmethod initialize-instance :after ((widget base-widget) &rest rest)
  (declare (ignore rest))
  (setf (gethash (widget-window widget) *widget-table*) widget))

(defgeneric remove-widget (widget))
(defgeneric close-widget (widget))
(defgeneric focus-widget (widget timestamp))
(defgeneric draw-on-focus-in (widget))
(defgeneric draw-on-focus-out (widget))

(macrolet ((define-null-method (name &rest args)
	     (let ((%name% (with-standard-io-syntax (format nil "~A" name))))
	       `(defmethod ,(intern %name%) ,args
		  (declare (ignorable ,@args))
		  (values)))))
  (define-null-method remove-widget widget)
  (define-null-method close-widget widget)
  (define-null-method focus-widget widget timestamp)
  (define-null-method draw-on-focus-in widget)
  (define-null-method draw-on-focus-out widget))

(defmethod remove-widget ((widget base-widget))
  (remhash (widget-window widget) *widget-table*))

(defun base-widget-p (widget)
  (typep widget 'base-widget))

(defun lookup-widget (window)
  (declare (optimize (speed 3) (safety 1)))
  (gethash window *widget-table*))

(declaim (inline lookup-widget))

;;;; The ROOT

(defclass root (base-widget)
  ((resize-status :initform nil :accessor root-resize-status)
   (move-status :initform nil :accessor root-move-status)
   (default-cursor :initform nil :accessor root-default-cursor)
   (current-active-decoration :initform nil)
   (decoration-theme :initform nil :accessor root-decoration-theme)
   (menu1 :initform nil)
   (menu2 :initform nil)
   (menu3 :initform nil)
   (window-menu :initform nil)
   (client-list :initform (make-hash-table))
   (desktop :initform nil :accessor root-desktop)
   (vscreens :initform (create-eclipse-virtual-screens) :reader root-vscreens)))

(defmethod get-root-desktop ((root root) &optional window-p)
  (with-slots (desktop) root
    (when (first desktop)
      (if window-p (widget-window (first desktop)) (first desktop)))))
(defmethod add-desktop-application ((root root) (desktop base-widget))
  (push desktop (root-desktop root)))
(defmethod remove-desktop-application ((root root) (desktop base-widget))
  (setf (root-desktop root) (delete desktop (root-desktop root))))

(defun dismiss-move (root)
  (with-slots (move-status current-active-decoration) root
    (when (and (eql *move-mode* :opaque) move-status)
      (setf move-status nil
	    current-active-decoration nil)
      (xlib:ungrab-pointer *display*))))

;;;; Application

(defconstant +unfocusable-mask+
  '(:property-change :enter-window :visibility-change))
(defconstant +focusable-mask+ '(:focus-change . #.+unfocusable-mask+))

(defclass application (base-widget)
  ((master :initarg :master :reader application-master)
   (desktop-number :initform 0 :accessor application-desktop-number)
   (input-model :initform nil :initarg :input :reader application-input-model)
   (icon :initform nil :initarg :icon :reader application-icon)
   (iconic-p :initform nil :accessor application-iconic-p)
   (wants-focus-p :initform nil :accessor application-wants-focus-p)
   (unobscured-p :initform nil :accessor application-unobscured-p)))

(defmethod close-widget ((application application))
  (with-slots (window) application
    (if (member :wm_delete_window (xlib:wm-protocols window))
	(send-wm-protocols-client-message window :wm_delete_window)
        (kill-client-window window))))

(defmethod focus-widget ((application application) timestamp)
  (with-slots (window input-model) application
    (set-focus input-model window timestamp)))

(defun undecore-application (application &key state)
  (with-slots (window master icon) application
    (if master
	(with-slots (window frame-style) master
	  (multiple-value-bind (x y) (window-position window)
	    (incf x (style-left-margin frame-style)) 
	    (incf y (style-top-margin frame-style))
	    (xlib:reparent-window window *root-window* x y)))
        (event-process (make-event :destroy-notify :window window) *root*))
    (when state (setf (wm-state window) state))))

(defun find-input-model (window)
  (let* ((hint (ignore-errors (xlib:get-property window :WM_HINTS)))
	 (protocols (ignore-errors (xlib:wm-protocols window)))
	 (input-p (and hint (logbitp 0 (first hint)) (= 1 (second hint))))
	 (take-focus-p (member :wm_take_focus protocols)))
    (cond ((and (not input-p) (not take-focus-p)) :no-input)
	  ((and (not input-p) take-focus-p) :globally-active)
	  ((and input-p (not take-focus-p)) :passive)
	  ((and input-p take-focus-p) :locally-active))))

(defun create-application (window master)
  (let* ((input (find-input-model window))
	 (app (make-instance 
	         'application :window window :master master :input input)))
    (ignore-errors 
      (create-icon app (or master *root*))
      (grab-button window :any '(:button-press) :sync-pointer-p t)
      (setf (xlib:window-event-mask window) 
	    (if (eq input :no-input) +unfocusable-mask+ +focusable-mask+)))
    app))

(defun kill-client-window (window)
  (xlib:kill-client (xlib:drawable-display window) (xlib:window-id window)))

(defmethod focused-p ((application application))
  (xlib:window-equal (widget-window application) (xlib:input-focus *display*)))

(defun application-p (widget)
  (typep widget 'application))

;;;; Buttons

(defclass button (base-widget)
  ((master :initarg :master :reader button-master)
   (item-to-draw :initarg :item-to-draw :accessor button-item-to-draw)))

(defconstant +std-button-mask+
  '(:button-press :button-release :button-motion :owner-grab-button :exposure))

(defmethod draw-on-focus-in ((button button))
  (with-slots (master item-to-draw window gcontext) button
    (xlib:clear-area window)
    (typecase item-to-draw
      (string (draw-centered-text window gcontext item-to-draw))
      (xlib:pixmap 
        (multiple-value-bind (width height) (drawable-sizes window)
	  (xlib:with-gcontext (gcontext :tile item-to-draw :fill-style :tiled)
	    (xlib:draw-rectangle window gcontext 0 0 width height t)))))))

(defmethod draw-on-focus-out ((button button))
  (xlib:clear-area (widget-window button)))

(defun button-p (widget)
  (typep widget 'button))

;; When calling this function arguments non optional are
;; :parent :x :y :width :height
;; the others are optional.
(defun create-button (button-type &key parent x y width height gcontext
				  item background master (border-width 0)
				  (border *black*)
				  (gravity :north-west)
				  (cursor (root-default-cursor *root*))
				  (event-mask +std-button-mask+))
  (make-instance
      button-type
      :window (xlib:create-window
	          :parent parent :x x :y y
		  :width width :height height :border-width border-width
		  :background background :border border
		  :gravity gravity :bit-gravity (if item :north-west :forget)
		  :cursor cursor :event-mask event-mask)
      :gcontext gcontext
      :item-to-draw item
      :master master))

;;;; Box button
;; Use it for displaying short message in window, that do not require
;; any user intervention (no OK/Cancel confirmation).

(defclass box-button (button) ())

(defun create-message-box (messages
			   &key parent (border-width 1) (background *white*))
  (setf messages (apply #'concatenate 'string messages))
  (let ((message-box
	 (create-button
	    'box-button
	    :parent parent :event-mask '(:exposure)
	    :x 0 :y 0 :width 1 :height 1 :border-width border-width
	    :background background :item messages
	    :gcontext *gcontext*)))
    (setf (xlib:window-override-redirect (widget-window message-box)) :on
	  (button-item-to-draw message-box) messages)
    message-box))

(defmethod (setf button-item-to-draw) (message (box box-button))
  (multiple-value-bind (width height)
      (xlib:text-extents (xlib:gcontext-font *gcontext*) message)
    (incf width 40) 
    (incf height 20)
    (with-slots (window) box
      (multiple-value-bind (children parent) (xlib:query-tree window)
	(declare (ignore children))
	(let ((x (ash (- (xlib:drawable-width parent) width) -1))
	      (y (ash (- (xlib:drawable-height parent) height) -1)))
	  (setf (drawable-sizes window) (values width height)
		(window-position window) (values x y)
		(slot-value box 'item-to-draw) message))))))

;; Self destructing message box after 2 seconds.
(defun timed-message-box (window &rest messages)
  (with-slots (window) (create-message-box messages :parent window)
    (xlib:map-window window)
    (pt:arm-timer 2 (lambda ()
		      (xlib:display-finish-output *display*)
		      (remhash window *widget-table*)
		      (xlib:destroy-window window)))))

;;;; Push button
;; Every body knows what a push button is.

(defclass push-button (button)
  ((armed :initform nil :accessor button-armed)
   (active-p :initform nil :accessor button-active-p)))

(defconstant +push-button-mask+ '(:exposure . #.+pointer-event-mask+))

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

;;;; Standard decoration buttons

(defclass title-bar (push-button) 
  ((vmargin :initform 0)
   (hmargin :initform 0)
   (parent :initform nil)))
  
(defclass close-button (push-button) ())
(defclass iconify-button (push-button) ())
(defclass maximize-button (push-button) ())
(defclass menu-button (push-button) ())

;; Those are master edges and master corners
(defclass edge (button) ())

(defclass top (edge) ())
(defclass top-left (edge) ())
(defclass top-right (edge) ())
(defclass right (edge) ())
(defclass left (edge) ())
(defclass bottom (edge) ())
(defclass bottom-right (edge) ())
(defclass bottom-left (edge) ())

(defvar +corner-cursor+ nil)
(defvar +side-cursor+ nil)
(defconstant +edge-event-mask+ 
  '(:button-press :button-release :button-motion :owner-grab-button))

(defun init-edges-cursors ()
  (setf +corner-cursor+
	(list (get-x-cursor *display* :xc_top_left_corner)
	      (get-x-cursor *display* :xc_top_right_corner)
	      (get-x-cursor *display* :xc_bottom_left_corner)
	      (get-x-cursor *display* :xc_bottom_right_corner))
	+side-cursor+
	(list (get-x-cursor *display* :xc_right_side)
	      (get-x-cursor *display* :xc_left_side)
	      (get-x-cursor *display* :xc_top_side)
	      (get-x-cursor *display* :xc_bottom_side))))

;;;; Icon

(defclass icon (push-button)
  ((desiconify-p :initform nil :accessor icon-desiconify-p)
   (application :initarg :application :reader icon-application)))

(defun icon-p (widget)
  (typep widget 'icon))

(defun create-icon (application master
		    &optional (gcontext *gcontext*) (bg-color *black*))
  (with-slots (window icon) application
    (let ((background (clx-ext::wm-hints-icon-pixmap window))
	  (width 45) (height 60))
      (if (typep background 'xlib:pixmap)
	  (multiple-value-setq (width height) (drawable-sizes background))
	  (setf background nil))
      (ignore-errors
	(when (and background (= 1 (xlib:drawable-depth background)))
	  (let ((pix (xlib:create-pixmap
		        :drawable window :width width :height height
			:depth (xlib:drawable-depth window))))
	    (xlib:copy-plane background gcontext 1 0 0 width height pix 0 0)
	    (setf background pix))))
      (setf icon (create-button
		    'icon
		    :parent *root-window* :master master
		    :x 750 :y 50 :width width :height height
		    :gcontext gcontext
		    :item (unless background (wm-name window))
		    :background (or background bg-color))
	    (slot-value icon 'application) application)
      icon)))

(defmethod iconify ((application application))
  (setf (application-iconic-p application) t
	(application-wants-focus-p application) *focus-new-mapped-window*)
  (xlib:unmap-window (widget-window application))
  (xlib:map-window (widget-window (application-icon application)))
  (when (eq *focus-type* :on-click)
    (give-focus-to-next-widget-in-desktop *root*)))

(defmethod uniconify ((icon icon))
  (with-slots (application desiconify-p) icon
    (setf desiconify-p nil)
    (with-slots (window desktop-number) application
      (unless (= desktop-number (current-desk))
	(remove-from-vscreen (root-vscreens *root*) window :n desktop-number)))
    (unmap-icon-window icon)
    (xlib:map-window (widget-window application))))

(defmethod unmap-icon-window ((icon icon))
  (with-slots (window master application) icon
    (xlib:unmap-window window)
    (setf (application-iconic-p application) nil)
    (unless (decoration-p master)
      (with-slots (window) application
        (setf (window-desktop-num window)
	      (if (stick-p window) +any-desktop+ (current-desk))
	      (wm-state window) 1)))))
