;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: widgets.lisp,v 1.32 2004/01/22 22:44:24 ihatchondo Exp $
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
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Base Widget

(defclass base-widget ()
  ((window :initarg :window :reader widget-window)
   (gcontext :initarg :gcontext :reader widget-gcontext :allocation :class)))

(defgeneric remove-widget (widget)
  (:documentation "Remove widget from internal cache."))

(defgeneric close-widget (widget)
  (:documentation "Close an application according to the ICCCM protocol."))

(defgeneric focus-widget (widget timestamp))

(defgeneric focused-p (widget)
  (:documentation "Return T if the given widget has the focus."))

(defgeneric put-on-top (widget)
  (:documentation "sets the widget stacking order on top of the others."))

(defgeneric put-on-bottom (widget)
  (:documentation
   "Sets the widget stacking order on bottom of the others \(except if any
   widget with :_net_wm_type_desktop is present and widget is or an 
   application or a decoration\)."))

(defgeneric shade (widget)
  (:documentation "shade/un-shade an application that is decorated."))

(defgeneric shaded-p (widget)
  (:documentation
   "Returns true if the widget is shaded in the sens of the extended window
    manager specification."))

(defgeneric root-manager (widget)
  (:documentation
   "Returns the root-window child that is the place holder for indicating that
   a netwm manager is present."))

(defgeneric repaint (widget theme-name focus)
  (:documentation
   "This method is dedicated to widget repaint such as every buttons, icons,
   edges ...
   It is specialized on widget type, theme name (via an eql specializer) and a
   boolean that indicate if the corresponding toplevel (type decoration) is or
   not focused.

   Except for standard expose events, the repaint dispatching on focus change
   will be perform according to parts-to-redraw-on-focus list given in
   define-theme."))

(defmethod initialize-instance :after ((widget base-widget) &rest rest)
  (declare (ignore rest))
  (setf (gethash (widget-window widget) *widget-table*) widget))

(macrolet ((define-null-method (name &rest args)
	     (let ((%name% (with-standard-io-syntax (format nil "~A" name))))
	       `(defmethod ,(intern %name%) ,args
		  (declare (ignorable ,@args))
		  (values)))))
  (define-null-method remove-widget widget)
  (define-null-method close-widget widget)
  (define-null-method focus-widget widget timestamp)
  (define-null-method repaint widget theme-name focus)
  (define-null-method root-manager widget)
  (define-null-method shaded-p widget))

(defmethod remove-widget ((widget base-widget))
  (remhash (widget-window widget) *widget-table*))

(defmethod root-manager ((widget base-widget))
  (root-manager (lookup-widget (xlib:drawable-root (widget-window widget)))))

(defmethod put-on-top ((widget base-widget))
  (setf (xlib:window-priority (widget-window widget)) :above))

(defmethod put-on-bottom ((widget base-widget))
  (setf (xlib:window-priority (widget-window widget)) :above))

(defun base-widget-p (widget)
  (typep widget 'base-widget))

(defun lookup-widget (window)
  "Returns the associated widget if any."
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
   (properties-manager-window :initform nil :initarg :manager)
   (menu1 :initform nil)
   (menu2 :initform nil)
   (menu3 :initform nil)
   (window-menu :initform nil)
   (client-list :initform (make-hash-table))
   (desktop :initform nil :accessor root-desktop)
   (sm-conn :initform nil :accessor root-sm-conn)))

(defmethod root-manager ((root root))
  (slot-value root 'properties-manager-window))

(defmethod get-root-desktop ((root root) &optional window-p)
  (with-slots (desktop) root
    (when (first desktop)
      (if window-p (widget-window (first desktop)) (first desktop)))))
(defmethod add-desktop-application ((root root) (desktop base-widget))
  (push desktop (root-desktop root)))
(defmethod remove-desktop-application ((root root) (desktop base-widget))
  (setf (root-desktop root) (delete desktop (root-desktop root))))

(defmethod focus-widget ((root root) timestamp)
  (declare (ignorable timestamp))
  (xlib:set-input-focus *display* :pointer-root :pointer-root)
  (setf (netwm:net-active-window (widget-window root)) :none))

(defun dismiss-move-resize (root)
  (with-slots (resize-status move-status current-active-decoration) root
    (when (or *verbose-move* *verbose-resize*) (undraw-geometry-info-box))
    (when (or (and (eql *move-mode* :opaque) move-status)
	      (and (eql *resize-mode* :opaque) resize-status))
      (setf (values move-status resize-status current-active-decoration) nil)
      (xlib:ungrab-pointer *display*))))

(defun close-sm-connection (root &key (exit-p t))
  (with-slots (sm-conn) root
    (sm-lib:close-sm-connection sm-conn)
    (setf sm-conn nil)
    (when exit-p (error 'exit-eclipse))))

;;;; Application

(defconstant +application-mask+
  '(:property-change :enter-window :visibility-change :focus-change))

(defconstant +properties-to-delete-on-withdrawn+
  '(:_net_wm_state :_net_wm_desktop :_win_workspace))

(defclass application (base-widget)
  ((master :initarg :master :reader application-master)
   (input-model :initform nil :initarg :input :reader application-input-model)
   (icon :initform nil :initarg :icon :reader application-icon)
   (iconic-p :initform nil :accessor application-iconic-p)
   (wants-iconic-p :initform nil :accessor application-wants-iconic-p)
   (wants-focus-p :initform nil :accessor application-wants-focus-p)
   (initial-geometry :initform (make-geometry) :initarg :initial-geometry)
   (full-geometry :initform (make-geometry) :initarg :full-geometry)
   (type :initarg :type :accessor application-type)))

(defmethod remove-widget :after ((app application))
  (case (application-type app)
    (:_net_wm_window_type_desktop (remove-desktop-application *root* app))
    (:_net_wm_window_type_dock (update-workarea-property *root*))))

(defmethod close-widget ((application application))
  (with-slots (window) application
    (if (member :wm_delete_window (xlib:wm-protocols window))
	(send-wm-protocols-client-message window :wm_delete_window)
        (kill-client-window window))))

(defmethod focus-widget ((application application) timestamp)
  (with-slots (window input-model) application
    (set-focus input-model window timestamp)))

(defmethod focused-p ((application application))
  (loop with window = (widget-window application)
	with foc = (xlib:input-focus *display*)
	until (or (xlib:window-equal window foc) (not (xlib:window-p foc)))
	do (multiple-value-bind (children parent) (xlib:query-tree foc)
	     (declare (ignore children))
	     (setq foc parent))
	finally (return (xlib:window-equal window foc))))

(defmethod shaded-p ((widget application))
  (member :_net_wm_state_shaded (netwm:net-wm-state (widget-window widget))))

(defmethod shade ((application application))
  (with-slots (master) application
    (and master (shade master))))

(defmethod put-on-top ((widget application))
  (with-slots (master window input-model) widget
    (unless (or (eq *focus-type* :none) (focused-p widget))
      (set-focus input-model window nil))
    (setf (window-priority (if master (widget-window master) window)) :above)))

(defmethod put-on-bottom ((widget application))
  (with-slots (master window) widget
    (let ((desk-w (get-root-desktop *root* t)))
      (setf (window-priority (if master (widget-window master) window) desk-w)
	    (if desk-w :above :below)))))

(defun fullscreenable-p (application) 
  (with-slots (window) application
    (let ((hint (ignore-errors (xlib:wm-normal-hints window))))
      (symbol-macrolet ((max-w (xlib:wm-size-hints-max-width hint))
			(max-h (xlib:wm-size-hints-max-height hint)))
	  (and (if max-w (= max-w (screen-width)) t)
	       (if max-h (= max-h (screen-height)) t))))))

(defsetf fullscreen-mode (application) (mode)
  "Mode may be (or :on :off). Put or remove application in or from fullscreen."
  `(with-slots (window (fgeometry full-geometry) master icon) ,application
     (if (eq ,mode :on)
	 ;; put in fullscreen mode.
	 (with-event-mask (*root-window*)
	   (multiple-value-bind (x y w h) (window-geometry window)
	     (when master
	       (with-slots (children (master-win window) frame-style) master
		 (multiple-value-setq (x y) (window-position master-win))
		 (setf (slot-value master 'old-frame-style) frame-style)
		 (setf (decoration-frame-style master)
		       (theme-default-style (lookup-theme "no-decoration")))))
	     (setf (geometry fgeometry) (values x y w h))
	     (if (xlib:query-extension *display* "XFree86-VidModeExtension")
		 (let* ((scr (first (xlib:display-roots *display*)))
			(ml (xlib:xfree86-vidmode-get-mode-line *display* scr)))
		   (multiple-value-setq (x y) 
		     (xlib:xfree86-vidmode-get-viewport *display* scr))
		   (setf w (xlib:mode-info-hdisplay ml)
			 h (xlib:mode-info-vdisplay ml)))
		 (setf x 0 y 0 w (screen-width) h (screen-height)))
	     (configure-window window :x x :y y :width w :height h))
	   (focus-widget ,application 0))
	 ;; revert: restore precedent geometry and decoration style.
	 (with-event-mask (*root-window*)
	   (setf (drawable-sizes window) (geometry-sizes fgeometry))
	   (unless (window-not-decorable-p window)
	     (setf (decoration-frame-style master)
		   (slot-value master 'old-frame-style)))
	   (multiple-value-bind (x y) (geometry-coordinates fgeometry)
	     (with-slots (window) (or master ,application)
	       (configure-window window :x x :y y)))))
     ;; reset appropriately _net_wm_state property.
     (let ((prop (netwm:net-wm-state window)))
       (if (eq ,mode :on)
	   (pushnew :_net_wm_state_fullscreen prop)
	   (setf prop (delete :_net_wm_state_fullscreen prop)))
       (setf (netwm:net-wm-state window) prop))))

(defun undecore-application (application &key state)
  (with-slots (window master icon) application
    (if master
	(multiple-value-bind (x y)
	    (xlib:translate-coordinates window 0 0 *root-window*)
	  (xlib:reparent-window window *root-window* x y))
        (event-process (make-event :destroy-notify :window window) *root*))
    (when state
      (setf (wm-state window) state)
      (when (= state 0)
	(delete-properties window +properties-to-delete-on-withdrawn+)))))

(defun find-input-model (window)
  "Returns the input model keyword of this window according to ICCCM (4.1.7)."
  (let* ((hint (ignore-errors (xlib:get-property window :WM_HINTS)))
	 (protocols (ignore-errors (xlib:wm-protocols window)))
	 (input-p (and hint (logbitp 0 (first hint)) (= 1 (second hint))))
	 (take-focus-p (ignore-errors (member :wm_take_focus protocols))))
    (cond ((and (not input-p) (not take-focus-p)) :no-input)
	  ((and (not input-p) take-focus-p) :globally-active)
	  ((and input-p (not take-focus-p)) :passive)
	  ((and input-p take-focus-p) :locally-active))))

(defun make-initital-geometry (window &optional (geometry (make-geometry)))
  (multiple-value-bind (x y w h) (window-geometry window)
    (let ((hint (ignore-errors (xlib:wm-normal-hints window))))
      (setf (geometry geometry)
	    (values (or (when hint (xlib:wm-size-hints-x hint)) x)
		    (or (when hint (xlib:wm-size-hints-y hint)) y)
		    (or (when hint (xlib:wm-size-hints-width hint)) w)
		    (or (when hint (xlib:wm-size-hints-height hint)) h)))
      geometry)))

(defun create-application (window master)
  (let* ((input (find-input-model window))
	 (type (ignore-errors (netwm:net-wm-window-type window)))
	 (desktop-p (member :_net_wm_window_type_desktop type))
	 (dock-p (member :_net_wm_window_type_dock type))
	 (initial-geometry (make-initital-geometry window))
	 (app (make-instance 'application
		:window window :master master :input input :type type
		:initial-geometry initial-geometry
		:full-geometry (copy-geometry initial-geometry))))
    (ignore-errors 
      (create-icon app master)
      (if (or desktop-p dock-p)
	  (let* ((prec-desk (get-root-desktop *root* t))
		 (stack-mode (if prec-desk :above :below))
		 (netwm-state (ignore-errors (netwm:net-wm-state window))))
	    (pushnew :_net_wm_state_skip_pager netwm-state)
	    (pushnew :_net_wm_state_skip_taskbar netwm-state)
	    (when desktop-p
	      (pushnew :_net_wm_state_sticky netwm-state)
	      (add-desktop-application *root* app)
	      (setf (xlib:window-priority window prec-desk) stack-mode))
	    (setf (netwm:net-wm-state window) netwm-state
		  (window-desktop-num window) +any-desktop+))
	  (grab-button window :any '(:button-press) :sync-pointer-p t))
      (setf (xlib:window-event-mask window) +application-mask+))
    app))

(defun kill-client-window (window)
  (xlib:kill-client (xlib:drawable-display window) (xlib:window-id window)))

(defun application-p (widget)
  (typep widget 'application))

;;;; Buttons

(defclass button (base-widget)
  ((master :initarg :master :reader button-master)
   (item-to-draw :initarg :item-to-draw :accessor button-item-to-draw)))

(defconstant +std-button-mask+
  '(:button-press :button-release :button-motion :owner-grab-button :exposure))

(defmethod repaint ((widget button) theme-name (focus t))
  (declare (ignorable theme-name focus))
  (with-slots (item-to-draw window gcontext) widget
    (xlib:clear-area window)
    (typecase item-to-draw
      (string (draw-centered-text window gcontext item-to-draw))
      (xlib:pixmap (draw-pixmap window gcontext item-to-draw)))))

(defmethod repaint ((widget button) theme-name (focus null))
  (declare (ignorable theme-name focus))
  (xlib:clear-area (widget-window widget)))

(defmethod shaded-p ((widget button))
  (with-slots (window) (get-child (button-master widget) :application)
    (member :_net_wm_state_shaded (netwm:net-wm-state window))))

(defun button-p (widget)
  (typep widget 'button))

(declaim (inline draw-pixmap))
(defun draw-pixmap (window gctxt pix &key (x 0) (y 0) width height)
  "Draw, and tile if necessary, the pixmap in the given region in the window."
  (multiple-value-bind (w h) (drawable-sizes window)
    (unless width (setf width w))
    (unless height (setf height h)))
  (if (= (xlib:drawable-depth pix) 1)
      (xlib:copy-plane pix gctxt 1 0 0 width height window x y)
      (xlib:with-gcontext (gctxt :tile pix :fill-style :tiled :ts-x x :ts-y y)
	(xlib:draw-rectangle window gctxt x y width height t))))

;; When calling this function arguments non optional are
;; :parent :x :y :width :height
;; the others are optional.
(defun create-button (button-type &key parent x y width height
				  item background master (border-width 0)
				  (border *black*)
				  (gravity :north-west)
				  (cursor (root-default-cursor *root*))
				  (event-mask +std-button-mask+))
  (when (and (not (xlib:cursor-p cursor)) (keywordp cursor))
    (setf cursor (get-x-cursor *display* cursor)))
  (make-instance
      button-type
      :window (xlib:create-window
	          :parent parent :x x :y y
		  :width width :height height :border-width border-width
		  :background background :border border
		  :gravity gravity :bit-gravity (if item :north-west :forget)
		  :cursor cursor :event-mask event-mask)
      :item-to-draw item :master master))

;;;; Box button
;; Use it for displaying short message in window, that do not require
;; any user intervention (no OK/Cancel confirmation).

(defclass box-button (button)
  ((pixmap :initform nil :initarg :pixmap-to-display :accessor message-pixmap)))

(defun create-message-box (messages &key parent pixmap
			                 (border-width 1)
			                 (background *white*))
  (setf messages (apply #'concatenate 'string messages))
  (let ((message-box
	 (create-button
	    'box-button
	    :parent parent :event-mask '(:exposure :visibility-change)
	    :x 0 :y 0 :width 1 :height 1 :border-width border-width
	    :background background :item messages)))
    (setf (xlib:window-override-redirect (widget-window message-box)) :on
	  (button-item-to-draw message-box) messages
	  (message-pixmap message-box) pixmap)
    message-box))

(defmethod (setf button-item-to-draw) (m (box box-button))
  (with-slots (window (gctxt gcontext) pixmap) box
    (multiple-value-bind (width height)
	(xlib:text-extents (xlib:gcontext-font gctxt) m :translate #'translate)
      (incf width 20) (incf height 20)
      (when pixmap
	(setf height (max (+ 20 (xlib:drawable-height pixmap)) height))
	(incf width (+ 10 (xlib:drawable-width pixmap))))
      (multiple-value-bind (children parent) (xlib:query-tree window)
	(declare (ignore children))
	(let ((x (ash (- (xlib:drawable-width parent) width) -1))
	      (y (ash (- (xlib:drawable-height parent) height) -1)))
	  (setf (drawable-sizes window) (values width height)
		(window-position window) (values x y)
		(slot-value box 'item-to-draw) m))))))

(defmethod repaint ((widget box-button) theme-name focus &aux x)
  (declare (ignorable theme-name focus))
  (with-slots (window item-to-draw gcontext pixmap) widget
    (xlib:clear-area window)
    (when pixmap
      (multiple-value-bind (w h) (drawable-sizes pixmap)
	(draw-pixmap window gcontext pixmap :x 10 :y 10 :width w :height h)
	(setf x (+ w 20))))
    (draw-centered-text window gcontext item-to-draw :color *black* :x x)))

;; Self destructing message box after 2 seconds.
(defun timed-message-box (window &rest messages)
  "Map a message box, of parent `window'. Its life time is 2 seconds."
  (with-slots (window) (create-message-box messages :parent window)
    (xlib:map-window window)
    (pt:arm-timer 2 (lambda ()
		      (xlib:display-finish-output *display*)
		      (remhash window *widget-table*)
		      (xlib:destroy-window window)))))

;;;; Push button
;; Everybody knows what a push button is.

(defclass push-button (button)
  ((armed :initform nil :accessor button-armed)
   (active-p :initform nil :accessor button-active-p)))

(defconstant +push-button-mask+ '(:exposure . #.+pointer-event-mask+))

(defmethod focus-widget ((button push-button) timestamp)
  (focus-widget (button-master button) timestamp))

(defmethod event-process ((event enter-notify) (b push-button))
  (when (button-armed b)
    (setf (button-active-p b) t)))

(defmethod event-process ((event leave-notify) (b push-button))
  (when (button-armed b)
    (setf (button-active-p b) nil)))

(defmethod event-process :around ((event button-press) (b push-button))
  (setf (button-armed b) t
	(button-active-p b) t)
  (when (next-method-p)
    (call-next-method)))

(defmethod event-process :around ((event button-release) (b push-button))
  (with-slots (armed active-p) b
    (if (and armed active-p)
	(when (next-method-p) (call-next-method))
        (event-process event *root*))
    (setf armed nil
	  (button-active-p b) nil)))

(defmethod (setf button-active-p) :after (value (button push-button))
  (declare (ignorable value))
  (with-slots (window master) button
    ;; We may have irrelevant invokation of this method when it's invoked
    ;; after a close-widget has been done (such as after clicking on the 
    ;; close button of a decoration or something similar). To avoid
    ;; inconsistant X requests we'll ensure that the widget still exists.
    (when (and (decoration-p master) (lookup-widget window))
      (with-slots (name) (decoration-frame-style master)
	(repaint button name (focused-p master))))))

(defmethod repaint ((widget push-button) theme-name (focus t))
  (declare (ignorable theme-name focus))
  (with-slots (window gcontext armed active-p item-to-draw) widget
    (xlib:clear-area window)
    (let ((p (and armed active-p (push-button-pixmap widget :focused-click))))
      (when (or p item-to-draw)
	(draw-pixmap window gcontext (or p item-to-draw))))))

(defmethod repaint ((widget push-button) theme-name (focus null))
  (declare (ignorable theme-name focus))
  (with-slots (window gcontext armed active-p) widget
    (xlib:clear-area window)
    (let ((pixmap (push-button-pixmap widget :unfocused-click)))
      (when (and armed active-p pixmap)
	(draw-pixmap window gcontext pixmap)))))

(defun push-button-pixmap (pbutton pixmap-index)
  (with-slots (frame-style) (button-master pbutton)
    (aref (frame-item-pixmaps frame-style (widget->frame-item-key pbutton))
	  (case pixmap-index
	    (:focused-click 3)
	    (:unfocused-click 2)
	    (:focused-unclick 1)
	    ((and (numberp pixmap-index) (<= 0 pixmap-index 3)) pixmap-index)
	    (t 0)))))

;;;; Standard decoration buttons

(defclass title-bar (push-button) 
  ((timestamp :initform 0)
   (vmargin :initform 0)
   (hmargin :initform 0)
   (parent :initform nil)))

(defmethod repaint ((widget title-bar) theme-name focus)
  (declare (ignorable theme-name focus))
  (with-slots (item-to-draw window gcontext) widget
    (xlib:clear-area window)
    (when item-to-draw
      (draw-centered-text window gcontext item-to-draw))))

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

(defconstant +corner-cursors+ 
  '(:xc_top_left_corner :xc_top_right_corner 
    :xc_bottom_left_corner :xc_bottom_right_corner))
(defconstant +side-cursors+ 
  '(:xc_right_side :xc_left_side :xc_top_side :xc_bottom_side))
(defconstant +edge-event-mask+
  '(:button-press :button-release :button-motion :owner-grab-button))

;;;; Icon

(defclass icon (push-button)
  ((desiconify-p :initform nil :accessor icon-desiconify-p)
   (creation-time :initform (get-universal-time) :accessor icon-creation-time)
   (application :initarg :application :reader icon-application)
   (pixmap-to-free :initform nil :reader icon-pixmap-to-free)))

(defun icon-p (widget)
  (typep widget 'icon))

(defun create-icon (application master &optional (bg-color *black*))
  (with-slots (window icon gcontext) application
    (let* ((bkgrd (decode-netwm-icon-pixmap window (netwm:net-wm-icon window)))
	   (width 45) (height 20) (pixmap-to-free bkgrd))
      (unless bkgrd 
	(setf bkgrd (ignore-errors (clx-ext::wm-hints-icon-pixmap window))))
      (ignore-errors
	(if (typep bkgrd 'xlib:pixmap)
	    (multiple-value-setq (width height) (drawable-sizes bkgrd))
	    (setf bkgrd nil)))
      (ignore-errors
	(when (and bkgrd (= 1 (xlib:drawable-depth bkgrd)))
	  (let ((pix (xlib:create-pixmap
		        :drawable window :width width :height height
			:depth (xlib:drawable-depth window))))
	    (xlib:copy-plane bkgrd gcontext 1 0 0 width height pix 0 0)
	    (setf bkgrd pix))))
      (setf icon (create-button
		    'icon
		    :event-mask '(:pointer-motion-hint . #.+std-button-mask+)
		    :parent *root-window* :master master
		    :x 0 :y 0 :width width :height height
		    :item (unless bkgrd (wm-icon-name window))
		    :background (or bkgrd bg-color)))
      (setf (slot-value icon 'pixmap-to-free) pixmap-to-free)
      (setf (slot-value icon 'application) application)
      icon)))

(defun icon-sort-creation-order (icon1 icon2)
  (< (icon-creation-time icon1) (icon-creation-time icon2)))

(defun icon-sort-name (icon1 icon2)
  (string< (xlib:wm-name (widget-window (icon-application icon1)))
	   (xlib:wm-name (widget-window (icon-application icon2)))))

(defun icon-sort-type (icon1 icon2)
  (let ((c1 (application-class (icon-application icon1)))
	(c2 (application-class (icon-application icon2))))
  (or (string< (cdr c1) (cdr c2))
      (and (string= (cdr c1) (cdr c2))
	   (string< (car c1) (car c2))))))

(defun icon-box-update ()
  (unless (eq *icon-box-fill* :top-right)
    (timed-message-box (widget-window *root*)
      "Only :top-right fill is currently supported"))
  (let ((icons (stable-sort
		(loop for val being each hash-value in *widget-table*
		      when (icon-p val) collect val)
		(or *icon-box-sort-function* #'icon-sort-creation-order))))
    (flet ((absx (v) (if (< v 0) (+ (screen-width) v) v))
	   (absy (v) (if (< v 0) (+ (screen-height) v) v)))
      (let* ((box-tlx (absx (aref *icon-box* 0))) ;top left x
	     (box-tly (absy (aref *icon-box* 1)))
	     (box-brx (absx (aref *icon-box* 2))) ;bottom right y
	     (box-bry (absy (aref *icon-box* 3)))
	     (box-trx box-brx)
	     (box-try box-tly)
	     (box-blx box-tlx)
	     (box-bly box-bry)
	     (box-sizex (- box-trx box-tlx))
	     (box-sizey (- box-bry box-try))
	     (prev-icon-window nil)
	     (maxedge 0)
	     (basex 0)
	     (basey nil))
	(declare (ignore box-sizey box-sizex box-blx))
	(dolist (icon icons)
	  (let* ((icon-window (widget-window icon))
		 (icon-x (xlib:drawable-x icon-window))
		 (icon-y (xlib:drawable-y icon-window)))
	    (setq maxedge (max (xlib:drawable-width icon-window) maxedge)
		  basex (- box-trx maxedge *icon-box-sep*)
		  basey (if (not basey)
			    box-try
			    (+ basey
			       (xlib:drawable-height prev-icon-window)
			       *icon-box-sep*)))
	    (when (> basey box-bry)
	      (setq basey box-try
		    basex (- basex maxedge *icon-box-sep*)
		    maxedge (xlib:drawable-width icon-window)))
	    (when (< basex box-tlx)
	      ;;if the box overflows, we put the icon into (0,0)
	      (setq basex 0 basey 0))
	    (cond
	     ((and (= basex icon-x) (= basey icon-y)))
	     ((or (< 0 basex box-tlx) (> basex box-trx)
		  (< 0 basey box-tly) (> basey box-bly)))
	     (t
	      (xlib:with-state (icon-window)
		(setf (window-position icon-window) (values basex basey)))))
	    (setq prev-icon-window icon-window)))))))

(defsetf icon-priority (icon) (priority)
  "restack the window of the given icon according the given priority."
  (with-gensym (p sibling)
    `(let ((,p ,priority) ,sibling)
       (when (eq ,priority :below)
	 (setf ,sibling (get-root-desktop *root* t)
	       ,p (if ,sibling :above :below)))
       (setf (xlib:window-priority (widget-window ,icon) ,sibling) ,p))))

(defmethod close-widget ((widget icon))
  (close-widget (icon-application widget)))

(defmethod remove-widget :after ((widget icon))
  (with-slots (pixmap-to-free) widget
    (when pixmap-to-free
      (xlib:free-pixmap pixmap-to-free))))

(defmethod repaint ((widget icon) theme-name focus)
  (declare (ignorable theme-name focus))
  (with-slots (window item-to-draw gcontext) widget
    (xlib:clear-area window)
    (draw-centered-text window gcontext item-to-draw :color *white*)))

(defmethod iconify ((application application))
  (icon-box-update)
  (with-slots (window iconic-p wants-focus-p icon master) application
    (when (shaded-p application)
      (shade application))
    (setf iconic-p t wants-focus-p t)
    (when (eq (xlib:window-map-state window) :unmapped)
      (setf (wm-state window) 3))
    (xlib:unmap-window window)
    (when master
      (xlib:unmap-window (widget-window master)))
    (when (stringp (slot-value icon 'item-to-draw))
      (setf (slot-value icon 'item-to-draw) (wm-icon-name window)))
    (when *icon-hints*
      (xlib:map-window (widget-window icon))
      (setf (icon-priority icon) :below))
    (when (eq *focus-type* :on-click)
      (give-focus-to-next-widget-in-desktop))))

(defmethod uniconify ((icon icon))
  (with-slots (application desiconify-p) icon
    (when (shaded-p application)
      (shade application))
    (setf desiconify-p nil)
    (with-slots (window) application
      (setf (window-desktop-num window)
	    (if (stick-p window) +any-desktop+ (current-desk))))
    (unmap-icon-window icon)
    (xlib:map-window (widget-window application))))

(defmethod unmap-icon-window ((icon icon))
  (with-slots (window master application) icon
    (xlib:unmap-window window)
    (setf (application-iconic-p application) nil)
    (unless master
      (with-slots (window) application
	(setf (wm-state window) 1)))))
