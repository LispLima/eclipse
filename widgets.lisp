;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: widgets.lisp,v 1.60 2010-04-02 09:57:53 ihatchondo Exp $
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
  (:method (widget) nil)
  (:documentation "Remove widget from internal cache."))

(defgeneric close-widget (widget)
  (:method (widget) nil)
  (:documentation "Close an application according to the ICCCM protocol."))

(defgeneric focus-widget (widget timestamp)
  (:method (widget timestamp) nil))

(defgeneric focused-p (widget)
  (:documentation "Return T if the given widget has the focus."))

(defgeneric put-on-top (widget)
  (:documentation "sets the widget stacking order on top of the others."))

(defgeneric put-on-bottom (widget)
  (:documentation
   "Sets the widget stacking order on bottom of the others \(except if any
    widget with :_net_wm_type_desktop is present and widget is or an 
    application or a decoration\)."))

(defgeneric maximize (widget code &key fill-p)
  (:method (widget code &key fill-p)
    (declare (ignorable widget code fill-p)))
  (:documentation "Maximize/Unmaximize a widget. If already maximized then 
    restores the sizes of the widget before its maximization. During 
    maximization the widget will be enlarged to cover the whole screen except
    any existing panels (e.g applications with the :_net_wm_window_type_dock
    atom present in there _net_wm_window_type property.
     - widget (base-widget): the widget to (un)maximize.
     - code (integer 1 3): 
      -- 1 operates on width and height.
      -- 2 operates on height. 
      -- 3 operates on width.
     - :fill-p (boolean): If NIL, cover the whole screen (except dock type
     applications). If T, finds the first region of the screen that does
     not overlap applications not already overlapped by the widget."))

(defgeneric shade (widget)
  (:documentation "shade/un-shade an application that is decorated."))

(defgeneric shaded-p (widget)
  (:method (widget) nil)
  (:documentation
   "Returns true if the widget is shaded in the sens of the extended window
    manager specification."))

(defgeneric widget-position-fix-p (widget)
  (:method (widget) nil)
  (:documentation "Returns T if one of the state :win_state_fixed_position
   :_net_wm_state_sticky is set for the widget."))

(defgeneric repaint (widget theme focus)
  (:method (widget theme focus) nil)
  (:documentation
   "This method is dedicated to widget repaint such as every buttons, icons,
    edges ...

    It is specialized on widget type, frame-style theme and a boolean that
    indicate if the corresponding toplevel (type decoration) is or not focused.

    Except for standard expose events, the repaint dispatching on focus change
    will be perform according to parts-to-redraw-on-focus list given in
    define-theme."))

(defmethod initialize-instance :after ((widget base-widget) &rest rest)
  (declare (ignore rest))
  (save-widget (widget-window widget) widget))

(defmethod remove-widget ((widget base-widget))
  (clear-widget (widget-window widget)))

(defmethod put-on-top ((widget base-widget))
  (setf (xlib:window-priority (widget-window widget)) :above))

(defmethod put-on-bottom ((widget base-widget))
  (setf (xlib:window-priority (widget-window widget)) :below))

(defun base-widget-p (widget)
  (typep widget 'base-widget))

(defun lookup-widget (window)
  "Returns the associated widget if any."
  (declare (optimize (speed 3) (safety 0)))
  (declare (inline getwinhash))
  (getwinhash window *widget-table*))

(defun save-widget (window widget)
  (declare (optimize (speed 3) (safety 0)))
  (setf (getwinhash window *widget-table*) widget))

(defun clear-widget (window)
  (declare (optimize (speed 3) (safety 0)))
  (declare (inline remwinhash))
  (remwinhash window *widget-table*))

(declaim (inline lookup-widget save-widget clear-widget))

(defclass standard-property-holder (base-widget) ())

;;;; The ROOT

(defclass root (base-widget)
  ((property-holder :initarg :manager :accessor root-property-holder)
   (resize-status :initform nil :accessor root-resize-status)
   (move-status :initform nil :accessor root-move-status)
   (default-cursor :initform nil :accessor root-default-cursor)
   (current-active-widget :initform nil)
   (decoration-theme :initform nil :accessor root-decoration-theme)
   (menu1 :initform nil)
   (menu2 :initform nil)
   (menu3 :initform nil)
   (window-menu :initform nil)
   (client-list :initform (make-hash-table))
   (desktop :initform nil :writer (setf root-desktop))
   (sm-conn :initform nil :accessor root-sm-conn)))

(defmethod root-desktop ((root root) &optional window-p)
  (with-slots (desktop) root
    (when (first desktop)
      (if window-p (widget-window (first desktop)) (first desktop)))))

(defmethod add-desktop-application ((root root) (desktop base-widget))
  (push desktop (slot-value root 'desktop)))

(defmethod remove-desktop-application ((root root) (desktop base-widget))
  (setf (root-desktop root) (delete desktop (slot-value root 'desktop))))

(defmethod focus-widget ((root root) timestamp)
  (declare (ignorable timestamp))
  (xlib:set-input-focus *display* :pointer-root :pointer-root)
  (setf (netwm:net-active-window (widget-window root)) :none))

(defun dismiss-move-resize (root)
  (with-slots (resize-status move-status current-active-widget) root
    (when (or *verbose-move* *verbose-resize*) (undraw-geometry-info-box))
    (when (or move-status resize-status)
      (setf (values move-status resize-status current-active-widget) nil)
      (xlib:ungrab-server *display*)
      (xlib:ungrab-pointer *display*))))

(defun close-sm-connection (root &key (exit-p t))
  (with-slots (sm-conn) root
    (sm-lib:close-sm-connection sm-conn)
    (setf sm-conn nil)
    (when exit-p (error 'exit-eclipse :close-application-p t))))

;;;; Application

(define-constant +application-mask+
    '(:property-change :enter-window :visibility-change :focus-change)
  :test #'equalp)

(define-constant +properties-to-delete-on-withdrawn+
    '(:_net_wm_state :_net_wm_desktop :_win_workspace)
  :test #'equalp)

(defclass application (base-widget)
  ((master :initarg :master :reader application-master)
   (active-p :initform nil :accessor application-active-p)
   (input-model :initform nil :initarg :input :reader application-input-model)
   (icon :initform nil :initarg :icon :reader application-icon)
   (iconic-p :initform nil :accessor application-iconic-p)
   (wants-iconic-p :initform nil :accessor application-wants-iconic-p)
   (wants-focus-p :initform nil :accessor application-wants-focus-p)
   (initial-geometry :initform (make-geometry) :initarg :initial-geometry)
   (full-geometry :initform (make-geometry) :initarg :full-geometry)
   (type :initarg :type :accessor application-type)
   (dialogs :initform nil :writer (setf application-dialogs))
   (transient-for :initform nil :initarg :transient-for
                  :accessor application-transient-for)))

(defmethod application-dialogs ((application application))
  "Returns all dialog applications of an application (including dialog of a
   dialog). If this application is a transient-for (ICCCM 4.1.2.6) then the
   returned list is the dialogs of its leader."
  (labels ((find-all-dialogs (leader)
	     (loop for dialog in (reverse (slot-value leader 'dialogs))
		   append (cons dialog (find-all-dialogs dialog)))))
    (let* ((leader (application-leader application))
	   (dialogs (find-all-dialogs leader)))
      (if (eq leader application) dialogs (cons leader dialogs)))))

(defmethod (setf application-wants-iconic-p) :after (value (app application))
  (loop for dialog in (application-dialogs app)
	do (setf (slot-value dialog 'wants-iconic-p) value)))

(defmethod remove-widget :after ((application application))
  (with-slots (type transient-for icon) application
    (cond ((member :_net_wm_window_type_desktop type)
	   (remove-desktop-application *root* application))
	  ((member :_net_wm_window_type_dock type)
	   (update-workarea-property *root*)))
    (when transient-for
      (with-slots (dialogs) transient-for
	(setf dialogs (delete application dialogs))))
    (remove-widget icon)
    (ignore-errors (update-lists application 0 *root*))
    (ignore-errors (xlib:destroy-window (widget-window icon)))))

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
	until (or (not (xlib:window-p foc)) (xlib:window-equal window foc))
	do (multiple-value-bind (children parent) (xlib:query-tree foc)
	     (declare (ignore children))
	     (setq foc parent))
	finally
          (return (and (xlib:window-p foc) (xlib:window-equal window foc)))))

(defmethod shaded-p ((widget application))
  (member :_net_wm_state_shaded (netwm:net-wm-state (widget-window widget))))

(defmethod widget-position-fix-p ((application application))
  (with-slots (window) application
    (or (member :win_state_fixed_position (gnome:win-state window))
	(member :_net_wm_state_sticky (netwm:net-wm-state window)))))

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
    (let ((desk-w (root-desktop *root* t)))
      (setf (window-priority (if master (widget-window master) window) desk-w)
	    (if desk-w :above :below)))))

(defun application-netwm-type-p (application type)
  "Returns t if application is of the given type."
  (member type (application-type application)))

(defun application-panel-p (application)
  "Returns t if application is a panel (e.g: _net_wm_window_type_dock)."
  (member :_net_wm_window_type_dock (application-type application)))

(defun fullscreenable-p (application) 
  (with-slots (window) application
    (let ((hint (ignore-errors (wm-normal-hints window))))
      (symbol-macrolet ((max-h (and hint (xlib:wm-size-hints-max-height hint)))
                        (max-w (and hint (xlib:wm-size-hints-max-width hint))))
	  (and (if max-w (= max-w (screen-width)) t)
	       (if max-h (= max-h (screen-height)) t))))))

(defun fullscreen-sizes (display)
  "Returns the fullscreen x, y, width and height as a multiple value."
  (if (xlib:query-extension display "XFree86-VidModeExtension")
      (let* ((screen (xlib:display-default-screen display))
            (ml (xlib:xfree86-vidmode-get-mode-line display screen)))
        (multiple-value-bind (x y)
            (xlib:xfree86-vidmode-get-viewport display screen)
          (values x y (xlib:mode-info-hdisplay ml) (xlib:mode-info-vdisplay ml)))
        (values 0 0 (screen-width) (screen-height)))))

;; Maximization helpers.
(defun find-max-geometry (application direction fill-p &key x y w h)
  (multiple-value-bind (rx ry rw rh)
      (rectangle-geometry
       (find-largest-empty-area
           application
           :area-include-me-p (or (/= 1 direction) fill-p)
           :panels-only-p (not fill-p)
           :direction (case direction (2 :vertical) (3 :horizontal) (t :both))))
    (with-slots (window master) application
      (with-slots ((hm hmargin) (vm vmargin))
          (if master (decoration-frame-style master)
              (theme-default-style (lookup-theme "no-decoration")))
        (symbol-macrolet ((minw (aref wmsh 0)) (minh (aref wmsh 1))
                          (maxw (aref wmsh 2)) (maxh (aref wmsh 3))
                          (incw (aref wmsh 4)) (inch (aref wmsh 5))
                          (basew (aref wmsh 6)) (baseh (aref wmsh 7)))
          (let* ((wmsh (recompute-wm-normal-hints window hm vm))
                 (ww (or w (check-size (- rw hm) basew incw minw maxw)))
                 (hh (or h (check-size (- rh vm) baseh inch minh maxh))))
            (when (> (+ ww hm) rw) (decf ww incw))
            (when (> (+ hh vm) rh) (decf hh inch))
            (make-geometry :w ww :h hh :x (or x rx) :y (or y ry))))))))

(defun compute-max-geometry
    (application x y w h direction fill-p vert-p horz-p)
  (symbol-macrolet 
	((ix (geometry-x initial-geometry)) (iy (geometry-y initial-geometry))
	 (iw (geometry-w initial-geometry)) (ih (geometry-h initial-geometry)))
    (with-slots (initial-geometry) application
      (case direction
	;; Unmaximize or Maximize in both directions
	(1 (if (or horz-p vert-p)
	       (copy-geometry initial-geometry)
	       (find-max-geometry application direction fill-p)))
	;; Unmaximize or Maximize Vertically
	(2 (if vert-p
	       (make-geometry :x x :y iy :w w :h ih)
	       (find-max-geometry application direction fill-p :x x :w w)))
	;; Unmaximize or Maximize Horizontally
	(3 (if horz-p
	       (make-geometry :x ix :y y :w iw :h h)
	       (find-max-geometry application direction fill-p :y y :h h)))))))

(defmethod maximize (application code &key (fill-p *maximize-fill*))
  (with-slots ((app-window window) initial-geometry full-geometry master)
      application
    (when (shaded-p master) (shade master))
    (let* ((new-g)
	   (m-window (if master (widget-window master) app-window))
	   (prop (netwm:net-wm-state app-window))
	   (fullscreen-p (member :_net_wm_state_fullscreen prop))
	   (vert-p (car (member :_net_wm_state_maximized_vert prop)))
	   (horz-p (car (member :_net_wm_state_maximized_horz prop))))
      (multiple-value-bind (x y) (window-position m-window)
	(multiple-value-bind (w h) (drawable-sizes app-window)
	  (unless (or horz-p vert-p)
	    (if fullscreen-p
		(setf initial-geometry (copy-geometry full-geometry))
		(setf (geometry initial-geometry) (values x y w h))))
	  (setf new-g (compute-max-geometry
		          application x y w h code fill-p vert-p horz-p))))
      ;; Updates net-wm-state property content.
      (when (and (= 1 code) (or horz-p vert-p))
	(setf (values horz-p vert-p) (values t t)))
      (unless (= 3 code)
	(if vert-p 
	    (setf prop (delete :_net_wm_state_maximized_vert prop))
	    (pushnew :_net_wm_state_maximized_vert prop)))
      (unless (= 2 code)
	(if horz-p
	    (setf prop (delete :_net_wm_state_maximized_horz prop))
	    (pushnew :_net_wm_state_maximized_horz prop)))
      ;; Resize.
      (if fullscreen-p
	  (setf full-geometry new-g)
	  (setf (window-position m-window) (geometry-coordinates new-g)
		(drawable-sizes app-window) (geometry-sizes new-g)))
      ;; Update property. 
      (setf (netwm:net-wm-state app-window) prop))))

(defsetf application-frame-style (application) (frame-style)
  `(with-slots (left-margin right-margin top-margin bottom-margin) ,frame-style
     (setf (netwm:net-frame-extents (widget-window ,application))
           (values left-margin right-margin top-margin bottom-margin))))

(defsetf fullscreen-mode (application) (mode)
  "Mode may be (or :on :off). Put or remove application in or from fullscreen."
  `(set-fullscreen-mode ,application ,mode))

(defun set-fullscreen-mode (application mode)
  (with-slots (window (fgeometry full-geometry) master icon) application
    ;; reset appropriately _net_wm_state property.
    (let ((prop (netwm:net-wm-state window)))
      (if (eq mode :on)
          (pushnew :_net_wm_state_fullscreen prop)
          (setf prop (delete :_net_wm_state_fullscreen prop)))
      (setf (netwm:net-wm-state window) prop))
    (if (eq mode :on)
        ;; put in fullscreen mode.
        (with-event-mask ((xlib:drawable-root window))
          (multiple-value-bind (x y w h) (window-geometry window)
            (when master
              (with-slots (children (master-win window) frame-style) master
                (multiple-value-setq (x y) (window-position master-win))
                (setf (slot-value master 'old-frame-style) frame-style)
                (setf (decoration-frame-style master)
                      (theme-default-style (lookup-theme "no-decoration")))))
            (setf (geometry fgeometry) (values x y w h))
            (multiple-value-setq (x y w h)
              (fullscreen-sizes (xlib:window-display window)))
            (configure-window window :x x :y y :width w :height h))
          (focus-widget application 0))
        ;; revert: restore precedent geometry and decoration style.
        (with-event-mask ((xlib:drawable-root window))
          (setf (drawable-sizes window) (geometry-sizes fgeometry))
          (unless (window-not-decorable-p window)
            (setf (decoration-frame-style master)
                  (slot-value master 'old-frame-style)))
          (multiple-value-bind (x y) (geometry-coordinates fgeometry)
            (with-slots (window) (or master application)
              (configure-window window :x x :y y)))))))

(defun application-leader (application)
  "Returns the \"leader\" of an application. The leader is computed 
   recursively from the transient-for application hint."
  (with-slots (transient-for) application
    (if transient-for (application-leader transient-for) application)))

(defun migrate-application (application new-screen-number)
  "Put an application, all its related dialogs and the top-level it is
   transient-for (if any) to the a new virtual screen."
  (with-slots (master window transient-for iconic-p) application
    (let* ((focused-p (focused-p application))
	   (unmap-p (/= new-screen-number +any-desktop+ (current-desk)))
	   (operation (if (or iconic-p unmap-p)
                          #'xlib:unmap-window
                          #'xlib:map-window)))
      (flet ((migrate (application)
	       (with-slots (master window) application
		 (when (shaded-p application) (shade application))
 		 (setf (window-desktop-num window) new-screen-number)
		 (with-event-mask ((xlib:drawable-root window))
		   (let ((master-window (when master (widget-window master))))
		     (funcall operation (or master-window window))
		     (when master-window
		       (with-event-mask (master-window)
			 (funcall operation window)))))
		 (setf (application-wants-focus-p application) nil))))
	(unless (= (window-desktop-num window) new-screen-number)
	  (mapc #'migrate (application-dialogs application))
	  (unless transient-for (migrate application))
	  (when (and unmap-p focused-p (eq *focus-type* :on-click))
	    (give-focus-to-next-widget-in-desktop)))))))

(defun undecore-application (application &key state)
  "Removes all decoration of this application widget and reparent it to root."
  (with-slots (window master icon) application
    (if master
        (let ((root-window (xlib:drawable-root window)))
          (multiple-value-bind (x y)
              (xlib:translate-coordinates window 0 0 root-window)
            (xlib:reparent-window window root-window x y)
            (event-process (make-event :destroy-notify) master)))
        (event-process (make-event :destroy-notify :window window) *root*))
    (when state
      (setf (wm-state window) state)
      (when (= state 0)
	(delete-properties window +properties-to-delete-on-withdrawn+)))))

(defun computes-transient-for (application)
  "Sets and returns the transient-for slot of an application. If this
   application is a transient-for (ICCCM 4.1.2.6) then it will be added to
   the dialogs list of its leader."
  (with-slots (transient-for (win window)) application
    (let ((transient (lookup-widget (ignore-errors (xlib:transient-for win)))))
      (when (and transient (not (eq *root* transient)))
        (pushnew application (slot-value transient 'dialogs)))
      (when (and transient-for (not (equal transient-for transient)))
        (with-slots (dialogs) transient-for
          (setf dialogs (delete application dialogs))))
      (setf transient-for transient))))

(defun find-input-model (window)
  "Returns the input model keyword of this window according to ICCCM (4.1.7)."
  (let* ((hint (ignore-errors (xlib:get-property window :WM_HINTS)))
	 (protocols (ignore-errors (xlib:wm-protocols window)))
	 (input-p (and hint (logbitp 0 (first hint)) (= 1 (second hint))))
	 (take-focus-p (ignore-errors (member :wm_take_focus protocols))))
    (when (or (not hint) (not (logbitp 0 (first hint))))
      ;; The input model is not set in the property. For some application
      ;; that forgot to precise it we'll act as if it was (otherwise they 
      ;; can't get focused).
      (setf input-p t))
    (cond ((and (not input-p) (not take-focus-p)) :no-input)
	  ((and (not input-p) take-focus-p) :globally-active)
	  ((and input-p (not take-focus-p)) :passive)
	  ((and input-p take-focus-p) :locally-active))))

(defun make-initital-geometry (window &optional (geometry (make-geometry)))
  "Returns the initial-geometry of a window. The initial geometry will be 
   computed according to the wm-normal-hints property if present or to the
   actual geometry of the specified window. If the optional geometry is 
   given then it will be filled and returned."
  (multiple-value-bind (x y w h) (window-geometry window)
    (let ((hint (ignore-errors (wm-normal-hints window))))
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
      (computes-transient-for app)
      (create-icon app master)
      (if (or desktop-p dock-p)
	  (let* ((prec-desk (root-desktop *root* t))
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

(eval-when (:compile-toplevel :load-toplevel)
  (define-constant +std-button-mask+
      '(:button-press :button-release :button-motion :owner-grab-button :exposure)
    :test #'equalp))

(defmethod repaint ((widget button) theme (focus t))
  (declare (ignorable theme focus))
  (with-slots (item-to-draw window gcontext) widget
    (xlib:clear-area window)
    (typecase item-to-draw
      (string (draw-centered-text window gcontext item-to-draw))
      (xlib:pixmap (draw-pixmap window gcontext item-to-draw)))))

(defmethod repaint ((widget button) theme (focus null))
  (declare (ignorable theme focus))
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

(defun create-button (button-type &key parent x y width height
				  item background master (border-width 0)
				  (border *black*)
				  (gravity :north-west)
				  (cursor (root-default-cursor *root*))
				  (event-mask +std-button-mask+))
  ;; When calling this function arguments non optional are
  ;; :parent :x :y :width :height
  ;; the others are optional.
  (when (and (not (xlib:cursor-p cursor)) (keywordp cursor))
    (setf cursor (get-x-cursor *display* cursor)))
  (make-instance button-type
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
	 (create-button 'box-button
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

(defmethod repaint ((widget box-button) theme focus &aux x)
  (declare (ignorable theme focus))
  (with-slots (window item-to-draw gcontext pixmap) widget
    (xlib:clear-area window)
    (when pixmap
      (multiple-value-bind (w h) (drawable-sizes pixmap)
	(draw-pixmap window gcontext pixmap :x 10 :y 10 :width w :height h)
	(setf x (+ w 20))))
    (draw-centered-text window gcontext item-to-draw :color *black* :x x)))

(defun timed-message-box (window &rest messages)
  "Map a small box, of parent `window',  displaying the given string messages.
   This box will automatically destroyed two seconds after being mapped."
  (let ((box (create-message-box messages :parent window)))
    (with-slots (window) box
      (xlib:map-window window)
      (pt:arm-timer 2 (lambda ()
                        (xlib:display-finish-output *display*)
                        (remove-widget box)
                        (xlib:destroy-window window))))))

;;;; Push button
;; Everybody knows what a push button is.

(defclass push-button (button)
  ((armed :initform nil :accessor button-armed)
   (active-p :initform nil :accessor button-active-p)))

(define-constant +push-button-mask+ '(:exposure . #.+pointer-event-mask+)
  :test #'equalp)

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
    (when (decoration-p master)
      (with-slots (theme) (decoration-frame-style master)
	(repaint button theme (focused-p master))))))

(defmethod repaint ((widget push-button) theme (focus t))
  (declare (ignorable theme focus))
  (with-slots (window gcontext armed active-p item-to-draw) widget
    (xlib:clear-area window)
    (let ((p (and armed active-p (push-button-pixmap widget :focused-click))))
      (when (or p item-to-draw)
	(draw-pixmap window gcontext (or p item-to-draw))))))

(defmethod repaint ((widget push-button) theme (focus null))
  (declare (ignorable theme focus))
  (with-slots (window gcontext armed active-p) widget
    (xlib:clear-area window)
    (let ((pixmap (push-button-pixmap widget :unfocused-click)))
      (when (and armed active-p pixmap)
	(draw-pixmap window gcontext pixmap)))))

(defun push-button-pixmap (pbutton pixmap-index)
  (with-slots ((astyle frame-style)) (button-master pbutton)
    (let ((pixs (frame-item-pixmaps astyle (widget->frame-item-key pbutton))))
      (declare (type pixmaps pixs))
      (aref pixs
	    (case pixmap-index
	      (:focused-click 3)
	      (:unfocused-click 2)
	      (:focused-unclick 1)
	      ((and (numberp pixmap-index) (<= 0 pixmap-index 3)) pixmap-index)
	      (t 0))))))

;;;; Standard decoration buttons

(defclass title-bar (push-button) 
  ((timestamp :initform 0)
   (vmargin :initform 0)
   (hmargin :initform 0)
   (parent :initform nil)))

(defmethod repaint ((widget title-bar) theme focus)
  (declare (ignorable theme focus))
  (with-slots (item-to-draw window gcontext) widget
    (xlib:clear-area window)
    (when item-to-draw
      (draw-centered-text window gcontext item-to-draw))))

(defclass close-button (push-button) ())
(defclass iconify-button (push-button) ())
(defclass maximize-button (push-button) ())
(defclass menu-button (push-button) ())

;; Those are master edges and master corners

(define-constant +edge-event-mask+
    '(:button-press :button-release :button-motion :owner-grab-button)
  :test #'equalp)

(defclass edge (button)
  ((gravity :initform :north-west :accessor edge-gravity)
   (cursor :initform :xc_left_ptr :accessor edge-cursor)))

(defmethod initialize-instance :after ((edge edge) &rest options)
  (declare (ignore options))
  (with-slots (window cursor gravity) edge
    (setf (xlib:window-gravity window) gravity)
    (setf (xlib:window-cursor window)
	  (get-x-cursor (xlib:drawable-display window) cursor))))

(defclass top (edge)
  ((cursor :initform :xc_top_side)))

(defclass top-left (edge)
  ((cursor :initform :xc_top_left_corner)))

(defclass top-right (edge)
  ((gravity :initform :north-east)
   (cursor :initform :xc_top_right_corner)))

(defclass right (edge)
  ((gravity :initform :north-east)
   (cursor :initform :xc_right_side)))

(defclass left (edge)
  ((cursor :initform :xc_left_side)))

(defclass bottom (edge)
  ((gravity :initform :south-west)
   (cursor :initform :xc_bottom_side)))

(defclass bottom-right (edge)
  ((gravity :initform :south-east)
   (cursor :initform :xc_bottom_right_corner)))

(defclass bottom-left (edge)
  ((gravity :initform :south-west)
   (cursor :initform :xc_bottom_left_corner)))

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
      (setf icon (create-button 'icon
		   :event-mask '(:pointer-motion-hint . #.+std-button-mask+)
		   :parent (xlib:drawable-root window) :master master
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
  (declare (type (simple-array integer (4)) *icon-box*))
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
  "Restack the window of the given icon according the given priority."
  (with-gensym (p sibling)
    `(let ((,p ,priority) ,sibling)
       (when (eq ,priority :below)
	 (setf ,sibling (root-desktop *root* t)
	       ,p (if ,sibling :above :below)))
       (setf (xlib:window-priority (widget-window ,icon) ,sibling) ,p))))

(defmethod close-widget ((widget icon))
  (close-widget (icon-application widget)))

(defmethod remove-widget :after ((widget icon))
  (with-slots (pixmap-to-free) widget
    (and pixmap-to-free (xlib:free-pixmap pixmap-to-free))
    (setf pixmap-to-free nil)))

(defmethod repaint ((widget icon) theme focus)
  (declare (ignorable theme focus))
  (with-slots (window item-to-draw gcontext) widget
    (xlib:clear-area window)
    (draw-centered-text window gcontext item-to-draw :color *white*)))

(defmethod iconify ((application application))
  (icon-box-update)
  (flet ((_iconify_ (application &optional (map-icons *icon-hints*))
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
	     (when map-icons
	       (xlib:map-window (widget-window icon))
	       (setf (icon-priority icon) :below)))))
    (with-slots (transient-for) application
      (let ((seq (application-dialogs application)))
	(mapc (lambda (a) (_iconify_ a nil)) (if transient-for (cdr seq) seq))  
	(_iconify_ (if transient-for (car seq) application))))
    (when (eq *focus-type* :on-click)
      (give-focus-to-next-widget-in-desktop))))

(defmethod uniconify ((icon icon))
  (flet ((_uniconify_ (application)
	   (with-slots (icon window) application
	     (when (shaded-p application)
	       (shade application))
	     (setf (icon-desiconify-p icon) nil)
	     (setf (window-desktop-num window)
		   (if (stick-p window) +any-desktop+ (current-desk)))
	     (unmap-icon-window icon)
	     (xlib:map-window window))))
    (with-slots (application) icon
      (unless (application-transient-for application)
	(_uniconify_ application))
      (mapc #'_uniconify_ (application-dialogs application)))))

(defmethod unmap-icon-window ((icon icon))
  (with-slots (window master application) icon
    (xlib:unmap-window window)
    (setf (application-iconic-p application) nil)
    (unless master
      (with-slots (window) application
	(setf (wm-state window) 1)))))
