;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: virtual-screen.lisp,v 1.19 2004/02/12 23:30:22 ihatchondo Exp $
;;;
;;; Copyright (C) 2002 Iban HATCHONDO
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

;;;; Virtual screen

(in-package :ECLIPSE-INTERNALS)

;;;; Private

(defun window-belongs-to-vscreen-p
    (win scr-num iconify-p skip-taskbar skip-desktop skip-dock)
  (when (lookup-widget win)
    (let ((n (or (window-desktop-num win) -1))
	  (wm-state (car (wm-state win)))
	  (netwm-type (netwm:net-wm-window-type win))
	  (netwm-state (netwm:net-wm-state win)))
      (and (or (= n scr-num) (= n +any-desktop+))
	   (or (eq wm-state 1) (and iconify-p (eq wm-state 3)))
	   (not (and skip-taskbar
		     (member :_net_wm_state_skip_taskbar netwm-state)))
	   (not (and skip-taskbar
		     (member :win_hints_skip_taskbar (gnome:win-hints win))))
	   (not (and skip-desktop
		     (member :_net_wm_window_type_desktop netwm-type)))
	   (not (and skip-dock
		     (member :_net_wm_window_type_dock netwm-type)))))))

(defun map-or-unmap-vscreen (fun scr-num)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type function fun))
  (loop for widget being each hash-value in *widget-table*
	when (application-p widget) do
	  (with-slots (window master) widget
	    (when (and (eq (window-desktop-num window) scr-num)
		       (eq (car (wm-state window)) 1))
	      (let ((mwindow (when master (widget-window master))))
		(funcall fun (or mwindow window))
		(when (and mwindow (not (shaded-p widget)))
		  (with-event-mask (mwindow)
		    (funcall fun window))))))))

;;;; Public

(defmacro current-desk () `(current-vscreen *root-window*))

(defun current-vscreen (win)
  "Get the current virtual screen index. The window parameter must be
  the window that owns the win_workspace or _net_current_desktop property."
  (or (netwm:net-current-desktop win) (gnome:win-workspace win) 0))

(defun number-of-virtual-screens (win)
  "Get the number of virtual screens. The window parameter must be the window
  that owns the win_workspace_count or _net_number_of_desktops property."
  (or (gnome:win-workspace-count win) (netwm:net-number-of-desktops win) 1))

(defsetf number-of-virtual-screens () (n)
  `(with-slots (window) *root*
     (let ((nb-vscreens (number-of-virtual-screens window))
	   (cur (current-vscreen window)))
       (unless (or (zerop ,n) (= ,n nb-vscreens))
	 (when (< ,n nb-vscreens)
	   (loop for widget being each hash-value in *widget-table*
		 when (application-p widget) do
	           (with-slots ((win window)) widget
		     (let ((i (or (ignore-errors (window-desktop-num win)) -1)))
		       (when (and (>= i ,n) (/= i +any-desktop+))
			 (setf (window-desktop-num win) (1- ,n)))))))
	 (cond ((> cur (1- ,n))	(change-vscreen *root* :n (1- ,n)))
	       ((= cur (1- ,n))	(map-or-unmap-vscreen #'xlib:map-window cur)))
	 (setf (netwm:net-desktop-viewport window) (make-viewport-property ,n)
	       (gnome:win-workspace-count window) ,n
	       (netwm:net-number-of-desktops window) ,n)
	 (initialize-eclipse-desktop-pointer-positions *root*)
	 (update-workarea-property *root*)))))

(defun input-focus (display)
  "Find the application that is currently focused if anyone is."
  (loop with w = (xlib:input-focus display)
	until (or (not (xlib:window-p w)) (application-p (lookup-widget w)))
	do (multiple-value-bind (children parent) (xlib:query-tree w)
	     (declare (ignore children))
	     (setf w parent))
	finally (return w)))

(defmethod change-vscreen ((root root) &key direction n)
  (declare (type (or null function) direction))
  (with-slots ((rw window)) root
    (let* ((nb-vscreens (number-of-virtual-screens rw))
	   (cur (netwm:net-current-desktop rw))
	   (new (if direction (mod (funcall direction cur 1) nb-vscreens) n)))
      (unless (integerp new)
	(error "No destination given to change-vscreen~%"))
      (when (and (< -1 new nb-vscreens) (/= cur new))
	(with-event-mask (rw)
	  ;; If focus policy is on click: save the latest focused application.
	  (when (eq *focus-type* :on-click)
	    (let ((widget (lookup-widget (input-focus *display*))))
	      (when (application-p widget)
		(setf (application-wants-focus-p widget) t))))
	  (xlib:set-input-focus *display* :pointer-root :pointer-root)
	  (xlib:with-server-grabbed (*display*)
	    (with-pointer-grabbed (rw nil)
	      (map-or-unmap-vscreen #'xlib:unmap-window cur)
	      (map-or-unmap-vscreen #'xlib:map-window new))))
	(setf (gnome:win-workspace rw) new
	      (netwm:net-current-desktop rw) new)
	(when *save-and-restore-pointer-position-during-workspace-switch*
	  (setf (eclipse-desktop-pointer-positions rw cur)
		(xlib:query-pointer rw))
	  (multiple-value-call #'xlib:warp-pointer
	    rw (eclipse-desktop-pointer-positions rw new)))
	(when (eq *focus-type* :on-click)
	  (give-focus-to-next-widget-in-desktop))
	(when *change-desktop-message-active-p*
	  (timed-message-box rw (or (nth new (workspace-names rw))
				    (format nil "WORKSPACE ~D" new))))))))

(defun screen-content (scr-num 
		       &key (predicate #'window-belongs-to-vscreen-p) iconify-p
		            (skip-taskbar t) (skip-desktop t) (skip-dock t))
  "Returns the list of application's windows that represent the contents
  of the given virtual screen. 
   :iconify-p to include or not iconfied windows (default nil).
   :skip-taskbar to include window with skip-taskbar hint (default t).
   :skip-desktop to include window with desktop window type (default t).
   :skip-dock to include window with dock window type (default t).
   :predicate a function of six arguments:
     window screen-number iconify-p skip-taskbar-p skip-desktop-p skip-dock-p."
  (declare (type function predicate))
  (loop with i = (if (eql scr-num +any-desktop+) (current-desk) scr-num)
	for w in (query-application-tree *root-window*)
	if (funcall predicate w i iconify-p skip-taskbar skip-desktop skip-dock)
	collect w))

(defun give-focus-to-next-widget-in-desktop ()
  "Gives the focus to the window that is on top of the stacking order."
  (loop	with focus-dest = nil
	for window in (reverse (screen-content (current-desk)))
	when (eq :viewable (xlib:window-map-state window))
	do (with-slots (input-model wants-focus-p) (lookup-widget window)
	     (unless (eq input-model :no-input)
	       (when wants-focus-p
		 (setf focus-dest (lookup-widget window))
		 (loop-finish))
	       (unless focus-dest (setf focus-dest window))))
	finally 
	  (typecase focus-dest
	    (application (put-on-top focus-dest))
	    (xlib:window (focus-widget (lookup-widget focus-dest) nil))
	    (t (focus-widget *root* nil)))))

(defmethod circulate-window
    ((root root) &key direction (nth 0) icon-p windows (desk (current-desk)))
  (unless windows
    (setf windows (reverse (screen-content desk :iconify-p icon-p))))
  (or windows (return-from circulate-window nil))
  (let ((length (length windows)))
    (setf nth (mod nth length))
    (let ((above-p (eq direction :above))
	  (focus-dest (nth nth windows))
	  (first (lookup-widget (car windows))))
      ;; Grab the pointer to avoid enter notify events race concurrence
      ;; between the window hierarchy change and the warp-pointer call.
      (with-pointer-grabbed ((widget-window root) nil)
	(when (and (/= length 1) icon-p (application-wants-iconic-p first))
	  (iconify first))
	(flet ((set-window-priority (window sibling priority)
		 (with-slots (master) (lookup-widget window)
		   (when master (setf window (widget-window master))))
		 (when (lookup-widget sibling)
		   (with-slots (master) (lookup-widget sibling)
		     (when master (setf sibling (widget-window master)))))
		 (setf (window-priority window sibling) priority)))
	  (cond ((= length 1) (set-window-priority focus-dest nil :above))
		((= nth 0)
		 (let ((sibling (if above-p (last windows) (cdr windows))))
		   (set-window-priority (car windows) (car sibling) :below)
		   (setf focus-dest (second windows))))
		((or (and (= nth (1- length)) (not above-p))
		     (and (= nth 1) above-p))
		 (set-window-priority focus-dest nil :above))
		(t (unless above-p
		     (setf focus-dest (nth (incf nth) windows)))
		   (set-window-priority (car windows) focus-dest :below)
		   (set-window-priority focus-dest nil :above))))
	(with-slots (master) (setf focus-dest (lookup-widget focus-dest))
	  (when (and icon-p (application-iconic-p focus-dest))
	    (uniconify (application-icon focus-dest))
	    (setf (application-wants-iconic-p focus-dest) t))
	  (when master (setf focus-dest master)))
	(when *warp-pointer-when-cycle*
	  (xlib:warp-pointer (widget-window focus-dest) 8 5)))
      (when *focus-when-window-cycle*
	(focus-widget focus-dest 0))
      focus-dest)))
