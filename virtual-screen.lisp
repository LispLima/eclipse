;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: virtual-screen.lisp,v 1.9 2003/10/06 17:57:26 ihatchondo Exp $
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

(defun window-belongs-to-vscreen-p (win scr-num iconify-p)
  (when (lookup-widget win)
    (let ((n (or (window-desktop-num win) -1))
	  (wm-state (car (wm-state win)))
	  (netwm-type (netwm:net-wm-window-type win)))
      (and (or (= n scr-num) (= n +any-desktop+))
	   (or (eq wm-state 1) (and iconify-p (eq wm-state 3)))
	   (not (member :win_hints_skip_taskbar (gnome:win-hints win)))
	   (not (member :_net_wm_state_skip_taskbar (netwm:net-wm-state win)))
	   (not (member :_net_wm_window_type_desktop netwm-type))
	   (not (member :_net_wm_window_type_dock netwm-type))))))

(defun map-or-unmap-vscreen (fun scr-num)
  (loop for widget being each hash-value in *widget-table*
	when (application-p widget) do
	  (with-slots (window master) widget
	    (when (and (eq (window-desktop-num window) scr-num)
		       (eq (car (wm-state window)) 1))
	      (funcall fun (if master (widget-window master) window))))))

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
	       (netwm:net-number-of-desktops window) ,n)))))

(defun input-focus (display)
  "Find the application that is currently focused if anyone is."
  (loop with w = (xlib:input-focus display)
	until (or (not (xlib:window-p w)) (application-p (lookup-widget w)))
	do (multiple-value-bind (children parent) (xlib:query-tree w)
	     (declare (ignore children))
	     (setf w parent))
	finally (return w)))

(defmethod change-vscreen ((root root) &key direction n)
  (with-slots (window) root
    (let* ((nb-vscreens (number-of-virtual-screens window))
	   (cur (netwm:net-current-desktop window))
	   (new (if direction (mod (funcall direction cur 1) nb-vscreens) n)))
      (unless (integerp new)
	(error "No destination given to change-vscreen~%"))
      (when (and (< -1 new nb-vscreens) (/= cur new))
	(with-event-mask (window)
	  ;; If focus policy is on click: save the latest focused application.
	  (when (eq *focus-type* :on-click)
	    (let ((widget (lookup-widget (input-focus *display*))))
	      (when (application-p widget)
		(setf (application-wants-focus-p widget) t))))
	  (xlib:set-input-focus *display* :pointer-root :pointer-root)
	  (with-pointer-grabbed (window nil)
	    (map-or-unmap-vscreen #'xlib:map-window new)
	    (map-or-unmap-vscreen #'xlib:unmap-window cur)))
	(setf (gnome:win-workspace window) new
	      (netwm:net-current-desktop window) new)
	(when *change-desktop-message-active-p*
	  (timed-message-box window
			     (or (nth new (workspace-names window))
				 (format nil "WORKSPACE ~D" new))))))))

(defun get-screen-content (scr-num &key iconify-p)
  "Returns the list of application's windows that represent the contents
  of the given virtual screen. Use :iconify-p t to includes iconfied windows"
  (loop for win in (query-application-tree *root-window*)
	when (window-belongs-to-vscreen-p win scr-num iconify-p) collect win))

(defun give-focus-to-next-widget-in-desktop ()
  "Gives the focus to the window that is on top of the stacking order."
  (loop	with given-p = nil
	for window in (reverse (get-screen-content (current-desk)))
	when (eq :viewable (xlib:window-map-state window))
	do (with-slots (input-model) (lookup-widget window)
	     (unless (eq input-model :no-input)
	       (set-focus input-model window 0)
	       (setf given-p t)
	       (loop-finish)))
	finally 
	  (unless given-p
	    (xlib:set-input-focus *display* :pointer-root :pointer-root))))

(defmethod circulate-window ((root root) &key direction (nth 0) icon-p)
  (let* ((wins (reverse (get-screen-content (current-desk) :iconify-p icon-p)))
	 (length (length wins)))
    (or wins (return-from circulate-window nil))
    (setf nth (mod nth length))
    (let ((above-p (eq direction :above))
	  (focus-dest (nth nth wins))
	  (first (lookup-widget (car wins))))
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
		 (let ((sibling (if above-p (last wins) (cdr wins))))
		   (set-window-priority (car wins) (car sibling) :below)
		   (setf focus-dest (second wins))))
		((or (and (= nth (1- length)) (not above-p))
		     (and (= nth 1) above-p))
		 (set-window-priority focus-dest nil :above))
		(t (unless above-p
		     (setf focus-dest (nth (incf nth) wins)))
		   (set-window-priority (car wins) focus-dest :below)
		   (set-window-priority focus-dest nil :above))))
	(with-slots (master) (setf focus-dest (lookup-widget focus-dest))
	  (when (and icon-p (application-iconic-p focus-dest))
	    (uniconify (application-icon focus-dest))
	    (setf (application-wants-iconic-p focus-dest) t))
	  (when master (setf focus-dest master)))
	(when *warp-pointer-when-cycle*
	  (xlib:warp-pointer (widget-window focus-dest) 8 5)))
      (when (and (eq *focus-type* :on-click) *focus-when-window-cycle*)
	(focus-widget focus-dest 0))
      focus-dest)))
