;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: virtual-screen.lisp,v 1.5 2003/08/28 14:50:35 hatchond Exp $
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

(defun window-belongs-to-vscreen-p (window scr-num iconify-p)
  (let ((n (or (gnome-desktop-num window) -1))
	(wm-state (car (wm-state window)))
	(netwm-type (netwm:net-wm-window-type window)))
    (and (or (= n scr-num) (= n +any-desktop+))
	 (or (eq wm-state 1) (and iconify-p (eq wm-state 3)))
	 (not (member :win_hints_skip_taskbar (gnome:win-hints window)))
	 (not (member :_net_wm_state_skip_taskbar (netwm:net-wm-state window)))
	 (not (member :_net_wm_window_type_desktop netwm-type))
	 (not (member :_net_wm_window_type_dock netwm-type)))))

(defun map-or-unmap-vscreen (fun scr-num)
  (loop for widget being each hash-value in *widget-table*
	when (application-p widget) do
	  (with-slots (window master) widget
	    (when (and (eq (gnome-desktop-num window) scr-num)
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
		     (let ((i (or (ignore-errors (gnome-desktop-num win)) -1)))
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
      (when (< -1 new nb-vscreens)
	(with-event-mask (window)
	  ;; If focus policy is on click: save the latest focused application.
	  (when (eq *focus-type* :on-click)
	    (let ((widget (lookup-widget (input-focus *display*))))
	      (when (application-p widget)
		(setf (application-wants-focus-p widget) t))))
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

(defmethod circulate-window ((root root) &key direction)
  (let ((screen-wins (get-screen-content (current-desk))))
    (or screen-wins (return-from circulate-window nil))
    (when (= 1 (length screen-wins)) (setf direction :above))
    (let* ((above-p (eq direction :above))
	   (wins (if above-p screen-wins (reverse screen-wins)))
	   (desktop (and (eql direction :below) (get-root-desktop root t)))
	   (one (lookup-widget (first wins)))
	   (two (if above-p one (lookup-widget (second wins)))))
      (with-slots (master) one (when master (setf one master)))
      (with-slots (master) two (when master (setf two master)))
      (when (and (eq direction :below) desktop)
	(setf direction :above above-p t))
      (with-slots (window) two
	(and (not above-p) *warp-pointer-when-cycle* 
	     (xlib:warp-pointer window 8 5))
        (setf (window-priority (widget-window one) desktop) direction)
	(and above-p *warp-pointer-when-cycle* (xlib:warp-pointer window 8 5))
	(and (eq *focus-type* :on-click) *focus-when-window-cycle*
	     (focus-widget two 0))))))
