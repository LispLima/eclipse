;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: input.lisp,v 1.3 2003/03/19 09:53:26 hatchond Exp $
;;;
;;; ECLIPSE. The Common Lisp Window Manager.
;;; Copyright (C) 2000, 2001, 2002 Iban HATCHONDO
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

;;; Event processing.

;; Most generals methods.

(defmethod event-process (event widget)
  (declare (ignorable event widget))
  (values))

(defmethod event-process ((event mapping-notify) null-widget)
  (declare (ignorable null-widget))
  (with-slots (request start count) event
    (case request
      (:keyboard 
       (when (keycode-registered-p start count)
	 (unregister-all-keystrokes)
	 (xlib:mapping-notify *display* request start count)
	 (register-all-keystrokes)))
      (:modifier 
       (when (kb:modifier-map-changed-p *display*)
	 (kb:init-keyboard *display*)
	 (unregister-all-keystrokes)
	 (unregister-all-mouse-strokes)
	 (xlib:mapping-notify *display* request start count)
	 (register-all-keystrokes)
	 (register-all-mouse-strokes)))
      (:pointer nil))))

(defmethod event-process ((event configure-request) (widget base-widget))
  (declare (ignorable widget)) ;; should only be root ?
  (with-slots (window x y width height stack-mode value-mask above-sibling)
      event
    (xlib:with-state (window)
      (when (logbitp 0 value-mask) (setf (xlib:drawable-x window) x))
      (when (logbitp 1 value-mask) (setf (xlib:drawable-y window) y))
      (when (logbitp 2 value-mask) (setf (xlib:drawable-width window) width))
      (when (logbitp 3 value-mask) (setf (xlib:drawable-height window) height))
      (when (logbitp 6 value-mask)
	(setf (xlib:window-priority window above-sibling) stack-mode)))
    ;; Acording to the ICCCM we should send a synthetic configure-notify,
    ;; when we move a window but without resizing it.
    (unless (or (logbitp 2 value-mask) (logbitp 3 value-mask))
      (send-configuration-notify window))
    ))

;; Specialized ones.

;;; Events for the root window

(defmethod event-process ((event map-request) (root root))
  (if (lookup-widget (event-window event))
      (with-slots (icon) (lookup-widget (event-window event))
	(when icon (uniconify icon))
	(xlib:map-window (event-window event)))
      (xlib:with-server-grabbed (*display*)
	(procede-decoration (event-window event) (root-vscreens root)))))

(defmethod event-process ((event unmap-notify) (root root))
  (declare (ignorable root))
  (with-slots (window send-event-p) event
    (when (application-p (lookup-widget window))
      (if (and send-event-p (eql (xlib:window-map-state window) :unmapped))
	  ;; client withdraws its top-level (ICCCM 4.2.1)
	  (undecore-application (lookup-widget window) :state 0)
	  (setf (wm-state window) 3)))))

(defmethod event-process ((event destroy-notify) (root root))
  (xlib:with-server-grabbed (*display*)
    (let ((app (lookup-widget (event-window event))))
      (when (and (application-p app) (not (application-master app)))
	(ignore-errors (update-lists app 0 root))
	(mapc #'remove-widget (list app (application-icon app)))
	(xlib:destroy-window (widget-window (application-icon app)))
	(when (eql app (get-root-desktop root))
	  (remove-desktop-application root app)))))
  (when (eq *focus-type* :on-click)
    (give-focus-to-next-widget-in-desktop root)))

(defmethod event-process ((event enter-notify) (root root))
  (with-slots (kind mode) event
    (with-slots (resize-status move-status) root
      (when (and (not (eq *focus-type* :on-click))
		 (not (or resize-status move-status))
		 (eq kind :inferior) (or (eq mode :normal) (eq mode :ungrab)))
	(xlib:set-input-focus *display* :pointer-root :pointer-root)))))

(defmethod event-process ((event focus-in) (root root))
  (when (eql (event-kind event) :pointer)
    (if (eql *focus-type* :on-click)
	(give-focus-to-next-widget-in-desktop root)
        (setf (netwm:net-active-window *root-window*) :none))))

(defmethod event-process ((event client-message) (root root))
  (with-slots (type data event-window) event
    (case type
      ((or :_WIN_WORKSPACE :_NET_CURRENT_DESKTOP)
       (with-slots (vscreens) root
	 (unless (= (vs:current-screen vscreens) (aref data 0))
	   (change-vscreen vscreens nil (aref data 0)))))
      (:_NET_NUMBER_OF_DESKTOPS (setf *nb-vscreen* (aref data 0)))
      (:_NET_CLOSE_WINDOW (close-widget (lookup-widget event-window)))
      (:WM_PROTOCOLS
       (when (eq :wm_delete_window (id->atom-name (aref data 0)))
	 (close-widget (lookup-widget event-window)))))))

(defmethod event-process ((event keyboard-event) (root root))
  (with-slots (code state) event
    (let ((callback (lookup-keystroke code state)))
      (when callback (funcall callback event)))))

(defmethod event-process ((event button-press) (root root))
  (with-slots (menu1 menu2 menu3 vscreens resize-status move-status) root
    (with-slots (code state x y) event
      (let ((callback (lookup-mouse-stroke code state)))
	(when callback 
	  (funcall callback event)
  	  (xlib:allow-events *display* :async-pointer)
	  (return-from event-process nil)))
      (when (and (eql resize-status move-status) (< 0 code 4)) ; nil nil 1|2|3
	(when (= 2 (event-code event))
	  (when menu2 (destroy-substructure menu2))
	  (setf menu2 (make-running-menu vscreens)))
	(realize-pop-up (case code (1 menu1) (2 menu2) (3 menu3)) x y)))))

(defmethod event-process ((event motion-notify) (root root))
  (declare (optimize (speed 3)))
  (with-slots (move-status resize-status) root
    (when (or move-status resize-status)
      (let* ((master (slot-value root 'current-active-decoration))
	     (timestamp (event-time event))
	     (precedent-timestamp (decoration-precedent-time master)))
	(declare (type (unsigned-byte 32) timestamp precedent-timestamp))
	(if (decoration-active-p master)
	    (when (or (< timestamp precedent-timestamp)
		      (> (- timestamp precedent-timestamp) 15))
	      (setf (decoration-precedent-time master) timestamp)
	      (cond (move-status (move-widget master event *verbose-move*))
		    (resize-status (resize master event))))
	    (with-slots (root-x root-y) event
	      (format t "The pointer has been frozen !!~%")
	      (setf (decoration-active-p master) t)
	      (event-process
	          (make-event :button-release :root-x root-x :root-y root-y)
		  root)))))))

(defmethod event-process ((event button-release) (root root))
  (with-slots (move-status resize-status current-active-decoration
	       menu1 menu2 menu3 window-menu) root
    (cond (move-status   (finalize-move current-active-decoration))
	  (resize-status (finish-resize current-active-decoration))
	  (t
	   (with-slots (code) event
	     (when window-menu
	       (destroy-substructure window-menu)
	       (setf window-menu nil))
	     (when (< 0 code 4)
	       (destroy-substructure (case code (1 menu1) (2 menu2) (3 menu3)))
	       (when (= code 2) (setf menu2 nil))))))))

(defmethod event-process :after ((event button-release) (root root))
  (with-slots (move-status resize-status current-active-decoration) root
    (when (or move-status resize-status)
      (xlib:ungrab-server *display*)
      (xlib:ungrab-pointer *display*)
      (setf (decoration-active-p current-active-decoration) nil
	    current-active-decoration nil
	    move-status nil
	    resize-status nil))))

;;; Events for master (type: decoration)

(defmethod event-process ((event configure-notify) (master decoration))
  (when (application-p (lookup-widget (event-window event)))
    (with-slots (event-window window x y) event
      (with-slots (left-margin top-margin) (decoration-frame-style master)
	(multiple-value-bind (old-x old-y) (window-position window)
	  (when (eql (decoration-application-gravity master) :static)
	    (decf x left-margin) (decf y top-margin))
	  (unless (= old-x left-margin) (setf (xlib:drawable-x event-window) x))
	  (unless (= old-y top-margin) (setf (xlib:drawable-y event-window) y)))
	  (resize-from (get-child master :application))
	(with-event-mask (event-window)
	  (update-edges-geometry master)
	  (setf (window-position window) (values left-margin top-margin))
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
    (when (application-p (lookup-widget window))
      (unmap-icon-window (get-child master :icon))
      (xlib:map-window event-window)
      (setf (xlib:window-priority event-window) :above
	    (wm-state window) 1)
      (unless (stick-p window)
	(setf (window-desktop-num window) (current-desk))))))

(defmethod event-process ((event destroy-notify) (master decoration))
  (with-event-mask (*root-window*)
    (xlib:with-server-grabbed (*display*)
      (xlib:destroy-window (widget-window master))
      (ignore-errors (update-lists (get-child master :application) 0 *root*))
      (mapc #'remove-widget (cons master (decoration-children master)))
      (xlib:destroy-window (get-child master :icon :window t))      
      (dismiss-move-resize *root*)))
  (when (eq *focus-type* :on-click)
    (give-focus-to-next-widget-in-desktop *root*)))

(defmethod event-process ((event visibility-notify) (master decoration))
  (event-process event (get-child master :application)))

;; Focus management

(defmethod event-process ((event enter-notify) (master decoration))
  (event-process event (get-child master :application)))

;;; Events for an application

(defmethod event-process ((event visibility-notify) (application application))
  (with-slots (wants-focus-p unobscured-p input-model window) application
    (setf unobscured-p (eq (event-state event) :unobscured))
    (when (and unobscured-p wants-focus-p)
      (set-focus input-model window 0)
      (setf wants-focus-p nil))))

(defmethod event-process ((event enter-notify) (application application))
  (with-slots (window input-model) application
    (unless (or (eq *focus-type* :on-click) (focused-p application))
      (set-focus input-model window (event-time event)))))

(defmethod event-process ((event button-press) (application application))
  (with-slots (window unobscured-p input-model) application
    (xlib:allow-events *display* :replay-pointer)
    (unless unobscured-p (put-on-top window))
    (when (and (not (focused-p application)) (eq *focus-type* :on-click))
      (set-focus input-model window (event-time event)))))

(defmethod event-process ((event focus-out) (application application))
  (with-slots (master) application
    (when (and master (not (eql (event-mode event) :while-grabbed)))
      (draw-unfocused-decoration master))))

(defmethod event-process ((event focus-in) (application application))
  (with-slots (master window) application
    (unless (eql (event-mode event) :ungrab)
      ;; Put the focused window in first position in its screen
      ;; if focus policy is on-click.
      (when (eq *focus-type* :on-click)
	(vs:restack-window window (vs:nth-vscreen (root-vscreens *root*))))
      (when master (draw-focused-decoration master))
      (setf (netwm:net-active-window *root-window*) window))))

(defmethod event-process ((event property-notify) (app application))
  (with-slots (window master) app
    (case (event-atom event)
      (:WM_NORMAL_HINTS
       (when master
	 (with-slots (hmargin vmargin) (decoration-frame-style master)
	   (multiple-value-bind (gravity wm-sizes) 
	       (recompute-wm-normal-hints window hmargin vmargin)
	     (setf (slot-value master 'wm-size-hints) wm-sizes
		   (decoration-application-gravity master) gravity)))))
      ((or :WM_NAME :_NET_WM_NAME)
       (when master
	 (with-slots (window item-to-draw) (get-child master :title-bar)
	   (setf item-to-draw (wm-name (widget-window app)))
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
         (let ((action (aref data 0))
	       (prop (netwm:net-wm-state window))
	       (prop1 (id->atom-name (aref data 1)))
	       (prop2 (unless (zerop (aref data 2))
			(id->atom-name (aref data 2)))))
	   (case action
	     (0 (setf prop (remove prop1 prop))
		(when prop2 (setf prop (remove prop2 prop))))
	     (1 (push prop1 prop)
		(when prop2 (push prop2 prop))))
	   (setf (netwm:net-wm-state window) prop)
	   (when (or (eq prop1 :_net_wm_state_hidden)
		     (eq prop2 :_net_wm_state_hidden))
	     (if (= action 0) (uniconify application) (iconify application)))
	   (when (or (eql prop1 :_net_wm_state_fullscreen)
		     (eql prop2 :_net_wm_state_fullscreen))
	     (setf (full-screen-mode application) (if (= action 0) :off :on)))
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
		(master-window (and master (widget-window master))))
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

(defun redraw-icon-and-box-button (exposure-event button &key (color *white*))
  (when (zerop (event-count exposure-event))
    (with-slots (window item-to-draw gcontext) button
      (xlib:clear-area window)
      (draw-centered-text window gcontext item-to-draw :color color))))

(defmethod event-process ((event exposure) (button button))
  (when (zerop (event-count event))
    (with-slots (master) button
      (if (or (not master) (focused-p master))
	  (draw-on-focus-in button)
          (draw-on-focus-out button)))))

(defmethod event-process ((event exposure) (box box-button))
  (redraw-icon-and-box-button event box :color *black*))

(defmethod event-process ((event button-release) (close close-button))
  (close-widget (get-child (button-master close) :application)))

(defmethod event-process ((event button-release) (icon-b iconify-button))
  (iconify (get-child (button-master icon-b) :application)))

(defmethod event-process ((event button-press) (button menu-button))
  (with-slots (window-menu) *root*
    (with-slots (master window armed active-p) button
      (and window-menu (destroy-substructure window-menu))
      (setf window-menu (make-menu-button-menu master))
      (realize-pop-up window-menu (event-root-x event) (event-root-y event))
      (setf armed nil active-p nil))))

;; Maximization
(defmethod event-process ((event button-release) (max-b maximize-button))
  (when (< (event-code event) 4)
    (maximize-window (button-master max-b) (event-code event))))

;; Initialize the resize process.
(defmethod event-process ((event button-press) (edge edge))
  (with-slots (master) edge
    (setf (xlib:window-priority (widget-window master)) :above
	  (decoration-active-p master) t)
    (where-is-pointer edge)
    (update-clone master)))

;; Activate the resize process. (finally give the hand to root)
(defmethod event-process ((event motion-notify) (edge edge))
  (with-slots (window gcontext active-p) (button-master edge)
    (with-slots (resize-status move-status current-active-decoration) *root*
      (when (and active-p (not (or resize-status move-status)))
	(grab-root-pointer)
	(setf resize-status t
	      current-active-decoration (button-master edge))
	(when (eq *resize-mode* :box)
	  (xlib:grab-server *display*)
	  (draw-window-grid window gcontext *root-window*))
	(when *verbose-resize* (initialize-geometry-info-box))))))

(defmethod event-process ((event button-release) (edge edge))
  (with-slots (active-p) (button-master edge)
    (if active-p (setf active-p nil) (event-process event *root*))))

;; initialize the move process.
(defmethod event-process ((event button-press) (title title-bar))
  (with-slots (master armed active-p) title
    (setf (xlib:window-priority (widget-window master)) :above)
    (unless (event-send-event-p event)
      (initialize-move master event))))

;; start the movement.
(defmethod event-process ((event motion-notify) (title title-bar))
  (with-slots (move-status resize-status current-active-decoration) *root*
    (with-slots (window gcontext active-p) (button-master title)
      (when (and active-p (not (or move-status resize-status)))
	(setf move-status t
	      current-active-decoration (button-master title))
	(grab-root-pointer)
	(when *verbose-move* (initialize-geometry-info-box))
	(when (eql *move-mode* :box)
	  (xlib:grab-server *display*)
	  (update-clone (button-master title))
	  (draw-window-grid window gcontext *root-window*))))))

(defmethod event-process ((event button-release) (title title-bar))
  (with-slots (master) title
    (setf (decoration-active-p master) nil)
    (when (eq *focus-type* :on-click)
      (with-slots (input-model window) (get-child master :application)
	(set-focus input-model window (event-time event))))))

;;; events for an icon

(defmethod event-process ((event exposure) (icon icon))
  (redraw-icon-and-box-button event icon))

(defmethod event-process ((event button-press) (icon icon))
  (setf (icon-desiconify-p icon) t
	(xlib:window-priority (widget-window icon)) :above)
  (initialize-move icon event))

(defmethod event-process ((event motion-notify) (icon icon))
  (move-widget icon event)
  (setf (icon-desiconify-p icon) nil))

(defmethod event-process ((event button-release) (icon icon))
  (when (icon-desiconify-p icon)
    (uniconify icon)))
