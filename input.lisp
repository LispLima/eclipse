;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: input.lisp,v 1.10 2003/09/07 01:37:34 hatchond Exp $
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

;; handle the selection clear, to be able to stop window managing when needed.
(defmethod event-process ((event selection-clear) null-widget)
  (declare (ignorable null-widget))
  (with-slots (event-window selection) event
    (when (and (xlib:window-equal event-window (root-manager *root*))
	       (string= selection +xa-wm+))
      (loop for val being each hash-value in *widget-table*
	    when (application-p val) do (undecore-application val))
      (xlib:display-finish-output *display*)
      (setf (xlib:window-event-mask *root-window*) '())
      (xlib:destroy-window (root-manager *root*))
      (xlib:display-finish-output *display*)
      (xlib:close-display *display*)
      (%quit%))))

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
	(setf (window-priority window above-sibling) stack-mode)))
    ;; Acording to the ICCCM we should send a synthetic configure-notify,
    ;; when we move a window but without resizing it.
    (unless (or (logbitp 2 value-mask) (logbitp 3 value-mask))
      (send-configuration-notify window))
    ))

(defmethod event-process :after ((event destroy-notify) (widget base-widget))
  (with-slots (window) event
    (when (or (decoration-p widget) (application-p (lookup-widget window)))
      (if (eq *focus-type* :on-click)
	  (give-focus-to-next-widget-in-desktop)
          (multiple-value-bind (x y s child) (xlib:query-pointer *root-window*)
	    (declare (ignore x y s))
	    (let ((e (make-event :enter-notify :kind :inferior :mode :normal)))
	      (event-process e (or (lookup-widget child) *root*))))))))

;; Specialized ones.

;;; Events for the root window

(defmethod event-process ((event map-request) (root root))
  (if (lookup-widget (event-window event))
      (with-slots (icon) (lookup-widget (event-window event))
	(when icon (uniconify icon))
	(xlib:map-window (event-window event)))
      (xlib:with-server-grabbed (*display*)
	(procede-decoration (event-window event)))))

(defmethod event-process ((event unmap-notify) (root root))
  (declare (ignorable root))
  (with-slots (window send-event-p) event
    (let ((widget (lookup-widget window)))
      (typecase widget
	(application
         (let ((state (ignore-errors (xlib:window-map-state window))))
	   (when state
	     (if (and send-event-p (eql state :unmapped))
		 ;; client withdraws its top-level (ICCCM 4.2.1)
		 (undecore-application widget :state 0)
		 (setf (wm-state window) 3)))))
	(decoration 
	 (setf (wm-state (get-child widget :application :window t)) 3))))))

(defmethod event-process ((event destroy-notify) (root root))
  (xlib:with-server-grabbed (*display*)
    (let ((app (lookup-widget (event-window event))))
      (when (and (application-p app) (not (application-master app)))
	(ignore-errors (update-lists app 0 root))
	(mapc #'remove-widget (list app (application-icon app)))
	(xlib:destroy-window (widget-window (application-icon app)))
	(when (member app (root-desktop root))
	  (remove-desktop-application root app))))))

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
	(give-focus-to-next-widget-in-desktop)
        (setf (netwm:net-active-window (widget-window root)) :none))))

(defmethod event-process ((event client-message) (root root))
  (with-slots (type data event-window) event
    (case type
      ((or :_WIN_WORKSPACE :_NET_CURRENT_DESKTOP)
       (with-slots (window) root
	 (unless (= (number-of-virtual-screens window) (aref data 0))
	   (change-vscreen root :n (aref data 0)))))
      (:_NET_NUMBER_OF_DESKTOPS 
       (setf (number-of-virtual-screens) (aref data 0)))
      (:_NET_CLOSE_WINDOW (close-widget (lookup-widget event-window)))
      (:WM_PROTOCOLS
       (when (eq :wm_delete_window (id->atom-name (aref data 0)))
	 (close-widget (lookup-widget event-window)))))))

(defmethod event-process ((event keyboard-event) (root root))
  (with-slots (code state) event
    (let ((callback (lookup-keystroke code state)))
      (when callback
	(xlib:allow-events *display* :async-keyboard)
	(funcall callback event)))))

(defmethod event-process ((event button-press) (root root))
  (with-slots (menu1 menu2 menu3 resize-status move-status) root
    (with-slots (code state x y) event
      (let ((callback (lookup-mouse-stroke code state)))
	(when callback 
	  (funcall callback event)
  	  (xlib:allow-events *display* :async-pointer)
	  (return-from event-process nil)))
      (when (and (eql resize-status move-status) (< 0 code 4)) ; nil nil 1|2|3
	(when (= 2 (event-code event))
	  (when menu2 (destroy-substructure menu2))
	  (setf menu2 (make-running-menu root)))
	(realize-pop-up (case code (1 menu1) (2 menu2) (3 menu3)) x y)))))

(defmethod event-process ((event motion-notify) (root root))
  (declare (optimize (speed 3)))
  (with-slots
	(move-status resize-status (master current-active-decoration)) root
    (when (or move-status resize-status)
      (let ((timestamp (event-time event))
	    (precedent-timestamp (decoration-precedent-time master)))
	(declare (type (unsigned-byte 32) timestamp precedent-timestamp))
	(if (decoration-active-p master)
	    (when (or (< timestamp precedent-timestamp)
		      (> (- timestamp precedent-timestamp) 15))
	      (setf (decoration-precedent-time master) timestamp)
	      (cond (move-status (move-widget master event *verbose-move*))
		    (resize-status (resize master event *verbose-resize*))))
	    (progn
	      (format t "The pointer has been frozen !!~%")
	      (setf (decoration-active-p master) t)
	      (event-process (make-event :button-release) root)))))))

(defmethod event-process ((event button-release) (root root))
  (with-slots (move-status resize-status (master current-active-decoration)
	       menu1 menu2 menu3 window-menu) root
    (cond (move-status (finalize-move master *verbose-move*))
	  (resize-status (finish-resize master *verbose-resize*))
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
    (with-slots ((ev-win event-window) window x y) event
      (with-slots (left-margin top-margin) (decoration-frame-style master)
	(multiple-value-bind (old-x old-y) (window-position window)
	  (when (eql (decoration-application-gravity master) :static)
	    (decf x left-margin) (decf y top-margin))
	  (unless (= old-x left-margin) (setf (xlib:drawable-x ev-win) x))
	  (unless (= old-y top-margin) (setf (xlib:drawable-y ev-win) y)))
	  (resize-from (get-child master :application))
	(with-event-mask (ev-win)
	  (update-edges-geometry master)
	  (setf (window-position window) (values left-margin top-margin))
	  (send-configuration-notify window))))))

(defmethod event-process ((event reparent-notify) (master decoration))
  (unless (xlib:window-equal (event-event-window event) (event-parent event))
    (event-process (make-event :destroy-notify) master)))

(defmethod event-process ((event unmap-notify) (master decoration))
  (with-slots (event-window) event
    (xlib:unmap-window event-window)))

(defmethod event-process ((event map-notify) (master decoration))
  (with-slots (window event-window) event
    (when (application-p (lookup-widget window))
      (unless (eq (xlib:window-map-state window) :unmapped)
	(unmap-icon-window (get-child master :icon))
	(xlib:map-window event-window)
	(setf (window-priority event-window) :above
	      (wm-state window) 1)
	(setf (window-desktop-num window)
	      (if (stick-p window) +any-desktop+ (current-desk)))))))

(defmethod event-process ((event destroy-notify) (master decoration))
  (with-event-mask (*root-window*)
    (xlib:with-server-grabbed (*display*)
      (xlib:destroy-window (widget-window master))
      (ignore-errors (update-lists (get-child master :application) 0 *root*))
      (mapc #'remove-widget (cons master (decoration-children master)))
      (xlib:destroy-window (get-child master :icon :window t))      
      (dismiss-move-resize *root*))))

(defmethod event-process ((event visibility-notify) (master decoration))
  (event-process event (get-child master :application)))

;; Focus management

(defmethod event-process ((event enter-notify) (master decoration))
  (event-process event (get-child master :application)))

;;; Events for an application

(defmethod event-process ((event visibility-notify) (application application))
  (with-slots (wants-focus-p input-model window) application
    (when (and (eq (event-state event) :unobscured) wants-focus-p)
      (set-focus input-model window 0)
      (setf wants-focus-p nil))))

(defmethod event-process ((event enter-notify) (application application))
  (with-slots (window input-model) application
    (unless (or (eq *focus-type* :on-click) (focused-p application))
      (set-focus input-model window (event-time event)))))

(defmethod event-process ((event button-press) (application application))
  (xlib:allow-events *display* :replay-pointer)
  (put-on-top application))

(defmethod event-process ((event focus-out) (application application))
  (with-slots (master) application
    (with-slots (mode) event
      (unless (or (not master) (eql mode :while-grabbed) (eql mode :grab))
	(dispatch-repaint master :focus nil)))))

(defmethod event-process ((event focus-in) (application application))
  (with-slots (master window) application
    (when (and master (not (eql (event-mode event) :ungrab)))
      (dispatch-repaint master :focus t)
      (setf (netwm:net-active-window *root-window*) window))))

(defmethod event-process ((event property-notify) (app application))
  (with-slots (window master) app
    (case (event-atom event)
      (:WM_NORMAL_HINTS
       (when master
	 (with-slots (hmargin vmargin) (decoration-frame-style master)
	   (with-slots (application-gravity wm-size-hints) master
	     (multiple-value-setq (wm-size-hints application-gravity)
		 (recompute-wm-normal-hints window hmargin vmargin))))))
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
	 (let* ((to-change (aref data 0))
		(mask (or (gnome:win-state window :result-type t) 0))
		(new-mask (logior (logandc1 (aref data 0) mask)
				  (logand (aref data 0) (aref data 1)))))
	   (setf (gnome:win-state window) new-mask)
	   ;; win_state_sticky
	   (when (logbitp 0 to-change)
	     (cond ((and (logbitp 0 mask) (not (logbitp 0 new-mask)))
		    (setf (window-desktop-num window) (current-desk)))
		   ((logbitp 0 new-mask)
		    (setf (window-desktop-num window) +any-desktop+)
		    (xlib:map-window window))))
	   ;; win_state_maximized_vert
	   (when (logbitp 2 to-change) (maximize-window application 2))
	   ;; win_state_maximized_horiz
	   (when (logbitp 3 to-change) (maximize-window application 3))
	   ;; win_state_shaded
	   (when (and (logbitp 5 to-change) master) (shade master))))
	(:_NET_WM_STATE
         (let ((mode (aref data 0))
	       (p (netwm:net-wm-state window))
	       (p1 (id->atom-name (aref data 1)))
	       (p2 (and (/= 0 (aref data 2)) (id->atom-name (aref data 2)))))
	   (when (= 2 mode) (return-from event-process nil)) ; toggle.
	   (when (= mode (if (member p1 p) 1 0)) (setf p1 nil))
	   (when (= mode (if (member p2 p) 1 0)) (setf p2 nil))
	   (macrolet ((or-eql (val &rest vars)
			`(or ,@(loop for v in vars collect `(eql ,v ,val)))))
	     (when (or-eql :_net_wm_state_hidden p1 p2)
	       (if (= mode 0) (uniconify application) (iconify application)))
	     (when (or-eql :_net_wm_state_fullscreen p1 p2)
	       (when (fullscreenable-p application)
		 (setf (fullscreen-mode application) (if (= mode 0) :off :on))))
	     (when (or-eql :_net_wm_state_maximized_vert p1 p2)
	       (maximize-window application 2))
	     (when (or-eql :_net_wm_state_maximized_horz p1 p2)
	       (maximize-window application 3))
	     (when (and master (or-eql :_net_wm_state_shaded p1 p2))
	       (shade master))
	     (when (or-eql :_net_wm_state_above p1 p2)
	       (put-on-top application))
	     (when (or-eql :_net_wm_state_below p1 p2)
	       (put-on-bottom application)))))
	(:_NET_WM_DESKTOP
	 (let* ((cur-desk (gnome-desktop-num window))
		(new-desk (aref data 0))
		(master-window (and master (widget-window master))))
	   (unless (= cur-desk new-desk)
	     (when (shaded-p application) (shade application))
	     (setf (window-desktop-num window) new-desk)
	     (if (or (= new-desk +any-desktop+) (= new-desk (current-desk)))
		 (xlib:map-window (or master-window window))
		 (with-event-mask (*root-window*)
		   (xlib:unmap-window (or master-window window)))))))
	(:WM_CHANGE_STATE
	 (when (= 3 (aref data 0)) (iconify application)))))))

;;; Events for buttons

(defmethod event-process ((event exposure) (button button))
  (when (zerop (event-count event))
    (with-slots (master) button
      (let ((name (theme-name (root-decoration-theme *root*))))
	(repaint button name (and master (focused-p master)))))))

(defmethod event-process ((event exposure) (box box-button))
  (repaint box (theme-name (root-decoration-theme *root*)) nil))

(defmethod event-process ((event button-release) (close close-button))
  (close-widget (get-child (button-master close) :application)))

(defmethod event-process ((event button-release) (icon-b iconify-button))
  (with-slots (master) icon-b
    (iconify (get-child master :application))))

(defmethod event-process ((event button-press) (button menu-button))
  (with-slots (window-menu) *root*
    (with-slots (master window armed active-p) button
      (when (eq *focus-type* :on-click) (focus-widget button 0))
      (and window-menu (destroy-substructure window-menu))
      (setf window-menu (make-menu-button-menu master))
      (realize-pop-up window-menu (event-root-x event) (event-root-y event))
      (setf armed nil active-p nil))))

;; Maximization
(defmethod event-process ((event button-release) (max-b maximize-button))
  (when (< (event-code event) 4)
    (with-slots (master) max-b
      (maximize-window (get-child master :application)  (event-code event)))
    (when (eq *focus-type* :on-click) (focus-widget max-b 0))))

;; Initialize the resize process.
(defmethod event-process ((event button-press) (edge edge))
  (with-slots (master) edge
    (unless (shaded-p master)
      (setf (window-priority (widget-window master)) :above
	    (decoration-active-p master) t)
      (where-is-pointer edge)
      (update-clone master))))

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
    (setf (window-priority (widget-window master)) :above)
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
  (with-slots (master timestamp) title
    ;; for shading after double click.
    (with-slots (time send-event-p) event
      (unless send-event-p
	(when (< (- time timestamp) *double-click-speed*) (shade master))
	(setf timestamp time)))
    ;; the rest of the work.
    (setf (decoration-active-p master) nil)
    (when (eq *focus-type* :on-click)
      (with-slots (input-model window) (get-child master :application)
	(set-focus input-model window (event-time event))))))

;;; events for an icon

(defmethod event-process ((event exposure) (icon icon))
  (repaint icon (theme-name (root-decoration-theme *root*)) nil))

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
