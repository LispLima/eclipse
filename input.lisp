;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: input.lisp,v 1.52 2008-08-29 14:57:47 ihatchondo Exp $
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

(deftype client-message-data ()
  `(simple-array (or xlib:card8 xlib:card16 xlib:card32) (*)))

;; Most general methods.

(defmethod event-process ((event map-request) null-widget)
  ;; When a client has requested to withdraw its top-level and wants then
  ;; to map it back so quickly that we haven't got honored yet its withdrawal
  ;; demand, this event is in fact send to the wrong parent and should be re
  ;; directed to the real parent of the client top-level.
  ;; The events schema is: 
  ;;   1 -> map-request => procede-decoration
  ;;   2 -> unmap-notify => unmap master
  ;;   3 -> synthetic unmap-notify => undecore-application
  ;;   4 -> map-request 
  ;; What happen is that the client ask for mapping its top-level (4) after 
  ;; sending the synthetic unmap-notify (3), but WE have not handled it yet
  ;; (otherwise said undecore-application has not been called). So Xserver 
  ;; generates a map-request event for the known parent: US ! But as soon
  ;; as undecore-application will occurred we won't be the parent anymore
  ;; and this event should rather have been sent to the real parent 
  ;; (aka the root window).
  (with-slots (window (candidat event-window)) event
    (multiple-value-bind (children parent) (xlib:query-tree window)
      (declare (ignore children))
      (unless (xlib:window-equal candidat parent)
        (xlib:send-event parent :map-request
            '(:substructure-redirect) :window window :event-window parent)))))

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
	 (unregister-all-keystrokes)
	 (unregister-all-mouse-strokes)
	 (xlib:mapping-notify *display* request start count)
	 (register-all-keystrokes)
	 (register-all-mouse-strokes)))
      (:pointer nil))))

(defmethod event-process ((ev configure-request) (widget base-widget))
  (declare (ignorable widget))
  (with-slots (window value-mask x y width height above-sibling stack-mode) ev
    (configure-window window
      :x (and (logbitp 0 value-mask) x)
      :y (and (logbitp 1 value-mask) y)
      :width (and (logbitp 2 value-mask) width)
      :height (and (logbitp 3 value-mask) height)
      :stack-mode (and (logbitp 6 value-mask) stack-mode)
      :sibling above-sibling)))

(defmethod event-process :after ((event destroy-notify) (widget base-widget))
  (with-slots (window) event
    (with-slots ((caw current-active-widget)) *root*
      (when caw
	(if (or (eq widget caw) (eq (lookup-widget window) caw))
	    (dismiss-move-resize *root*)
	    (return-from event-process nil))))
    (when (or (decoration-p widget) (application-p (lookup-widget window)))
      (if (eq *focus-type* :on-click)
	  (give-focus-to-next-widget-in-desktop) 
	  (multiple-value-bind (x y s child)
              (xlib:query-pointer (xlib:drawable-root (widget-window widget)))
	    (declare (ignore x y s))
	    (let ((e (make-event :enter-notify :kind :nonlinear :mode :normal)))
	      (event-process e (or (lookup-widget child) *root*))))))))

(defmethod event-process ((event client-message) window)
  (declare (ignorable window))
  (with-slots ((window event-window)) event
    (case (event-type event) 
      (:_net_request_frame_extents
       (with-slots (left-margin right-margin top-margin bottom-margin)
	   (find-decoration-frame-style
	       (if (window-not-decorable-p window)
		   (lookup-theme "no-decoration")
		   (root-decoration-theme *root*))
	       window)
	 (setf (netwm:net-frame-extents window)
	       (values left-margin right-margin top-margin bottom-margin)))))))

;; Specialized ones.

(defmethod event-process ((e selection-clear) (w standard-property-holder))
  ;; handle the selection clear to stop window managing (see ICCCM 2.8).
  (declare (ignorable w))
  (when (string= (event-selection e) +xa-wm+)
    (error 'exit-eclipse)))

(defmethod event-process ((e property-notify) (w standard-property-holder))
  (with-slots (atom time state event-window) e
    (when (and (eql atom :_net_active_window) (eql state :new-value))
      (let* ((window (netwm:net-active-window event-window :window-list t))
	     (widget (lookup-widget window)))
	(when (application-p widget)
	  (set-focus (application-input-model widget) window time))))))

;;; Events for the root window

(defmethod event-process ((event map-request) (root root))
  (if (lookup-widget (event-window event))
      (with-slots (icon) (lookup-widget (event-window event))
	(when icon (uniconify icon))
	(xlib:map-window (event-window event)))
      (procede-decoration (event-window event))))

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
         (let ((application (get-child widget :application)))
           (if (application-iconic-p application)
               (setf (wm-state (widget-window application)) 3)
               (with-slots (window send-event-p) event
                 (setf send-event-p t)
                 (setf window (widget-window application))
                 (format t "about to withdraw: ~a ~%" (wm-name window))
                 (event-process event root)))))))))

(defmethod event-process ((event destroy-notify) (root root))
  (let ((app (lookup-widget (event-window event))))
    (when (and (application-p app) (not (application-master app)))
      (remove-widget app))))

(defmethod event-process ((event enter-notify) (root root))
  (with-slots (resize-status move-status) root
    (with-slots (kind mode) event
      (when (and (eq kind :inferior) (or (eq mode :normal) (eq mode :ungrab)))
	(unless (or (eq *focus-type* :on-click) resize-status move-status)
	  (focus-widget root nil))))))

(defmethod event-process ((event focus-in) (root root))
  (when (eql (event-kind event) :pointer)
    (if (eql *focus-type* :on-click)
	(give-focus-to-next-widget-in-desktop)
        (setf (netwm:net-active-window (widget-window root)) :none))))

(defmethod event-process ((event client-message) (root root))
  (let ((data (event-data event)))
    (declare (type client-message-data data))
    (case (event-type event)
      ((or :_win_workspace :_net_current_desktop)
       (change-vscreen root :n (aref data 0)))
      (:_net_number_of_desktops 
       (setf (number-of-virtual-screens) (aref data 0)))
      (:wm_protocols
       (when (eq :wm_delete_window (id->atom-name (aref data 0)))
	 (close-widget (lookup-widget (event-event-window event))))))))

(defmethod event-process ((event keyboard-event) (root root))
  (with-slots (code state) event
    (let ((callback (lookup-keystroke code state)))
      (when callback
	(xlib:allow-events *display* :async-keyboard)
	(funcall (the function callback) event)))))

(defmethod event-process :around ((event pointer-event) (root root))
  (with-slots (code state) event
    (let ((callback (lookup-mouse-stroke code state)))
      (if callback 
	  (unwind-protect (funcall (the function callback) event)
	    (xlib:allow-events *display* :async-pointer))
	  (and (next-method-p) (call-next-method))))))

(defmethod event-process ((event button-press) (root root))
  (with-slots (menu1 menu2 menu3 resize-status move-status) root
    (with-slots (code x y) event
      (when (and (eql resize-status move-status) (< 0 code 4)) ; nil nil 1|2|3
	(when (= 2 (event-code event))
	  (when menu2 (destroy-substructure menu2))
	  (setf menu2 (make-running-menu root)))
	(realize-pop-up (case code (1 menu1) (2 menu2) (3 menu3)) x y)))))

(defmethod event-process ((event motion-notify) (root root))
  (declare (optimize (speed 3)))
  (with-slots (move-status resize-status (widget current-active-widget)) root
    (when (or move-status resize-status)
      (if (slot-value widget 'active-p)
	  (when (event-hint-p event)
	    (cond (move-status
		   (move-widget widget event *verbose-move* *move-mode*))
		  (resize-status
		   (resize widget event *verbose-resize* *resize-mode*)))
	    (xlib:query-pointer (widget-window root)))
	  (progn
	    (format t "The pointer has been frozen !!~%")
	    (setf (slot-value widget 'active-p) t)
	    (event-process (make-event :button-release) root))))))

(defmethod event-process ((event button-release) (root root))
  (with-slots (move-status resize-status (widget current-active-widget)
	       menu1 menu2 menu3 window-menu) root
    (cond (move-status (finish-move widget *verbose-move* *move-mode*))
	  (resize-status (finish-resize widget *verbose-resize* *resize-mode*))
	  (t
	   (with-slots (code) event
	     (when window-menu
	       (destroy-substructure window-menu)
	       (setf window-menu nil))
	     (when (< 0 code 4)
	       (destroy-substructure (case code (1 menu1) (2 menu2) (3 menu3)))
	       (when (= code 2) (setf menu2 nil))))))))

(defmethod event-process :after ((event button-release) (root root))
  (with-slots (move-status resize-status current-active-widget) root
    (when (or move-status resize-status)
      (setf (slot-value current-active-widget 'active-p) nil)
      (dismiss-move-resize root))))

;;; Events for master (type: decoration)

(defmethod event-process ((event map-request) (master decoration))
  (xlib:map-window (event-window event)))

(defmethod event-process ((event configure-notify) (master decoration))
  (with-slots ((master-window event-window) (app-window window)) event
    (when (application-p (lookup-widget app-window))
      (with-window-gravity 
	  (master-window (decoration-application-gravity master))
	(resize-from (lookup-widget app-window)))
      (with-event-mask (master-window)
	(update-edges-geometry master)))))

(defmethod event-process ((event reparent-notify) (master decoration))
  (unless (xlib:window-equal (event-event-window event) (event-parent event))
    (event-process (make-event :destroy-notify) master)))

(defmethod event-process ((event unmap-notify) (master decoration))
  (xlib:unmap-window (widget-window master)))

(defmethod event-process ((event map-notify) (master decoration))
  (with-slots ((app-window window) (master-window event-window)) event
    (when (application-p (lookup-widget app-window))
      (unless (eq (xlib:window-map-state app-window) :unmapped)
	(unmap-icon-window (get-child master :icon))
	(xlib:map-window master-window)
	(setf (window-desktop-num app-window)
	      (if (stick-p app-window) +any-desktop+ (current-desk)))
	(setf (window-priority master-window) :above
	      (wm-state app-window) 1)))))

(defmethod event-process ((event destroy-notify) (master decoration))
  (xlib:destroy-window (widget-window master))
  (mapc #'remove-widget (cons master (decoration-children master))))

(defmethod event-process ((event visibility-notify) (master decoration))
  (event-process event (get-child master :application)))

(defmethod event-process ((event enter-notify) (master decoration))
  (unless (eq (event-kind event) :inferior)
    (event-process event (get-child master :application))))

;;; Events for an application

(defmethod event-process ((event visibility-notify) (application application))
  (with-slots (wants-focus-p input-model window) application
    (when (and (not (eq (event-state event) :fully-obscured)) wants-focus-p)
      (set-focus input-model window 0)
      (setf wants-focus-p nil))))

(defmethod event-process ((event enter-notify) (application application))
  (with-slots (window input-model) application
    (unless (or (eq *focus-type* :on-click) (focused-p application))
      (set-focus input-model window (event-time event)))))

(defmethod event-process ((event button-press) (application application))
  (unwind-protect (put-on-top application)
    (xlib:allow-events *display* :replay-pointer)))

(defmethod event-process ((event focus-out) (application application))
  (with-slots (master) application
    (with-slots (mode kind) event
      (unless (or (not master) (eql mode :while-grabbed) (eql mode :grab))
	(unless (eql kind :inferior)
	  (dispatch-repaint master :focus nil))))))

(defmethod event-process ((event focus-in) (application application))
  (with-slots (master window) application
    (unless (eql (event-mode event) :grab)
      (when master (dispatch-repaint master :focus t))
      (setf (netwm:net-active-window (xlib:drawable-root window)) window)
      (xlib:delete-property
          (widget-window (root-property-holder *root*))
	  :_net_active_window))))

(defmethod event-process ((event property-notify) (app application))
  (with-slots (window master type transient-for initial-geometry) app
    (case (event-atom event)
      (:wm_normal_hints
       ;; recompute decoration wm-size-hints and initial-geometry.
       (when master
         (with-slots (hmargin vmargin) (decoration-frame-style master)
           (let ((old-wmsh (decoration-wm-size-hints master)))
             (with-slots (application-gravity (wmsh wm-size-hints)) master
               (multiple-value-setq (wmsh application-gravity)
		 (recompute-wm-normal-hints window hmargin vmargin))
               ;; wm-size-hints: '#(minw minh maxw maxh incw inch basew baseh).
               (symbol-macrolet ((minw (aref wmsh 0)) (minh (aref wmsh 1))
                                 (maxw (aref wmsh 2)) (maxh (aref wmsh 3))
                                 (incw (aref wmsh 4)) (inch (aref wmsh 5))
                                 (basew (aref wmsh 6)) (baseh (aref wmsh 7)))
                 (multiple-value-bind (w h) (geometry-sizes initial-geometry)
                   (let ((rw (/ (- w (aref old-wmsh 6)) (aref old-wmsh 4)))
                         (rh (/ (- h (aref old-wmsh 7)) (aref old-wmsh 5))))
                     (setf (geometry-w initial-geometry)
                           (max (min (+ (* rw incw) basew) maxw) minw))
                     (setf (geometry-h initial-geometry)
                           (max (min (+ (* rh inch) baseh) maxh) minh))))))))))
      ((:wm_name :_net_wm_name)
       (when (and master (get-child master :title-bar))
	 (with-slots (window item-to-draw) (get-child master :title-bar)
	   (setf item-to-draw (wm-name (widget-window app)))
	   (xlib:queue-event *display* :exposure :window window :count 0))))
      ((:_net_wm_strut_partial :_net_wm_strut)
       (when (member :_net_wm_window_type_dock type)
	 (update-workarea-property *root*)))
      (:wm_state (update-lists app (car (wm-state window)) *root*))
      (:wm_transient_for (computes-transient-for app)))))

(defmethod event-process ((event client-message) (application application))
  (let ((data (event-data event)))
    (declare (type client-message-data data))
    (with-slots (master window iconic-p icon) application
      (case (event-type event)
	(:wm_change_state (when (= 3 (aref data 0)) (iconify application)))
	(:_win_state
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
	   (when (logbitp 2 to-change) (maximize application 2))
	   ;; win_state_maximized_horiz
	   (when (logbitp 3 to-change) (maximize application 3))
	   ;; win_state_shaded
	   (when (and (logbitp 5 to-change) master) (shade master))))
	(:_net_wm_state
         (let ((mode (aref data 0))
	       (p (netwm:net-wm-state window))
	       (p1 (id->atom-name (aref data 1)))
	       (p2 (and (/= 0 (aref data 2)) (id->atom-name (aref data 2)))))
	   (when (= 2 mode) (return-from event-process nil)) ; toggle.
	   (when (= mode (if (member p1 p) 1 0)) (setf p1 nil))
	   (when (= mode (if (member p2 p) 1 0)) (setf p2 nil))
	   (flet ((set-netwm-state (s mode)
		    (setf (netwm:net-wm-state window)
			  (if (= 0 mode) (setf p (remove s p)) (pushnew s p)))))
	     (macrolet ((or-eql (val &rest vars)
			  `(or ,@(loop for v in vars collect `(eql ,v ,val)))))
	       (when (or-eql :_net_wm_state_hidden p1 p2)
		 (if (= mode 0) (uniconify application) (iconify application)))
	       (when (or-eql :_net_wm_state_fullscreen p1 p2)
		 (when (fullscreenable-p application)
		   (setf (fullscreen-mode application)
			 (if (= mode 0) :off :on))))
	       (when (or-eql :_net_wm_state_maximized_vert p1 p2)
		 (maximize application 2))
	       (when (or-eql :_net_wm_state_maximized_horz p1 p2)
		 (maximize application 3))
	       (when (and master (or-eql :_net_wm_state_shaded p1 p2))
		 (shade master))
	       (when (or-eql :_net_wm_state_sticky p1 p2)
		 (set-netwm-state :_net_wm_state_sticky mode))
	       (when (or-eql :_net_wm_state_above p1 p2)
		 (set-netwm-state :_net_wm_state_above mode)
		 (put-on-top application))
	       (when (or-eql :_net_wm_state_below p1 p2)
		 (set-netwm-state :_net_wm_state_below mode)
		 (put-on-bottom application))
               (when (or-eql :_net_wm_state_demands_attention p1 p2)
                 (set-netwm-state :_net_wm_state_demands_attention mode))))))
	(:_net_moveresize_window
	 (let ((value-mask (logand #x0F (ash (aref data 0) -8)))
	       (gravity (logand #xFF (aref data 0))))
	   (configure-window window
	     :x (when (logbitp 0 value-mask) (aref data 1))
	     :y (when (logbitp 1 value-mask) (aref data 2))
	     :width (when (logbitp 2 value-mask) (aref data 3))
	     :height (when (logbitp 3 value-mask) (aref data 4))
	     :gravity (unless (zerop gravity)
			(svref '#(:unmap :north-west :north :north-east :west
				  :center :east :south-west :south :south-east
				  :static) gravity)))))
	(:_net_restack_window
	 (let ((sibling (xlib::lookup-window *display* (aref data 1))))
	   (configure-window window :stack-mode :above :sibling sibling)))
	(:_net_active_window
	 (cond ((shaded-p application) (shade application))
	       (iconic-p (uniconify icon)))
	 (with-slots ((pwindow window)) (root-property-holder *root*)
	   (let* ((length (length data))
		  (time (if (> length 1) (aref data 1) 0))
		  (wtime (or (net-wm-user-time pwindow) 0)))
	     (unless (> wtime time 0)
	       ;;(setf (netwm:net-wm-user-time pwindow) time)
	       (focus-widget application time)
	       (put-on-top application)))))
	(:_net_wm_desktop (migrate-application application (aref data 0)))
	(:_net_close_window (close-widget application))))))

;;; Events for buttons

(defmethod event-process ((event exposure) (button button))
  (when (zerop (event-count event))
    (let* ((master (slot-value button 'master))
	   (name (if master 
		     (slot-value (decoration-frame-style master) 'name)
		     (theme-name (root-decoration-theme *root*)))))
      (repaint button name (and master (focused-p master))))))

(defmethod event-process ((event exposure) (box box-button))
  (repaint box (theme-name (root-decoration-theme *root*)) nil))

(defmethod event-process ((event button-release) (close close-button))
  (close-widget (get-child (button-master close) :application)))

(defmethod event-process ((event button-release) (icon-b iconify-button))
  (iconify (get-child (button-master icon-b) :application)))

(defmethod event-process ((event button-press) (button menu-button))
  (with-slots (window-menu) *root*
    (with-slots (master window armed active-p) button
      (when (eq *focus-type* :on-click)
	(focus-widget button (event-time event)))
      (and window-menu (destroy-substructure window-menu))
      (setf window-menu (make-menu-button-menu master))
      (realize-pop-up window-menu (event-root-x event) (event-root-y event))
      (setf armed nil active-p nil))))

;; Maximization
(defmethod event-process ((event button-release) (max-b maximize-button))
  (when (< (event-code event) 4)
    (with-slots (master) max-b
      (let ((state (event-state event)) 
	    (fill-p *maximize-fill*)
	    (mod (kb:modifier->modifier-mask *display* *maximize-modifer*)))
	(unless (eq 0 (logand mod state))		       
	  (setf fill-p (not *maximize-fill*)))
	(maximize master (event-code event) :fill-p fill-p)))
    (when (eq *focus-type* :on-click)
      (focus-widget max-b (event-time event)))))

;; Initialize the resize process.
(defmethod event-process ((event button-press) (edge edge))
  (with-slots (master) edge
    (unless (shaded-p master)
      (initialize-resize master edge event))))

;; Activate the resize process. (finally give the hand to root)
(defmethod event-process ((event motion-notify) (edge edge))
  (with-slots (master) edge
    (activate-move-resize
        master *root* 'resize-status *resize-mode* *verbose-resize*)))

(defmethod event-process ((event button-release) (edge edge))
  (with-slots (active-p) (button-master edge)
    (if active-p (setf active-p nil) (event-process event *root*))))

;; Initialize the move process.
(defmethod event-process ((event button-press) (title title-bar))
  (unless (event-send-event-p event)
    (initialize-move (button-master title) event)))

;; Start the movement.
(defmethod event-process ((event motion-notify) (title title-bar))
  (with-slots (master armed active-p) title
    (activate-move-resize master *root* 'move-status *move-mode* *verbose-move*)
    (setf armed nil active-p nil)))

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

;;; Events for an icon

(defmethod event-process ((event button-press) (icon icon))
  (setf (icon-desiconify-p icon) t)
  (setf (icon-priority icon) :above)
  (initialize-move icon event))

(defmethod event-process ((event motion-notify) (icon icon))
  (declare (optimize (speed 3)))
  (when (event-hint-p event)
    (move-widget icon event)
    (xlib:query-pointer (event-event-window event))
    (setf (icon-desiconify-p icon) nil)))

(defmethod event-process ((event button-release) (icon icon))
  (if (icon-desiconify-p icon)
      (uniconify icon)
      (setf (icon-priority icon) :below)))
      
;;; Events for Message Box

(defmethod event-process ((event visibility-notify) (box box-button))
  (setf (xlib:window-priority (widget-window box)) :above))
