;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: wm.lisp,v 1.9 2003/03/19 10:15:23 hatchond Exp $
;;;
;;; ECLIPSE. The Common Lisp Window Manager.
;;; Copyright (C) 2000, 2001, 2002 Iban HATCHONDO
;;; Copyright (C) 2000
;;;          Julien BONINFANTE,
;;;          Aymeric LACORTE,
;;;          Jocelyn FRECHOT
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

;; usefull for having a quick and short garbadge collection.
#+:cmu (setf extensions:*bytes-consed-between-gcs* 400000
	     extensions:*gc-verbose* nil)

(in-package :ECLIPSE-INTERNALS)

;;;; Decoration

(defclass decoration (base-widget)
  ((children :initarg :children :accessor decoration-children)
   (active-p :initform nil :accessor decoration-active-p)
   (maximized :initform nil :accessor decoration-maximized)
   (time :initform 0 :accessor decoration-precedent-time :allocation :class)
   (initial-geometry :initform (make-geometry))
   (wm-size-hints :initarg :wm-size-hints :reader decoration-wm-size-hints)
   (frame-style :initarg :frame-style :accessor decoration-frame-style)
   (application-gravity 
     :initarg :application-gravity
     :initform :north-west
     :accessor decoration-application-gravity)
   ))

(defconstant +decoration-event-mask+
  '(:substructure-notify :visibility-change :enter-window :owner-grab-button))

(defmethod get-child ((master decoration) label &key window)
  (let ((widget (getf (decoration-children master) label)))
    (if (and widget window) (widget-window widget) widget)))

(defmethod decoration-min-width ((master decoration))
  (with-slots (hmargin) (decoration-frame-style master)
    (+ hmargin (aref (slot-value master 'wm-size-hints) 0))))
(defmethod decoration-min-height ((master decoration))
  (with-slots (vmargin) (decoration-frame-style master)
    (+ vmargin (aref (slot-value master 'wm-size-hints) 1))))
(defmethod decoration-max-width ((master decoration))
  (with-slots (hmargin) (decoration-frame-style master)
    (+ hmargin (aref (slot-value master 'wm-size-hints) 2))))
(defmethod decoration-max-height ((master decoration))
  (with-slots (vmargin) (decoration-frame-style master)
    (+ vmargin (aref (slot-value master 'wm-size-hints) 3))))
(defmethod decoration-base-width ((master decoration))
  (with-slots (hmargin) (decoration-frame-style master)
    (+ hmargin (aref (slot-value master 'wm-size-hints) 6))))
(defmethod decoration-base-height ((master decoration))
  (with-slots (vmargin) (decoration-frame-style master)
    (+ vmargin (aref (slot-value master 'wm-size-hints) 7))))
(defmethod decoration-inc-sizes ((master decoration))
  (with-slots (wm-size-hints) master
    (values (aref wm-size-hints 4) (aref wm-size-hints 5))))

(defmethod decoration-wm-hints ((master decoration))
  (with-slots (wm-size-hints) master
    (values (decoration-min-width master) (decoration-min-height master)
	    (decoration-max-width master) (decoration-max-height master)
	    (aref wm-size-hints 4) (aref wm-size-hints 5)
	    (decoration-base-width master) (decoration-base-height master))))

(defmethod focused-p ((master decoration))
  (with-slots (window) (get-child master :application)
    (xlib:window-equal window (xlib:input-focus *display*))))

(defmethod focus-widget ((master decoration) timestamp)
  (with-slots (window input-model) (get-child master :application)
    (set-focus input-model window timestamp)))

(defmethod (setf decoration-frame-style) :after (frame-style (master decoration))
  (with-slots (window children) master
    (with-slots (left-margin top-margin hmargin vmargin) frame-style
      (with-event-mask (window)
	(loop with app = nil and icon = nil
	      for (key val) on children by #'cddr
	      do (case key
		   (:application (setf app val))
		   (:icon (setf icon val))
		   (t (when (typep val 'edge)		    
			(xlib:destroy-window (widget-window val)))
		      (remove-widget val)))
	      finally (setf children (list :application app :icon icon)))
	(let* ((app-window (widget-window (getf children :application)))
	       (width (+ (xlib:drawable-width app-window) hmargin))
	       (height (+ (xlib:drawable-height app-window) vmargin)))
	  (setf (window-position app-window) (values left-margin top-margin)
		(drawable-sizes window) (values width height))
	  (make-edges master)
	  (make-corner master width height)
	  (make-title-bar master (wm-name app-window))
	  (update-edges-geometry master)
	  (xlib:map-subwindows window))))))

(defun decoration-p (widget)
  (typep widget 'decoration))

(defun draw-focused-decoration (master)
  (mapc #'(lambda (k) (draw-on-focus-in (get-child master k)))
	(style-parts-to-redraw-on-focus (decoration-frame-style master))))

(defun draw-unfocused-decoration (master)
  (mapc #'(lambda (k) (draw-on-focus-out (get-child master k)))
	(style-parts-to-redraw-on-focus (decoration-frame-style master))))

(defun title-bar-horizontal-p (master)
  (eq :horizontal (style-title-bar-direction (decoration-frame-style master))))

(defun make-menu-button (master parent-window)
  (with-slots (children frame-style gcontext) master
    (when (frame-item-exist-p frame-style :menu-button)
      (let ((pixmaps (frame-item-pixmaps frame-style :menu-button))
	    (horizontal-p (title-bar-horizontal-p master)))
	(multiple-value-bind (width height) (drawable-sizes (aref pixmaps 0))
	  (multiple-value-bind (tw th) (drawable-sizes parent-window)
	    (setf (getf children :menu-button)
		  (create-button 
		     'menu-button
		     :parent parent-window :master master
		     :background (aref pixmaps 0) 
		     :item (aref pixmaps 1)
		     :width width :height height
		     :event-mask '(:owner-grab-button . #.+push-button-mask+)
		     :gcontext gcontext
		     :x (if horizontal-p 0 (ash (- tw width) -1))
		     :y (if horizontal-p (ash (- th height) -1) 0)))))))))

(defun make-buttons-bar (master parent-window)
  (with-slots (children frame-style gcontext) master
    (flet ((make-container (horizontal-p)
	     (xlib:create-window 
	        :parent parent-window
		:x 0 :y 0 :width 1 :height 1 
		:background :parent-relative 
		:gravity (if horizontal-p :north-east :north-west))))
      (loop initially (when (zerop (style-nb-buttons frame-style)) (return))
	    with horizontal-p = (title-bar-horizontal-p master)
	    with container = (make-container horizontal-p)
	    and (x y width height) = '(0 0 0 0)
	    for type in '(iconify-button maximize-button close-button)
	    for child in '(:icon-b :maximize :close)
	    for pixmaps = (frame-item-pixmaps frame-style child)
	    for bkgrd = (aref pixmaps 0)
	    when (frame-item-exist-p frame-style child)
	    do (multiple-value-setq (width height) (drawable-sizes bkgrd))
	       (setf (getf children child)
		     (create-button type
				    :parent container :master master
				    :background bkgrd :item (aref pixmaps 1)
				    :x x :y y :width width :height height
				    :event-mask +push-button-mask+
				    :gcontext gcontext))
	       (if horizontal-p (incf x width) (incf y height))
	    finally 
	      (multiple-value-bind (w h) (drawable-sizes parent-window)
		(if (zerop x) (incf x width) (incf y height))
		(setf (drawable-sizes container) (values x y)
		      (window-position container)
		      (if horizontal-p
			  (values (- w x) (ash (- h y) -1))
			  (values (ash (- w x) -1) 0)))
		(xlib:map-subwindows container)
		(return container))))))	    

(defun make-title-bar (master name)
  (with-slots (children frame-style gcontext) master
    (let* ((title-pos (style-title-bar-position frame-style))
	   (horizontal-p (case title-pos ((:top :bottom) t)))
	   (widget (getf children title-pos))
	   (parent (widget-window widget))
	   (button-container (make-buttons-bar master parent))
	   (menu-button (make-menu-button master parent))
	   (pixmaps (frame-item-pixmaps frame-style title-pos))
	   (bcw 0) (bch 0) (mbw 0) (mbh 0) (title))
      (when button-container
	(multiple-value-setq (bcw bch) (drawable-sizes button-container)))
      (when menu-button
	(multiple-value-setq (mbw mbh)
	  (drawable-sizes (widget-window menu-button))))
      (setf title
	    (create-button 'title-bar :parent parent :master master
			   :width 1 :height 1
			   :x (if horizontal-p mbw 0)
			   :y (if horizontal-p 0 bch)
			   :gcontext gcontext :event-mask +push-button-mask+
			   :background (aref pixmaps 0) :item name)
	    (slot-value title 'parent) parent
	    (getf children :title-bar) title
	    (xlib:window-background parent) :parent-relative
	    (xlib:window-event-mask parent) nil)
      (if horizontal-p 
	  (setf (slot-value title 'hmargin) (+ bcw mbw))
	  (setf (slot-value title 'vmargin) (+ bch mbh)))
      (xlib:map-subwindows parent)
      title)))

(defun update-title-bar-sizes (title-bar)
  (with-slots (parent vmargin hmargin window) title-bar
    (multiple-value-bind (width height) (drawable-sizes parent)
      (setf (drawable-sizes window)
	    (values (- width hmargin) (- height vmargin))))))

(defun edge-position (style edge-key w h)
  (with-slots (top-left-w top-left-h top-right-h bottom-left-w) style
    (case edge-key
      (:top (values top-left-w 0))
      (:right (values (- w (frame-item-width style :right)) top-right-h))
      (:bottom (values bottom-left-w (- h (frame-item-height style :bottom))))
      (:left (values 0 top-left-h)))))

(defun make-edges (master)
  (with-slots (children window frame-style gcontext) master
    (multiple-value-bind (width height) (drawable-sizes window)
      (loop for type in '(right left top bottom)
	    for child in '(:right :left :top :bottom)
	    for gravity in '(:north-east :north-west :north-west :south-west)
	    for cursor in +side-cursor+
	    for pixmaps = (frame-item-pixmaps frame-style child)
	    for background = (aref pixmaps 0)
	    for hilighted = (aref pixmaps 1)
	    for event-mask = (if hilighted +std-button-mask+ +edge-event-mask+)
	    for (x y) = (multiple-value-list 
			   (edge-position frame-style child width height))
	    when (frame-item-exist-p frame-style child)
	    do (setf (getf children child)
		     (create-button type
				    :parent window :master master
				    :gravity gravity
				    :background background
				    :item hilighted 
				    :event-mask event-mask
				    :x x :y y
				    :width (pixmap-width background)
				    :height (pixmap-height background)
				    :gcontext gcontext :cursor cursor))))))

(defun make-corner (master width height)
  (with-slots (children window frame-style gcontext) master
    (loop for type in '(top-left top-right bottom-left bottom-right)
	  for gravity in '(:north-west :north-east :south-west :south-east)
	  for child in '(:top-left :top-right :bottom-left :bottom-right)
	  for cursor in +corner-cursor+
	  for pixmaps = (frame-item-pixmaps frame-style child)
	  for bkgrd = (aref pixmaps 0)
	  for hilighted = (aref pixmaps 1)
	  for event-mask = (if hilighted +std-button-mask+ +edge-event-mask+)
	  for (w h) = (and bkgrd (multiple-value-list (drawable-sizes bkgrd)))
	  for (x . y) in '((0 . 0) (nil . 0) (0 . nil) (nil . nil))
	  when (frame-item-exist-p frame-style child)
	  do (setf (getf children child)
		   (create-button type
				  :parent window :master master
				  :background bkgrd :item hilighted
				  :event-mask event-mask
				  :gravity gravity
				  :x (or x (- width w))
				  :y (or y (- height h))
				  :width w :height h
				  :gcontext gcontext :cursor cursor)))))

(defun update-edges-geometry (master)
  (multiple-value-bind (w h) (drawable-sizes (widget-window master))
    (let* ((top (get-child master :top :window t))
	   (l (get-child master :left :window t))
	   (r (get-child master :right :window t))
	   (b (get-child master :bottom :window t))
	   (title (get-child master :title-bar)))
      (with-slots (top-left-w top-left-h top-right-w top-right-h
		   bottom-right-w bottom-right-h bottom-left-w bottom-left-h)
	  (decoration-frame-style master)
	(when l
	  (setf (xlib:drawable-height l) (- h top-left-h bottom-left-h)))
	(when r 
	  (setf (xlib:drawable-height r) (- h top-right-h bottom-right-h)))
	(when b
	  (setf (xlib:drawable-width b) (- w bottom-left-w bottom-right-w)))
	(when top
	  (setf (xlib:drawable-width top) (- w top-left-w top-right-w))))
      (update-title-bar-sizes title))))

(defun recompute-wm-normal-hints (window hmargin vmargin)
  (let ((hints (or (ignore-errors (xlib:wm-normal-hints window))
		   (xlib:make-wm-size-hints)))
	(max-ww (screen-width))
	(max-hh (screen-height)))
    (symbol-macrolet ((min-w (xlib:wm-size-hints-min-width hints))
		      (min-h (xlib:wm-size-hints-min-height hints))
		      (max-w (xlib:wm-size-hints-max-width hints))
		      (max-h (xlib:wm-size-hints-max-height hints))
		      (inc-w (xlib:wm-size-hints-width-inc hints))
		      (inc-h (xlib:wm-size-hints-height-inc hints))
		      (base-w (xlib:wm-size-hints-base-width hints))
		      (base-h (xlib:wm-size-hints-base-height hints))
		      (g (xlib:wm-size-hints-win-gravity hints)))
       (unless (eq g :static)
	 (decf max-ww hmargin)
	 (decf max-hh vmargin))
       (setf min-w (max (or (or min-w base-w) 1) 1)
	     min-h (max (or (or min-h base-h) 1) 1)
	     base-w (max (or (or base-w min-w) 1) 1)
	     base-h (max (or (or base-h min-h) 1) 1)
	     inc-w (or inc-w 1) 
	     inc-h (or inc-h 1))
      (setf max-ww (- max-ww (mod (- max-ww base-w) inc-w))
	    max-hh (- max-hh (mod (- max-hh base-h) inc-h)))
      (unless (and max-w (<= min-w max-w max-ww)) (setf max-w max-ww))
      (unless (and max-h (<= min-h max-h max-hh)) (setf max-h max-hh))
      (setf min-w (min min-w max-w)
	    min-h (min min-h max-h)
	    base-w (min base-w max-w)
	    base-h (min base-h max-h))
      (multiple-value-bind (w h) (drawable-sizes window)
	(unless (<= min-w w max-w) 
	  (setf (xlib:drawable-width window) (min (max min-w w) max-w)))
	(unless (<= min-h h max-h) 
	  (setf (xlib:drawable-height window) (min (max min-h h) max-h))))
      (values g (vector min-w min-h max-w max-h inc-w inc-h base-w base-h)))))

(defun make-decoration (application-window application)
  (let* ((theme (root-decoration-theme *root*))
	 (style (find-decoration-frame-style theme application-window)))
    (with-slots (hmargin vmargin left-margin top-margin) style
      (multiple-value-bind (gravity wm-sizes) 
	  (recompute-wm-normal-hints application-window hmargin vmargin)
	(multiple-value-bind (x y width height) 
	    (window-geometry application-window)
	  (let* ((window (xlib:create-window
			    :parent *root-window*
			    :width (+ width hmargin)
			    :height (+ height vmargin)
			    :background :parent-relative
			    :event-mask +decoration-event-mask+
			    :x (max 0 (- x left-margin))
			    :y (max 0 (- y top-margin))
			    :do-not-propagate-mask 
			    '(:button-press :button-release)))
		 (master (make-instance 
			     'decoration
			     :window window
			     :frame-style style
			     :gcontext *gcontext*
			     :children (list :application application)
			     :application-gravity gravity
			     :wm-size-hints wm-sizes)))
	    (make-edges master)
	    (make-corner master (+ width hmargin) (+ height vmargin))
	    (make-title-bar master (wm-name application-window))
	    (update-edges-geometry master)
	    master))))))

(defun decore-application (window application &key (map t))
  (let* ((master (make-decoration window application))
	 (master-window (widget-window master))
	 (left-margin (style-left-margin (decoration-frame-style master)))
	 (top-margin (style-top-margin (decoration-frame-style master))))
    (with-slots (icon) application
      (setf (getf (decoration-children master) :icon) icon
	    (slot-value icon 'master) master
	    (slot-value application 'master) master
	    (xlib:drawable-border-width window) 0))
    (with-event-mask (master-window)
      (xlib:map-subwindows master-window))
    (with-event-mask (master-window (when map +decoration-event-mask+))
      (xlib:reparent-window window master-window left-margin top-margin))
    (when map (xlib:map-window window))
    master))

(defun maximize-window (master button-code)
  (with-slots (maximized window initial-geometry wm-size-hints) master
    (let* ((app (get-child master :application))
	   (app-window (widget-window app))
	   (prop (ignore-errors (netwm:net-wm-state app-window)))
	   (full-screen-p (member :_net_wm_state_fullscreen prop))
	   (max-w (aref wm-size-hints 2))
	   (max-h (aref wm-size-hints 3))
	   new-sizes)
      (multiple-value-bind (x y) (window-position window)
	(multiple-value-bind (w h) (drawable-sizes app-window)
	  (unless maximized 
	    (if full-screen-p
		(setf initial-geometry (slot-value app 'initial-geometry))
		(setf (geometry initial-geometry) (values x y w h))))
	  (case button-code
	    ;; Unmaximize or Maximize in both directions
	    (1 (if maximized
		   (setf new-sizes initial-geometry
			 maximized nil)
		   (setf maximized (list :vertical t :horizontal t) 
			 new-sizes (make-geometry :w max-w :h max-h))))
	    ;; Unmaximize or Maximize Vertically
	    (2 (if (remf maximized :vertical)
		   (setf new-sizes 
			 (make-geometry :x x :y (geometry-y initial-geometry)
					:w w :h (geometry-h initial-geometry)))
		   (setf (getf maximized :vertical) t
			 new-sizes (make-geometry :x x :w w :h max-h))))
	    ;; Unmaximize or Maximize Horizontally
	    (3 (if (remf maximized :horizontal)
		   (setf new-sizes 
			 (make-geometry :x (geometry-x initial-geometry) :y y
					:w (geometry-w initial-geometry) :h h))
		   (setf (getf maximized :horizontal) t
			 new-sizes (make-geometry :y y :w max-w :h h)))))))
      (if full-screen-p
	  (setf (slot-value app 'initial-geometry) new-sizes)
	  (setf (window-position window) (geometry-coordinates new-sizes)
		(drawable-sizes app-window) (geometry-sizes new-sizes)))
      (if (getf maximized :vertical)
	  (pushnew :_net_wm_state_maximized_vert prop)
	  (setf prop (delete :_net_wm_state_maximized_vert prop)))
      (if (getf maximized :horizontal)
	  (pushnew :_net_wm_state_maximized_horz prop)
	  (setf prop (delete :_net_wm_state_maximized_horz prop)))
      (setf (netwm:net-wm-state app-window) prop))))

;;;; Focus management. According to ICCCM

(defgeneric set-focus (input-model window timestamp))

(defmethod set-focus ((input-model (eql :globally-active)) window timestamp)
  (send-wm-protocols-client-message window :wm_take_focus timestamp))

(defmethod set-focus ((input-model (eql :locally-active)) window timestamp)
  (send-wm-protocols-client-message window :wm_take_focus timestamp)
  (xlib:set-input-focus *display* window :parent))

(defmethod set-focus ((input-model (eql :passive)) window timestamp)
  (declare (ignorable timestamp))
  (xlib:set-input-focus *display* window :parent))

(defmethod set-focus ((input-model (eql :no-input)) window timestamp)
  (declare (ignorable window timestamp))
  (xlib:set-input-focus *display* :pointer-root :pointer-root)
  (values))

;; Next is methods for menu-3 who permit to manage any window :
;;  choose an action in the menu and click on a window
;;  to perform this action.

;; protocol for treating events
(defgeneric menu-3-process (event widget &rest rest))

(defmethod menu-3-process (event widget &rest rest)
  (declare (ignorable rest))
  (event-process event widget)
  nil)

(defmethod menu-3-process ((event pointer-event) (w base-widget) &rest rest)
  (declare (ignore rest))
  (xlib:ungrab-pointer *display*)
  t)

(defmethod menu-3-process ((ev motion-notify) (master decoration) &key key)
  (when (decoration-active-p master)
    (cond ((eql key :resize)
	   (with-slots (root-x root-y) ev
	     (find-corner root-x root-y (widget-window master)))
	   (event-process ev (get-child master :bottom-right)))
	  ((eql key :move) (event-process ev (get-child master :title-bar))))
    t))

(defmethod menu-3-process ((event enter-notify) (master decoration) &rest rest)
  (declare (ignore rest))
  (with-slots (window) master
    (xlib:grab-pointer window +pointer-event-mask+ :cursor *cursor-2*))
  nil)

(defmethod menu-3-process ((event leave-notify) (master decoration) &rest rest)
  (declare (ignore rest))
  (xlib:ungrab-pointer *display*)
  nil)

(defmethod menu-3-process ((ev button-press) (master decoration) &key key)
  (case key
    (:kill (kill-client-window (get-child master :application :window t)))
    (:close (close-widget (get-child master :application)))
    (:resize (event-process ev (get-child master :bottom-right)))
    (:move (event-process ev (get-child master :title-bar))))
  (when (or (eq key :close) (eq key :kill)) (xlib:ungrab-pointer *display*)))

(defun define-menu-3 (action)
  (lambda ()
    (with-root-cursor (*cursor-2*)
      (destroy-substructure (slot-value *root* 'menu3))
      (loop for event = (get-next-event *display* :force-output-p t)
	    for widget = (lookup-widget (event-event-window event))
	    until (menu-3-process event widget :key action)))))

;;;; Misc.

(defun put-on-top (window)
  (let ((widget (lookup-widget window)))
    (when (application-p widget)
      (unless (eq *focus-type* :none)
	(set-focus (application-input-model widget) window nil))
      (when (application-master widget)
	(setf window (widget-window (application-master widget)))))
    (setf (xlib:window-priority window) :above)))

(defun raise-window (window vs)
  (lambda ()
    (case (first (wm-state window))
      (1 (unless (eq (vs:current-screen vs) (gnome:win-workspace window))
	   (change-vscreen vs nil (gnome:win-workspace window))))
      (3 (uniconify (slot-value (lookup-widget window) 'icon))))
    (put-on-top window)))

(defun make-menu-button-menu (master)
  (let ((appli (get-child master :application))
	(menu))
    (with-slots (window desktop-number) appli      
      (macrolet ((add (pair) `(push ,pair menu)))
	(labels ((send-message (n)
		   (let ((message (make-event :client-message
					      :type :_net_wm_desktop
					      :data (vector n))))
		     (lambda () (event-process message appli))))
		 (sendTO ()
		   (loop with names = (workspace-names *root-window*)
			 for i from 0 below *nb-vscreen*
			 for s = (or (pop names) (format nil "workspace ~a" i))
			 unless (= i (current-desk))
			 collect (cons s (send-message i))))
		 (maximize () 
		   (if (decoration-maximized master) "Unmaximize" "Maximize")))
	  (add (cons "Iconify" (lambda () (iconify appli))))
	  (add (cons "Destroy" (lambda () (kill-client-window window))))
	  (add (cons "Close  " (lambda () (close-widget appli))))
	  (add (cons (maximize) (lambda () (maximize-window master 1))))
	  (if (= desktop-number +any-desktop+)
	      (add (cons "Un-pin" (send-message (current-desk))))
	      (add (cons "Pin   " (send-message +any-desktop+))))
	  (add (cons "Send to" (sendTO))))))
    (apply #'make-pop-up *root* menu)))

(defun make-desk-entries (vscreens index)
  (loop for win across (aref (vs:vscreens vscreens) index)
	for state = (= 1 (first (wm-state win)))
	collect (cons (format nil "~:[[ ~A ]~;~A~]" state (wm-name win))
		      (raise-window win vscreens))))

(defun make-running-menu (vscreens)
  (loop with names = (workspace-names *root-window*)
	for i from 0 below (vs:number-of-virtual-screen vscreens)
	for name = (or (pop names) (format nil "workspace ~A" i))
	collect (cons name (make-desk-entries vscreens i)) into entries
	finally (return (apply #'make-pop-up *root* entries))))

;; This is for updating:
;;  - the root-properties win_client_list, net_client_list(_stacking).
;;  - the virtual screens contents
(defun update-lists (app state root)
  (macrolet ((skip (window property-getter key)
	       `(cond ((= state 0) :remove)
		      ((member ,key (,property-getter ,window)) :remove)
		      (t :append))))
    (with-slots (window iconic-p) app
      (when (and (= state 3) (not iconic-p)) (setf state 0))
      (let ((mode (skip window netwm:net-wm-state :_NET_WM_STATE_SKIP_TASKBAR))
	    (wcl-mode (skip window gnome:win-hints :WIN_HINTS_SKIP_WINLIST))
	    (n (application-desktop-number app)))
	(unless (eq mode :remove)
	  (setf mode (skip window gnome:win-hints :WIN_HINTS_SKIP_TASKBAR)))
	(if (eq mode :remove)
	    (vs:remove-from-all (root-vscreens root) window)
	    (add-to-vscreen (root-vscreens root) window :n n))
	(with-slots (client-list) root
	  (unless (and (eql wcl-mode :append) (gethash window client-list))
	    (setf (netwm:net-client-list *root-window* :mode wcl-mode) window
		  (gnome:win-client-list *root-window* :mode wcl-mode) window))
	  (if (= state 0) 
	      (remhash window client-list)
	      (setf (gethash window client-list) window))
	  (loop with root = *root-window*
		for win in (query-application-tree)
		when (gethash win client-list) collect win into wins
		finally	(setf (netwm:net-client-list-stacking root) wins)))))))

(defun procede-decoration (win vscreens)
  (unwind-protect 
    (let ((application (create-application win nil))
	  (win-workspace (or (gnome-desktop-num win) +any-desktop+))
	  (stick-p (stick-p win)))
      (xlib:add-to-save-set win)
      (unless (or stick-p (< -1 win-workspace *nb-vscreen*))
	(setf win-workspace (vs:current-screen vscreens)))
      (setf (window-desktop-num win) win-workspace)
      (cond ((eql (motif-wm-decoration win) :OFF)
	     (setf (wm-state win) 1)
	     (if (or (= win-workspace (vs:current-screen vscreens)) stick-p)
		 (xlib:map-window win)
		 (with-event-mask (*root-window*)
		   (xlib:unmap-window win))))
	    ((or (= win-workspace (vs:current-screen vscreens)) stick-p)
	     (decore-application win application))
	    (t (with-event-mask (*root-window*)
		 (decore-application win application :map nil)
		 (update-lists application 1 *root*))))
      (if (member :_net_wm_window_type_desktop (netwm:net-wm-window-type win))
	  (let* ((prec-desk (get-root-desktop *root* t))
		 (stack-mode (if prec-desk :above :below)))
	    (add-desktop-application *root* application)
	    (xlib:ungrab-button win :any :modifiers #x8000)
	    (setf (xlib:window-priority win prec-desk) stack-mode))
	  (with-slots (wants-focus-p input-model) application
	    (unless (eq input-model :no-input)	      
	      (setf wants-focus-p *focus-new-mapped-window*)))))))

;;;; The main loop.

(defun eclipse-internal-loop ()
  (let ((exit 0)
	(time))

    ;; Sets the root window pop-up menu
    (nconc *menu-1-items* (acons "Exit" (lambda () (setf exit 1)) '()))
    ; I want to disable the exit part for the moment.
    (setf *menu-1-items* (butlast *menu-1-items*)) 
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
		(member :_net_wm_state_hidden (netwm:net-wm-state window))
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
		(event-process event (lookup-widget event-window)))))
	  (when pt:preprogrammed-tasks (pt:execute-preprogrammed-tasks))
	  (case exit
	    (1 (loop for val being each hash-value in *widget-table*
		     do (close-widget val))
	       (setf time 10 exit 2))
	    (2 (return))))))
    (format t "Main loop exited~%")))
