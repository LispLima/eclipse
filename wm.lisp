;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: wm.lisp,v 1.18 2003/09/16 14:24:41 hatchond Exp $
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

;; usefull for having a quick and short garbage collection.
#+:cmu (setf extensions:*bytes-consed-between-gcs* 400000
	     extensions:*gc-verbose* nil)

(in-package :ECLIPSE-INTERNALS)

;;;; Decoration

(defclass decoration (base-widget)
  ((children :initarg :children :accessor decoration-children)
   (active-p :initform nil :accessor decoration-active-p)
   (time :initform 0 :accessor decoration-precedent-time :allocation :class)
   (wm-size-hints :initarg :wm-size-hints :reader decoration-wm-size-hints)
   (frame-style :initarg :frame-style :accessor decoration-frame-style)
   (application-gravity 
     :initarg :application-gravity
     :initform :north-west
     :accessor decoration-application-gravity)
   ))

(defun decoration-p (widget)
  (typep widget 'decoration))

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
  (focused-p (get-child master :application)))

(defmethod focus-widget ((master decoration) timestamp)
  (with-slots (window input-model) (get-child master :application)
    (set-focus input-model window timestamp)))

(defmethod shaded-p ((widget decoration))
  (with-slots (window) (get-child widget :application)
    (member :_net_wm_state_shaded (netwm:net-wm-state window))))

(defmethod shade ((master decoration))
  (with-slots (window frame-style) master
    (let* ((app-win (get-child master :application :window t))
	   (netwm-prop (netwm:net-wm-state app-win))
	   (gnome-prop (or (gnome:win-state app-win :result-type t) 0))
	   (shaded-p (member :_net_wm_state_shaded netwm-prop)))
      (setf gnome-prop (logand gnome-prop #x3DF)) ; supress :win_state_shaded
      (if shaded-p
	  (with-event-mask (window) 
	    ;; unshade.
	    (xlib:map-window app-win)
	    (resize-from (get-child master :application))
	    (setf netwm-prop (remove :_net_wm_state_shaded netwm-prop))
	    (setf netwm-prop (remove :_net_wm_state_hidden netwm-prop))
	    (unless (stick-p app-win)
	      (setf (window-desktop-num app-win) (current-desk))))
	  (with-event-mask (window)	    
	    ;; shade.
	    (with-slots ((vm vmargin) (hm hmargin)) frame-style
	      (if (title-bar-horizontal-p master)
		  (unless (= 0 vm) (setf (xlib:drawable-height window) vm))
		  (unless (= 0 hm) (setf (xlib:drawable-width window) hm))))
	    (xlib:unmap-window app-win)
	    (pushnew :_net_wm_state_shaded netwm-prop)
	    (pushnew :_net_wm_state_hidden netwm-prop)
	    (incf gnome-prop 32)))	; add win_state_shaded
      (setf (netwm:net-wm-state app-win) netwm-prop
	    (gnome:win-state app-win) gnome-prop))))

(defmethod (setf decoration-frame-style) :after (frame-style (master decoration))
  (with-slots (window children wm-size-hints) master
    (with-slots (left-margin top-margin (hm hmargin) (vm vmargin)) frame-style
      (with-event-mask (window)
	(let* ((application (getf children :application))
	       (icon (slot-value application 'icon))
	       (app-win (slot-value application 'window)))
	  (loop for (key val) on children by #'cddr
		unless (or (eq key :application) (eq key :icon))
		do (when (typep val 'edge)
		     (xlib:destroy-window (widget-window val)))
		   (remove-widget val))
	  (setf children (list :application application :icon icon)
		wm-size-hints (recompute-wm-normal-hints app-win hm vm))
	  (let ((width (+ (xlib:drawable-width app-win) hm))
		(height (+ (xlib:drawable-height app-win) vm)))
	    (setf (window-position app-win) (values left-margin top-margin)
		  (drawable-sizes window) (values width height))
	    (make-edges master)
	    (make-corner master width height)
	    (make-title-bar master (wm-name app-win))
	    (update-edges-geometry master)
	    (xlib:map-subwindows window))
	  (cond ((shaded-p application)
		 (if (title-bar-horizontal-p master)
		     (unless (= 0 vm) (setf (xlib:drawable-height window) vm))
		     (unless (= 0 hm) (setf (xlib:drawable-width window) hm)))
		 (xlib:unmap-window app-win))
		((application-iconic-p application)
		 (xlib:unmap-window app-win))))))))

(defmethod put-on-top ((widget decoration))
  (put-on-top (get-child widget :application)))

(defmethod put-on-bottom ((widget decoration))
  (put-on-bottom (get-child widget :application)))

(defmethod dispatch-repaint ((master decoration) 
			     &key (focus (focused-p master)))
  (declare (optimize (speed 3) (safety 1)))
  (with-slots (parts-to-redraw-on-focus name) (decoration-frame-style master)
    (declare (type string name))
    (mapc #'(lambda (k) (repaint (get-child master k) name focus))
	  parts-to-redraw-on-focus)))

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
		     :y (if horizontal-p (ash (- th height) -1) (- th height))
		     :x (if horizontal-p 0 (ash (- tw width) -1))))))))))

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
    (unless (eq :none (style-title-bar-position frame-style))
      (let* ((title-pos (style-title-bar-position frame-style))
	     (horizontal-p (case title-pos ((:top :bottom) t)))
	     (parent-widget (getf children title-pos))
	     (parent-window (widget-window parent-widget))
	     (button-container (make-buttons-bar master parent-window))
	     (menu-button (make-menu-button master parent-window))
	     (pixmaps (frame-item-pixmaps frame-style title-pos))
	     (bcw 0) (bch 0) (mbw 0) (mbh 0) (title))
	(when button-container
	  (multiple-value-setq (bcw bch) (drawable-sizes button-container)))
	(when menu-button
	  (multiple-value-setq (mbw mbh)
	    (drawable-sizes (widget-window menu-button))))
	(setf title
	      (create-button 'title-bar :parent parent-window :master master
			     :width 1 :height 1
			     :x (if horizontal-p mbw 0)
			     :y (if horizontal-p 0 bch)
			     :gcontext gcontext :event-mask +push-button-mask+
			     :background (aref pixmaps 0) :item name)
	      (slot-value title 'parent) parent-window
	      (getf children :title-bar) title
	      (xlib:window-background parent-window) :parent-relative
	      (xlib:window-event-mask parent-window) nil)
	(if horizontal-p 
	    (setf (slot-value title 'hmargin) (+ bcw mbw))
	    (setf (slot-value title 'vmargin) (+ bch mbh)))
	(xlib:map-subwindows parent-window)
	title))))

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
	    for cursor in +side-cursors+
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
	  for cursor in +corner-cursors+
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
  (declare (optimize (speed 3) (safety 0))
	   (inline update-edges-geometry))
  (macrolet
      ((update (edge size &rest frame-style-slots-size)
	 `(when ,edge
	    (setf (,(intern (format nil "DRAWABLE-~a" (symbol-name size)) :xlib)
		   (widget-window ,edge))
	          (with-slots (,@frame-style-slots-size) frame-style
		    (declare (type xlib:card16 ,@frame-style-slots-size))
		    (- ,size ,@frame-style-slots-size))))))
    (with-slots (frame-style window) master
      (multiple-value-bind (width height) (drawable-sizes window)
	(declare (type xlib:card16 width height))
	(update (get-child master :top) width top-left-w top-right-w)
	(update (get-child master :left) height top-left-h bottom-left-h)
	(update (get-child master :right) height top-right-h bottom-right-h)
	(update (get-child master :bottom) width bottom-left-w bottom-right-w)
	(update-title-bar-sizes (get-child master :title-bar))))))

(defun update-title-bar-sizes (title-bar)
  (declare (optimize (speed 3) (safety 0)))
  (when title-bar
    (with-slots (parent vmargin hmargin window) title-bar
      (declare (type xlib:card16 hmargin vmargin))
      (multiple-value-bind (width height) (drawable-sizes parent)
	(declare (type xlib:card16 width height))
	(setf (drawable-sizes window)
	      (values (- width hmargin) (- height vmargin)))))))

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
      (values (vector min-w min-h max-w max-h inc-w inc-h base-w base-h) g))))

(defun make-decoration (application-window application)
  (let* ((theme (root-decoration-theme *root*))
	 (style (find-decoration-frame-style theme application-window)))
    (with-slots (hmargin vmargin left-margin top-margin) style
      (multiple-value-bind (wm-sizes gravity) 
	  (recompute-wm-normal-hints application-window hmargin vmargin)
	(multiple-value-bind (x y width height) 
	    (window-geometry application-window)
	  (let* ((window (xlib:create-window
			    :parent (xlib:drawable-root application-window)
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

(defun maximize-window (application button-code)
  (with-slots ((app-window window) initial-geometry full-geometry master)
      application
    (when (shaded-p master) (shade master))
    (let* ((new-sizes)
	   (m-window (if master (widget-window master) app-window))
	   (prop (netwm:net-wm-state app-window))
	   (fullscreen-p (member :_net_wm_state_fullscreen prop))
	   (vert-p (member :_net_wm_state_maximized_vert prop))
	   (horz-p (member :_net_wm_state_maximized_horz prop))	   
	   (wm-size-hints (if master
			      (slot-value master 'wm-size-hints)
			      (recompute-wm-normal-hints app-window 0 0))))
      (multiple-value-bind (x y) (window-position m-window)
	(multiple-value-bind (w h) (drawable-sizes app-window)
	  (unless (or horz-p vert-p)
	    (if fullscreen-p
		(setf initial-geometry (copy-geometry full-geometry))
		(setf (geometry initial-geometry) (values x y w h))))
	  (symbol-macrolet ((ix (geometry-x initial-geometry))
			    (iy (geometry-y initial-geometry)) 
			    (iw (geometry-w initial-geometry))
			    (ih (geometry-h initial-geometry))
			    (maxw (aref wm-size-hints 2))
			    (maxh (aref wm-size-hints 3)))
	    (case button-code
	      ;; Unmaximize or Maximize in both directions
	      (1 (if (or horz-p vert-p)
		     (setf new-sizes (copy-geometry initial-geometry)
			   horz-p t vert-p t)
		     (setf new-sizes (make-geometry :w maxw :h maxh))))
	      ;; Unmaximize or Maximize Vertically
	      (2 (if vert-p
		     (setf new-sizes (make-geometry :x x :y iy :w w :h ih))
		     (setf new-sizes (make-geometry :x x :w w :h maxh))))
	      ;; Unmaximize or Maximize Horizontally
	      (3 (if horz-p
		     (setf new-sizes (make-geometry :x ix :y y :w iw :h h))
		     (setf new-sizes (make-geometry :y y :w maxw :h h))))))))
      (unless (= 3 button-code)
	(if vert-p 
	    (setf prop (delete :_net_wm_state_maximized_vert prop))
	    (pushnew :_net_wm_state_maximized_vert prop)))
      (unless (= 2 button-code)
	(if horz-p
	    (setf prop (delete :_net_wm_state_maximized_horz prop))
	    (pushnew :_net_wm_state_maximized_horz prop)))
      (if fullscreen-p
	  (setf full-geometry new-sizes)
	  (setf (window-position m-window) (geometry-coordinates new-sizes)
		(drawable-sizes app-window) (geometry-sizes new-sizes)))
      (setf (netwm:net-wm-state app-window) prop))))

;;;; Focus management. According to ICCCM

(defgeneric set-focus (input-model window timestamp)
  (:documentation "Set focus to the given window according to the input model.
Input model can be :globally-active :locally-active :passive :no-input.
For more information on the input-model sementic see ICCCM 4.1.7"))

(defmethod set-focus ((input-model (eql :globally-active)) window timestamp)
  (send-wm-protocols-client-message window :wm_take_focus (or timestamp 0)))

(defmethod set-focus ((input-model (eql :locally-active)) window timestamp)
  (when (eql (xlib:window-map-state window) :viewable)
    (send-wm-protocols-client-message window :wm_take_focus (or timestamp 0))
    (xlib:set-input-focus *display* window :pointer-root)))

(defmethod set-focus ((input-model (eql :passive)) window timestamp)
  (declare (ignorable timestamp))
  (when (eql (xlib:window-map-state window) :viewable)
    (xlib:set-input-focus *display* window :pointer-root)))

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
	   (activate-move-resize
	       master *root* 'resize-status *resize-mode* *verbose-resize*))
	  ((eql key :move)
	   (activate-move-resize
	       master *root* 'move-status *move-mode* *verbose-move*)))
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
    (:resize (initialize-resize master nil ev))
    (:move (initialize-move master ev)))
  (when (or (eq key :close) (eq key :kill)) (xlib:ungrab-pointer *display*)))

(defun define-menu-3 (action)
  (lambda ()
    (with-root-cursor (*cursor-2*)
      (destroy-substructure (slot-value *root* 'menu3))
      (loop for event = (get-next-event *display* :force-output-p t)
	    for widget = (lookup-widget (event-event-window event))
	    until (menu-3-process event widget :key action)))))

;;;; Misc.

(defun make-desktop-menu (root callback-maker &key realize)
  "Realize a root pop-up menu with as many entry as existing desktop. It attach
   to each entry a callback realized with the given `callback-maker' function.
   The callback-maker function should be a function of one argument of type
   integer that will be the index of the desktop entry. It may return a lambda
   or sub menu entries. If :realize is nil (the default value) it returns the
   menu entries otherwise a pop-up-menu object is return."
  (loop with root-window = (widget-window root)
	with names = (workspace-names root-window)
	for i from 0 below (number-of-virtual-screens root-window)
	for name = (or (pop names) (format nil "workspace ~D" i))
	collect (cons name (funcall callback-maker i)) into entries
	finally
	  (return (if realize (apply #'make-pop-up root entries) entries))))

(defun make-running-menu (root)
  "Realize the root pop-up menu that shows all applications ordered by desktop."
  (flet ((make-desktop-entries (index)
	   (loop for w in (get-screen-content index :iconify-p t)
		 for widget = (lookup-widget w)
		 for state = (= 1 (first (wm-state w)))
		 collect (cons (format nil "~:[[ ~A ]~;~A~]" state (wm-name w))
			       (lambda ()
				 (case (first (wm-state w))
				   (1 (change-vscreen root :n index))
				   (3 (uniconify (slot-value widget 'icon))))
				 (put-on-top widget))))))
    (make-desktop-menu root #'make-desktop-entries :realize t)))

(defun make-menu-button-menu (master)
  (let* ((app (get-child master :application))
	 (appw (widget-window app))
	 (net-wm-state (netwm:net-wm-state appw))
	 (client-message (make-event :client-message :type :_net_wm_desktop))
	 (shade-str (if (shaded-p app) "Un-shade" "Shade"))
	 (max-str (if (or (member :_net_wm_state_maximized_vert net-wm-state)
			  (member :_net_wm_state_maximized_horz net-wm-state))
		      "Un-maximize" "Maximize")))
    (flet ((send-message (n)
	     (lambda ()
	       (setf (slot-value client-message 'data) (vector n))
	       (event-process client-message app))))
      (make-pop-up
          *root*
	  (cons "Send to" (make-desktop-menu *root* #'send-message))
	  (if (stick-p appw)
	      (cons "Un-pin" (send-message (current-desk)))
	      (cons "Pin   " (send-message +any-desktop+)))
	  (cons max-str (lambda () (maximize-window app 1)))
	  (cons shade-str (lambda () (shade master)))
	  (cons "Close  " (lambda () (close-widget app)))
	  (cons "Destroy" (lambda () (kill-client-window appw)))
	  (cons "Iconify" (lambda () (iconify app)))))))

;; This is for updating the root properties
;; win_client_list, net_client_list(_stacking).
(defun update-lists (app state root)
  "Update root properties win_client_list, net_client_list(_stacking), 
   by adjoining or removing the given application depending of state."
  (with-slots ((appw window) iconic-p) app
    (with-slots ((rw window) client-list) root
      (case (if (and (= state 3) (not iconic-p)) 0 state)
	(0 (remhash appw client-list)
	   (setf (netwm:net-client-list-stacking rw :mode :remove) appw
		 (gnome:win-client-list rw :mode :remove) appw
		 (netwm:net-client-list rw :mode :remove) appw))
	(1 (let ((up2date (gethash appw client-list)))
	     (setf (gethash appw client-list) appw)
	     (update-client-list-stacking root)
	     (unless up2date
	       (setf (netwm:net-client-list rw :mode :append) appw))
	     (if (member :win_hints_skip_winlist (gnome:win-hints appw))
		 (setf (gnome:win-client-list rw :mode :remove) appw)
	         (unless up2date
		   (setf (gnome:win-client-list rw :mode :append) appw)))))))))

(defun update-client-list-stacking (root)
  "Recompute ans set the root property net_client_list_stacking."
  (with-slots (window client-list) root
    (loop for win in (query-application-tree window)
	  when (gethash win client-list) collect win into wins
	  finally (setf (netwm:net-client-list-stacking window) wins))))

(defun window-not-decorable-p (window)
  (let ((netwm-type (netwm:net-wm-window-type window)))
    (or (eql (motif-wm-decoration window) :OFF)
	(member :_net_wm_window_type_splash netwm-type)
	(member :_net_wm_window_type_desktop netwm-type)
	(member :_net_wm_window_type_dock netwm-type))))

(defun procede-decoration (window)
  (let ((scr-num (current-desk))
	(application (create-application window nil))
	(win-workspace (or (window-desktop-num window) +any-desktop+))
	(stick-p (stick-p window))
	(netwm-type (netwm:net-wm-window-type window)))
    (xlib:add-to-save-set window)
    (unless (or stick-p
		(< -1 win-workspace (number-of-virtual-screens *root-window*)))
      (setf win-workspace scr-num))
    (setf (window-desktop-num window) win-workspace)
    (cond ((or (window-not-decorable-p window)
	       (member :_net_wm_state_fulscreen (netwm:net-wm-state window)))
	   (setf (wm-state window) 1)
	   (if (or (= win-workspace scr-num) stick-p)
	       (xlib:map-window window)
	       (with-event-mask (*root-window*)
		 (xlib:unmap-window window))))
	  ((or (= win-workspace scr-num) stick-p)
	   (decore-application window application))
	  (t (with-event-mask (*root-window*)
	       (decore-application window application :map nil)	       
	       (setf (wm-state window) 1)
	       (update-lists application 1 *root*))))
    (unless (member :_net_wm_window_type_desktop netwm-type)
      (with-slots (wants-focus-p input-model) application
	(unless (eq input-model :no-input)	      
	  (setf wants-focus-p *focus-new-mapped-window*))))))

;;;; The main loop.

(defun eclipse-internal-loop ()
  (let ((exit 0)
	(time))

    ;; Sets the root window pop-up menu
    (nconc *menu-1-items* (acons "Exit" (lambda () (setf exit 1)) '()))
    (with-slots (menu1 menu3) *root*
      (setf menu1 (apply #'make-pop-up *root* *menu-1-items*)
	    menu3 (make-pop-up *root*
			       (cons "Move" (define-menu-3 :move))
			       (cons "Resize" (define-menu-3 :resize))
			       (cons "Close" (define-menu-3 :close))
			       (cons "Kill" (define-menu-3 :kill)))))

    ;; Queue events for dressing windows already displayed at start time.
    (flet ((ignorable-window-p (window)
	     (let* ((wmh (xlib:get-property window :WM_HINTS))
		    (initial-state (and wmh (logbitp 1 (car wmh)) (third wmh)))
		    (wm-state (car (wm-state window))))
	       (or
		(eql (xlib:window-override-redirect window) :ON)
		(when (or (eq initial-state 0) (eq wm-state 0))
		  (xlib:unmap-window window)
		  t)
		(and (not wm-state)
		     (eq (xlib:window-map-state window) :unmapped))))))

      (xlib:with-event-queue (*display*)
	(mapc #'(lambda (window)
		  (unless (ignore-errors (ignorable-window-p window))
		    (xlib:queue-event *display*
				      :map-request
				      :event-window *root-window*
				      :window window)))
	      (xlib:query-tree *root-window*))))

    ;; Main loop
    (loop
      (catch 'general-error
	(handler-bind ((end-of-file #'handle-end-of-file-condition)
		       (already-handled-xerror 
		       #'handle-already-handled-xerror-condition)
		       (error #'handle-general-error-condition))
	  (let ((event (get-next-event *display* :discard-p t :timeout 2)))
	    (when event
	      (with-slots (event-window) event
		(event-process event (lookup-widget event-window)))))
	  (when pt:preprogrammed-tasks (pt:execute-preprogrammed-tasks))
	  (case exit
	    (1 (when *close-display-p*
		 (loop for val being each hash-value in *widget-table*
		       do (close-widget val)))
	       (setf time 10 exit 2))
	    (2 (return))))))
    (format t "Main loop exited~%")))
