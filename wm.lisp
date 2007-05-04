;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: wm.lisp,v 1.51 2005/03/13 23:37:07 ihatchondo Exp $
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
   (wm-size-hints :initarg :wm-size-hints :reader decoration-wm-size-hints)
   (frame-style :initarg :frame-style :accessor decoration-frame-style)
   (old-frame-style :initform nil :initarg :old-frame-style)
   (application-gravity 
     :initarg :application-gravity
     :initform :north-west
     :accessor decoration-application-gravity))
  (:documentation "Top level widget for application that will have a set of 
   decoration (e.g: those for which window-not-decorable-p will return NIL)."))

(defun decoration-p (widget)
  (typep widget 'decoration))

(defun title-bar-horizontal-p (master)
  (eq :horizontal (style-title-bar-direction (decoration-frame-style master))))

(defconstant +decoration-event-mask+
  '(:substructure-redirect :substructure-notify
    :visibility-change :enter-window :owner-grab-button))

(defmethod get-child ((master decoration) label &key window)
  "Returns the child widget/window labeled `label' or nil if no such
  child exists. label is a keyword."
  (let ((widget (getf (decoration-children master) label)))
    (if (and widget window) (widget-window widget) widget)))

(defmethod decoration-wm-hints ((master decoration))
  "Returns as a multiple value: minw minh maxw maxh incw inch basew baseh."
  (with-slots (frame-style (wmsh wm-size-hints)) master
    (declare (type (vector integer 8) wmsh))
    (with-slots ((hm hmargin) (vm vmargin)) frame-style
      (values (+ hm (svref wmsh 0)) (+ vm (svref wmsh 1))
	      (+ hm (svref wmsh 2)) (+ vm (svref wmsh 3))
	      (svref wmsh 4) (svref wmsh 5)
	      (+ hm (svref wmsh 6)) (+ vm (svref wmsh 7))))))

(defmethod focus-widget ((master decoration) timestamp)
  (with-slots (window input-model) (get-child master :application)
    (set-focus input-model window timestamp)))

(defmethod focused-p ((master decoration))
  (focused-p (get-child master :application)))

(defmethod widget-position-fix-p ((master decoration))
  (widget-position-fix-p (get-child master :application)))

(defmethod shaded-p ((widget decoration))
  (with-slots (window) (get-child widget :application)
    (member :_net_wm_state_shaded (netwm:net-wm-state window))))

(defmethod shade ((master decoration))
  (with-slots (window frame-style) master
    (let* ((app-win (get-child master :application :window t))
	   (netwm-prop (netwm:net-wm-state app-win))
	   (gnome-prop (or (gnome:win-state app-win :result-type t) 0)))
      (setf gnome-prop (logand gnome-prop #x3DF)) ; supress :win_state_shaded
      (if (member :_net_wm_state_shaded netwm-prop)
	  (with-event-mask (window) 
	    ;; unshade.
	    (xlib:map-window app-win)
	    (xlib:map-window window)
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

(defmethod close-widget ((widget decoration))
  (close-widget (get-child widget :application)))

(defmethod put-on-top ((widget decoration))
  (put-on-top (get-child widget :application)))

(defmethod put-on-bottom ((widget decoration))
  (put-on-bottom (get-child widget :application)))

(defmethod maximize ((widget decoration) code &key (fill-p *maximize-fill*))
  (maximize (get-child widget :application) code :fill-p fill-p))

(defmethod (setf decoration-frame-style) :after (astyle (master decoration))
  (with-slots (window children wm-size-hints) master
    (with-slots (left-margin top-margin (hm hmargin) (vm vmargin)) astyle
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
	    (setf (xlib:window-background window) (style-background astyle)
		  (window-position app-win) (values left-margin top-margin)
		  (drawable-sizes window) (values width height))
	    (make-frame-parts master)
	    (make-title-bar master (wm-name app-win))
	    (update-edges-geometry master)
	    (xlib:map-subwindows window))
	  (setf (application-frame-style application) astyle)
	  (cond ((shaded-p application)
		 (if (title-bar-horizontal-p master)
		     (unless (= 0 vm) (setf (xlib:drawable-height window) vm))
		     (unless (= 0 hm) (setf (xlib:drawable-width window) hm)))
		 (xlib:unmap-window app-win))
		((application-iconic-p application)
		 (xlib:unmap-window app-win))))))))

(defmethod dispatch-repaint ((master decoration) 
			     &key (focus (focused-p master)))
  (declare (optimize (speed 3) (safety 1)))
  (with-slots (parts-to-redraw-on-focus name) (decoration-frame-style master)
    (declare (type string name))
    (mapc #'(lambda (k) (repaint (get-child master k) name focus))
	  parts-to-redraw-on-focus)))

(defun recompute-wm-normal-hints (window hmargin vmargin)
  "Returns two value: a vector representing the wm-normal-hints property and
  the window gravity of the designed window. The wm-normal-hints property is
  recomputed in order to reflect the margin that a top level decoration widget
  (aka master) might introduce."
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

;;;; Decoration creation tools.

(defun make-menu-button (master parent-window)
  (with-slots (children frame-style) master
    (when (frame-item-exist-p frame-style :menu-button)
      (let ((pixmaps (frame-item-pixmaps frame-style :menu-button))
	    (horizontal-p (title-bar-horizontal-p master)))
	(declare (type pixmaps pixmaps))
	(multiple-value-bind (width height) (drawable-sizes (aref pixmaps 0))
	  (multiple-value-bind (tw th) (drawable-sizes parent-window)
	    (setf (getf children :menu-button)
		  (create-button 'menu-button
		    :parent parent-window :master master
		    :background (aref pixmaps 0) 
		    :item (aref pixmaps 1)
		    :width width :height height
		    :event-mask '(:owner-grab-button . #.+push-button-mask+)
		    :y (if horizontal-p (ash (- th height) -1) (- th height))
		    :x (if horizontal-p 0 (ash (- tw width) -1))))))))))

(defun make-buttons-bar (master parent-window)
  (with-slots (children (astyle frame-style)) master
    (flet ((make-container (horizontal-p)
	     (xlib:create-window 
	        :parent parent-window
		:x 0 :y 0 :width 1 :height 1 
		:background :parent-relative 
		:gravity (if horizontal-p :north-east :north-west))))
      (loop initially (when (zerop (style-nb-buttons astyle)) (return))
	    with horizontal-p = (title-bar-horizontal-p master)
	    with container = (make-container horizontal-p)
	    and (x y width height) = '(0 0 0 0)
	    for type in '(iconify-button maximize-button close-button)
	    for child in '(:icon-b :maximize :close)
	    for pixmaps of-type pixmaps = (frame-item-pixmaps astyle child)
	    for bkgrd = (aref pixmaps 0)
	    when (frame-item-exist-p astyle child)
	    do (multiple-value-setq (width height) (drawable-sizes bkgrd))
	       (setf (getf children child)
		     (create-button type
		       :parent container :master master
		       :background bkgrd :item (aref pixmaps 1)
		       :x x :y y :width width :height height
		       :event-mask +push-button-mask+))
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
  (with-slots (children frame-style) master
    (unless (eq :none (style-title-bar-position frame-style))
      (let* ((title-pos (style-title-bar-position frame-style))
	     (horizontal-p (case title-pos ((:top :bottom) t)))
	     (parent-widget (getf children title-pos))
	     (parent-window (widget-window parent-widget))
	     (button-container (make-buttons-bar master parent-window))
	     (menu-button (make-menu-button master parent-window))
	     (pixmaps (frame-item-pixmaps frame-style title-pos))
	     (bcw 0) (bch 0) (mbw 0) (mbh 0) (title))
	(declare (type pixmaps pixmaps))
	(when button-container
	  (multiple-value-setq (bcw bch) (drawable-sizes button-container)))
	(when menu-button
	  (multiple-value-setq (mbw mbh)
	    (drawable-sizes (widget-window menu-button))))
	(setf title
	      (create-button 'title-bar
		:parent parent-window :master master
		:width 1 :height 1
		:x (if horizontal-p mbw 0) :y (if horizontal-p 0 bch)
		:event-mask +push-button-mask+
		:background (aref pixmaps 0) :item name)
	      (slot-value title 'parent) parent-window
	      (getf children :title-bar) title
	      (xlib:window-background parent-window) :parent-relative
	      (xlib:window-event-mask parent-window) 0)
	(if horizontal-p 
	    (setf (slot-value title 'hmargin) (+ bcw mbw))
	    (setf (slot-value title 'vmargin) (+ bch mbh)))
	(xlib:map-subwindows parent-window)
	title))))

(defun edge-position (style edge-key width height)
  (with-slots (top-left-w top-left-h top-right-h bottom-left-w) style
    (multiple-value-bind (w h) (frame-item-sizes style edge-key)
      (case edge-key
	(:top (values top-left-w 0))
	(:right (values (- width w) top-right-h))
	(:bottom (values bottom-left-w (- height h)))
	(:left (values 0 top-left-h))
	(:top-left (values 0 0))
	(:top-right (values (- width w) 0))
	(:bottom-right (values (- width w) (- height h)))
	(:bottom-left (values 0 (- height h)))))))

(defvar *frame-parts*
  '(:right :left :top :bottom :top-left :top-right :bottom-left :bottom-right))

(defun make-frame-parts (master)
  (with-slots (children window (astyle frame-style)) master
    (multiple-value-bind (width height) (drawable-sizes window)
      (loop for child in *frame-parts*
	    for pixmaps of-type pixmaps = (frame-item-pixmaps astyle child)
	    for hilighted = (aref pixmaps 1)
	    for event-mask = (if hilighted +std-button-mask+ +edge-event-mask+)
	    when (frame-item-exist-p astyle child) do
	     (multiple-value-bind (x y)
		 (edge-position astyle child width height)
	       (multiple-value-bind (w h) (frame-item-sizes astyle child)
		 (setf (getf children child)
		       (create-button (intern (symbol-name child) :eclipse)
			 :parent window :master master
			 :background (aref pixmaps 0) :item hilighted
			 :event-mask event-mask
			 :x x :y y :width w :height h))))))))

;; Public.

(defun update-edges-geometry (master)
  (declare (optimize (speed 3) (safety 0)))
  (declare (inline update-edges-geometry))
  (macrolet
      ((update (edge size &rest frame-style-slots-size)
	 `(when ,edge
	    (setf (,(intern (format nil "DRAWABLE-~a" (symbol-name size)) :xlib)
		   (widget-window ,edge))
	          (with-slots (,@frame-style-slots-size) frame-style
		    (declare (type xlib:card16 ,@frame-style-slots-size))
		    (max 1 (- ,size ,@frame-style-slots-size)))))))
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
    (with-slots (parent window (vm vmargin) (hm hmargin)) title-bar
      (declare (type xlib:card16 vm hm))
      (multiple-value-bind (width height) (drawable-sizes parent)
	(declare (type xlib:card16 width height))
	(setf (drawable-sizes window)
	      (values (max 1 (- width hm)) (max 1 (- height vm))))))))

(defun initial-coordinates (app-window frame-style)
  "Returns as multiple values the decoration initial coordinates."
  (let ((hint (ignore-errors (xlib:wm-normal-hints app-window))))
    (with-slots (top-margin left-margin vmargin hmargin) frame-style
      (flet ((default-coordinates ()
               (let* ((n (or (window-desktop-num app-window) 0))
                      (k (if (= +any-desktop+ n) 0 (* 4 n)))
                      (areas (netwm:net-workarea *root-window*))
                      (ax (aref areas k)) (ay (aref areas (1+ k))))
                 (multiple-value-bind (x y) (window-position app-window)
                   (values (max ax (- x left-margin))
                           (max ay (- y top-margin)))))))
        (if (and hint (xlib:wm-size-hints-user-specified-position-p hint))
            (let ((x (xlib:wm-size-hints-x hint))
                  (y (xlib:wm-size-hints-y hint)))
              (if (and x y)
                  (case (xlib:wm-size-hints-win-gravity hint)
                    (:north-east (values (- x hmargin) y))
                    (:south-east (values (- x hmargin) (- y vmargin)))
                    (:south-west (values x (- y vmargin)))
                    (:static (values (- x left-margin) (- y top-margin)))
                    (t (values x y)))
                  (progn
                    (format t "user-specified-position-p t but x or y isn't.")
                    (default-coordinates))))
            (default-coordinates))))))

(defun make-decoration (app-window application &key theme)
  "Returns a newly initialized decoration to hold the given application."
  (unless theme (setf theme (root-decoration-theme *root*)))
  (let* ((dstyle (find-decoration-frame-style theme app-window))
	 (style dstyle))
    (when (member :_net_wm_state_fullscreen (netwm:net-wm-state app-window))
      (setf style (theme-default-style (lookup-theme "no-decoration"))))
    (with-slots (hmargin vmargin left-margin top-margin background) style
      (multiple-value-bind (wm-sizes gravity) 
	  (recompute-wm-normal-hints app-window hmargin vmargin)
	(multiple-value-bind (width height) (drawable-sizes app-window)
	  (multiple-value-bind (x y) (initial-coordinates app-window style)
	    (let* ((window (xlib:create-window
			       :parent (xlib:drawable-root app-window)
			       :x x :y y
			       :width (+ width hmargin)
			       :height (+ height vmargin)
			       :background background
			       :event-mask +decoration-event-mask+
			       :do-not-propagate-mask 
			       '(:button-press :button-release)))
		   (master (make-instance 'decoration
			     :window window
			     :old-frame-style dstyle :frame-style style
			     :children (list :application application)
			     :application-gravity gravity
			     :wm-size-hints wm-sizes)))
	      (make-frame-parts master)
	      (make-title-bar master (wm-name app-window))
	      (update-edges-geometry master)
	      (with-slots (icon) application
		(setf (getf (decoration-children master) :icon) icon
		      (slot-value icon 'master) master
		      (slot-value application 'master) master
		      (xlib:drawable-border-width app-window) 0))
	      master)))))))

(defun decore-application (window application &key (map t) theme)
  "Decores an application and map the resulting decoration as indicated
  by the :map keyword argument. (default value is T).
  Returns the newly created decoration instance."
  (let* ((master (make-decoration window application :theme theme))
	 (master-window (widget-window master))
	 (left-margin (style-left-margin (decoration-frame-style master)))
	 (top-margin (style-top-margin (decoration-frame-style master))))
    (with-event-mask (master-window)
      (xlib:map-subwindows master-window))
    (with-event-mask (master-window (when map +decoration-event-mask+))
      (xlib:reparent-window window master-window left-margin top-margin)
      (send-configuration-notify window))
    (setf (application-frame-style application) (decoration-frame-style master))
    ;; handle maximized states.
    (let* ((prop (netwm:net-wm-state window))
           (vert-p (member :_net_wm_state_maximized_vert prop))
           (horz-p (member :_net_wm_state_maximized_horz prop)))
      (when (or vert-p horz-p)
        (setf prop (delete :_net_wm_state_maximized_vert prop))
        (setf prop (delete :_net_wm_state_maximized_horz prop))
        (setf (netwm:net-wm-state window) prop)
        (maximize application (if (and horz-p vert-p) 1 (if horz-p 3 2)))))
    (when map (xlib:map-window window))
    master))

;;;; Focus management. According to ICCCM

(defgeneric set-focus (input-model window timestamp)
  (:documentation 
   "Set focus to the given window according to the input model.
   Input model can be :globally-active :locally-active :passive :no-input.
   For more information on the input-model sementic see ICCCM 4.1.7"))

(defmethod set-focus :around (input-model window timestamp)
  ;; If we have a valid timestamp then assign focus directly
  ;; otherwise set the net-active-window to provoke a property-notify event
  ;; on the root-property-holder. Then the property-notify event will handled
  ;; and will assign the focus with a valid timestamp.
  ;; It seems a bit complicated but this is the bettter way I found to not 
  ;; violate the ICCCM (section 4.1.7).
  (if (and timestamp (> timestamp 0))
      (and (next-method-p) (call-next-method))
      (with-slots ((ww window)) (root-property-holder *root*)
	(setf (netwm:net-active-window ww) window))))

(defmethod set-focus ((input-model (eql :globally-active)) window time)
  (send-wm-protocols-client-message window :wm_take_focus (or time 0)))

(defmethod set-focus ((input-model (eql :locally-active)) window time)
  (when (eql (xlib:window-map-state window) :viewable)
    (xlib:set-input-focus *display* window :pointer-root)
    (send-wm-protocols-client-message window :wm_take_focus (or time 0))))

(defmethod set-focus ((input-model (eql :passive)) window timestamp)
  (declare (ignorable timestamp))
  (when (eql (xlib:window-map-state window) :viewable)
    (xlib:set-input-focus *display* window :pointer-root)))

(defmethod set-focus ((input-model (eql :no-input)) window timestamp)
  (declare (ignorable window timestamp))
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
  (declare (ignorable event w rest))
  (xlib:ungrab-pointer *display*)
  t)

(defmethod menu-3-process ((event button-release) (app application) &key key)
  (declare (ignorable event))
  (cond ((eql key :move) (finish-move app *verbose-move* *move-mode*)))
  (call-next-method))

(defmethod menu-3-process ((ev button-release) (dec decoration) &key key)
  (cond	((eql key :resize) (finish-resize dec *verbose-resize* *resize-mode*))
	((eql key :move) (finish-move dec *verbose-resize* *resize-mode*)))
  (call-next-method))

(defmethod menu-3-process ((ev motion-notify) (app application) &key key)
  (when (eql key :move)
    (activate-move-resize app *root* 'move-status *move-mode* *verbose-move*)
    (application-active-p app)))

(defmethod menu-3-process ((ev motion-notify) (master decoration) &key key)
  (when (or (eql key :resize) (eql key :move))
    (multiple-value-call #'activate-move-resize
      master *root*
      (cond ((eql key :resize)
	     (values 'resize-status *resize-mode* *verbose-resize*))
	    ((eql key :move)
	     (values 'move-status *move-mode* *verbose-move*))))
    (decoration-active-p master)))

(defmethod menu-3-process ((event enter-notify) (app application) &rest rest)
  (declare (ignorable event rest))
  (with-slots (window) (or (application-master app) app)
    (xlib:grab-pointer window +pointer-event-mask+ :cursor *cursor-2*))
  nil)

(defmethod menu-3-process ((event enter-notify) (master decoration) &rest rest)
  (declare (ignorable event rest))
  (with-slots (window) master
    (xlib:grab-pointer window +pointer-event-mask+ :cursor *cursor-2*))
  nil)

(defmethod menu-3-process ((event leave-notify) (app application) &rest rest)
  (declare (ignore event rest))
  (unless (application-master app) (xlib:ungrab-pointer *display*))
  nil)

(defmethod menu-3-process ((event leave-notify) (master decoration) &rest rest)
  (declare (ignore event master rest))
  (xlib:ungrab-pointer *display*)
  nil)

(defmethod menu-3-process ((ev button-press) (app application) &key key)
  (with-slots (master window) app
    (case key
      (:kill (kill-client-window window))
      (:close (close-widget app))
      (:resize (when master (initialize-resize master nil ev)))
      (:move (initialize-move (or master app) ev)))
    (when (member key '(:close :kill)) (xlib:ungrab-pointer *display*))))

(defmethod menu-3-process ((ev button-press) (master decoration) &key key)
  (menu-3-process ev (get-child master :application) :key key))

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
  (declare (type function callback-maker))
  (loop with root-window = (widget-window root)
	with names = (workspace-names root-window)
	for i from 0 below (number-of-virtual-screens root-window)
	for name = (or (pop names) (format nil "workspace ~D" i))
	collect (cons name (funcall callback-maker i)) into entries
	finally
	  (return (if realize (apply #'make-pop-up root entries) entries))))

(defun make-running-menu (root)
  "Realize the root pop-up menu that shows all applications ordered by desktop."
  (labels
      ((raise (window index)
	 (lambda ()
	   (case (first (wm-state window))
	     (1 (change-vscreen root :n index))
	     (3 (uniconify (slot-value (lookup-widget window) 'icon))))
	   (put-on-top (lookup-widget window))))
       (make-desktop-entries (i)
	 (loop for w in (screen-content i :iconify-p t)
	       for state = (= 1 (first (wm-state w)))
	       for name = (format nil "~:[[ ~A ]~;~A~]" state (wm-name w))
	       collect (cons name (raise w i)) into entries
	       finally
	        (return (or entries (lambda () (change-vscreen root :n i)))))))
    (make-desktop-menu root #'make-desktop-entries :realize t)))

(defun make-menu-button-menu (master)
  (let* ((app (get-child master :application))
	 (appw (widget-window app))
	 (net-wm-state (netwm:net-wm-state appw))
	 (data (make-array 1 :element-type 'xlib:card32))
	 (xc-msg (make-event :client-message :data data :type :_net_wm_desktop))
	 (shade-str (if (shaded-p app) "Un-shade" "Shade"))
	 (max-str (if (or (member :_net_wm_state_maximized_vert net-wm-state)
			  (member :_net_wm_state_maximized_horz net-wm-state))
		      "Un-maximize" "Maximize")))
    (declare (type (simple-array xlib:card32 (1)) data))
    (flet ((send-message (n)
	     (lambda ()
	       (setf (aref data 0) n)
	       (event-process xc-msg app))))
      (make-pop-up
          *root*
	  (cons "Send to" (make-desktop-menu *root* #'send-message))
	  (if (stick-p appw)
	      (cons "Un-pin" (send-message (current-desk)))
	      (cons "Pin   " (send-message +any-desktop+)))
	  (cons max-str (lambda () (maximize app 1)))
	  (cons shade-str (lambda () (shade master)))
	  (cons "Close  " (lambda () (close-widget app)))
	  (cons "Destroy" (lambda () (kill-client-window appw)))
	  (cons "Iconify" (lambda () (iconify app)))))))

(defun remove-window-from-client-lists (window root)
  "Removes a window from the client lists root properties."
  (with-slots ((rw window) client-list) root
    (remhash window client-list)
    (setf (netwm:net-client-list-stacking rw :mode :remove) window
	  (gnome:win-client-list rw :mode :remove) window
	  (netwm:net-client-list rw :mode :remove) window)))

(defun add-window-in-client-lists (window root)
  "Add a window in the client lists root properties."
  (with-slots ((rw window) client-list) root
    (let ((up2date (gethash window client-list)))
      (setf (gethash window client-list) window)
      (update-client-list-stacking root)
      (unless up2date
	(setf (netwm:net-client-list rw :mode :append) window))
      (if (member :win_hints_skip_winlist (gnome:win-hints window))
	  (setf (gnome:win-client-list rw :mode :remove) window)
	  (unless up2date
	    (setf (gnome:win-client-list rw :mode :append) window))))))

(defun update-client-list-stacking (root)
  "Recompute and set the root property net_client_list_stacking."
  (with-slots (window client-list) root
    (loop for win in (query-application-tree window)
	  when (gethash win client-list) collect win into wins
	  finally (setf (netwm:net-client-list-stacking window) wins))))

(defun update-lists (app state root)
  "Update root properties win_client_list, net_client_list(_stacking), 
  by adjoining or removing the given application depending of state."
  (with-slots ((appw window) iconic-p) app
    (with-slots ((rw window) client-list) root
      (case (if (and (= state 3) (not iconic-p)) 0 state)
	(0 (remove-window-from-client-lists appw root))
	(1 (add-window-in-client-lists appw root))))))

(defun window-not-decorable-p (window &optional type)
  "Returns T if a window `should' not be decorated. Typically, a splash screen,
  a desktop (e.g. nautilus) or a dock (e.g. gnome panels) will be assumed as
  non-decorable windows, as well as windows holding the motif_wm_hints with the
  flag `no decoration at all'."
  (let ((netwm-type (or type (netwm:net-wm-window-type window))))
    (or (eql (motif-wm-decoration window) :OFF)
	(member :_net_wm_window_type_splash netwm-type)
	(member :_net_wm_window_type_desktop netwm-type)
	(member :_net_wm_window_type_dock netwm-type))))

(defun procede-decoration (window)
  "Decore, if necessary, add/update properties, map or not, etc a window."
  (let* ((time (or (ignore-errors (netwm:net-wm-user-time window)) 1))
	 (rw (xlib:drawable-root window))
	 (scr-num (current-vscreen rw))
	 (application (create-application window nil))
	 (win-workspace (or (window-desktop-num window) +any-desktop+))
	 (stick-p (stick-p window)))
    (xlib:add-to-save-set window)
    (unless (or stick-p	(< -1 win-workspace (number-of-virtual-screens rw)))
      (setf win-workspace scr-num))
    (setf (window-desktop-num window) win-workspace)
    (cond ((not (or (= win-workspace scr-num) stick-p))
	   (with-event-mask (rw)
	     (setf (wm-state window) 1)
	     (xlib:unmap-window window)
	     (unless (window-not-decorable-p window)
	       (decore-application window application :map nil))
	     (update-lists application 1 *root*)))
	  ((window-not-decorable-p window (application-type application))
	   (setf (wm-state window) 1)
	   (xlib:map-window window))
	  (t (decore-application window application :map t)))
    (with-slots (wants-focus-p input-model type) application
      (unless (member :_net_wm_window_type_desktop type)
	(unless (or (zerop time) (eq input-model :no-input))
	  (setf wants-focus-p *focus-new-mapped-window*)))
      (when (member :_net_wm_window_type_dock type)
	(update-workarea-property *root*)))))

;;;; The main loop.

(define-condition exit-eclipse (error)
  ((close-application-p
     :initform nil :initarg :close-application-p
     :type boolean :reader close-application-p)))

(defun eclipse-internal-loop ()
  (let* ((exit 0) time)

    ;; Sets the root window pop-up menu
    (when *menu-1-exit-p*
      (nconc *menu-1-items* (acons "Exit" (lambda () (setf exit 1)) '())))
    (with-slots (menu1 menu3) *root*
      (setf menu1 (apply #'make-pop-up *root* *menu-1-items*)
	    menu3 (make-pop-up *root*
			       (cons "Move" (define-menu-3 :move))
			       (cons "Resize" (define-menu-3 :resize))
			       (cons "Close" (define-menu-3 :close))
			       (cons "Kill" (define-menu-3 :kill)))))

    ;; Dress windows already displayed at start time.
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

      (xlib:with-server-grabbed (*display*)
	(mapc (lambda (w)
		(unless (ignore-errors (ignorable-window-p w))
		  (procede-decoration w)))
	      (xlib:query-tree *root-window*))))

    ;; Main loop
    (loop
      (catch 'general-error
	(handler-case
	    (let ((event (get-next-event *display* :discard-p t :timeout 2)))
	      (when event
		(with-slots (event-window) event
		  (event-process event (lookup-widget event-window)))
		(xlib:display-finish-output *display*))
	      (when pt:preprogrammed-tasks (pt:execute-preprogrammed-tasks))
	      (with-slots (sm-conn) *root*
		(when sm-conn (handle-session-manager-request sm-conn *root*)))
	      (case exit
		(1 (loop for val being each hash-value in *widget-table*
			 when (application-p val) 
			   if *close-display-p* do (close-widget val)
			   else do (undecore-application val))
		   (setf time 10 exit 2))
		(2 (when (root-sm-conn *root*)
		     (close-sm-connection *root* :exit-p nil))
		   (xlib:display-finish-output *display*)
		   (setf (xlib:window-event-mask *root-window*) 0)
		   (let ((win (netwm:net-supporting-wm-check *root-window*)))
		     (xlib:destroy-window win))
		   (xlib:display-finish-output *display*)
		   (return))))
	  (exit-eclipse (c)
	    (setf *close-display-p* (close-application-p c) exit 1))
	  (end-of-file (c) (handle-end-of-file-condition c))
	  (already-handled-xerror () nil)
	  (error (c) (handle-error-condition c)))))
    (format t "~%Main loop exited~%")))
