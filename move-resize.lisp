;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: move-resize.lisp,v 1.5 2003/09/30 12:18:36 hatchond Exp $
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

;;;; Functions for displaying window sizes/positions in a box-button object.

(defparameter *geometry-info-box* nil)

(deftypedparameter (signed-byte 16) *delta-x* 0)
(deftypedparameter (signed-byte 16) *delta-y* 0)

(defun undraw-geometry-info-box ()
  (when *geometry-info-box*
    (xlib:unmap-window (widget-window *geometry-info-box*))))

(defun initialize-geometry-info-box ()
  (unless *geometry-info-box*
    (setf *geometry-info-box* (create-message-box nil :parent *root-window*)))
  (with-slots (window) *geometry-info-box*
    (xlib:map-window window)
    (setf (xlib:window-priority window) :above)))

(defun display-infos (message)
  "Display the given message in the geometry-info-box window."
  (declare (optimize (speed 3) (safety 0)))  
  (setf (button-item-to-draw *geometry-info-box*) message)
  (repaint *geometry-info-box* nil nil))

(defun display-coordinates (x y)
  "Display the given coordinates in the geometry-info-box window."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (signed-byte 16) x y))
  (display-infos (format nil "~:[+~;~]~D ~:[+~;~]~D" (< x 0) x (< y 0) y)))

(defun display-geometry (width height)
  "Display the given width and height in the geometry-info-box window."
  (declare (optimize (speed 3) ))  
  (display-infos (format nil "~D x ~D" width height)))

;;;; A master "clone" for resize and move.

(defparameter *clone* nil)

(defun initialize-clone ()
  (let ((win (xlib:create-window 
	        :parent *root-window* :x 0 :y 0 :width 100 :height 100)))
    (setf *clone* (make-decoration win (create-application win nil)))))

(defun update-clone (master)
  (multiple-value-bind (x y w h) (window-geometry (widget-window master))
    (with-slots (window wm-size-hints frame-style) *clone*
      (setf wm-size-hints (decoration-wm-size-hints master)
	    frame-style (decoration-frame-style master)
	    (drawable-sizes window) (values w h)
	    (window-position window) (values x y)))))

(defun draw-window-grid (window gctxt dest-window)
  "Draw a 3x3 grid representing the future window geometry on the dest-window."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (x y width height) (window-geometry window)
    (declare (type (signed-byte 16) x y)
	     (type (unsigned-byte 16) width height))
    (decf width) (decf height)
    (xlib:with-gcontext (gctxt :function 8 :subwindow-mode :include-inferiors)
      (xlib:draw-rectangle dest-window gctxt x y width height)
      (let ((w (round width 3))
	    (h (round height 3)))
	(declare (type (unsigned-byte 16) w h))
	(xlib:draw-segments
	    dest-window gctxt
	    (list x (+ h y) (+ x width) (+ h y)
		  x (+ y (* 2 h)) (+ x width) (+ y (* 2 h))
		  (+ x w) y (+ w x) (+ y height)
		  (+ x (* 2 w)) y (+ x (* 2 w)) (+ y height)))))))

(defun activate-move-resize (master root status mode verbose-p)
  "Sets some internal values for the future move or resize animations."
  (with-slots ((master-window window) gcontext active-p) master
    (with-slots (resize-status move-status current-active-decoration window) root
      (when (and active-p (not (or resize-status move-status)))
	(or *clone* (initialize-clone))
	(update-clone master)
	(grab-root-pointer)
	(setf (slot-value root status) t
	      current-active-decoration master)
	(when (eq mode :box)
	  (xlib:grab-server *display*)
	  (draw-window-grid master-window gcontext window))
	(when verbose-p
	  (initialize-geometry-info-box))))))

;;;; Resize.

(defparameter *card-point* :se)

(defun initialize-resize (master edge pointer-event)
  "Initialize the internal settings for the resize process."
  (setf (window-priority (widget-window master)) :above)
  (if (base-widget-p edge)
      (where-is-pointer edge)
      (with-slots (root-x root-y) pointer-event
	(find-corner root-x root-y (widget-window master))))
  (let ((prop (netwm:net-wm-state (get-child master :application :window t))))
    (when (member :_net_wm_state_maximized_vert prop)
      (case *card-point*
	((:ne :se) (setf *card-point* :east))
	((:nw :sw) (setf *card-point* :west))
	((:north :south) (setf *card-point* nil))))
    (when (member :_net_wm_state_maximized_horz prop)
      (case *card-point*
	((:ne :nw) (setf *card-point* :north))
	((:se :sw) (setf *card-point* :south))
	((:east :west) (setf *card-point* nil))))
    (setf (decoration-active-p master) (if *card-point* t nil))))

(defgeneric resize-from (widget)
  (:documentation 
   "Resize an application or a master according to the given master or
    application respectively."))

(defmethod resize-from ((master decoration))
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (window frame-style) master
    (let ((hmargin (style-hmargin frame-style))
	  (vmargin (style-vmargin frame-style)))
      (declare (type (unsigned-byte 16) hmargin vmargin))
      (multiple-value-bind (w h) (drawable-sizes window)
	(declare (type xlib:card16 w h))
	(setf (drawable-sizes (get-child master :application :window t))
	      (values (- w hmargin) (- h vmargin)))))))

(defmethod resize-from ((application application))
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (window master) application
   (let ((hmargin (style-hmargin (decoration-frame-style master)))
	 (vmargin (style-vmargin (decoration-frame-style master))))
     (declare (type (unsigned-byte 16) hmargin vmargin))
     (multiple-value-bind (w h) (drawable-sizes window)
       (declare (type xlib:card16 w h))
       (setf (drawable-sizes (widget-window master))
	     (values (+ w hmargin) (+ h vmargin)))))))

(defun where-is-pointer (widget)
  "Initialize the resize process when activated from one the decoration edge." 
  (declare (optimize (speed 3) (safety 0)))
  (setf *delta-x* 0
	*delta-y* 0
	*card-point*
	(typecase widget
	  (top :north)
	  (top-left :nw)
	  (left :west)
	  (bottom-left :sw)
	  (bottom :south)
	  (bottom-right :se)
	  (right :east)
	  (top-right :ne)
	  (t :se))))

(defun find-corner (root-x root-y window)
  "Initialize the resize process when activated from somewhere else 
  than a decoration edge."
  (declare (optimize (speed 3) (safety 0))
	   (type xlib:int16 root-x root-y))
  (multiple-value-bind (x y w h) (window-geometry window)
    (declare (type xlib:int16 x y)
	     (type xlib:card16 w h))
    (when (>= root-x (+ x (floor w 2))) (incf x w))
    (when (>= root-y (+ y (floor h 2))) (incf y h))
    (setf *delta-x* (- root-x x)
	  *delta-y* (- root-y y)
	  *card-point*
	  (if (>= *delta-x* 0)
	      (if (>= *delta-y* 0) :nw :sw)
	      (if (>= *delta-y* 0) :ne :se)))))

(defun check-size (size base inc min-size max-size)
  "If the given size respects all the given constraints, then return size. 
  Otherwise returns the nearest satisfying size."
  (declare (optimize (speed 3) (safety 0))
	   (type xlib:card16 size base inc min-size max-size))
  (if (< min-size size max-size)
      (let ((k (mod (- size base) inc)))
	(declare (type xlib:card16 k))
	(if (= k 0) size (+ inc (- size k))))
      (max min-size (min size max-size))))

(defmethod resize ((master decoration) (event motion-notify)
		   &optional verbose-p mode)
  (declare (optimize (speed 3) (safety 0))
	   (inline update-edges-geometry %resize%))
  (if (eql mode :opaque)
      (with-event-mask ((slot-value master 'window))
	(%resize% master event verbose-p)
	(update-edges-geometry master)
	(resize-from master))
      (with-slots (window gcontext) *clone*
	(draw-window-grid window gcontext *root-window*)
	(%resize% *clone* event verbose-p)
	(draw-window-grid window gcontext *root-window*))))	

(defun %resize% (master motion-notify-event verbose-p)
  (declare (optimize (speed 3) (safety 0))
	   (inline check-size))
  (let* ((master-win (widget-window master))
	 (root-x (event-root-x motion-notify-event))
	 (root-y (event-root-y motion-notify-event)))
    (declare (type xlib:int16 root-x root-y))
    (decf root-x *delta-x*)
    (decf root-y *delta-y*)
    (multiple-value-bind (x y width height) (window-geometry master-win)
      (declare (type xlib:int16 x y)
	       (type xlib:card16 width height))
      (multiple-value-bind (tmp-width tmp-height new-x new-y)
	  (ecase *card-point*
	    (:north (values width (+ height (- y root-y)) x root-y))
	    (:ne (values (- root-x x) (+ height (- y root-y)) x root-y))
	    (:east (values (- root-x x) height x y))
	    (:se (values (- root-x x) (- root-y y) x y))
	    (:south (values width (- root-y y) x y))	
	    (:sw (values (+ width (- x root-x)) (- root-y y) root-x y))
	    (:west (values (+ width (- x root-x)) height root-x y))
	    (:nw (values (+ width (- x root-x)) (+ height (- y root-y))
			 root-x root-y)))
	(declare (type xlib:int16 new-x new-y)
		 (type xlib:card16 tmp-width tmp-height))
	(multiple-value-bind (minw minh maxw maxh incw inch basew baseh)
	    (decoration-wm-hints master)
	  (declare (type xlib:card16 minw minh maxw maxh incw inch basew baseh))
	  (let ((new-width (check-size tmp-width basew incw minw maxw))
		(new-height (check-size tmp-height baseh inch minh maxh)))
	    (declare (type xlib:card16 new-width new-height))
	    (when verbose-p
	      (display-geometry (/ (- new-width basew) incw)
				(/ (- new-height baseh) inch)))
	    (case *card-point*
	      ((or :north :ne) (incf new-y (- tmp-height new-height)))
	      ((or :west :sw) (incf new-x (- tmp-width new-width)))
	      (:nw (incf new-y (- tmp-height new-height))
		   (incf new-x (- tmp-width new-width))))
	    (xlib:with-state (master-win)
	      (setf (window-position master-win) (values new-x new-y))
	      (setf (drawable-sizes master-win)
		    (values new-width new-height)))))))))

;; Finish the resize process:
;; call when button-release on root
;; and when root-resize-status is not nil.
(defun finish-resize (master &optional verbose-p mode)
  "Terminate the resize work. (undraw grid, geometry infos, ...)"
  (when verbose-p (undraw-geometry-info-box))
  (with-slots (window gcontext) master
    (when (and (decoration-active-p master) (eql mode :box))
      (draw-window-grid (widget-window *clone*) gcontext *root-window*)
      (multiple-value-bind (x y w h)
	  (window-geometry (widget-window *clone*))
	(setf (window-position window) (values x y)
	      (drawable-sizes window) (values w h))
	(resize-from master))))
  (setf *card-point* nil))

;;;; Move.
	  
(defmethod initialize-move ((widget base-widget) (event button-press))
  "Initialize internal values for animating the future widget movements."
  (with-slots (window active-p) widget
    (setf (window-priority window) :above)
    (setf active-p  t
	  *delta-x* (- (event-root-x event) (xlib:drawable-x window))
	  *delta-y* (- (event-root-y event) (xlib:drawable-y window)))))

(defmethod initialize-move :after ((master decoration) (event button-press))
  (let ((app-window (get-child master :application :window t)))
    (when (or (member :win_state_fixed_position (gnome:win-state app-window))
	      (member :_net_wm_state_sticky (netwm:net-wm-state app-window)))
      (setf (decoration-active-p master) nil))))

(defun move-widget (widget event &optional verbose-p mode)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (window active-p gcontext) widget
    (when active-p
      (let ((new-x (- (the (signed-byte 16) (event-root-x event)) *delta-x*))
	    (new-y (- (the (signed-byte 16) (event-root-y event)) *delta-y*))
	    (scr-w (screen-width)) (scr-h (screen-height)))
	(declare (type (signed-byte 16) new-x new-y))
	(declare (type (unsigned-byte 16) scr-w scr-h))
	(multiple-value-bind (x y w h)
	    (window-geometry (if (eq mode :box) (widget-window *clone*) window))
	  (declare (type (signed-byte 16) x y))
	  (declare (type (unsigned-byte 16) w h))
	  (when (and (>= x 0) (< -40 new-x 0)) (setf new-x 0))
	  (when (and (>= y 0) (< -40 new-y 0)) (setf new-y 0))
	  (when (and (>= (- scr-w x w) 0) (< -40 (- scr-w new-x w) 0))
	    (setf new-x (- scr-w w)))
	  (when (and (>= (- scr-h y h) 0) (< -40 (- scr-h new-y h) 0))
	    (setf new-y (- scr-h h))))
	(when verbose-p (display-coordinates new-x new-y))
	(if (and (decoration-p widget) (eql mode :box))
	    (with-slots (window) *clone*	    
	      (draw-window-grid window gcontext *root-window*)
	      (setf (window-position window) (values new-x new-y))
	      (draw-window-grid window gcontext *root-window*))
	    (setf (window-position window) (values new-x new-y)))))))

(defun finish-move (master &optional verbose-p mode) 
  "Terminate the move work. (undraw grid, geometry infos, ..."
  (when (eql mode :box)
    (with-slots (window gcontext) *clone*
      (draw-window-grid window gcontext *root-window*)
      (setf (window-position (widget-window master)) (window-position window))))
  (setf (decoration-active-p master) nil)
  (when verbose-p (undraw-geometry-info-box))
  (when (get-child master :title-bar)
    (with-slots (armed active-p) (get-child master :title-bar)
      (setf armed nil active-p nil)))
  (send-configuration-notify (get-child master :application :window t)))
