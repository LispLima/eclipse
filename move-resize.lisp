;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: move-resize.lisp,v 1.2 2003/03/16 01:18:13 hatchond Exp $
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
    (setf *geometry-info-box* 
	  (create-message-box '("nil") :parent *root-window* :border-width 0)))
  (with-slots (window) *geometry-info-box*
    (xlib:map-window window)
    (setf (xlib:window-priority window) :above)))

(defun display-infos (message)
  (declare (optimize (speed 3) (safety 0)))  
  (with-slots (window item-to-draw gcontext) *geometry-info-box*
    (xlib:clear-area window)
    (setf (button-item-to-draw *geometry-info-box*) message)
    (multiple-value-bind (w h) (drawable-sizes window)
      (declare (type (unsigned-byte 16) w h))
      (xlib:with-gcontext (gcontext :foreground *black*)
        (xlib:draw-rectangle window gcontext 0 0 (1- w) (1- h))))
    (draw-centered-text window gcontext item-to-draw :color *black*)))

(defun display-coordinates (x y)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (signed-byte 16) x y))
  (display-infos (format nil "~:[+~;~]~A ~:[+~;~]~A" (< x 0) x (< y 0) y)))

(defun display-geometry (width height)
  (declare (optimize (speed 3) ))  
  (display-infos (format nil "~A x ~A" width height)))

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

;;;; Draw a 3x3 grid representing the future window geometry (under box mode).

(defun draw-window-grid (window gctxt dest-window)
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

;;;; Resize.

(defparameter *card-point* :se)

(defun where-is-pointer (widget)
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
  (declare (optimize (speed 3) (safety 0))
	   (type xlib:card16 size base inc min-size max-size))
  (if (< min-size size max-size)
      (let ((k (mod (- size base) inc)))
	(declare (type xlib:card16 k))
	(if (= k 0) size (+ inc (- size k))))
      (max min-size (min size max-size))))

(defmethod resize-from ((master decoration))
  " Resize the application window according to the master sizes"
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
  " Resize the master window according to the application sizes"
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (window master) application
   (let ((hmargin (style-hmargin (decoration-frame-style master)))
	 (vmargin (style-vmargin (decoration-frame-style master))))
     (declare (type (unsigned-byte 16) hmargin vmargin))
     (multiple-value-bind (w h) (drawable-sizes window)
       (declare (type xlib:card16 w h))
       (setf (drawable-sizes (widget-window master))
	     (values (+ w hmargin) (+ h vmargin)))))))

(defmethod resize ((master decoration) (event motion-notify))
  (declare (optimize (speed 3) (safety 0)))
  (if (eq *resize-mode* :opaque)
      (with-event-mask ((slot-value master 'window))
	(%resize% master event)
	(update-edges-geometry master)
	(resize-from master))
      (with-slots (window) *clone*
	(draw-window-grid window *gcontext* *root-window*)
	(%resize% *clone* event)
	(draw-window-grid window *gcontext* *root-window*))))	

(defun %resize% (master motion-notify-event)
  (declare (optimize (speed 3) (safety 0)))
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
	    (when *verbose-resize*
	      (display-geometry (/ (- new-width basew) incw)
				(/ (- new-height baseh) inch)))
	    (case *card-point*
	      ((or :north :ne) (incf new-y (- tmp-height new-height)))
	      ((or :west :sw) (incf new-x (- tmp-width new-width)))
	      (:nw (incf new-y (- tmp-height new-height))
		   (incf new-x (- tmp-width new-width))))
	    (setf (window-position master-win) (values new-x new-y))
	    (setf (drawable-sizes master-win)
		  (values new-width new-height))))))))

;; Finish the resize process :
;; call when button-release on root
;; and when root-resize-status is not nil.
(defun finish-resize (master)
  (when *verbose-resize* (undraw-geometry-info-box))
  (with-slots (window gcontext) master
    (when (and (decoration-active-p master) (eq *resize-mode* :box))
      (draw-window-grid (widget-window *clone*) gcontext *root-window*)
      (multiple-value-bind (x y w h)
	  (window-geometry (widget-window *clone*))
	(setf (window-position window) (values x y)
	      (drawable-sizes window) (values w h))
	(resize-from master))))
  (setf *card-point* nil))

;;;; Move.
	  
(defmethod initialize-move ((widget base-widget) (event button-press))
  (with-slots (window active-p) widget
    (setf active-p  t
	  *delta-x* (- (event-root-x event) (xlib:drawable-x window))
	  *delta-y* (- (event-root-y event) (xlib:drawable-y window)))))

(defmethod initialize-move :after ((master decoration) (event button-press))
  (let ((app-window (get-child master :application :window t)))
    (when (or (member :WIN_STATE_FIXED_POSITION (gnome:win-state app-window))
	      (member :_NET_WM_STATE_STICKY (netwm:net-wm-state app-window)))
      (setf (decoration-active-p master) nil))))

(defun move-widget (widget event &optional display-info-p)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (window active-p gcontext) widget
    (when active-p
      (let ((new-x (- (the (signed-byte 16) (event-root-x event)) *delta-x*))
	    (new-y (- (the (signed-byte 16) (event-root-y event)) *delta-y*)))
	(when display-info-p (display-coordinates new-x new-y))
	(if (and (decoration-p widget) (eql *move-mode* :box))
	    (with-slots (window) *clone*	    
	      (draw-window-grid window gcontext *root-window*)
	      (setf (window-position window) (values new-x new-y))
	      (draw-window-grid window gcontext *root-window*))
	    (setf (window-position window) (values new-x new-y)))))))

(defun finalize-move (master)
  (when (eql *move-mode* :box)
    (with-slots (window gcontext) *clone*
      (draw-window-grid window gcontext *root-window*)
      (setf (window-position (widget-window master)) (window-position window))))
  (setf (decoration-active-p master) nil)
  (when *verbose-move* (undraw-geometry-info-box))
  (with-slots (armed active-p) (get-child master :title-bar)
    (setf armed nil active-p nil))
  (send-configuration-notify (get-child master :application :window t)))
