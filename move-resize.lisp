;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: move-resize.lisp,v 1.16 2004/04/27 17:43:40 ihatchondo Exp $
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

(defun update-*clone* (x y w h decoration-frame-style &optional wm-hints)
  (with-slots (window wm-size-hints frame-style) *clone*
    (setf wm-size-hints wm-hints
	  frame-style decoration-frame-style
	  (drawable-sizes window) (values w h)
	  (window-position window) (values x y))))

(defun draw-window-grid (window gctxt dest-window)
  "Draw a 3x3 grid representing the future window geometry on the dest-window."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (x y width height) (window-geometry window)
    (declare (type (signed-byte 16) x y))
    (declare (type (unsigned-byte 16) width height))
    (decf width) (decf height)
    (xlib:with-gcontext
	(gctxt :function boole-xor :subwindow-mode :include-inferiors)
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

(defun activate-move-resize (widget root status mode verbose-p)
  "Sets some internal values for the future move or resize animations."
  (with-slots (resize-status move-status current-active-widget window) root
    (with-slots ((widget-window window) gcontext active-p) widget
      (when (and active-p (not (or resize-status move-status)))
	(or *clone* (initialize-clone))
	(update-clone widget)
	(grab-root-pointer)
	(setf (slot-value root status) t
	      current-active-widget widget)
	(when verbose-p
	  (initialize-geometry-info-box)
	  (multiple-value-bind (x y w h) (window-geometry widget-window)
	    (if (and (eq status 'resize-status) (decoration-p widget))
		(multiple-value-bind (a b c d iw ih bw bh)
		    (decoration-wm-hints widget)
		  (declare (ignore a b c d))
		  (display-geometry (/ (- w bw) iw) (/ (- h bh) ih)))
		(display-coordinates x y))))
	(when (eq mode :box)
	  (xlib:grab-server *display*)
	  (draw-window-grid widget-window gcontext window))))))

(defgeneric update-clone (widget)
  (:documentation "Initilize a widget clone for move/resize animations."))

(defmethod update-clone ((application application))
  (multiple-value-bind (x y w h) (window-geometry (widget-window application))
    (update-*clone*
        x y w h (theme-default-style (lookup-theme "no-decoration")))))

(defmethod update-clone ((master decoration))
  (multiple-value-bind (x y w h) (window-geometry (widget-window master))
    (with-slots (frame-style wm-size-hints) master
      (update-*clone* x y w h frame-style wm-size-hints))))

;;;; Resize.

(defparameter *card-point* :se)

(defgeneric resize-from (widget)
  (:documentation "Resize a decoration or an application depending on widget"))

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

(defun where-is-pointer (widget)
  "Initialize the resize process when activated from one the decoration edge." 
  (declare (optimize (speed 3) (safety 0)))
  (setf (values *delta-x* *delta-y*) (values 0 0)
	*card-point* (typecase widget
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
  (multiple-value-bind (x y width height) (window-geometry window)
    (let ((corners '#(#.'#(:nw :north :nw :west) #.'#(:north :ne :east :ne)
		      #.'#(:se :east :se :south) #.'#(:west :sw :south :sw))))
      (declare (type (simple-array (simple-array keyword (4)) (4)) corners))
      (labels ((find-c (x y rx ry w h)
		 (if (<= x rx (+ x (floor w 2)))
		     (if (<= y ry (+ y (floor h 2))) 0 3)
		     (if (<= y ry (+ y (floor h 2))) 1 2)))
	       (get-card (x y rx ry w h)
		 (let ((corner (find-c x y rx ry w h)))
		   (setf w (floor w 2))
		   (setf h (floor h 2))
		   (when (or (= corner 1) (= corner 2)) (incf x w))
		   (when (or (= corner 2) (= corner 3)) (incf y h))
		   (aref (aref corners corner) (find-c x y rx ry w h)))))
	(setf *card-point* (get-card x y root-x root-y width height))
	(when (member *card-point* '(:ne :east :se)) (incf x width))
	(when (member *card-point* '(:se :south :sw)) (incf y height))
	(setf *delta-x* (- root-x x)
	      *delta-y* (- root-y y))))))

(defun check-size (size base inc min-size max-size)
  "If the given size respects all the given constraints, then return size. 
  Otherwise returns the nearest satisfying size."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type xlib:card16 size base inc min-size max-size))
  (if (< min-size size max-size)
      (let ((k (mod (- size base) inc)))
	(declare (type xlib:card16 k))
	(if (= k 0) size (+ inc (- size k))))
      (max min-size (min size max-size))))

(defun %resize% (master motion-notify-event verbose-p)
  (declare (optimize (speed 3) (safety 0)))
  (declare (inline check-size))
  (let* ((master-win (widget-window master))
	 (root-x (event-root-x motion-notify-event))
	 (root-y (event-root-y motion-notify-event)))
    (declare (type xlib:int16 root-x root-y))
    (decf root-x *delta-x*)
    (decf root-y *delta-y*)
    (multiple-value-bind (x y width height) (window-geometry master-win)
      (declare (type xlib:int16 x y))
      (declare (type xlib:card16 width height))
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
	(declare (type xlib:int16 new-x new-y))
	(declare (type xlib:card16 tmp-width tmp-height))
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
	      ((:north :ne) (incf new-y (- tmp-height new-height)))
	      ((:west :sw) (incf new-x (- tmp-width new-width)))
	      (:nw (incf new-y (- tmp-height new-height))
		   (incf new-x (- tmp-width new-width))))
	    (xlib:with-state (master-win)
	      (setf (window-position master-win) (values new-x new-y))
	      (setf (drawable-sizes master-win)
		    (values new-width new-height)))))))))

(defun finish-resize (master &optional verbose-p mode)
  "Terminate the resize work. (undraw grid, geometry infos, ...)"
  ;; Finish the resize process:
  ;; called when button-release on root and root-resize-status is not nil.
  (with-slots (window gcontext) master
    (when (and (decoration-active-p master) (eql mode :box))
      (draw-window-grid (widget-window *clone*) gcontext *root-window*)
      (multiple-value-bind (x y w h)
	  (window-geometry (widget-window *clone*))
	(setf (window-position window) (values x y)
	      (drawable-sizes window) (values w h))
	(resize-from master))))
  (when verbose-p (undraw-geometry-info-box))
  (setf *card-point* nil))

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

(defmethod resize ((master decoration) (event motion-notify)
		   &optional verbose-p mode)
  (declare (optimize (speed 3) (safety 0)))
  (declare (inline update-edges-geometry %resize%))
  (if (eql mode :opaque)
      (with-event-mask ((slot-value master 'window))
	(%resize% master event verbose-p)
	(update-edges-geometry master)
	(resize-from master))
      (with-slots (window gcontext) *clone*
	(draw-window-grid window gcontext *root-window*)
	(%resize% *clone* event verbose-p)
	(draw-window-grid window gcontext *root-window*))))	

;;;; Move.

(defvar *screen-windows* nil)

(defgeneric initialize-move (widget event)
  (:documentation "Initialize internal values for animating the future widget
    movements."))

(defgeneric finalize-move (widget)
  (:documentation "finalize (send synthetic configure-notify ..."))

(defun region-intersect-region-p (x y w h x2 y2 w2 h2)
  "Returns true if the rectangular regions, described by the two four-uple
  `x y w h', have a not empty intersection."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (signed-byte 16) x y x2 y2))
  (declare (type (unsigned-byte 16) w h w2 h2))
  (and (<= x (+ x2 w2)) (<= x2 (+ x w)) (<= y (+ y2 h2)) (<= y2 (+ y h))))

(defun region-intersect-window-in-screen (x y w h &rest windows-to-skip)
  "Returns a window list that has an intersection with the given region
  (defines by the four-uple `x y w h'). The windows-to-skip argument is 
  a list of window that should not be used."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (signed-byte 16) x y))
  (declare (type (unsigned-byte 16) w h))
  (declare (inline region-intersect-region-p))
  (loop for win in *screen-windows*
	for widget = (lookup-widget win)
	for master = (when widget (application-master widget))
	when master do (setf win (widget-window master)) end
	when (and (or widget master)
		  (not (member win windows-to-skip :test #'xlib:window-equal))
		  (multiple-value-bind (x2 y2 w2 h2) (window-geometry win)
		    (declare (type (signed-byte 16) x2 y2))
		    (declare (type (unsigned-byte 16) w2 h2))
		    (region-intersect-region-p x y w h x2 y2 w2 h2)))
	collect win))

(defun perform-dock (window x y)
  "Returns the new coordinates of the window if it needs do be docked on
  one or two window present on that desktop. Otherwise x and y will be 
  returned. Arguments x, y represent the hypotheticals future coordinates."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (signed-byte 16) x y))
  (multiple-value-bind (x1 y1 w1 h1) (window-geometry window)
    (declare (type (signed-byte 16) x1 y1))
    (declare (type (unsigned-byte 16) w1 h1))
    (loop with x-already-set-p and y-already-set-p
	  for win in (region-intersect-window-in-screen x y w1 h1 window)
	  do (multiple-value-bind (x2 y2 w2 h2) (window-geometry win)
	       (declare (type (signed-byte 16) x2 y2))
	       (declare (type (unsigned-byte 16) w2 h2))
	       (unless x-already-set-p
		 (cond ((and (<= (+ x1 w1) x2) (<= -40 (- x2 x w1) 0))
			(setf x (- x2 w1))  (setf x-already-set-p t))
		       ((and (>= x1 (+ x2 w2)) (<= -40 (- x x2 w2) 0))
			(setf x (+ x2 w2)) (setf x-already-set-p t))))
	       (unless y-already-set-p
		 (cond ((and (>= y1 (+ y2 h2)) (<= -40 (- y y2 h2) 0))
			(setf y (+ y2 h2)) (setf y-already-set-p t))
		       ((and (<= (+ y1 h1) y2) (<= -40 (- y2 y h1) 0))
			(setf y (- y2 h1)) (setf y-already-set-p t)))))
	  when (and x-already-set-p y-already-set-p) do (loop-finish)
	  finally (return (values x y)))))

(defun perform-root-dock (window x y)
  "Returns the new coordinates of the window if it needs do be docked
  on the root window. Otherwise x and y will be returned. 
  Arguments x, y represent the hypotheticals future coordinates."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (signed-byte 16) x y))
  (multiple-value-bind (x1 y1 w1 h1) (window-geometry window)
    (declare (type (signed-byte 16) x1 y1))
    (declare (type (unsigned-byte 16) w1 h1))
    (and (>= x1 0) (< -40 x 0) (setf x 0))
    (and (>= y1 0) (< -40 y 0) (setf y 0))
    (let ((scr-w (screen-width)) (scr-h (screen-height)))
      (declare (type (unsigned-byte 16) scr-w scr-h))
      (and (>= (- scr-w x1 w1) 0) (< -40 (- scr-w x w1) 0)
	   (setf x (- scr-w w1)))
      (and (>= (- scr-h y1 h1) 0) (< -40 (- scr-h y h1) 0)
	   (setf y (- scr-h h1)))))
  (values x y))

(defun move-widget (widget event &optional verbose-p mode)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (window active-p gcontext) widget
    (when active-p
      (let ((new-x (- (the (signed-byte 16) (event-root-x event)) *delta-x*))
	    (new-y (- (the (signed-byte 16) (event-root-y event)) *delta-y*)))
	(declare (type (signed-byte 16) new-x new-y))
	(let ((aux (if (eq mode :box) (widget-window *clone*) window)))
	  (declare (inline perform-dock perform-root-dock))
	  (when *standard-window-edge-resistant-p*
	    (multiple-value-setq (new-x new-y)
	      (perform-dock aux new-x new-y)))
	  (when *screen-edge-resistant-p*
	    (multiple-value-setq (new-x new-y)
	      (perform-root-dock aux new-x new-y))))
	(when verbose-p (display-coordinates new-x new-y))
	(if (and (or (decoration-p widget) (application-p widget))
		 (eql mode :box))
	    (with-slots (window) *clone*	    
	      (draw-window-grid window gcontext *root-window*)
	      (setf (window-position window) (values new-x new-y))
	      (draw-window-grid window gcontext *root-window*))
	    (setf (window-position window) (values new-x new-y)))))))

(defun finish-move (widget &optional verbose-p mode) 
  "Terminate the move work (undraw grid, geometry infos, ...)."
  (with-slots ((widget-window window) active-p) widget
    (when (eql mode :box)
      (with-slots (window gcontext) *clone*
	(draw-window-grid window gcontext *root-window*)
	(setf (window-position widget-window) (window-position window))))
    (setf active-p nil)
    (when verbose-p (undraw-geometry-info-box)))
  (setf *screen-windows* nil)
  (finalize-move widget))

(defmethod initialize-move ((widget base-widget) (event button-press))
  (with-slots (window active-p) widget
    (setf (window-priority window) :above)
    (unless (widget-position-fix-p widget)
      (setf active-p  t
	    *delta-x* (- (event-root-x event) (xlib:drawable-x window))
	    *delta-y* (- (event-root-y event) (xlib:drawable-y window))
	    *screen-windows* (screen-content (current-desk)
					     :skip-dock nil
					     :skip-taskbar nil)))))

(defmethod finalize-move ((master decoration))
  (when (get-child master :title-bar)
    (with-slots (armed active-p) (get-child master :title-bar)
      (setf armed nil active-p nil)))
  (send-configuration-notify (get-child master :application :window t)))

(defmethod finalize-move ((application application))
  (send-configuration-notify (widget-window application)))