;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: rectangles.lisp,v 1.3 2004/02/13 15:20:26 ihatchondo Exp $
;;;
;;; This file is part of Eclipse.
;;; Copyright (C) 2003 Iban HATCHONDO
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

(in-package :ECLIPSE-INTERNALS)

(defstruct rectangle
  (ulx 0 :type (signed-byte 16))
  (uly 0 :type (signed-byte 16))
  (lrx 0 :type (signed-byte 16))
  (lry 0 :type (signed-byte 16)))

(declaim (inline rectangle-coordinates))
(defun rectangle-coordinates (r)
  (values (rectangle-ulx r) (rectangle-uly r)
	  (rectangle-lrx r) (rectangle-lry r)))

(declaim (inline rectangle-surface))
(defun rectangle-surface (rectangle)
  "Compute the area of a rectangle. The value NIL represents an empty rectangle"
  (if (null rectangle) 0
      (multiple-value-bind (ulx uly lrx lry) (rectangle-coordinates rectangle)
	(* (- lrx ulx) (- lry uly)))))

(declaim (inline rectangle-width))
(defun rectangle-width (rect)
  "Returns the width of a rectangle."
  (if (null rect) 0 (- (rectangle-lrx rect) (rectangle-ulx rect))))

(declaim (inline rectangle-height))
(defun rectangle-height (rect)
  "Returns the height of a rectangle."
  (if (null rect) 0 (- (rectangle-lry rect) (rectangle-uly rect))))

(defun rectangle-surface< (rectangle1 rectangle2)
  (< (rectangle-surface rectangle1) (rectangle-surface rectangle2)))

(defun rectangle-surface>= (rectangle1 rectangle2)
  (>= (rectangle-surface rectangle1) (rectangle-surface rectangle2)))

(defun rectangle-surface= (rectangle1 rectangle2)
  (= (rectangle-surface rectangle1) (rectangle-surface rectangle2)))

(defun rectangle-width>= (rect1 rect2)
  (>= (rectangle-width rect1) (rectangle-width rect2)))

(defun rectangle-height>= (rect1 rect2)
  (>= (rectangle-height rect1) (rectangle-height rect2)))

(defun sub-rectangles (outside inside)
  "Returns the four (if exists) sub rectangles defined by the internal
   rectangle in the outside one. The returned list will be sort ascending
   order."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ulx1 uly1 lrx1 lry1) (rectangle-coordinates outside)
    (declare (type (signed-byte 16) ulx1 uly1 lrx1 lry1))
    (multiple-value-bind (ulx2 uly2 lrx2 lry2) (rectangle-coordinates inside)
      (declare (type (signed-byte 16) ulx2 uly2 lrx2 lry2))
      (let ((seq (list)))
	(when (< uly1 uly2) ; defines the north sub rectangle.
	  (push (make-rectangle :ulx ulx1 :uly uly1 :lrx lrx1 :lry uly2) seq))
	(when (< ulx1 ulx2) ; defines the west sub rectangle.
	  (push (make-rectangle :ulx ulx1 :uly uly1 :lrx ulx2 :lry lry1) seq))
	(when (< lry2 lry1) ; defines the south sub rectangle.
	  (push (make-rectangle :ulx ulx1 :uly lry2 :lrx lrx1 :lry lry1) seq))
	(when (< lrx2 lrx1) ; defines the east sub rectangle.
	  (push (make-rectangle :ulx lrx2 :uly uly1 :lrx lrx1 :lry lry1) seq))
	(stable-sort seq #'rectangle-surface>=)))))

(defun overlap-p (rect1 rect2)
  "Returns true if rectangle1 intersects rectangle2."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ulx1 uly1 lrx1 lry1) (rectangle-coordinates rect1)
    (declare (type (signed-byte 16) ulx1 uly1 lrx1 lry1))
    (multiple-value-bind (ulx2 uly2 lrx2 lry2) (rectangle-coordinates rect2)
      (declare (type (signed-byte 16) ulx2 uly2 lrx2 lry2))
      (and (< ulx1 lrx2) (< ulx2 lrx1) (< uly1 lry2) (< uly2 lry1)))))

(defun include-p (rect1 rect2)
  "Return true if rectangle1 is included in rectangle2."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ulx1 uly1 lrx1 lry1) (rectangle-coordinates rect1)
    (declare (type (signed-byte 16) ulx1 uly1 lrx1 lry1))
    (multiple-value-bind (ulx2 uly2 lrx2 lry2) (rectangle-coordinates rect2)
      (declare (type (signed-byte 16) ulx2 uly2 lrx2 lry2))
      (and (<= ulx1 ulx2) (<= uly1 uly2) (<= lrx2 lrx1) (<= lry2 lry1)))))

(defun rectangle-intersection (rect1 rect2)
  "Returns the intersection of rect1 and rect2 or nil of they do not overlap."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (ulx1 uly1 lrx1 lry1) (rectangle-coordinates rect1)
    (declare (type (signed-byte 16) ulx1 uly1 lrx1 lry1))
    (multiple-value-bind (ulx2 uly2 lrx2 lry2) (rectangle-coordinates rect2)
      (declare (type (signed-byte 16) ulx2 uly2 lrx2 lry2))
      (when (and (< ulx1 lrx2) (< ulx2 lrx1) (< uly1 lry2) (< uly2 lry1))
	(make-rectangle :ulx (max ulx1 ulx2) :uly (max uly1 uly2)
			:lrx (min lrx1 lrx2) :lry (min lry1 lry2))))))

(defun rectangle-intersection* (rectangle rectangles)
  "Return all the intersections of rectangle with each rectangle of the list."
  (loop for rect in rectangles when (rectangle-intersection rectangle rect)
	collect it))

(defun find-empty-rectangles (space obstacles order)
  "Returns all the largest empty rectangles of space according 
   to the obstacles list sorted according order."
  (setf obstacles (stable-sort
		      (rectangle-intersection* space obstacles) 
		      #'rectangle-surface>=))
  (unless obstacles (return-from find-empty-rectangles (list space)))
  (loop for ss in (sub-rectangles space (car obstacles))
	nconc (find-empty-rectangles ss (cdr obstacles) order) into result
	finally (return (stable-sort result order))))

(defun window->rectangle (window)
  "Returns the rectangle that represent this window."
  (multiple-value-bind (x y w h) (window-geometry window)
    (make-rectangle :ulx x :uly y :lrx (+ x w) :lry (+ y h))))

(defun compute-screen-rectangles (application &optional filter-overlap-p)
  "Gets screen content according to desktop number and filter all windows that 
   are overlaped by the given one except if filter-overlap-p is NIL. Returns a
   list of rectangles that represent all the founded windows."
  (with-slots (window master) application
    (multiple-value-bind (xx yy ww hh) 
	(window-geometry (if master (widget-window master) window))
      (flet ((predicate (win n icon taskbar desktop dock)
	       (cond 
		 ((xlib:window-equal window win) nil)
		 ((window-belongs-to-vscreen-p win n icon taskbar desktop dock)
		  (not (and filter-overlap-p
			    (multiple-value-bind (x y w h)
				(with-slots ((m master)) (lookup-widget win)
				  (window-geometry (if m (widget-window m) win)))
			      (and (< xx (+ x w)) (< x (+ xx ww))
				   (< yy (+ y h)) (< y (+ yy hh)))))))
		 (t (window-panel-p win n icon)))))
	(mapcar 
	    (lambda (win)
	      (with-slots ((m master)) (lookup-widget win)
		(window->rectangle (if m (widget-window m) win))))
	    (screen-content (window-desktop-num window)
			    :predicate #'predicate :skip-taskbar nil))))))

(defun find-all-panel-rectangles (scr-num &key (predicate #'window-panel-p))
  "Gets all panel type window on this virtual desktop. Return a list of
   rectangles representing all the founded windows."
  (mapcar
      (lambda (win)
	(multiple-value-bind (l r to b lsy ley rsy rey tsx tex bsx bex)
	    (netwm:net-wm-strut-partial win)
	  (multiple-value-bind (w h) (drawable-sizes (xlib:drawable-root win))
	    (unless l
	      (multiple-value-setq (l r to b) (netwm:net-wm-strut win))
	      (multiple-value-setq (lsy ley rsy rey tsx tex bsx bex)
		(values 0 h 0 h 0 w 0 w))
	      (unless (and l r to b)
		(setf (values l r to b) (values 0 0 0 0))))
	    (cond
	      ((/= 0 l) (make-rectangle :ulx 0 :uly lsy :lrx l :lry ley))
	      ((/= 0 r) (make-rectangle :ulx (- w r) :uly rsy :lrx w :lry rey))
	      ((/= 0 to) (make-rectangle :ulx tsx :uly 0 :lrx tex :lry to))
	      ((/= 0 b) (make-rectangle :ulx bsx :uly (- h b) :lrx bex :lry h))
	      (t (window->rectangle win))))))
      (screen-content scr-num :predicate predicate)))

(defun window-panel-p (window scr-num iconify-p &rest options)
  "Returns true if window is a panel (in the sens of Gnome/KDE panel)."
  (declare (ignorable options))
  (when (lookup-widget window)
    (let ((n (or (window-desktop-num window) -1))
	  (wm-state (car (wm-state window))))
      (and (or (= n scr-num) (= n +any-desktop+))
	   (or (eq wm-state 1) (and iconify-p (eq wm-state 3)))
	   (or (multiple-value-bind (resource class)
		   (xlib:get-wm-class window)
		 (declare (ignore resource))
		 (string= class "Panel"))
	       (member :_net_wm_window_type_dock
		       (netwm:net-wm-window-type window)))))))

(defun find-largest-empty-area (application &key area-include-me-p
				                 (panels-only-p t) direction
				                 (filter-overlap-p t))
  "Returns as a multiple values the coordinates of the largest empty area on
   the desktop of the application and a bolean indicating that such area exists.
   - If :area-include-me-p is T then the searched area WILL contain the
     application (default is NIL).
   - If :panels-only-p is T (the default), the research will not includes any
     other windows of the desktop. Otherwise all mapped windows on the desktop
     will be taken in account.
   - If :filter-overlap-p is T (the default) then overlapped applications will
     be ignored as obstacles. Otherwise They will be kept as obstacles. 
   - :direction (or :vertical :horizontal :both) to indicate wat kind of 
     region the search should be looking for."
  (with-slots (window (m master)) application
    (multiple-value-bind (w h) (drawable-sizes (xlib:drawable-root window))
      (let ((app-rect (window->rectangle (if m (widget-window m) window)))
	    (rectangles (find-empty-rectangles
			    (make-rectangle :lrx w :lry h)
			    (if panels-only-p
				(find-all-panel-rectangles 
				 (window-desktop-num window))
				(compute-screen-rectangles 
				    application filter-overlap-p))
			    (case direction
			      (:horizontal #'rectangle-width>=)
			      (:vertical #'rectangle-height>=)
			      (t #'rectangle-surface>=)))))
	;; clip the application window rectangle to fit in the root one.
	(when (< (rectangle-ulx app-rect) 0) (setf (rectangle-ulx app-rect) 0))
	(when (< (rectangle-uly app-rect) 0) (setf (rectangle-uly app-rect) 0))
	(when (> (rectangle-lrx app-rect) w) (setf (rectangle-lrx app-rect) w))
	(when (> (rectangle-lry app-rect) h) (setf (rectangle-lry app-rect) h))
	;; returns the appropriated area.
	(multiple-value-call #'values
	  (if rectangles
	      (rectangle-coordinates
	       (if area-include-me-p
		   (loop for r in rectangles until (include-p r app-rect)
			 finally (return r))
		   (car rectangles)))
	      (values 0 0 w h))
	  (if rectangles T NIL))))))
