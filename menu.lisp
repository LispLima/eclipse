;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: menu.lisp,v 1.8 2004-11-30 23:48:10 ihatchondo Exp $
;;;
;;; This file is part of Eclipse
;;; Copyright (C) 2000, 2001, 2002 Iban HATCHONDO, Robert STRANDH
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
    				
;;; Pop-up menu

(in-package :ECLIPSE-INTERNALS)

(defvar *gctxt*)
(defvar *black*)
(defvar *white*)
(defvar *background1*) 
(defvar *background2*)
(defvar *menu-item-margin* 2)
(defvar *default-menu-height* 20)
(defvar *max-char-width* 7)
(defvar *menu-color*
  (xlib:make-color :red 0.8 :blue 0.8 :green 0.8)
  "The menu background color")
(defvar *menu-hilighted-color*
  (xlib:make-color :red 0.85 :blue 0.85 :green 0.85)
  "The menu entry color hilighted (e.g: when the mouse passes over it)")

(defgeneric menu-root (item))
(defgeneric menu-root-application-window (item))
(defgeneric arm (item))
(defgeneric arm-branch (item))
(defgeneric disarm (item))
(defgeneric destroy-substructure (item))
(defmethod destroy-substructure ((item null)) nil)


(declaim (inline get-max-item-width))
(defun get-max-item-width (items)
  (* *max-char-width*
     (+ 3 (loop for item in items maximize (length (slot-value item 'text))))))

(defun make-raise-window (window)
  (when window
    (multiple-value-bind (width height) (drawable-sizes window)
      (setf (xlib:gcontext-foreground *gctxt*) *background2*)
      (xlib:draw-rectangle window *gctxt* 0 0 width height t)
      (setf (xlib:gcontext-foreground *gctxt*) *white*)
      (xlib:draw-lines window *gctxt* (list 1 height 1 1 1 1 width 1))
      (setf (xlib:gcontext-foreground *gctxt*) *black*)
      (xlib:draw-lines window *gctxt*
		       (list 1 (1- height) (1- width) (1- height)
			     (1- width) (1- height) (1- width) 0)))))

(defun highlight (item)
  (declare (inline draw-glyphs))
  (with-slots (window armed text) item
    (when window
      (let ((height (xlib:drawable-height window)))
	(make-raise-window window)
	(setf (xlib:gcontext-foreground *gctxt*) *black*)
	(draw-glyphs window *gctxt* 7 (floor height 3/2) text)))))

(defun make-background-pixmap (window width height)
  (let ((pix (xlib:create-pixmap
	         :drawable window
		 :width width
		 :height height
		 :depth (xlib:drawable-depth window))))
    (make-raise-window pix)
    pix))

;;; menu-widget

(defclass menu-widget ()
  ((armed :initform nil)))

;;; menu-item

(defclass menu-item (menu-widget)
  ((window :initform nil)
   (client :initform nil :initarg :client)
   (text :initarg :text)))

(defun realize-menu-items (parent width items
			   &key map
			        (x *menu-item-margin*) (y *menu-item-margin*))
  (mapc #'(lambda (item)
	    (with-slots (window) item
	      (setf window
		    (xlib:create-window
		       :parent parent
		       :x x :y y
		       :width width
		       :height *default-menu-height*
		       :background *background1*
		       :override-redirect :on
		       :event-mask '(:exposure
				     :button-press
				     :button-release
				     :enter-window
				     :leave-window
				     :key-press
				     :key-release
				     :owner-grab-button)))
              (save-widget window item)
	      (incf y *default-menu-height*)
	      (when map (xlib:map-window window))))
	 items))

(defmethod menu-root ((item menu-item))
  (menu-root (slot-value item 'client)))

(defmethod menu-root-application-window ((item menu-item))
  (menu-root-application-window (slot-value item 'client)))

(defmethod arm ((item menu-item))
  (with-slots (client armed) item
    (unless armed
      (arm client)
      (mapc #'disarm (slot-value client 'items))
      (setf (slot-value item 'armed) t)
      (event-process (make-instance 'exposure) item))))

(defmethod arm-branch ((item menu-item))
  (with-slots (client armed) item
    (if armed
	(mapc #'destroy-substructure (slot-value item 'items))
	(progn
	  (arm client)
	  (mapc #'destroy-substructure (slot-value client 'items))
	  (arm item)))))

(defmethod event-process ((event exposure) (item menu-item))
  (declare (inline draw-glyphs))
  (with-slots (window armed client text) item
    (when window
      (multiple-value-bind (width height) (drawable-sizes window)
	(setf (xlib:gcontext-foreground *gctxt*) *background1*)
	(xlib:draw-rectangle window *gctxt* 0 0 width height t)
	(setf (xlib:gcontext-foreground *gctxt*) *black*)
	(draw-glyphs window *gctxt* 7 (floor height 3/2) text)))))

(defmethod event-process ((event leave-notify) (item menu-item))
  (when (eql :ungrab (event-mode event))
    (destroy-substructure (menu-root item))))

(defmethod event-process ((event enter-notify) (item menu-item))
  (when (slot-value (slot-value item 'client) 'armed)
    (arm-branch item)))

(defmethod event-process ((event button-press) (item menu-item))
  (arm-branch item))

;;; menu-leaf-item

(defclass menu-leaf-item (menu-item)
  ((callback :initform nil :initarg :callback :reader menu-leaf-callback)))

(defun make-menu-leaf-item (text parent callback)
  (make-instance 'menu-leaf-item :text text :callback callback :client parent))

(defmethod destroy-substructure ((item menu-leaf-item))
  (setf (slot-value item 'armed) nil))

(defmethod disarm ((item menu-item))
  (setf (slot-value item 'armed) nil)
  (event-process (make-instance 'exposure) item))

(defmethod event-process :after ((event exposure) (item menu-leaf-item))
  (with-slots (armed) item
    (when armed
      (highlight item))))

(defmethod event-process ((event button-press) (item menu-leaf-item)))

(defmethod event-process ((event button-release) (item menu-leaf-item))
  (with-slots (armed callback) item
    (when armed
      (when callback (funcall (the function callback)))
      (destroy-substructure (menu-root item)))))

(defmethod event-process ((event leave-notify) (item menu-leaf-item))
  (disarm item))

;;; sub-menu

(defclass sub-menu (menu-item)
  ((items :initarg :items)
   (item-container :initform nil)
   (bottomp :initform nil :initarg :bottomp)
   (has-substructure :initform nil)))

(defun make-sub-menu (item parent &optional bottomp)
  (let ((sub-menu (make-instance 'sub-menu
		    :text (car item)
		    :client parent
		    :bottomp bottomp)))
    (setf (slot-value sub-menu 'items)
	  (mapcar #'(lambda (item)
		      (if (or (null (cdr item)) (functionp (cdr item)))
			  (make-menu-leaf-item (car item) sub-menu (cdr item))
			  (make-sub-menu item sub-menu)))
		  (cdr item)))
    sub-menu))

(defun make-substructure (sub-menu)
  (with-slots (window items has-substructure bottomp item-container) sub-menu
    (let ((root-window (menu-root-application-window sub-menu))
	  (subheight (* *default-menu-height* (length items)))
	  (subwidth (+ (get-max-item-width items) (* 2 *menu-item-margin*))))
      (multiple-value-bind (x y)
	  (xlib:translate-coordinates window 0 0 root-window)
	(if bottomp
	    (incf y (xlib:drawable-height window))
	    (incf x (xlib:drawable-width window)))
	(incf subheight (* 2 *menu-item-margin*))
	(if (< (xlib:drawable-width root-window) (+ x subwidth))
	    (decf x (+ subwidth (xlib:drawable-width window) *menu-item-margin*))
	    (incf x *menu-item-margin*))
	(when (< (xlib:drawable-height root-window) (+ y subheight))
	  (decf y (- subheight *default-menu-height*)))
	(setf item-container (xlib:create-window
			      :parent root-window
			      :x x :y y
			      :width subwidth
			      :height subheight
			      :override-redirect :on
			      :background (make-background-pixmap
					      root-window
					      subwidth
					      subheight))))
      (save-widget item-container sub-menu)
      (decf subwidth (* 2 *menu-item-margin*))
      (realize-menu-items item-container subwidth items))))

(defmethod destroy-substructure ((item sub-menu))
  (with-slots (items armed has-substructure window item-container) item
    (when has-substructure
      (mapc #'(lambda (item)
		(destroy-substructure item)
                (clear-widget (slot-value item 'window))
		(setf (slot-value item 'window) nil))
	    items)
      (clear-widget item-container)
      (xlib:destroy-window item-container))
    (setf armed nil
	  has-substructure nil
	  item-container nil)))

(defmethod arm-branch :after ((sub-menu sub-menu))
  (with-slots (has-substructure item-container window) sub-menu
    (unless has-substructure
      (make-substructure sub-menu)
      (xlib:map-window item-container)
      (xlib:map-subwindows item-container)
      (setf has-substructure t))))

(defmethod event-process :after ((event exposure) (item sub-menu))
  (with-slots (armed has-substructure) item
    (when (or armed has-substructure)
      (highlight item))))

(defmethod event-process ((event button-release) (item sub-menu))
  (with-slots (armed) item
    (when armed
      (destroy-substructure (menu-root item)))))

(defmethod event-process ((event leave-notify) (sub-menu sub-menu))
  (declare (ignorable sub-menu))
  nil)

;;; pop-up menu

(defclass pop-up-menu (menu-widget)
  ((window :initform nil)
   (client :initform nil :initarg :client)
   (items :initarg :items)))

(defun make-pop-up (application &rest menu-pop-up-items)
  (let ((pop-up (make-instance 'pop-up-menu)))
    (with-slots (client items) pop-up
      (setf client application
	    items
	    (mapcar #'(lambda (item)
			(if (or (null (cdr item)) (functionp (cdr item)))
			    (make-menu-leaf-item (car item) pop-up (cdr item))
			    (make-sub-menu item pop-up)))
		    menu-pop-up-items)))
    pop-up))

(defun realize-pop-up (pop-up-menu x y)
  (with-slots (window menu-present armed items) pop-up-menu
    (when window (destroy-substructure pop-up-menu))
    (let ((root-window (menu-root-application-window pop-up-menu))
	  (subheight (* *default-menu-height* (length items)))
	  (subwidth (get-max-item-width items)))
      (when (< (xlib:drawable-width root-window) (+ x subwidth))
	(decf x subwidth))
      (when (< (xlib:drawable-height root-window) (+ y subheight))
	(decf y subheight))
      (setf window (xlib:create-window
		      :parent root-window
		      :x x :y y
		      :width (+ subwidth (* 2 *menu-item-margin*))
		      :height (+ subheight (* 2 *menu-item-margin*))
		      :override-redirect :on
		      :background (make-background-pixmap
				     root-window
				     (+ subwidth (* 2 *menu-item-margin*))
				     (+ subheight (* 2 *menu-item-margin*))))
	    armed t)
      (save-widget window pop-up-menu)
      (xlib:map-window window)
      (realize-menu-items window subwidth items :map t))))

(defmethod destroy-substructure ((pop-up-menu pop-up-menu))
  (with-slots (window items) pop-up-menu
    (when window
      (mapc #'(lambda (item)
		(destroy-substructure item)
                (clear-widget (slot-value item 'window))
		(setf (slot-value item 'window) nil))
	    items)
      (xlib:destroy-window window)
      (clear-widget window)
      (setf (slot-value pop-up-menu 'armed) nil
	    window nil))))

(defmethod arm ((widget pop-up-menu))
  (setf (slot-value widget 'armed) t))

(defmethod disarm ((widget pop-up-menu))
  (setf (slot-value widget 'armed) nil))

(defmethod menu-root ((widget pop-up-menu))
  widget)

(defmethod menu-root-application-window ((widget pop-up-menu))
  (slot-value (slot-value widget 'client) 'window))

