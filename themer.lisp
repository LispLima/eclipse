;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: themer.lisp,v 1.5 2003/09/08 12:58:05 hatchond Exp $
;;;
;;; This file is part of Eclipse.
;;; Copyright (C) 2002 Iban HATCHONDO
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

(defparameter *themes* (make-hash-table :test #'equal))

(declaim (inline lookup-theme))
(defun lookup-theme (name)
  "return named theme or nil"
  (declare (optimize (speed 3) (safety 1)))
  (gethash name *themes*))

(defclass frame-style ()
  ((name 
     :initform "no name"
     :type string
     :initarg :theme-name
     :reader frame-style-theme-name)
   (title-bar-position
     :initform :top
     :type keyword
     :initarg :title-bar-position
     :reader style-title-bar-position)
   (parts-to-redraw-on-focus
     :initform nil
     :type list
     :initarg :parts-to-redraw-on-focus
     :reader style-parts-to-redraw-on-focus)
   (nb-buttons
     :initform 0
     :initarg :nb-buttons
     :reader style-nb-buttons)
   (pixmap-table 
     :initform (make-hash-table) 
     :reader style-pixmap-table)
   (frame-item-pixmaps 
     :initform (make-hash-table) 
     :reader style-frame-item-pixmaps)
   (top-left-w
     :initform 0
     :initarg :top-left-w
     :type (unsigned-byte 16)
     :reader top-left-w)
   (top-left-h
     :initform 0
     :initarg :top-left-h
     :type (unsigned-byte 16)
     :reader top-left-h)
   (top-right-w
     :initform 0
     :initarg :top-right-w
     :type (unsigned-byte 16)
     :reader top-right-w)
   (top-right-h
     :initform 0
     :initarg :top-right-h
     :type (unsigned-byte 16)
     :reader top-right-h)
   (bottom-right-w
     :initform 0
     :initarg :bottom-right-w
     :type (unsigned-byte 16)
     :reader bottom-right-w)
   (bottom-right-h
     :initform 0
     :initarg :bottom-right-h
     :type (unsigned-byte 16)
     :reader bottom-right-h)
   (bottom-left-w
     :initform 0
     :initarg :bottom-left-w
     :type (unsigned-byte 16)
     :reader bottom-left-w)
   (bottom-left-h
     :initform 0
     :initarg :bottom-left-h
     :type (unsigned-byte 16)
     :reader bottom-left-h)
   (left-margin
     :initform 0
     :type (unsigned-byte 16)
     :initarg :left-margin
     :reader style-left-margin)
   (right-margin
     :initform 0
     :type (unsigned-byte 16)
     :initarg :right-margin
     :reader style-right-margin)
   (top-margin
     :initform 0
     :type (unsigned-byte 16)
     :initarg :top-margin
     :reader style-top-margin)
   (bottom-margin
     :initform 0
     :type (unsigned-byte 16)
     :initarg :bottom-margin
     :reader style-bottom-margin)
   (vmargin
     :initform 0
     :type (unsigned-byte 16)
     :initarg :vmargin
     :reader style-vmargin)
   (hmargin
     :initform 0
     :type (unsigned-byte 16)
     :initarg :hmargin
     :reader style-hmargin)     
   ))

(defclass default-style (frame-style) ())
(defclass transient-style (frame-style) ())

(defun default-style-p (astyle)
  (typep astyle 'default-style))

(defun transient-style-p (astyle)
  (typep astyle 'transient-style))

(defun widget->frame-item-key (widget)
  "returns the keyword or nil that correspond to the widget."
  (typecase widget
    (close-button :close)
    (maximize-button :maximize)
    (iconify-button :icon-b)
    (menu-button :menu-button)
    (title-bar :title-bar)
    (top :top)
    (top-left :top-left)
    (top-right :top-right)
    (right :right)
    (left :left)
    (bottom :bottom)
    (bottom-right :bottom-right)
    (bottom-left :bottom-left)))

(defmethod style-title-bar-direction ((style frame-style))
  (case (style-title-bar-position style)
    ((:top :bottom) :horizontal)
    ((:right :left) :vertical)))

(defmethod get-pixmap ((style frame-style) pixmap-key)
  "returns the pixmap associated with the given pixmap keyword."
  (gethash pixmap-key (style-pixmap-table style)))

(defmethod frame-item-pixmaps ((style frame-style) frame-item-key)
  "returns the pixmaps array associated with the given frame item keyword."
  (or (gethash frame-item-key (style-frame-item-pixmaps style))
      '#(nil nil nil nil)))

(defmethod frame-item-exist-p ((style frame-style) frame-item-key)
  (gethash frame-item-key (style-frame-item-pixmaps style)))

(defmethod frame-item-sizes ((style frame-style) frame-item-key)
  "returns the sizes, as a multiple value, of the frame item according to
   the sizes of the first pixmap in its associated pixmaps array."
  (let ((pixmap (aref (frame-item-pixmaps style frame-item-key) 0)))
    (if (xlib:pixmap-p pixmap) (drawable-sizes pixmap) (values 0 0))))

(defmethod frame-item-width ((style frame-style) frame-item-key)
  "returns the width of the frame item according to the width of the
   first pixmap in its associated pixmaps array."
  (let ((pixmap (aref (frame-item-pixmaps style frame-item-key) 0)))
    (if (xlib:pixmap-p pixmap) (xlib:drawable-width pixmap) 0)))

(defmethod frame-item-height ((style frame-style) frame-item-key)
  "returns the height of the frame item according to the width of the
   first pixmap in its associated pixmaps array."
  (let ((pixmap (aref (frame-item-pixmaps style frame-item-key) 0)))
    (if (xlib:pixmap-p pixmap) (xlib:drawable-height pixmap) 0)))

(defmethod frame-button-sizes ((style frame-style))
  (with-slots (frame-item-pixmaps) style
    (let ((bpixmaps (or (gethash :close frame-item-pixmaps)
			(gethash :maximize frame-item-pixmaps)
			(gethash :icon-b frame-item-pixmaps))))
      (and bpixmaps (drawable-sizes (aref bpixmaps 0))))))

(defmethod free-frame-style ((style frame-style))
  "Release all X resources that are associated with this style."
  (with-slots (pixmap-table frame-item-pixmaps) style
    (loop for pixmap being each hash-value in pixmap-table
	  for id = (xlib:drawable-id pixmap)
	  when (xlib::lookup-resource-id (xlib:drawable-display pixmap) id)
	  do (ignore-errors (xlib:free-pixmap pixmap)))
    (clrhash pixmap-table)
    (clrhash frame-item-pixmaps)
    (setf pixmap-table nil 
	  frame-item-pixmaps nil)))

(defclass theme ()
  ((name 
     :initform "no name" 
     :type string
     :initarg :name 
     :reader theme-name)
   (default-style
     :initform nil
     :initarg :default-style
     :reader theme-default-style)
   (transient-style
     :initform nil
     :initarg :transient-style
     :reader theme-transient-style)
   ))

;;;; build-in no decoration theme.

(defpackage "NO-DECORATION-ECLIPSE-THEME" (:size 0))

(setf (gethash "no-decoration" *themes*)
      (make-instance 
          'theme :name "no-decoration"
	  :default-style (make-instance
			     'default-style
			     :theme-name "no-decoration"
			     :title-bar-position :none)))

;;;; misc functions.

(defun pixmap-width (pixmap)
  (if (xlib:pixmap-p pixmap) (xlib:drawable-width pixmap) 0))

(defun pixmap-height (pixmap)
  (if (xlib:pixmap-p pixmap) (xlib:drawable-height pixmap) 0))

(defun load-pnm->pixmap (directory fname window)
  (setf fname (concatenate 'string directory fname ".pnm"))
  (when (probe-file fname)
    (xlib:image-pixmap window (ppm:load-ppm-into-clx-image fname window))))

(defun find-decoration-frame-style (theme window)
  (with-slots (default-style transient-style) theme
    (if (ignore-errors (window-transient-p window))
	(or transient-style default-style)
	default-style)))

(defun ensure-theme-directory-exists (theme-dir)
  (unless (char= (char theme-dir (1- (length theme-dir))) #\/)
    (setf theme-dir (format nil "~A/" theme-dir)))
  (unless (directory theme-dir)
    (setf theme-dir (if (directory (eclipse-path "themes/" theme-dir))
			(eclipse-path "themes/" theme-dir)
			(eclipse-path "themes/microGUI/"))))
  theme-dir)

;;;; theme manipulation.

;; I defined this here, just to avoid compilation warnings.
;; But it doesn't matter, because just before loading a theme 
;; (fmakunbound 'initialize-frame) is called.
(defun initialize-frame (directory-name window) 
  (declare (ignorable directory-name window))
  (values))

(defun free-theme (name)
  "Release all X resources that are associated with the named theme."
  (with-slots (default-style transient-style) (gethash name *themes*)
    (and default-style (free-frame-style default-style))
    (and transient-style (free-frame-style transient-style)))
  (remhash name *themes*)
  (unuse-package (format nil "~:@(~A~)-ECLIPSE-THEME" name)))

(defun load-theme (root-window name)
  "loads and returns theme named by parameter name. Themes are cached."
  (unless (gethash name *themes*)
    (fmakunbound 'initialize-frame)
    (setf name (ensure-theme-directory-exists name))
    (load (concatenate 'string name "theme.o"))
    (setf name (theme-name (initialize-frame name root-window))))
  (use-package (format nil "~:@(~A~)-ECLIPSE-THEME" name))
  (gethash name *themes*))

(defun make-keyword (string)
  (intern (with-standard-io-syntax (string-upcase string)) :keyword))

(defmacro define-theme ((theme-name) (&rest forms))
  (flet 
      ((define-style (style-to-define items directory window &optional style)
	 `(with-slots ((pixmap-table eclipse::pixmap-table)
		       (frame-item-pixmaps eclipse::frame-item-pixmaps)
		       (nb-buttons eclipse::nb-buttons)
		       (top-margin eclipse::top-margin)
		       (bottom-margin eclipse::bottom-margin)
		       (left-margin eclipse::left-margin)
		       (right-margin eclipse::right-margin)
		       (vmargin eclipse::vmargin)
		       (hmargin eclipse::hmargin)
		       (top-left-w eclipse::top-left-w)
		       (top-left-h eclipse::top-left-h)
		       (top-right-w eclipse::top-right-w)
		       (top-right-h eclipse::top-right-h)
		       (bottom-right-w eclipse::bottom-right-w)
		       (bottom-right-h eclipse::bottom-right-h)
		       (bottom-left-w eclipse::bottom-left-w)
		       (bottom-left-h eclipse::bottom-left-h)) ,style-to-define
	    ,@(loop for (key names) in items
		    for array = (make-array 4 :initial-element nil)
		    when (eq key :custom)
		    do (setf array (make-array (length names))) end
		    when (case key ((:close :icon-b :maximize) t))
		    collect `(incf nb-buttons) end
		    collect `(setf (gethash ,key frame-item-pixmaps) ,array)
		    nconc  
		     (loop for name in names
			   for i from 0
			   for k = (eclipse::make-keyword name)
			   for pix = (gensym)
			   for form = 
			    `(let ((,pix 
				    (or ,(and style 
					      `(eclipse:get-pixmap ,style ,k))
					(eclipse:load-pnm->pixmap 
					    ,directory ,name ,window))))
			       (setf (gethash ,k pixmap-table) ,pix)
			       (setf (aref ,array ,i) ,pix))
			   collect form))
	    (multiple-value-setq (top-left-w top-left-h)
	      (eclipse:frame-item-sizes ,style-to-define :top-left))
	    (multiple-value-setq (top-right-w top-right-h)
	      (eclipse:frame-item-sizes ,style-to-define :top-right))
	    (multiple-value-setq (bottom-right-w bottom-right-h)
	      (eclipse:frame-item-sizes ,style-to-define :bottom-right))
	    (multiple-value-setq (bottom-left-w bottom-left-h)
	      (eclipse:frame-item-sizes ,style-to-define :bottom-left))
	    (setf top-margin (eclipse:frame-item-height ,style-to-define :top))
	    (setf bottom-margin 
	          (eclipse:frame-item-height ,style-to-define :bottom))
	    (setf left-margin (eclipse:frame-item-width ,style-to-define :left))
	    (setf right-margin
	          (eclipse:frame-item-width ,style-to-define :right))
	    (setf hmargin (+ left-margin right-margin)
		  vmargin (+ top-margin bottom-margin))))

       (parse-args (args)
	 (destructuring-bind (style
			      (&key (title-bar-position :top))
			      (&key parts-to-redraw-on-focus)
			      &rest rest) 
	     args
	   (list style 
		 title-bar-position
		 (if (eq parts-to-redraw-on-focus :all)
		     (loop for (key nil) in rest
			   when (eq key title-bar-position) 
			   do (setq key :title-bar) end
			   unless (eq key :custom) collect key)
		     parts-to-redraw-on-focus)
		 rest))))
	 
    (destructuring-bind ((style1 title-pos1 parts-to-redraw-on-focus1 items1)
			 (style2 title-pos2 parts-to-redraw-on-focus2 items2))
	(mapcar #'parse-args forms)

      (let ((fs1 (gensym))
	    (fs2 (gensym)))
	`(defun initialize-frame (dir window)
	   (let ((,fs1 ,(and items1 `(make-instance 
				         ',(intern (symbol-name style1)
						   "ECLIPSE-INTERNALS")
				         :theme-name ,theme-name
				         :title-bar-position ,title-pos1
				         :parts-to-redraw-on-focus
				         ',parts-to-redraw-on-focus1)))
		 (,fs2 ,(and items2 `(make-instance
				         ',(intern (symbol-name style2)
						   "ECLIPSE-INTERNALS")
				         :theme-name ,theme-name
				         :title-bar-position ,title-pos2
				         :parts-to-redraw-on-focus
				         ',parts-to-redraw-on-focus2))))
	     ,(unless items2 `(declare (ignorable ,fs2)))
	     ,(when items1 (define-style fs1 items1 `dir `window))
	     ,(when items2 (define-style fs2 items2 `dir `window fs1))
	     (setf (gethash ,theme-name eclipse::*themes*)
		   (make-instance 'eclipse::theme 
				  :name ,theme-name 
				  ,@(and style1 `(,style1 ,fs1))
				  ,@(and style2 `(,style2 ,fs2))))))))))

