;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: themer.lisp,v 1.1 2002/11/07 14:54:27 hatchond Exp $
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

(defclass frame-style ()
  ((title-bar-position
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

(defmethod style-title-bar-direction ((style frame-style))
  (case (style-title-bar-position style)
    ((:top :bottom) :horizontal)
    ((:right :left) :vertical)))

(defmethod get-pixmap ((style frame-style) key)
  (gethash key (style-pixmap-table style)))

(defmethod frame-item-pixmaps ((style frame-style) key)
  (or (gethash key (style-frame-item-pixmaps style))
      '#(nil nil nil)))

(defmethod frame-item-exist-p ((style frame-style) key)
  (gethash key (style-frame-item-pixmaps style)))

(defmethod frame-item-sizes ((style frame-style) key)
  (let ((pixmap (aref (frame-item-pixmaps style key) 0)))
    (if (xlib:pixmap-p pixmap) (drawable-sizes pixmap) (values 0 0))))

(defmethod frame-item-width ((style frame-style) key)
  (let ((pixmap (aref (frame-item-pixmaps style key) 0)))
    (if (xlib:pixmap-p pixmap) (xlib:drawable-width pixmap) 0)))

(defmethod frame-item-height ((style frame-style) key)
  (let ((pixmap (aref (frame-item-pixmaps style key) 0)))
    (if (xlib:pixmap-p pixmap) (xlib:drawable-height pixmap) 0)))

(defmethod frame-button-sizes ((style frame-style))
  (with-slots (frame-item-pixmaps) style
    (let ((bpixmaps (or (gethash :close frame-item-pixmaps)
			(gethash :maximize frame-item-pixmaps)
			(gethash :icon-b frame-item-pixmaps))))
      (and bpixmaps (drawable-sizes (aref bpixmaps 0))))))

(defmethod free-frame-style ((style frame-style))
  (with-slots (pixmap-table frame-item-pixmaps) style
    (loop for pixmap being each hash-value in pixmap-table
	  do (xlib:free-pixmap pixmap))
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
  (with-slots (default-style transient-style) (gethash name *themes*)
    (and default-style (free-frame-style default-style))
    (and transient-style (free-frame-style transient-style)))
  (remhash name *themes*))

(defun load-theme (root-window name)
  (fmakunbound 'initialize-frame)
  (setf name (ensure-theme-directory-exists name))
  (load (concatenate 'string name "theme.o"))
  (or (gethash name *themes*) (initialize-frame name root-window)))

(defun make-keyword (string)
  (intern (with-standard-io-syntax (string-upcase string)) :keyword))

(defmacro define-theme ((theme-name) (&rest forms))
  (flet 
      ((define-style (style-to-define items &optional style)
	 `(with-slots (pixmap-table frame-item-pixmaps nb-buttons
		       top-margin bottom-margin left-margin right-margin
		       vmargin hmargin top-left-w top-left-h
		       top-right-w top-right-h bottom-right-w bottom-right-h
		       bottom-left-w bottom-left-h) ,style-to-define
	    ,@(loop for (key names) in items
		    for array = (make-array 3 :initial-element nil)
		    when (eq key :custom)
		    do (setf array (make-array (length names))) end
		    when (case key ((:close :icon-b :maximize) t))
		    collect `(incf nb-buttons) end
		    collect `(setf (gethash ,key frame-item-pixmaps) ,array)
		    nconc  
		     (loop for name in names
			   for i from 0
			   for k = (make-keyword name)
			   for pix = (gensym)
			   for form = 
			    `(let ((,pix 
				    (or ,(and style `(get-pixmap ,style ,k))
					(load-pnm->pixmap dir ,name window))))
			       (setf (gethash ,k pixmap-table) ,pix)
			       (setf (aref ,array ,i) ,pix))
			   collect form))
	    (multiple-value-setq (top-left-w top-left-h)
	      (frame-item-sizes ,style-to-define :top-left))
	    (multiple-value-setq (top-right-w top-right-h)
	      (frame-item-sizes ,style-to-define :top-right))
	    (multiple-value-setq (bottom-right-w bottom-right-h)
	      (frame-item-sizes ,style-to-define :bottom-right))
	    (multiple-value-setq (bottom-left-w bottom-left-h)
	      (frame-item-sizes ,style-to-define :bottom-left))
	    (setf top-margin (frame-item-height ,style-to-define :top)
	          bottom-margin (frame-item-height ,style-to-define :bottom)
	          left-margin (frame-item-width ,style-to-define :left)
		  right-margin (frame-item-width ,style-to-define :right)
		  hmargin (+ left-margin right-margin)
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
				         ',(intern (symbol-name style1)) 
				         :title-bar-position ,title-pos1
				         :parts-to-redraw-on-focus
				         ',parts-to-redraw-on-focus1)))
		 (,fs2 ,(and items2 `(make-instance
				         ',(intern (symbol-name style2)) 
				         :title-bar-position ,title-pos2
				         :parts-to-redraw-on-focus
				         ',parts-to-redraw-on-focus2))))
	     ,(unless items2 `(declare (ignorable ,fs2)))
	     ,(when items1 (define-style fs1 items1))
	     ,(when items2 (define-style fs2 items2 fs1))
	     (setf (gethash ,theme-name *themes*)
		   (make-instance 'theme 
				  :name ,theme-name 
				  ,@(and style1 `(,style1 ,fs1))
				  ,@(and style2 `(,style2 ,fs2))))))))))
