;;; -*- Mode: Lisp; Package: PPM -*-
;;; $Id: image-reader.lisp,v 1.13 2005/02/03 22:47:05 ihatchondo Exp $
;;;
;;; This a ppm image reader for CLX
;;; This file is part of Eclipse
;;; Copyright (C) 2000, 2001 Iban HATCHONDO
;;; Copyright (C) 2000 Frederic BRUNEL
;;; contact : hatchond@yahoo.fr
;;;           brunel@mail.dotcom.fr
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

(common-lisp:in-package :common-lisp-user)

(defpackage ppm
  (:use common-lisp)
  (:size 50)
  (:export
   #:initialize
   #:image #:gray-scale-image #:colored-24-image #:p5 #:p6
   #:image-width #:image-height #:image-pixels #:image-max-level #:image-pixel
   #:make-image-from-stream #:make-p5-image #:make-p6-image
   #:with-pnm-header
   #:load-ppm #:load-ppm-into-clx-image #:image->clx-image
   ))

(in-package :PPM)

(declaim (optimize (speed 3) (safety 1) (debug 0) (compilation-speed 0)))

;;;; some internal types.

(deftype card-32 () `(unsigned-byte 32))
(deftype card-29 () `(unsigned-byte 29))
(deftype card-24 () `(unsigned-byte 24))
(deftype card-16 () `(unsigned-byte 16))
(deftype card-8 () `(unsigned-byte 8))
(deftype card-6 () `(unsigned-byte 6))
(deftype card-4 () `(unsigned-byte 4))

(deftype pixarray-1 () '(simple-array bit (* *)))
(deftype pixarray-4 () '(simple-array card-4 (* *)))
(deftype pixarray-8 () '(simple-array card-8 (* *)))
(deftype pixarray-16 () '(simple-array card-16 (* *)))
(deftype pixarray-24 () '(simple-array card-24 (* *)))
(deftype pixarray-32 () '(simple-array card-32 (* *)))
(deftype pixarray ()
  '(or pixarray-1 pixarray-4 pixarray-8 pixarray-16 pixarray-24 pixarray-32))

(deftype clx-array () 'pixarray)

(deftype color-table () '(simple-array fixnum (256)))

;;;; x color utilities.

(defvar *gray-table* (make-array 256 :element-type 'fixnum))
(defvar *red-table* (make-array 256 :element-type 'fixnum))
(defvar *green-table* (make-array 256 :element-type 'fixnum))
(defvar *blue-table* (make-array 256 :element-type 'fixnum))

(declaim (type color-table *gray-table* *red-table* *green-table* *blue-table*))

(defun initialize-color-tables (colormap r-table g-table b-table)
  (declare (type color-table r-table g-table b-table))
  (loop for i of-type card-16 from 0 to 255
	for r = (xlib:make-color :red (/ i 255) :green 0 :blue 0)
	for g = (xlib:make-color :red 0 :green (/ i 255) :blue 0)
	for b = (xlib:make-color :red 0 :green 0 :blue (/ i 255))
	do (setf (aref r-table i) (xlib:alloc-color colormap r)
		 (aref g-table i) (xlib:alloc-color colormap g)
		 (aref b-table i) (xlib:alloc-color colormap b))))

(defun initialize-gray-table (colormap gray-table)
  (declare (type color-table gray-table))
  (loop	with m of-type card-8 = 255
	for i of-type card-16 from 0 to m
	for rgb = (xlib:make-color :red (/ i m) :green (/ i m) :blue (/ i m))
	do (setf (aref gray-table i) (xlib:alloc-color colormap rgb))))

;; Public color utilities.

(defun initialize (colormap)
  (initialize-gray-table colormap *gray-table*)
  (initialize-color-tables colormap *red-table* *green-table* *blue-table*))

(defun get-gray (index)
  (declare (type card-8 index))
  (aref *gray-table* index))

(defun get-color (r-index g-index b-index)
  (declare (type card-8 r-index g-index b-index))
  (logior (the fixnum (aref *red-table* r-index))
	  (the fixnum (aref *green-table* g-index))
	  (the fixnum (aref *blue-table* b-index))))

;;;; Images
;; Protocol class

(defclass image ()
  ((max-level :initarg :max-level :type card-8 :reader image-max-level)
   (pixels :initarg :pixels :type pixarray :reader image-pixels)))

(defgeneric image-width (image))
(defgeneric image-height (image))
(defgeneric image-pixel (image x y))
(defgeneric image-pixels (image))
(defgeneric (setf image-pixel) (x y pixel image))
(defgeneric make-image-from-stream (type stream width height mlevel))

(defmethod image-width ((image image))
  (cadr (array-dimensions (image-pixels image))))

(defmethod image-height ((image image))
  (car (array-dimensions (image-pixels image))))

;; Gray scale image

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass gray-scale-image (image)
  ((pixels :type pixarray-8))))

(defmethod image-pixel ((image gray-scale-image) x y)
  (aref (the pixarray-8 (image-pixels image)) y x))

(defmethod (setf image-pixel) (x y pixel (image gray-scale-image))
  (setf (aref (the pixarray-8 (image-pixels image)) y x) pixel))

(defun gray->x-gray (pixel)
  (declare (type card-8 pixel))
  (get-gray pixel))
  
;; Colored image

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass colored-24-image (image)
  ((pixels :type pixarray-24))))

(defmethod image-pixel ((image colored-24-image) x y)
  (aref (the pixarray-24 (image-pixels image)) y x))

(defmethod (setf image-pixel) (x y pixel (image colored-24-image))
  (setf (aref (the pixarray-24 (image-pixels image)) y x) pixel))

(defmacro red-component (pixel)
  `(the (unsigned-byte 8) (logand (ash ,pixel -16) 255)))

(defmacro green-component (pixel)
  `(the (unsigned-byte 8) (logand (ash ,pixel -8) 255)))

(defmacro blue-component (pixel)
  `(the (unsigned-byte 8) (logand ,pixel 255)))

(defun color->x-color (pix)
  (declare (type card-24 pix))
  (get-color (red-component pix) (green-component pix) (blue-component pix)))

;; PNM supported formats

(defclass p5 (gray-scale-image) ())

(defun make-p5-image (pixels &optional (max-level 255))
  (make-instance 'p5 :pixels pixels :max-level max-level))

(defmethod make-image-from-stream ((type (eql :P5)) stream width height mlevel)
  (declare (type card-16 width height))
  (declare (type card-8 mlevel))
  (loop with size of-type card-32 = (* width height)
        with pixels = (make-array (list height width) :element-type 'card-8)
	with vec = (make-array size :element-type 'card-8 :displaced-to pixels)
	with offset of-type card-32 = 0
	while (< offset size)
	do (setf offset (read-sequence vec stream :start offset))
	finally (return (make-p5-image pixels mlevel))))

(defclass p6 (colored-24-image) ())

(defun make-p6-image (pixels &optional (max-level 255))
  (make-instance 'p6 :pixels pixels :max-level max-level))

(defmethod make-image-from-stream ((type (eql :P6)) stream width height mlevel)
  (declare (type card-16 width height))
  (declare (type card-8 mlevel))
  (loop with size of-type card-32 = (* width height)
	with cache-size of-type card-32 = (the card-32 (min size 21000))
	with aux = (make-array (* 3 cache-size) :element-type 'card-8)
	for start of-type card-32 from 0 by cache-size below size
	for end of-type card-32 = (min (+ start cache-size) size)
	with data = (make-array (list height width) :element-type 'card-24)
	with vec = (make-array size :element-type 'card-24 :displaced-to data)
	do (loop with offset of-type card-32 = 0
		 while (< offset (* 3 (the card-32 (- end start))))
		 do (setf offset (read-sequence aux stream :start offset)))
	   (loop for i of-type card-32 from start below end
		 for j of-type card-32 from 0 by 3
		 do (setf (aref vec i)
			  (the card-24
			    (+ (ash (the card-8 (aref aux j)) 16)
			       (ash (the card-8 (aref aux (1+ j))) 8)
			       (the card-8 (aref aux (+ 2 j)))))))
	finally (return (make-p6-image data mlevel))))

;;;; Macros.

(defvar *ppm-readtable* (copy-readtable))

(defmacro with-pnm-header
    ((stream pnm-type &key width height max-level) &body body)
  "The macro with-pnm-header establishes a lexical environment for referring to
  the pnm image attirbutes: pnm-type, width, height, max-level."
  (let ((var1 (gensym)) (var2 (gensym)) (var3 (gensym)))
    `(progn
       (set-syntax-from-char #\# #\; *ppm-readtable*)
       (flet ((parse (stream)
		(let ((*readtable* *ppm-readtable*))
		  (read stream))))
	 (let ((,pnm-type (intern (format nil "~a" (parse ,stream)) :keyword))
	       (,(or width var1) (parse ,stream))
	       (,(or height var2) (parse ,stream))
	       (,(or max-level var3) (parse ,stream)))
	   ,@(unless width `((declare (ignore ,var1))))
	   ,@(unless height `((declare (ignore ,var2))))
	   ,@(unless max-level `((declare (ignore ,var3))))
	   ,@body)))))

;;;; Load functions.

(defun load-ppm (filename)
  "Returns an image instance that contains a representation of a pnm image."  
  (with-open-file (stream filename
		   #+SB-UNICODE :external-format
		   #+SB-UNICODE :latin-1)
    (with-pnm-header (stream type :width width :height height :max-level max)
      (declare (type card-16 width height))
      (declare (type card-8 max))
      (with-open-file (byte-stream filename :element-type 'card-8 
		       #+SB-UNICODE :external-format
		       #+SB-UNICODE :latin-1)
	(unless (file-position byte-stream (file-position stream))
	  (error "could not reposition image data stream"))
	(make-image-from-stream type byte-stream width height max)))))

(defun image->clx-image (image drawable)
  "Returns a clx image representation of an image."
  (loop with getter = (typecase image
			(gray-scale-image #'gray->x-gray)
			(colored-24-image #'color->x-color)
			(t (error "unknow image type ~a" (type-of image))))
	with depth of-type card-8 = (xlib:drawable-depth drawable)
	with bits-per-pixel = (find-bits-per-pixel depth)
	with w = (image-width image) 
	with h = (image-height image)
	with type = `(unsigned-byte ,bits-per-pixel)
	with res of-type clx-array = (make-array (list h w) :element-type type)
	for y of-type card-16 from 0 below h
	do (loop for x of-type card-16 from 0 below w 
		 for pixel = (image-pixel image x y)
		 do (setf (aref res y x)
			  (funcall (the function getter) pixel)))
	finally (return (xlib:create-image
			    :width w :height h :depth depth :data res
			    :bits-per-pixel bits-per-pixel))))

(defun load-ppm-into-clx-image (filename drawable)
  "Returns a clx image representation of a pnm image readed in a pnm file."
  (let ((image (load-ppm filename)))
    (prog1 (image->clx-image image drawable)
      (setf (slot-value image 'pixels) nil))))

;;;; private routines.

(defun find-bits-per-pixel (depth)
  (declare (type card-8 depth))
  (cond ((>= depth 24) 32)
	((> depth 16) 24)
	((> depth 8) 16)
	((> depth 4) 8)
	((> depth 1) 4)
	(t depth 1)))
