;;; -*- Mode: Lisp; Package: PPM -*-
;;; $Id: image-reader.lisp,v 1.4 2003/02/10 14:10:58 hatchond Exp $
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
   picture gray-scale colored-24
   p5 p6
   internal-picture-width picture-width
   picture-height
   picture-data
   picture-max
   make-clx-data
   make-pnm
   load-ppm
   load-ppm-into-clx-image
   initialize
   ))

(in-package :PPM)

(declaim (optimize (speed 3) (safety 1)
		   (debug 0) (compilation-speed 0)))

(deftype card-32 () `(unsigned-byte 32))
(deftype card-29 () `(unsigned-byte 29))
(deftype card-24 () `(unsigned-byte 24))
(deftype card-16 () `(unsigned-byte 16))
(deftype card-8 () `(unsigned-byte 8))
(deftype card-6 () `(unsigned-byte 6))
(deftype card-4 () `(unsigned-byte 4))

(deftype picture-size () `(and (unsigned-byte 16) (satisfies plusp)))
(deftype color-table () '(simple-array fixnum (256)))

(deftype pixarray-1 () '(simple-array bit (* *)))
(deftype pixarray-4 () '(simple-array card-4 (* *)))
(deftype pixarray-8 () '(simple-array card-8 (* *)))
(deftype pixarray-16 () '(simple-array card-16 (* *)))
(deftype pixarray-24 () '(simple-array card-24 (* *)))
(deftype pixarray-32 () '(simple-array card-32 (* *)))
(deftype pixarray ()
  '(or pixarray-1 pixarray-4 pixarray-8 pixarray-16 pixarray-24 pixarray-32))


;;; Protocol class
(defclass picture ()
  ((width :initarg :width :type picture-size :accessor picture-width)
   (height :initarg :height :type picture-size :accessor picture-height)
   (max :initarg :max :type card-8 :accessor picture-max)
   (data :initarg :data :accessor picture-data)))

(defmethod initialize-instance :after ((image picture) &rest rest)
  (declare (ignore rest))
  (with-slots (width height data) image
    (declare (type picture-size width height))
    (when (typep image 'colored-24)
      (setf width (* (the picture-size width) 3)))
    (setf data (make-array (list height width) :element-type 'card-8))))

(defgeneric picture-width (picture))
(defgeneric internal-picture-width (picture))
(defgeneric make-clx-data (picture depth))

(defmethod picture-width ((picture picture))
  "Give the real width of the image"
  (slot-value picture 'width))

(defmethod internal-picture-width ((picture picture))
  "Give the width of the internal image representation"
  (slot-value picture 'width))

;;; Gray scale image
(defclass gray-scale (picture) ())

(defmethod make-clx-data ((image gray-scale) depth)
  (make-array (list (picture-height image) (picture-width image))
	      :element-type `(unsigned-byte ,depth)))

(defmethod create-ppm-internal ((image gray-scale) clx-data)
  (declare (type pixarray clx-data))
  (with-slots (width height data) image
    (loop for i of-type card-16 from 0 below height do
	  (loop for j of-type card-16 from 0 below width
		for gray = (get-gray (aref (the pixarray data) i j))
		do (setf (aref clx-data i j) gray)))))
	
;;; Colored image
(defclass colored-24 (picture) ())

(defmethod picture-width ((picture colored-24))
  "Give the real width of the image"
  (/ (the picture-size (slot-value picture 'width)) 3))

(defmethod make-clx-data ((image colored-24) depth)
  (make-array (list (picture-height image) (picture-width image))
	      :element-type `(unsigned-byte ,depth)))

(defmethod create-ppm-internal ((image colored-24) clx-data)
  (declare (type pixarray clx-data))
  (with-slots (width height data) image
    (loop for i of-type card-16 from 0 below height do
	  (loop for j of-type card-16 from 0 below width
		for k of-type card-16 from 0
		for color = (get-color
			        (aref (the pixarray data) i j)
				(aref (the pixarray data) i (incf j))
				(aref (the pixarray data) i (incf j)))
		do (setf (aref clx-data i k) color)))))

;; PNM supported formats
(defclass p5 (gray-scale) ())
(defclass p6 (colored-24) ())

(defun make-pnm (format width height max)
  (make-instance (intern (format nil "~A" format) :ppm)
		 :width width :height height :max max))

;;; Main work

(defun find-bits-per-pixel (depth)
  (declare (type card-8 depth))
  (cond ((>= depth 24) 32)
	((> depth 16) 24)
	((> depth 8) 16)
	((> depth 4) 8)
	((> depth 1) 4)
	(t depth 1)))

(defvar *ppm-readtable* (copy-readtable))

(defun create-ppm-image-from-stream (stream)
  (set-syntax-from-char #\# #\; *ppm-readtable*)
  (flet ((parse (stream)
	   (let ((*readtable* *ppm-readtable*))
	     (read stream))))
    (make-pnm (parse stream) (parse stream) (parse stream) (parse stream))))

(defun load-ppm (filename)
  (with-open-file (stream filename)
    (let* ((image (create-ppm-image-from-stream stream))
	   (size (* (the picture-size (picture-height image))
		    (the picture-size (internal-picture-width image))))
	   (tmp (make-array size
			    :element-type 'card-8
			    :displaced-to (picture-data image))))
      (declare (type card-32 size))
      (with-open-file (byte-stream filename :element-type '(unsigned-byte 8))
	(unless (file-position byte-stream (file-position stream))
	  (error "could not reposition image data stream"))
	(loop with offset of-type card-32 = 0
	    while (< offset size)
	    do (setf offset (read-sequence tmp byte-stream :start offset)))
      image))))

(defun load-ppm-into-clx-image (filename drawable)
  (let* ((depth (xlib:drawable-depth drawable))
	 (image (load-ppm filename))
	 (clx-data (make-clx-data image depth)))
    (create-ppm-internal image clx-data)
    (xlib:create-image :width (picture-width image)
		       :height (picture-height image)
		       :depth depth
		       :bits-per-pixel (find-bits-per-pixel depth)
		       :data clx-data)))

(defvar *red-table* (make-array 256 :element-type 'fixnum))
(defvar *green-table* (make-array 256 :element-type 'fixnum))
(defvar *blue-table* (make-array 256 :element-type 'fixnum))
(defvar *gray-table* (make-array 256 :element-type 'fixnum))

(declaim 
 (type color-table *blue-table* *red-table* *green-table* *gray-table*))

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
  (loop	for i of-type card-16 from 0 to 255
	for rgb = (xlib:make-color :red (/ i 255) :green (/ i 255) :blue (/ i 255))
	do (setf (aref gray-table i) (xlib:alloc-color colormap rgb))))
			
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
