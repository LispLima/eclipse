;;; -*- Mode: Lisp; Package: VIRTUAL-SCREEN -*-
;;; $Id: virtual-screen.lisp,v 1.3 2002/11/07 14:54:27 hatchond Exp $
;;;
;;; Copyright (C) 2002 Iban HATCHONDO
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

;;;; Virtual screen

(common-lisp:in-package :common-lisp-user)

(defpackage virtual-screen
  (:nicknames vs)
  (:use common-lisp)
  (:size 50)
  (:export virtual-screens
	   vscreens
	   current-screen
	   number-of-virtual-screen
	   nth-vscreen
	   set-vscreen-content
	   add-to-vscreen
	   remove-from-vscreen
	   add-to-all
	   remove-from-all
	   adjust-vscreens
	   restack-window
	   create-virtual-screens))

(in-package :virtual-screen)

(defclass virtual-screens ()
 ((virtual-sceen-array :accessor vscreens)
  (current-virtual-screen :initform 0 :accessor current-screen)
  (number-of-virtual-screen :initarg :number-of-virtual-screen
			    :initform 0 
			    :accessor number-of-virtual-screen)))

(defmethod initialize-instance :after ((vscreens virtual-screens) &rest rest)
  (declare (ignorable rest))
  (with-slots (number-of-virtual-screen virtual-sceen-array) vscreens
    (setf virtual-sceen-array 
	  (make-array number-of-virtual-screen
	      :element-type 'vector
	      :adjustable t
	      :initial-contents 
	      (loop for i from 0 below number-of-virtual-screen
		    collect (make-empty-screen))))))

(defun make-empty-screen ()
  (make-array 0 :fill-pointer t :initial-element nil))
    

(defmacro with-index-in-screens ((index vscreens) &body body)
  `(when (< -1 ,index (number-of-virtual-screen ,vscreens))
     ,@body))

(defmethod nth-vscreen ((vscreens virtual-screens) &optional n)
  (aref (vscreens vscreens) (or n (current-screen vscreens))))

(defmethod set-vscreen-content ((vscreens virtual-screens) elements
			       &key (n (current-screen vscreens)))
  (with-index-in-screens (n vscreens)
    (setf (aref (vscreens vscreens) n) elements)))

(defmethod add-to-vscreen ((vscreens virtual-screens) element
			  &key (n (current-screen vscreens))
			       (test #'xlib:window-equal))
  (with-index-in-screens (n vscreens)
    (unless (position element (aref (vscreens vscreens) n) :test test)
      (vector-push-extend element (aref (vscreens vscreens) n) 20))))

(defmethod remove-from-vscreen ((vscreens virtual-screens) element
				&key (n (current-screen vscreens))
				     (test #'xlib:window-equal))
  (with-index-in-screens (n vscreens)
    (delete element (aref (vscreens vscreens) n) :test test)))

(defmethod add-to-all ((vscreens virtual-screens) element 
		       &key (test #'xlib:window-equal))
  (loop for i from 0 below (number-of-virtual-screen vscreens) 
	unless (position element (aref (vscreens vscreens) i) :test test)
	do (vector-push-extend element (aref (vscreens vscreens) i) 20)))

(defmethod remove-from-all ((vscreens virtual-screens) element 
			    &key except (test #'xlib:window-equal))
  (loop for i from 0 below (number-of-virtual-screen vscreens)
	unless (and except (= i except)) do
	 (delete element (aref (vscreens vscreens) i) :test test)))
  
(defmethod adjust-vscreens ((vscreens virtual-screens) n 
			    &key map-when-reduce
			         (test #'xlib:window-equal))
  (let ((dimension (number-of-virtual-screen vscreens))
	(new-size (1- n))
	(vscreen (vscreens vscreens)))
    (when (> dimension n)
      (loop for i from (1+ new-size) below dimension
	    do (loop for w across (aref vscreen i)
		     do (vector-push-extend w (aref vscreen new-size) 20)))
      (delete-duplicates (aref vscreen new-size) :test test)
      (and map-when-reduce (map nil map-when-reduce (aref vscreen new-size))))
    (setf (number-of-virtual-screen vscreens) n)
    (adjust-array vscreen n :initial-element nil)
    (when (> n dimension)
      (loop for i from dimension below n
	    do (setf (aref vscreen i) (make-empty-screen))))))

(defun restack-window (window screen &key (position 0))
  (when (and (> (length screen) 1) (< position (length screen)) window)
    (let ((init-pos (position window screen :test #'xlib:window-equal)))
      (when (and init-pos (not (= init-pos position)))
	(rotatef (aref screen position) (aref screen init-pos))))))

(defun create-virtual-screens (n)
  (make-instance 'virtual-screens :number-of-virtual-screen n))

