;;; -*- Mode: Lisp; Package: User -*-
;;; $Id$
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
	      :element-type 'list
	      :initial-element '()
	      :adjustable t))))

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
			  &key (n (current-screen vscreens)))
  (with-index-in-screens (n vscreens)
    (pushnew element (aref (vscreens vscreens) n))))

(defmethod remove-from-vscreen ((vscreens virtual-screens) element
			  &key (n (current-screen vscreens)))
  (with-index-in-screens (n vscreens)
    (setf (aref (vscreens vscreens) n)
	  (remove element (aref (vscreens vscreens) n)))))

(defmethod add-to-all ((vscreens virtual-screens) element)
  (loop for i from 0 below (number-of-virtual-screen vscreens) 
	do (pushnew element (aref (vscreens vscreens) i))))

(defmethod remove-from-all ((vscreens virtual-screens) element &key except)
  (loop for i from 0 below (number-of-virtual-screen vscreens)
	unless (and except (= i except)) do
	  (setf (aref (vscreens vscreens) i)
		(remove element (aref (vscreens vscreens) i)))))

(defmethod adjust-vscreens ((vscreens virtual-screens) n &key map-when-reduce)
  (let ((dimension (number-of-virtual-screen vscreens))
	(new-size (1- n))
	(vscreen (vscreens vscreens)))
    (when (> dimension n)
      (loop for i from new-size below dimension nconc (aref vscreen i) into tmp
	    finally (setf (aref vscreen new-size) (copy-list tmp)))
      (when map-when-reduce (mapc map-when-reduce (aref vscreen new-size))))
    (setf (number-of-virtual-screen vscreens) n)
    (adjust-array vscreen n :initial-element '())))

(defun create-virtual-screens (n)
  (make-instance 'virtual-screens :number-of-virtual-screen n))

