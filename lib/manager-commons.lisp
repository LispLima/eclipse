;;; -*- Mode: Lisp; Package: MANAGER-COMMONS -*-
;;; $Id: $
;;;
;;; This is the CLX support for the managing with gnome.
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
;;;
;;; This package implements :
;;; some functions that the gnome-manager and exwm-manager use.

(common-lisp:in-package :common-lisp-user)

(defpackage manager-commons
  (:use common-lisp)
  (:import-from :xlib get-property change-property)
  (:size 50)
  (:export
   geometry-hint             make-geometry-hint
   geometry-hint-x           geometry-hint-y
   geometry-hint-width       geometry-hint-height

   encode-names              decode-names
   encode-mask               decode-mask

   utf8->strings
   string->utf8
   )
  (:documentation ""))

(in-package :MANAGER-COMMONS)

(declaim (optimize (speed 3)
		   (safety 1)
		   (debug 1)
		   (compilation-speed 0)))

(deftype card-32 () '(unsigned-byte 32))
(deftype card-16 () '(unsigned-byte 16))
(deftype card-8 () '(unsigned-byte 8))
(deftype int-16 () '(signed-byte 16))


(defmacro aref8 (array index)
  `(the (unsigned-byte 8) (aref ,array ,index)))

(defun utf8->strings (data)
  " Transform a vector of (unsigned-byte 8) - suppose to be the
  representation of null terminated strings encoded in utf8 format -
  into a list of simple strings"
  (declare (type simple-vector data))
  (loop with aux = nil
	with length of-type card-16 = (1- (array-dimension data 0))
	for i of-type (unsigned-byte 24) from 0 to length
	for c of-type card-8 = 0 do
	(unless (zerop (aref8 data i))
	  (if (logbitp 7 (aref8 data i))
	      (if (= #x40 (logand (aref8 data i) #x7c))
		  (when (logbitp 7 (aref8 data (1+ i)))
		    (setf c (logior (ash (logand (aref8 data i) #x3) 6) 
				    (logand (aref8 data (incf i)) #x3F))))
		  (loop do (incf i)
			while (and (logbitp 7 (aref8 data i))
				   (logbitp 6 (aref8 data i)))
			finally (decf i) (setf c 35))) ; #\#
	      (setf c (aref8 data i))))
	unless (= c 0) do (push c aux) end
	if (and aux (or (= c 0) (= i length)))
	collect (map 'string #'xlib:card8->char (reverse aux))
	and do (setf aux nil)))

(defun string->utf8 (string &key (null-terminated t))
  " Return a null terminated list, or not, containing the utf8 encoding
 of the given string."
  (declare (type string string))
  (loop with aux = (map 'vector #'xlib:char->card8 string)
	for car of-type (unsigned-byte 8) across (the simple-vector aux)
	if (< car #x80) collect car into v
	else collect (logior #xC0 (ash car -6)) into v
	     and collect (logior #x80 (logand #xBF car)) into v end
	finally (return (if null-terminated (concatenate 'list v '(0)) v))))

(defstruct geometry-hint
  (x      0 :type int-16)
  (y      0 :type int-16)
  (width  0 :type card-16)
  (height 0 :type card-16))

(defun get-geometry-hint (window atom)
  (let ((prop (get-property window atom)))
    (make-geometry-hint
        :x (first prop)
	:y (second prop)
	:width (third prop)
	:height (fourth prop))))

(defun set-geometry-hint (window hint atom)
  (change-property window
		   atom
		   (list (geometry-hint-x hint)
			 (geometry-hint-y hint)
			 (geometry-hint-width hint)
			 (geometry-hint-height hint))
		   :CARDINAL 32))

(defun encode-mask (key-vector key-list key-type)
  (declare (type (simple-array keyword (*)) key-vector)
           (type (or card-16 list) key-list))
  (typecase key-list
    (card-16 key-list)
    (list (loop with mask of-type card-16 = 0
		for key in key-list
		for bit = (position key key-vector :test #'eq)
		if bit do (setf mask (logior mask (the card-16 (ash 1 bit))))
		else do (xlib::x-type-error key key-type)
		finally (return mask)))))

(defun decode-mask (key-vector mask)
  (declare (type (simple-array keyword (*)) key-vector)
           (type (or card-16 null) mask))
  (when mask
    (loop for bit of-type card-16 from 0 below (length key-vector)
	  when (logbitp bit mask) collect (aref key-vector bit))))

(defun encode-names (strings)
  (loop for string in strings
	for car = (map '(vector card-8) #'xlib:char->card8 (string string))
	collect (concatenate '(vector card-8) car #(0)) into vector
	finally (return (apply #'concatenate '(vector card-8) vector))))

(defun decode-names (chars)
  (declare (type simple-vector chars))
  (loop with name = nil
	with length of-type card-16 = (1- (array-dimension chars 0))
	for char of-type card-8 across (the simple-vector chars)
	for i of-type card-16 from 0
	unless (= 0 char) do (push char name) end
	when (and name (or (= 0 char) (= i length)))
	collect (prog1 (map 'string #'xlib:card8->char (reverse name))
		  (setf name nil))))

(defun encode-string-property (atom rest)
  (case atom
    (:string (encode-names rest))
    (:utf8_string (apply #'concatenate 'list (mapcar #'string->utf8 rest)))))

(defun get-text-property (window property-atom)
  (multiple-value-bind (data type format)
      (get-property window property-atom :result-type 'vector)
    (when (and (= format 8) data) ;; is that true ??
      (case type
	(:string (decode-names data))
	(:utf8_string (utf8->strings data))))))

(defun set-workspace-names (window names type mode atom)
  (let ((workspace-names (get-text-property window atom)))
    (unless (eq mode :replace)
      (when (eq mode :remove)
	(rotatef names workspace-names)
	(setf mode :replace))
      (setf names (nset-difference names workspace-names :test #'string=))))
  (when names
    (change-property window
		     atom
		     (encode-string-property type names)
		     type 8
		     :mode mode)))

(defun set-atoms-property (window atoms atom-property &key (mode :replace))
  (change-property
       window
       atom-property
       (loop for name in atoms
	     collect (xlib:find-atom (xlib:drawable-display window) name))
       :ATOM
       32 :mode mode))

(declaim (inline get-atoms-property))
(defun get-atoms-property (window atom-property atom-list-p)
  (get-property
      window
      atom-property
      :transform
      (when atom-list-p
	(lambda (id) (xlib:atom-name (xlib:drawable-display window) id)))))

(declaim (inline get-window-property))
(defun get-window-property (window atom window-list-p)
  (get-property
         window
	 atom
	 :transform
	 (when window-list-p
	   (lambda (id)
	     (xlib::lookup-window (xlib:drawable-display window) id)))))

(defmacro make-window-list-seter (type atom &optional (data-type :WINDOW))
  (let ((primary (with-standard-io-syntax (format nil "~A" type)))
	(setter (with-standard-io-syntax (format nil "SET-~A" type))))
    `(defun ,(intern setter) (window win mode)
       (when win
	 (change-property
	     window
	     ,atom
	     (cond ((eq mode :remove)
		    (setf mode :replace)
		    (remove win (,(intern primary) window :window-list t)))
		   ((listp win) win)
		   (t (list win)))
	     ,data-type
	     32
	     :mode mode :transform #'xlib:window-id)))))
