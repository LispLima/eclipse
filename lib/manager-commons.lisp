;;; -*- Mode: Lisp; Package: MANAGER-COMMONS -*-
;;; $Id: manager-commons.lisp,v 1.6 2005/01/07 15:13:37 ihatchondo Exp $
;;;
;;; This is the CLX support for the managing with gnome.
;;;
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
;;;
;;; This package implements :
;;; some functions that the gnome-manager and exwm-manager use.

(common-lisp:in-package :common-lisp-user)

(defpackage manager-commons
  (:use common-lisp)
  (:import-from :xlib #:get-property #:change-property)
  (:size 50)
  (:export
   #:geometry-hint             #:make-geometry-hint
   #:geometry-hint-x           #:geometry-hint-y
   #:geometry-hint-width       #:geometry-hint-height

   #:encode-mask               #:decode-mask
   #:encode-strings            #:decode-strings
   
   #:utf8->strings
   #:string->utf8
   
   #:get-geometry-hint #:set-geometry-hint
   #:get-atoms-property #:set-atoms-property
   #:get-window-property #:define-window-list-property-accessor   
   #:get-text-property #:set-simple-text-property #:set-multiple-text-property
   )
  (:documentation ""))

(in-package :MANAGER-COMMONS)

(declaim (optimize (speed 3)
		   (safety 1)
		   (debug 1)
		   (compilation-speed 0)))

(declaim (inline get-atoms-property 
		 get-window-property))

(deftype card-32 () '(unsigned-byte 32))
(deftype card-16 () '(unsigned-byte 16))
(deftype card-8 () '(unsigned-byte 8))
(deftype int-16 () '(signed-byte 16))

(defstruct geometry-hint
  (x 0 :type int-16)
  (y 0 :type int-16)
  (width 0 :type card-16)
  (height 0 :type card-16))

(defmacro aref8 (array index)
  `(the (unsigned-byte 8) (aref ,array ,index)))

(defun get-geometry-hint (window property-atom)
  (let ((prop (get-property window property-atom)))
    (make-geometry-hint
        :x (first prop)
	:y (second prop)
	:width (third prop)
	:height (fourth prop))))

(defun set-geometry-hint (window hint property-atom)
  (change-property window property-atom
    (list (geometry-hint-x hint)
	  (geometry-hint-y hint)
	  (geometry-hint-width hint)
	  (geometry-hint-height hint))
    :CARDINAL
    32))

(defun get-atoms-property (window property-atom atom-list-p)
  "Returns a list of atom-name (if atom-list-p is t) otherwise returns
   a list of atom-id."
  (get-property window property-atom
    :transform (when atom-list-p
		 (lambda (id)
		   (xlib:atom-name (xlib:drawable-display window) id)))))

(defun get-window-property (window property-atom window-list-p)
  "Returns a list of window (if window-list-p is t) otherwise returns
   a list of window-id."
  (get-property window property-atom
    :transform (when window-list-p
		 (lambda (id)
		   (xlib::lookup-window (xlib:drawable-display window) id)))))

(defun utf8->strings (data)
  "Converts a vector of (unsigned-byte 8) that represents utf8 string(s) into
   into a list of strings."
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
  "Returns a null terminated list, or not, containing the utf8 encoding
  of the given string."
  (declare (type string string))
  (loop with aux = (map 'vector #'xlib:char->card8 string)
	for car of-type (unsigned-byte 8) across (the simple-vector aux)
	if (< car #x80) collect car into v
	else collect (logior #xC0 (ash car -6)) into v
	     and collect (logior #x80 (logand #xBF car)) into v end
	finally (return (if null-terminated (concatenate 'list v '(0)) v))))

(defun encode-mask (key-vector key-list key-type)
  "Converts a keyword list mask into its integer value mask.
   - KEY-VECTOR is a vector containg bit-position keywords. The position of
   the keyword in the vector indicates its bit position in the resulting mask
   - KEY-LIST is either a mask or a list of KEY-TYPE.
   Returns NIL when KEY-LIST is not a list or mask."
  (declare (type (simple-array keyword (*)) key-vector))
  (declare (type (or card-16 list) key-list))
  (typecase key-list
    (card-16 key-list)
    (list (loop with mask of-type card-16 = 0
		for key in key-list
		for bit = (position key key-vector :test #'eq)
		if bit do (setf mask (logior mask (the card-16 (ash 1 bit))))
		else do (xlib::x-type-error key key-type)
		finally (return mask)))))

(defun decode-mask (key-vector mask)
  "Converts an integer value mask into its keyword list mask.
  KEY-VECTOR is a vector containg bit-position keywords."
  (declare (type (simple-array keyword (*)) key-vector))
  (declare (type (or card-16 null) mask))
  (when mask
    (loop for bit of-type card-16 from 0 below (length key-vector)
	  when (logbitp bit mask) collect (aref key-vector bit))))

(defun encode-strings (&rest strings)
  "Converts a list of string into a vector of ISO Latin-1 characters, otherwise
   said (unsigned-byte 8). Each string are ended with the null character."
  (loop for string in strings
	for car = (map '(vector card-8) #'xlib:char->card8 (string string))
	collect (concatenate '(vector card-8) car #(0)) into vector
	finally (return (apply #'concatenate '(vector card-8) vector))))

(defun decode-strings (chars)
  "Converts a vector of ISO Latin-1 characters, - e.g: (unsigned-byte 8) -,
   into a list of strings. If the vector contains more than one string then
   string are null terminated."
  (declare (type simple-vector chars))
  (loop with name = nil
	with length of-type card-16 = (1- (array-dimension chars 0))
	for char of-type card-8 across (the simple-vector chars)
	for i of-type card-16 from 0
	unless (= 0 char) do (push char name) end
	when (and name (or (= 0 char) (= i length)))
	collect (prog1 (map 'string #'xlib:card8->char (reverse name))
		  (setf name nil))))

(defun get-text-property (window property-atom)
  "Returns the value of the property associated with `property-atom' as a 
  list of string."  
  (multiple-value-bind (data type format)
      (get-property window property-atom :result-type 'vector)
    (declare (type (member 8 16 32) format))
    (when (and (= format 8) data) ;; is that true ??
      (case type
	(:string (decode-strings data))
	(:utf8_string (utf8->strings data))))))

(defun set-simple-text-property (window string type property-atom)
  "Sets a string text property designates by `property-atom'."
  (change-property window property-atom
    (case type
      (:string (encode-strings string))
      (:utf8_string (string->utf8 string :null-terminated nil)))
    type 
    8))

(defun set-multiple-text-property (window strings type mode property-atom)
  "Sets a multiple string text property designates by `property-atom'.
   - STRINGS a simpla string or a list of string.
   - TYPE is one of :string :utf8_string
   - MODE is one of :replace :remove :append.
     :replace : replace the value of the property by the given strings.
     :remove  : removes the given strings from the property.
     :append  : append the given strings to the property."
  (let ((text-prop (get-text-property window property-atom)))
    (unless (eq mode :replace)
      (when (eq mode :remove)
	(rotatef strings text-prop)
	(setf mode :replace))
      (setf strings (nset-difference strings text-prop :test #'string=))))
  (when strings
    (change-property window property-atom
      (case type
	(:string (apply #'encode-strings strings))
	(:utf8_string
	 (apply #'concatenate 'list (mapcar #'string->utf8 strings))))
      type 
      8
      :mode mode)))

(defun set-atoms-property (window atoms property-atom &key (mode :replace))
  "Sets the property designates by `property-atom'. ATOMS is a list of atom-id
   or a list of keyword atom-names."
  (change-property window property-atom atoms :ATOM 32 
    :mode mode
    :transform (unless (integerp (car atoms))
		 (lambda (atom-key) 
		   (xlib:find-atom (xlib:drawable-display window) atom-key)))))

(defmacro define-window-list-property-accessor
    ((name) &key property-atom (data-type :window)
                 reader-documentation writer-documentation)
  "Generates window list based properties accessors: 
   - `name' [ function ] window &key window-list
     returns the value of the property named `property-atom' as a list of
     window if window-list is T, otherwise as a list of window-id.
   - (setf `name') (window &key window-id) (window-designator)
     to sets the property value.
   
   :reader-documentation (string): the reader function documentation string.
   :writer-documentation (string): the setf function documentation string."
  (let ((reader (intern (with-standard-io-syntax (format nil "~A" name)))))
    `(progn

       (defun ,reader (window &key window-list)
	 ,@(when reader-documentation `(,reader-documentation))
	 (get-window-property window ,property-atom window-list))

       (defsetf ,reader (window &key (mode :replace) window-id) (win)
	 ,@(when writer-documentation `(,writer-documentation))
	 `(when ,win
	    (change-property ,window ,',property-atom
	      (cond ((eq ,mode :remove)
		     (remove ,win
		         (,',reader ,window :window-list (not ,window-id))))
		    ((listp ,win) ,win)
		    (t (list ,win)))
	      ,',data-type
	      32
	      :mode (if (eq ,mode :remove) :replace ,mode)
	      :transform (unless ,window-id #'xlib:window-id)))))))
