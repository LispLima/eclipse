;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: ICE-buffer.lisp,v 1.4 2004/12/14 17:58:20 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: ICE Library
;;;   Created: 2004 01 15 15:28
;;;    Author: Iban Hatchondo <hatchond@labri.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2004 by Iban Hatchondo

;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;

(in-package :ICE-LIB)

;;;; Buffer definition.

(deftype bbuf () '(simple-array card8 (*)))

(defstruct (buffer (:constructor make-ice-buffer))
  (bbuf (make-array 1 :element-type 'card8) :type bbuf)
  (index 0 :type fixnum)
  (byte-order :LSBFirst :type ice-byte-order))

(defun make-buffer (size &optional (byte-order :LSBFirst))
  (declare (type ice-byte-order byte-order))
  (declare (type fixnum size))
  (make-ice-buffer
      :bbuf (make-array size :element-type 'card8)
      :byte-order byte-order))

(defun buffer-size (buffer)
  (declare (type buffer buffer))
  (array-dimension (buffer-bbuf buffer) 0))

(defun buffer-read-sequence (buffer stream &key (start 0) end)
  (read-sequence (buffer-bbuf buffer) stream :start start :end end))

(defun buffer-write-sequence (buffer stream &key (start 0) end)
  (write-sequence (buffer-bbuf buffer) stream :start start :end end))

(defmacro index+ (buffer &optional (value 1))
  `(incf (buffer-index ,buffer) ,value))

(defmacro baref (buffer index)
  `(aref (buffer-bbuf ,buffer) ,index))

;;;;

(defun pad-length (message-byte-length &optional (modulo 8))
  "Returns the number of bytes that should be add to a message of length
  `message-byte-length' (in byte) modulo `modulo' (default is 8)."
  (mod (- modulo (mod message-byte-length modulo)) modulo))

(defun vpos (elem vector)
  "Returns the position of an element in a vector. If element is not 
   found then raise an error."
  (declare (type vector vector))
  (or (position elem vector) (error "~a is not a member of ~a~%" elem vector)))

(defmacro with-gensym (symbols &body body)
  `(let ,(loop for s in symbols collect `(,s (gensym)))
     ,@body))

(defmacro define-accessor 
    (type ((&rest rargs) &body rbody) ((&rest wargs) &body wbody))
  "Defines macro accessor for a type. The first body form defines the reader 
   macro the second body form defines the writer macro."
  (let ((reader (sintern (format nil "BUFFER-READ-~a" type)))
	(writer (sintern (format nil "BUFFER-WRITE-~a" type))))
    `(progn
       (defmacro ,reader ,rargs ,@rbody)
       (defmacro ,writer ,wargs ,@wbody))))

(defmacro define-member8-accessor (type vector)
  `(define-accessor ,type 
     ((buffer) `(aref ,',vector (buffer-read-card8 ,buffer)))
     ((value buffer) `(buffer-write-card8 (vpos ,value ,',vector) ,buffer))))

(defmacro define-member16-accessor (type vector)
  `(define-accessor ,type 
     ((buffer) `(aref ,',vector (buffer-read-card16 ,buffer)))
     ((value buffer) `(buffer-write-card16 (vpos ,value ,',vector) ,buffer))))

(defmacro define-array-accessor (type element-type)
  (let ((read (sintern (format nil "BUFFER-READ-~a" element-type)))
	(write (sintern (format nil "BUFFER-WRITE-~a" element-type))))
    `(define-accessor ,type 
       ((buffer length)
	`(loop with size = ,length
	       with v = (make-array size :element-type ',',element-type)
	       for i from 0 below size
	       do (setf (aref v i) (,',read ,buffer))
               finally (return v)))
       ((array buffer)
	`(loop for elem across (the ,',type ,array)
	       do (,',write elem ,buffer))))))

;;;; buffer-{read,write}-<type> macros.

(define-member8-accessor ice-byte-order '#(:LSBFirst :MSBFirst))

(define-member8-accessor error-severity
  '#(:can-continue :fatal-to-protocol :fatal-to-connection))

(define-accessor card8
  ((buffer)
   (with-gensym (buff)
     `(let ((,buff ,buffer))
	(baref ,buff (1- (index+ ,buff))))))
  ((value buffer)
   (with-gensym (buff val)
     `(multiple-value-bind (,buff ,val) (values ,buffer ,value)
        (setf (baref ,buff (1- (index+ ,buff))) ,val)))))

(define-accessor boolean
  ((buffer) `(= 1 (buffer-read-card8 ,buffer)))
  ((value buffer) `(buffer-write-card8 (if ,value 1 0) ,buffer)))

(define-accessor card16
  ((buffer)
   (with-gensym (w1 w2 buff)
     `(let* ((,buff ,buffer)
	     (,w1 (buffer-read-card8 ,buff))
	     (,w2 (buffer-read-card8 ,buff)))
        (declare (type buffer ,buff))
	(ecase (buffer-byte-order ,buff)
	  (:MSBFirst (logior (ash ,w1 8) ,w2))
	  (:LSBFirst (logior (ash ,w2 8) ,w1))))))
  ((value buffer)
   (with-gensym (w1 w2 buff val)
     `(multiple-value-bind (,buff ,val) (values ,buffer ,value)
        (declare (type buffer ,buff))
        (let ((,w1 (ldb (byte 8 8) ,val))
	      (,w2 (ldb (byte 8 0) ,val)))
	  (ecase (buffer-byte-order ,buff)
	    (:MSBFirst 
	     (buffer-write-card8 ,w1 ,buff)
	     (buffer-write-card8 ,w2 ,buff))
	    (:LSBFirst 
	     (buffer-write-card8 ,w2 ,buff)
	     (buffer-write-card8 ,w1 ,buff))))))))

(define-accessor card32
  ((buffer)
   (with-gensym (w1 w2 w3 w4 buff)
     `(let* ((,buff ,buffer)
	     (,w1 (buffer-read-card8 ,buff))
	     (,w2 (buffer-read-card8 ,buff))
	     (,w3 (buffer-read-card8 ,buff))
	     (,w4 (buffer-read-card8 ,buff)))
	(declare (type buffer ,buff))
	(ecase (buffer-byte-order ,buff)
	  (:MSBFirst (logior (ash ,w1 24) (ash ,w2 16) (ash ,w3 8) ,w4))
	  (:LSBFirst (logior (ash ,w4 24) (ash ,w3 16) (ash ,w2 8) ,w1))))))
  ((value buffer)
   (with-gensym (w1 w2 w3 w4 buff val)
     `(multiple-value-bind (,buff ,val) (values ,buffer ,value)
        (declare (type buffer ,buff))
        (let ((,w1 (ldb (byte 8 24) ,val))
	      (,w2 (ldb (byte 8 16) ,val))
	      (,w3 (ldb (byte 8 8) ,val))
	      (,w4 (ldb (byte 8 0) ,val)))
	  (ecase (buffer-byte-order ,buff)
	    (:MSBFirst 
	     (buffer-write-card8 ,w1 ,buff)
	     (buffer-write-card8 ,w2 ,buff)
	     (buffer-write-card8 ,w3 ,buff)
	     (buffer-write-card8 ,w4 ,buff))
	    (:LSBFirst 
	     (buffer-write-card8 ,w4 ,buff)
	     (buffer-write-card8 ,w3 ,buff)
	     (buffer-write-card8 ,w2 ,buff)
	     (buffer-write-card8 ,w1 ,buff))))))))

(define-array-accessor data card8)

(define-accessor version
  ((buffer)
   (with-gensym (buff) 
     `(let ((,buff ,buffer))
        (declare (type buffer ,buff))
        (make-version (buffer-read-card16 ,buff) (buffer-read-card16 ,buff)))))
  ((version buffer)
   (with-gensym (buff ver) 
     `(multiple-value-bind (,buff ,ver) (values ,buffer ,version)
        (declare (type buffer ,buff))
        (declare (type version ,ver))
        (buffer-write-card16 (aref ,ver 0) ,buff)
        (buffer-write-card16 (aref ,ver 1) ,buff)))))

(define-accessor string
  ((buffer)
   (with-gensym (vector buff) 
     `(let* ((,buff (values ,buffer))
	     (,vector (buffer-read-data ,buff (buffer-read-card16 ,buff))))
	(declare (type buffer ,buff))
	(index+ ,buff (pad-length (+ (length ,vector) 2) 4))
	(map 'string #'code-char ,vector))))
  ((string buffer)
   (with-gensym (length buff s)
     `(multiple-value-bind (,buff ,s) (values ,buffer ,string)
        (declare (type buffer ,buff))
        (let ((,length (length ,s)))
	  (buffer-write-card16 ,length ,buff)
	  (buffer-write-data (map 'data #'char-code ,s) ,buff)
	  (index+ ,buff (pad-length (+ 2 ,length) 4)))))))

;; sequence type.

(define-array-accessor versions version)

(define-array-accessor strings string)

;;;; special readers.

(defmacro read-minor-opcode (buffer) `(baref ,buffer 1))

(defmacro read-major-opcode (buffer) `(baref ,buffer 0))
