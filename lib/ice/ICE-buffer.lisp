;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: ICE-buffer.lisp,v 1.1 2004/01/12 11:10:51 ihatchondo Exp $
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

(defun pad-length (message-byte-length &optional (modulo 8))
  "returns the number of bytes that should be add to a message of length
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
  "Define macro accessor for a type. The first body form defines the reader 
  macro the second body form defines the writer macro."
  (let ((reader (sintern (format nil "BUFFER-READ-~a" type)))
	(writer (sintern (format nil "BUFFER-WRITE-~a" type))))
    `(progn
       (defmacro ,reader ,rargs ,@rbody)
       (defmacro ,writer ,wargs ,@wbody))))

(defmacro define-member8-accessor (type vector)
  `(define-accessor ,type 
     ((byte-order buffer index)
      `(aref ,',vector (buffer-read-card8 ,byte-order ,buffer ,index)))
     ((value byte-order buffer index)
      (declare (ignore byte-order))
      `(setf (aref ,buffer (1- (incf ,index))) (vpos ,value ,',vector)))))

(defmacro define-member16-accessor (type vector)
  `(define-accessor ,type 
     ((byte-order buffer index)
      `(aref ,',vector (buffer-read-card16 ,byte-order ,buffer ,index)))
     ((value byte-order buffer index)
      `(buffer-write-card16
	(vpos ,value ,',vector)
	,byte-order ,buffer ,index))))

(defmacro define-array-accessor (type element-type)
  (let ((read (sintern (format nil "BUFFER-READ-~a" element-type)))
	(write (sintern (format nil "BUFFER-WRITE-~a" element-type))))
    `(define-accessor ,type 
       ((byte-order buffer index length)
	`(loop with size = ,length
	       with v = (make-array size :element-type ',',element-type)
	       for i from 0 below size
	       do (setf (aref v i) (,',read ,byte-order ,buffer ,index))
               finally (return v)))
       ((array byte-order buffer index)
	`(loop for elem across ,array
	       do (,',write elem ,byte-order ,buffer ,index))))))

;;;; buffer-{read,write}-<type> macros.

(define-member8-accessor ice-byte-order '#(:LSBFirst :MSBFirst))

(define-member8-accessor error-severity
  '#(:can-continue :fatal-to-protocol :fatal-to-connection))

(define-accessor card8
  ((byte-order buffer index)
   (declare (ignore byte-order))
   `(aref (the buffer ,buffer) (1- (incf ,index))))
  ((value byte-order buffer index)
   (declare (ignore byte-order))
   `(setf (aref (the buffer ,buffer) (1- (incf ,index))) ,value)))

(define-accessor boolean
  ((byte-order buffer index)
   `(= 1 (buffer-read-card8 ,byte-order ,buffer ,index)))
  ((value byte-order buffer index)
   `(buffer-write-card8 (if ,value 1 0) ,byte-order ,buffer ,index)))

(define-accessor card16
  ((byte-order buffer index)
   (with-gensym (w1 w2 bo buff)
     `(multiple-value-bind (,bo ,buff) (values ,byte-order ,buffer)
        (declare (type buffer ,buff))
        (let ((,w1 (buffer-read-card8 ,bo ,buff ,index))
	      (,w2 (buffer-read-card8 ,bo ,buff ,index)))
	  (ecase ,bo
	    (:MSBFirst (logior (ash ,w1 8) ,w2))
	    (:LSBFirst (logior (ash ,w2 8) ,w1)))))))
  ((value byte-order buffer index)
   (with-gensym (w1 w2 bo buff val)
     `(multiple-value-bind (,bo ,buff ,val)
          (values ,byte-order ,buffer ,value)
        (declare (type buffer ,buff))
        (let ((,w1 (ldb (byte 16 8) ,val))
	      (,w2 (ldb (byte 8 0) ,val)))
	  (ecase ,bo
	    (:MSBFirst 
	     (buffer-write-card8 ,w1 ,bo ,buff ,index)
	     (buffer-write-card8 ,w2 ,bo ,buff ,index))
	    (:LSBFirst 
	     (buffer-write-card8 ,w2 ,bo ,buff ,index)
	     (buffer-write-card8 ,w1 ,bo ,buff ,index))))))))

(define-accessor card32
  ((byte-order buffer index)
   (with-gensym (w1 w2 w3 w4 bo buff)
     `(multiple-value-bind (,bo ,buff) (values ,byte-order ,buffer)
        (declare (type buffer ,buff))
        (let ((,w1 (buffer-read-card8 ,bo ,buff ,index))
	      (,w2 (buffer-read-card8 ,bo ,buff ,index))
	      (,w3 (buffer-read-card8 ,bo ,buff ,index))
	      (,w4 (buffer-read-card8 ,bo ,buff ,index)))
	  (ecase ,bo
	    (:MSBFirst (logior (ash ,w1 24) (ash ,w2 16) (ash ,w3 8) ,w4))
	    (:LSBFirst (logior (ash ,w4 24) (ash ,w3 16) (ash ,w2 8) ,w1)))))))
  ((value byte-order buffer index)
   (with-gensym (w1 w2 w3 w4 bo buff val)
     `(multiple-value-bind (,bo ,buff ,val)
          (values ,byte-order ,buffer ,value)
        (declare (type buffer ,buff))
        (let ((,w1 (ldb (byte 32 24) ,val))
	      (,w2 (ldb (byte 24 16) ,val))
	      (,w3 (ldb (byte 16 8) ,val))
	      (,w4 (ldb (byte 8 0) ,val)))
	  (ecase ,bo
	    (:MSBFirst 
	     (buffer-write-card8 ,w1 ,bo ,buff ,index)
	     (buffer-write-card8 ,w2 ,bo ,buff ,index)
	     (buffer-write-card8 ,w3 ,bo ,buff ,index)
	     (buffer-write-card8 ,w4 ,bo ,buff ,index))
	    (:LSBFirst 
	     (buffer-write-card8 ,w4 ,bo ,buff ,index)
	     (buffer-write-card8 ,w3 ,bo ,buff ,index)
	     (buffer-write-card8 ,w2 ,bo ,buff ,index)
	     (buffer-write-card8 ,w1 ,bo ,buff ,index))))))))

(define-array-accessor data card8)

(define-accessor version
  ((byte-order buffer index)
   (with-gensym (bo buff) 
     `(multiple-value-bind (,bo ,buff) (values ,byte-order ,buffer)
        (declare (type buffer ,buff))
        (make-version (buffer-read-card16 ,bo ,buff ,index)
	              (buffer-read-card16 ,bo ,buff ,index)))))
  ((version byte-order buffer index)
   (with-gensym (bo buff ver) 
     `(multiple-value-bind (,bo ,buff ,ver)
          (values ,byte-order ,buffer ,version)
        (declare (type buffer ,buff))
        (buffer-write-card16 (aref ,ver 0) ,bo ,buff ,index)
        (buffer-write-card16 (aref ,ver 1) ,bo ,buff ,index)))))

(define-accessor string
  ((byte-order buffer index)
   (with-gensym (vector bo buff) 
     `(multiple-value-bind (,bo ,buff) (values ,byte-order ,buffer)     
        (declare (type buffer ,buff))
        (let ((,vector (buffer-read-data
			,bo ,buff ,index 
			(buffer-read-card16 ,bo ,buff ,index))))
	  (incf ,index (pad-length (+ (length ,vector) 2) 4))
	  (map 'string #'code-char ,vector)))))
  ((string byte-order buffer index)
   (with-gensym (length bo buff s)
     `(multiple-value-bind (,bo ,buff ,s)
          (values ,byte-order ,buffer ,string)
        (declare (type buffer ,buff))
        (let ((,length (length ,s)))
	  (buffer-write-card16 ,length ,bo ,buff ,index)
	  (buffer-write-data (map 'vector #'char-code ,s) ,bo ,buff ,index)
	  (incf ,index (pad-length (+ 2 ,length) 4)))))))

;; sequence type.

(define-array-accessor versions version)

(define-array-accessor strings string)

;;;; special readers.

(defmacro read-minor-opcode (buffer) `(aref ,buffer 1))

(defmacro read-major-opcode (buffer) `(aref ,buffer 0))

;;;;

(deftype buffer () '(simple-array card8 (*)))

(defun buffer-size (buffer)
  (declare (type buffer buffer))
  (array-dimension buffer 0))

(defun make-buffer (size)
  (make-array size :element-type 'card8))

