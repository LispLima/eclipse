;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id:$
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
      `(setf (aref ,buffer (1- (incf ,index))) (position ,value ,',vector)))))

(defmacro define-member16-accessor (type vector)
  `(define-accessor ,type 
     ((byte-order buffer index)
      `(aref ,',vector (buffer-read-card16 ,byte-order ,buffer ,index)))
     ((value byte-order buffer index)
      `(buffer-write-card16
	(position ,value ,',vector)
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
   `(aref ,buffer (1- (incf ,index))))
  ((value byte-order buffer index)
   (declare (ignore byte-order))
   `(setf (aref ,buffer (1- (incf ,index))) ,value)))

(define-accessor boolean
  ((byte-order buffer index)
   `(= 1 (buffer-read-card8 ,byte-order ,buffer ,index)))
  ((value byte-order buffer index)
   `(buffer-write-card8 (if ,value 1 0) ,byte-order ,buffer ,index)))

(define-accessor card16
  ((byte-order buffer index)
   (with-gensym (w1 w2 _bo _buff)
     `(multiple-value-bind (,_bo ,_buff) (values ,byte-order ,buffer)
        (let ((,w1 (buffer-read-card8 ,_bo ,_buff ,index))
	      (,w2 (buffer-read-card8 ,_bo ,_buff ,index)))
	  (ecase ,_bo
	    (:MSBFirst (logior (ash ,w1 8) ,w2))
	    (:LSBFirst (logior (ash ,w2 8) ,w1)))))))
  ((value byte-order buffer index)
   (with-gensym (w1 w2 _bo _buff _val)
     `(multiple-value-bind (,_bo ,_buff ,_val)
          (values ,byte-order ,buffer ,value)
        (let ((,w1 (ldb (byte 16 8) ,_val))
	      (,w2 (ldb (byte 8 0) ,_val)))
	  (ecase ,_bo
	    (:MSBFirst 
	     (buffer-write-card8 ,w1 ,_bo ,_buff ,index)
	     (buffer-write-card8 ,w2 ,_bo ,_buff ,index))
	    (:LSBFirst 
	     (buffer-write-card8 ,w2 ,_bo ,_buff ,index)
	     (buffer-write-card8 ,w1 ,_bo ,_buff ,index))))))))

(define-accessor card32
  ((byte-order buffer index)
   (with-gensym (w1 w2 w3 w4 _bo _buff)
     `(multiple-value-bind (,_bo ,_buff) (values ,byte-order ,buffer)
        (let ((,w1 (buffer-read-card8 ,_bo ,_buff ,index))
	      (,w2 (buffer-read-card8 ,_bo ,_buff ,index))
	      (,w3 (buffer-read-card8 ,_bo ,_buff ,index))
	      (,w4 (buffer-read-card8 ,_bo ,_buff ,index)))
	  (ecase ,_bo
	    (:MSBFirst (logior (ash ,w1 24) (ash ,w2 16) (ash ,w3 8) ,w4))
	    (:LSBFirst (logior (ash ,w4 24) (ash ,w3 16) (ash ,w2 8) ,w1)))))))
  ((value byte-order buffer index)
   (with-gensym (w1 w2 w3 w4 _bo _buff _val)
     `(multiple-value-bind (,_bo ,_buff ,_val)
          (values ,byte-order ,buffer ,value)
        (let ((,w1 (ldb (byte 32 24) ,_val))
	      (,w2 (ldb (byte 24 16) ,_val))
	      (,w3 (ldb (byte 16 8) ,_val))
	      (,w4 (ldb (byte 8 0) ,_val)))
	  (ecase ,_bo
	    (:MSBFirst 
	     (buffer-write-card8 ,w1 ,_bo ,_buff ,index)
	     (buffer-write-card8 ,w2 ,_bo ,_buff ,index)
	     (buffer-write-card8 ,w3 ,_bo ,_buff ,index)
	     (buffer-write-card8 ,w4 ,_bo ,_buff ,index))
	    (:LSBFirst 
	     (buffer-write-card8 ,w4 ,_bo ,_buff ,index)
	     (buffer-write-card8 ,w3 ,_bo ,_buff ,index)
	     (buffer-write-card8 ,w2 ,_bo ,_buff ,index)
	     (buffer-write-card8 ,w1 ,_bo ,_buff ,index))))))))

(define-array-accessor data card8)

(define-accessor version
  ((byte-order buffer index)
   (with-gensym (_bo _buff) 
     `(multiple-value-bind (,_bo ,_buff) (values ,byte-order ,buffer)
        (make-version (buffer-read-card16 ,_bo ,_buff ,index)
	              (buffer-read-card16 ,_bo ,_buff ,index)))))
  ((version byte-order buffer index)
   (with-gensym (_bo _buff _ver) 
     `(multiple-value-bind (,_bo ,_buff ,_ver)
          (values ,byte-order ,buffer ,version)
       (buffer-write-card16 (aref ,_ver 0) ,_bo ,_buff ,index)
       (buffer-write-card16 (aref ,_ver 1) ,_bo ,_buff ,index)))))

(define-accessor string
  ((byte-order buffer index)
   (with-gensym (vector _bo _buff) 
     `(multiple-value-bind (,_bo ,_buff) (values ,byte-order ,buffer)     
        (let ((,vector (buffer-read-data
			,_bo ,_buff ,index 
			(buffer-read-card16 ,_bo ,_buff ,index))))
	  (incf ,index (pad-length (+ (length ,vector) 2) 4))
	  (map 'string #'code-char ,vector)))))
  ((string byte-order buffer index)
   (with-gensym (length _bo _buff _s)
     `(multiple-value-bind (,_bo ,_buff ,_s)
          (values ,byte-order ,buffer ,string)
        (let ((,length (length ,_s)))
	  (buffer-write-card16 ,length ,_bo ,_buff ,index)
	  (buffer-write-data (map 'vector #'char-code ,_s) ,_bo ,_buff ,index)
	  (incf ,index (pad-length (+ 2 ,length) 4)))))))

;; sequence type.

(define-array-accessor versions version)

(define-array-accessor strings string)

;;;; special readers.

(defmacro read-minor-opcode (buffer) `(aref ,buffer 1))

(defmacro read-major-opcode (buffer) `(aref ,buffer 0))

;;;;

(defun make-buffer (size &key (element-type 'card8))
  (make-array size :element-type element-type))

