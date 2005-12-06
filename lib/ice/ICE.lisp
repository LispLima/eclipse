;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: ICE.lisp,v 1.4 2005/01/08 00:02:51 ihatchondo Exp $
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

;;; Protocol version

(defconstant +ice-proto-major+ 1)
(defconstant +ice-proto-minor+ 0)

;;; Protocol release and vendor names

(defconstant +release-name+ "CL-SM-1.0")
(defconstant +vendor-name+ "LoopFor & Mapcar corp")

;;; ICE minor opcodes

(defvar *ice-minor-opcodes* (make-array 25 :initial-element nil))

(defun register-protocol (protocol-opcode request-key-vector)
  "Cache the protocol request keys according to its opcode."
  (setf (svref *ice-minor-opcodes* protocol-opcode) request-key-vector))

(defmacro define-request (request-key-name code)
  "Creates the association between the request key and its opcode."
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (get ',request-key-name 'request-code) ,code)))

(defun decode-ice-minor-opcode (index &optional (protocol-opcode 0))
  "Returns the request key associated to this request index in the protocol
   designed by its opcode (0, the default design the ICE protocol)."
  (declare (type card8 index))
  (aref (the (simple-array keyword (*))
	  (svref *ice-minor-opcodes* protocol-opcode)) 
	index))
  
(defun encode-ice-minor-opcode (opcode-key)
  "Returns the request opcode associated with this request key."
  (declare (type keyword opcode-key))
  (get opcode-key 'request-code))

;;;; Errors

(defvar *errors* (list) "association list: (error-code . error-key)")

(defmacro define-error (error-key error-code)
  "Creates the association between the error keyword and the error code."
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (push (cons ,error-code ,error-key) *errors*)
     (setf (get ',error-key 'error-code) ,error-code)))

(defun decode-error (code)
  "Returns the error key associated to this code."
  (declare (type card16 code))
  (cdr (assoc code *errors*)))

(defun encode-error (key)
  "Returns the error code associated to this error key."
  (declare (type keyword key))
  (get key 'error-code))

;;;; Types

;; 
         
(deftype error-handler () `(or null function))
  
;; unsigned integer types.

(deftype card32 () `(unsigned-byte 32))
(deftype card24 () `(unsigned-byte 24))
(deftype card16 () `(unsigned-byte 16))
(deftype card8 () `(unsigned-byte 8))

;; complex types.

(deftype ice-byte-order () `(member :LSBFirst :MSBFirst))
(deftype data () `(simple-array card8 (*)))
(deftype version () `(simple-array card16 (2)))
(deftype versions () `(simple-array version (*)))
(deftype strings () `(or null (simple-array simple-string (*))))
(deftype error-severity ()
  `(member :can-continue :fatal-to-protocol :fatal-to-connection))

;;;; <type>-length functions.

(macrolet ((generate-integer-length-function (prefix bit-size)
	     (let ((name (intern (format nil "~a~d-LENGTH" prefix bit-size))))
	       `(defun ,name (value)
		  (declare (ignore value))
		  (declare (optimize (speed 3) (safety 0)))
		  ,(/ bit-size 8)))))
  (generate-integer-length-function card 32)
  (generate-integer-length-function card 24)
  (generate-integer-length-function card 16)
  (generate-integer-length-function card 8))

(defun ice-byte-order-length (value)
  (declare (ignore value))
  1)

(defun boolean-length (value)
  (declare (ignore value))
  1)

(defun error-severity-length (value)
  (declare (ignore value))
  1)

(defun data-length (data)
  (declare (type (or null data) data))
  (declare (optimize (speed 3) (safety 0)))
  (if (null data) 0 (length data)))

(defun version-length (version)
  (declare (ignore version))
  (declare (optimize (speed 3) (safety 0)))
  4)

(defun versions-length (versions)
  (declare (type versions versions))
  (* 4 (length versions)))

(defun string-length (string)
  (declare (type (or null string) string))
  (let ((l (+ 2 (if (null string) 0 (length string)))))
    (+ l (mod (- l) 4))))

(defun strings-length (strings)
  (declare (type strings strings))
  (if (null strings) 
      0 (loop for string across strings sum (string-length string))))

;;;; type constructor

(defun make-data (length &rest args &key (initial-element 0) &allow-other-keys)
  "Creates and returns an array constructed of the most specialized type that
   can accommodate elements of type (unsigned-byte 8). For the rest of the 
   options see common-lisp:make-array."
  (declare (type fixnum length))
  (if (getf args :initial-contents)
      (remf args :initial-element)
      (setf (getf args :initial-element) initial-element))
  (apply #'make-array length :element-type 'card8 args))

(defun make-version (major minor)
  (declare (type card16 major minor))
  (make-array 2 :element-type 'card16 :initial-contents (list major minor)))

(defun make-versions (length)
  (declare (type fixnum length))
  (make-array length :element-type '(or null version) :initial-element nil))

;;;; macros

(defmacro sintern (&body forms) `(intern (with-standard-io-syntax ,@forms)))

(defmacro kintern (&rest forms) `(intern ,@forms :keyword))
