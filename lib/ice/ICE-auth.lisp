;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: ICE-auth.lisp,v 1.3 2004/07/12 21:22:56 ihatchondo Exp $
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

;;;; ICEauthority file parsing.

(defclass ice-authority-entry ()
  ((protocol-name :initarg :protocol-name :type string)
   (protocol-data :initarg :protocol-data :type string)
   (network-id :initarg :network-id :type string)
   (auth-name :initarg :auth-name :type string)
   (auth-data :initarg :auth-data)))

(defun parse-ICEauthority-file (file-pathname &key filter)
  "Returns the list of entries contained in an ICEauthority file."
  (declare (type (or null function) filter))
  (let ((index 0) buff)
    (with-open-file (fs file-pathname :element-type '(unsigned-byte 8))
      (setf buff (make-array (file-length fs) :element-type '(unsigned-byte 8)))
      (read-sequence buff fs))
    (macrolet ((_read_ (buffer index)
		 `(loop with w1 = (ash (aref ,buffer (1- (incf ,index))) 8)
		        with w2 = (aref ,buffer (1- (incf ,index)))
		        with length = (logior w1 w2)
		        for i from ,index below (+ ,index length)
		        collect (aref ,buffer i) into s
		        finally (incf ,index length) (return s)))
	       (_read-string_ (buffer index) 
		 `(map 'string #'code-char (_read_ ,buffer ,index))))
      (loop while (< index (length buff))
	    for entry = (make-instance 'ice-authority-entry
			 :protocol-name (_read-string_ buff index)
			 :protocol-data (_read-string_ buff index)
			 :network-id (_read-string_ buff index)
			 :auth-name (_read-string_ buff index)
			 :auth-data (_read_ buff index))
	    if filter when (funcall filter entry) collect entry end
	    else collect entry))))

(defun read-ice-auth-file (&key filter)
  "Returns the list of entries contained in the default authorization file.
  If the ICEAUTHORITY environment variable is set this file will be used,
  otherwise the default authorization file will be: <HOME>/.ICEauthority."
  (let ((file-name 
	 (or (get-environment-variable "ICEAUTHORITY")
	     #+:cmu "home:.ICEauthority"
	     #-:cmu (concatenate
		     'string 
		     (let ((homedir (user-homedir-pathname)))
		       (or (when homedir (namestring homedir)) "~/"))
		     ".ICEauthority"))))
    (parse-ICEauthority-file file-name :filter filter)))

;;;; Authentication protocol utilities.

(defvar *ice-authentication-protocols* nil)

(defmacro register-ice-authentication-protocol (name handler)
  "Register a handler for the authentication protocol designed by the given
  name. If the authentication protocol needs some multiple authentication
  phases, then all phases should be handled in the handler. When invoked two
  arguments will be passed to the handler: an ice-connection and a request
  of type authentication-required-request."
  (let ((cons (gensym)))
    `(let ((,cons (assoc ,name *ice-authentication-protocols* :test #'string=)))
       (if ,cons
	   (setf (cdr ,cons) ,handler)
           (setf *ice-authentication-protocols*
		 (nconc *ice-authentication-protocols*
			(list (cons ,name ,handler))))))))

(defun get-protocol-handler (name)
  "Returns a handler function for handling the named authentication protocol.
  If no handler is register for this protocol then NIL is returned."
  (declare (type string name))
  (cdr (assoc name *ice-authentication-protocols* :test #'string=)))

(defun ice-auth-proto-names ()
  "Returns all authentication protocol names that has a regstered handler."
  (loop for (names . nil) in *ice-authentication-protocols* collect names))
