;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: dependent.lisp,v 1.2 2004/03/04 14:51:06 ihatchondo Exp $
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

(defun get-environment-variable (&optional (string "SESSION_MANAGER"))
  ;; Add some other system.
  #+excl (sys:getenv string)
  #+cmu (cdr (assoc string ext:*environment-list* :test #'string=))
  #+clisp (sys::getenv (string string))
  #+sbcl (sb-ext::posix-getenv string)
  #-(or excl cmu clisp sbcl) (error "GET-ENVIRONMENT-VARIABLE not implemented")
  )

(defun default-input-wait (input-stream timeout)
  "Sleep until there is input on the STREAM, or for TIMEOUT seconds,
  whichever comes first. If there was a timeout, return NIL."
  (declare (type stream input-stream))
  (declare (type fixnum timeout))
  (declare (optimize (speed 3) (safety 0) (debug 0)))

;;   #+clisp (multiple-value-bind (sec usec) (floor (or timeout 0))
;;             (#+lisp=cl ext:socket-status #-lisp=cl lisp:socket-status
;;                        input-stream (and timeout sec) (round usec 1d-6)))
;;   #+(or cmu scl)
;;   (#+mp mp:process-wait-until-fd-usable #-mp sys:wait-until-fd-usable
;;         (system:fd-stream-fd input-stream) :input timeout)
;;   #+(and sbcl net.sbcl.sockets)
;;   (net.sbcl.sockets:wait-for-input-data input-stream timeout)
;;   #+(and sbcl db-sockets)
;;   (sb-sys:wait-until-fd-usable
;;    (sb-sys:fd-stream-fd input-stream) :input timeout)
;;   #-(or clisp cmu (and sbcl (or net.sbcl.sockets db-sockets)) scl)

  (loop with ready-p of-type boolean = (listen input-stream)
	until (or ready-p (zerop timeout))
	do (decf timeout) (sleep 1)
	   (setf ready-p (listen input-stream))
	finally (return ready-p)))

(defun connect-to-local-peer (host path &key (kind :stream) (bin t))
  "connect to a local unix socket designed by the pathname and returns a 
  two way stream associated with."
  (declare (type simple-string host path) #-cmu (ignore kind))
  (declare (ignorable host))
  #+allegro (socket:make-socket :type :stream
                                :address-family :file
                                :connect :active
                                :remote-filename path)
  #+cmu (sys:make-fd-stream (ext:connect-to-unix-socket path kind)
                            :input t :output t :element-type
                            (if bin '(unsigned-byte 8) 'character))
  #+sbcl
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (sb-bsd-sockets:socket-make-stream socket
        :input t :output t :buffering :none
	:element-type (if bin '(unsigned-byte 8) 'character)))
  #-(or allegro cmu sbcl)
  (open path :element-type (if bin '(unsigned-byte 8) 'character)
        :direction :io))

(defun connect-to-tcp-peer (host port &optional (bin t))
  "connect to a tcp socket to host at port, and returns a two way stream
   associated with."
  (declare (type string host))
  (declare (type fixnum port))
  (declare (type boolean bin))
  #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
  #+clisp (#+lisp=cl ext:socket-connect #-lisp=cl lisp:socket-connect
		     port host :element-type
		     (if bin '(unsigned-byte 8) 'character))
  #+(or cmu scl)
  (sys:make-fd-stream (ext:connect-to-inet-socket host port)
		      :buffering (if bin :full :line)
		      :input t :output t :element-type
		      (if bin '(unsigned-byte 8) 'character))
  #+gcl (si:make-socket-stream host port bin) ; FIXME
  #+lispworks (comm:open-tcp-stream host port :direction :io :element-type
				    (if bin 'unsigned-byte 'base-char))
  #+mcl (ccl:make-socket :remote-host host :remote-port port
			 :format (if binary-p :binary :text))
  #+sbcl
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
		  :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket
				   (sb-bsd-sockets::host-ent-address
				    (sb-bsd-sockets:get-host-by-name host))
				   port)
    (sb-bsd-sockets:socket-make-stream socket
        :input t :output t :buffering (if bin :none :line)
	:element-type (if bin '(unsigned-byte 8) 'character)))
  #-(or allegro clisp cmu gcl lispworks mcl sbcl)
  (error 'not-implemented :proc (list 'open-socket host port bin)))

(defun connect-to-decnet-peer (host obj)
  "connect to a decnet socket to host at/on obj, and returns a two way stream
   associated with."
  (declare (ignore host obj))
  (error "decnet connection is not implemented."))
