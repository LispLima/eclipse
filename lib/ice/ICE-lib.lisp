;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: ICE-lib.lisp,v 1.4 2004/03/10 17:17:29 ihatchondo Exp $
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

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass ice-connection () 
  ((input-buffer 
    :initform nil :initarg :input-buffer :type buffer
    :accessor connection-input-buffer)
   (output-buffer
    :initform nil :initarg :output-buffer :type buffer
    :accessor connection-output-buffer)
   (input-byte-order
    :initform :MSBFirst :type ice-byte-order
    :accessor connection-input-byte-order)
   (output-byte-order
    :initform :MSBFirst :type ice-byte-order
    :accessor connection-output-byte-order)
   (ice-vendor
     :initform nil :initarg :ice-vendor :type string
     :accessor ice-vendor)
   (ice-release
     :initform nil :initarg :ice-release :type string
     :accessor ice-release)
   (connection-string 
     :initarg :connection-string :type string
     :accessor ice-connection-string)
   (protocol-version
     :initform nil :initarg :protocol-version :type card16
     :accessor ice-protocol-version)
   (protocol-revision
     :initform nil :initarg :protocol-revision :type card16
     :accessor ice-protocol-revision)
   (error-handler
     :initform #'signal-request-error :initarg :error-handler
     :type error-handler
     :accessor ice-error-handler)
   (stream 
     :initform nil :initarg :stream :type stream
     :accessor connection-stream)
   ))
) ;; End eval-when.

(deftype handlers () '(simple-array (simple-array (or null function) (*)) (*)))

(defgeneric ice-flush (ice-connection)
  (:documentation "attempts to ensure that any buffered output sent
   has reached its destination, and then returns."))

(defgeneric ice-connection-wait (ice-connection timeout)
  (:documentation "attemps to wait timeout second(s) or less if any 
   datas are available for reading on the input of an ice-connection 
   and returns T. Otherwise returns NIL."))

(defgeneric last-sended-message (ice-connection)
  (:documentation "Returns the last request sent on this ice-connection."))

(defgeneric last-received-message (ice-connection)
  (:documentation "Returns the last request received on this ice-connection."))

(defgeneric read-request (ice-connection)
  (:documentation "Returns the next request from an ice-connection input."))

(defmethod ice-flush ((conn ice-connection))
  (finish-output (connection-stream conn)))

(defmethod ice-connection-wait ((connection ice-connection) timeout)
  (default-input-wait (connection-stream connection) timeout))

(defmethod last-sended-message ((conn ice-connection))
  (with-slots (output-buffer output-byte-order) conn
    (declare (type buffer output-buffer))
    (decode-request
     (decode-ice-minor-opcode 
      (read-minor-opcode output-buffer)
      (read-major-opcode output-buffer))
     conn output-buffer output-byte-order)))

(defmethod last-received-message ((conn ice-connection))
  (with-slots (input-buffer input-byte-order) conn
    (declare (type buffer input-buffer))
    (decode-request
     (decode-ice-minor-opcode 
      (read-minor-opcode input-buffer)
      (read-major-opcode input-buffer))
     conn input-buffer input-byte-order)))

(defmethod read-request ((ice-connection ice-connection))
  (with-slots (input-buffer input-byte-order stream) ice-connection
    (declare (type (or null buffer) input-buffer))
    (setf input-buffer (make-buffer 4096))
    (read-sequence input-buffer stream :start 0 :end 8)
    (let* ((index 4)
	   (length (buffer-read-card32 input-byte-order input-buffer index)))
      (declare (type fixnum index length))
      (read-sequence input-buffer stream :start 8 :end (* 8 (1+ length))))
    (decode-request
       (decode-ice-minor-opcode 
	   (read-minor-opcode input-buffer)
	   (read-major-opcode input-buffer))
       ice-connection input-buffer input-byte-order)))

(defun process-request (ice-connection &key timeout handler (ice-flush-p t))
  "Invokes handler on each queued request until handler returns non-nil. If
  such a request is found, the non-nil handler value is returned by
  process-request. If handler returns nil for each request in the queue,
  process-request waits for another request to arrive. If timeout is non-nil
  and no request arrives within the specified timeout interval (given in
  seconds), process-request returns nil; if timeout is nil, process-request
  will not return until handler returns non-nil. process-request may wait only
  once on network data, and therefore timeout prematurely.

  If ice-flush-p is true, process-request first invokes ice-flush-p to send
  any buffered requests. (default is TRUE)

  If handler is an array, it is expected to be a two dimensional array
  containing handler functions for each request type of all active protocols.
  The vector index of the handler function for a particular request type is
  given by (aref (aref handler (request-major-opcode request))
                 (request-minor-opcode request)).

  A handler is always invoked with two arguments: ice-connection (an instance
  of ice-connection) and request (an instance of request)."
  (declare (type ice-connection ice-connection))
  (declare (type (or null fixnum) timeout))
  (declare (type (or null handlers function) handler))
  (declare (type boolean ice-flush-p))
  (loop with request = nil with return = nil with rhandler = handler do
	(when ice-flush-p (ice-flush ice-connection))
	(setf request (if timeout
			  (when (ice-connection-wait ice-connection timeout)
			    (read-request ice-connection))
			  (read-request ice-connection)))
	(cond ((null request) (loop-finish))
	      ((arrayp handler)
	       (with-slots ((m major-opcode) (n minor-opcode)) request
		 (setf rhandler (aref (aref handler m) n)))))
	(when rhandler (setf return (funcall rhandler ice-connection request)))
	until return
	finally (return return)))

(defmacro request-case
    ((ice-conn &key ice-flush-p timeout (place (gensym))) &body clauses)
  "Executes the matching clause for each queued request until a clause returns
  non-nil. The non-nil clause value is then returned. Each of the clauses is a
  list of the form (request-type-match [request-slots] &rest forms), where:

    * request-type-match -- Either a request type, a list of request-types,
      otherwise, or t. It is an error for the same key to appear in more than
      one clause.
    * request-slots -- If given, a list of (non-keyword) request slot symbols
      defined for the specified request type(s).
    * forms -- A list of forms that process the specified request type(s). The
      value of the last form is the value returned by the clause. 

  A clause matches a request if the request-type is equal to or a member of the
  request-match, or if the request-match is t or otherwise. If no otherwise or
  t clause appears, it is equivalent to having a final clause that returns nil.
  If request-slots is given, these symbols are bound to the value of the
  corresponding request slot in the clause forms. Each element of request-slots
  can also be a list of the form (variable request-slot), in which case the
  variable symbol is bound to the value of the request slot specified by the
  request-slot.

  If every clause returns nil for each request in the request queue, 
  request-case waits for another request to arrive. If :timeout is non-nil and
  no request arrives within the specified timeout interval (given in seconds), 
  request-case returns nil; if :timeout is nil, request-case will not return
  until a clause returns non-nil. 

  If ice-flush-p is true, request-case first invokes ice-flush to send any
  buffered requests.

  If place is given then it should be a symbol. It will be bound to the current
  handled request. If you need in any form to access the request object then
  give it a place."
  (flet ((compute-typecase-clauses (clauses)
	   (let ((otherwise (last clauses)))
	     (if (or (eq 'otherwise (caar otherwise)) (eq 't (caar otherwise)))
		 (setf clauses (butlast clauses))
		 (setf otherwise `((otherwise nil))))
	     (loop for clause in clauses
		   for type = (car clause)
		   for args = (cadr clause)
		   for body = (cddr clause)
		   when (listp type) do (push 'or type) end
		   when args do (setf args `((with-slots ,args ,place ,@body)))
		   collect `(,type ,@(or args body)) into forms
		   finally (return (append forms otherwise))))))
    `(loop for ,place = (process-request
			    ,ice-conn
			    :handler (lambda (x req) (declare (ignore x)) req)
			    :ice-flush-p ,ice-flush-p
                            :timeout ,timeout)
           for return = (typecase ,place ,@(compute-typecase-clauses clauses))
           until return
           finally (return return))))

(defun open-connection (network-ids &key connection must-authenticate-p)
  "Returns an ice-connection object if it succeeds. Otherwise an error 
  will be signaled. (its type will depend on the reason of the failure)
  
  The network-ids must be a list of network-id. An attempt will be made to use
  the first network-id. If this fails an attempt will be made to use the second
  one, and so on. Each network-id has the following format:
    local/<HOST-NAME>:<PATH>
    tcp/<HOST-NAME>:<PORT-NUMBER>
    decnet/<HOST-NAME>::<OBJ>

  Any authentication requirements are handled internally by the ICE Library.
  The method by which authentication data is obtained is implementation
  dependent. We only use and know the default use of the ICEauthority file.
  You will need to register your own methods for other authentication methods.
  To do so see and use register-ice-authentication-protocol.

  If connection is non nil it is expected to be of type or a sub-type of
  ice-connection. If given, it will be filled during the setup phase and
  returned."
  (declare (type (or null list) network-ids))
  (declare (type boolean must-authenticate-p))
  (declare (type (or null ice-connection) connection))
  (multiple-value-bind (chosen-network-id stream)
      (connect-to-peer network-ids)
    (declare (type (or null string) chosen-network-id))
    (declare (type (or null stream) stream))
    (when (or (null chosen-network-id) (null stream))
      (error "ICE: Failed to connect to any destination."))
    (unless connection (setf connection (make-instance 'ice-connection)))
    (setf (connection-stream connection) stream)
    (setf (ice-connection-string connection) chosen-network-id)
    ;; send our byte order and wait for peer answer.
    (post-request :byte-order connection :byte-order :MSBfirst)
    (let ((peer-byte-order (read-request connection)))
      (setf (connection-input-byte-order connection)
	    (byte-order-byte-order peer-byte-order)))
    ;; send connection-setup request and wait for connection reply.
    ;; authentication will take place behind the scene.
    (let ((protocols 
	   (available-authentication-protocols
	    "ICE" chosen-network-id (ice-auth-proto-names)))
	  (versions (make-default-versions)))
      (declare (type (simple-array string (*)) protocols))
      (declare (type versions versions))
      (post-request :connection-setup connection
	:number-of-versions-offered (length versions)
	:must-authenticate-p must-authenticate-p
	:vendor-name +vendor-name+
	:release-name +release-name+
	:authentication-protocol-names protocols
	:version-list versions
	:number-of-authentication-protocol-names-offered (length protocols))
      (request-case (connection :timeout nil :place request)
	(authentication-required ((index authentication-protocol-index))
	  (let ((handler (get-protocol-handler (aref protocols index))))
	    (declare (type function handler))
	    (funcall handler connection request))
	  (values))
	(connection-reply (vendor-name release-name version-index)
	  (let ((version (aref versions version-index)))
	    (setf (ice-protocol-revision connection) (aref version 1)
		  (ice-protocol-version connection) (aref version 0)
		  (ice-vendor connection) vendor-name
		  (ice-release connection) release-name)
	    ;; Finally returns the ice-connection instance.
	    connection))
	(t (error "ICE: bad state during connection: ~a." request))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;;;
;;;;                           utility routines                            ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-network-id (network-id)
  "returns as a multiple values: 
    connection-type  keyword (or :local :tcp :decnet)
    host name        string
    connection obj   (or string fixnum t). 
  The network-id string has the following format:
    local/<HOST-NAME>:<PATH>
    tcp/<HOST-NAME>:<PORT-NUMBER>
    decnet/<HOST-NAME>::<OBJ>
  If none of those format is founded an error will be signaled."  
  (declare (type string network-id))
  (let* ((slash (or (position #\/ network-id :test #'char=) 0))
	 (colon (or (position #\: network-id :test #'char= :start slash) 0)))
    (cond ((= 0 slash) (error "ICE: Badly formed network-id: no type present."))
	  ((= 0 colon) (error "ICE: Badly formed network-id: no colon delim."))
	  ((= slash (1- colon)) (error "ICE: Missing host in the network-id.")))
    (let ((type (subseq network-id 0 slash))    
	  (host-name (subseq network-id (1+ slash) colon))
	  (obj (subseq network-id (1+ colon))))
      (cond ((string= type "local") (setf type :local))
	    ((string= type "tcp")
	     (setf obj (parse-integer obj)) (setf type :tcp))
	    ((string= type "decnet")
	     (setf type :decnet)
	     ;; a second colon should be present.
	     (if (= 0 (or (position #\: (the string obj) :test #'char=) -1))
		 (setf obj (subseq obj 1))
	         (error "ICE: Invalid decnet network-id with one colon.")))
	    (t (error "ICE: type is not (or \"local\" \"tcp\" \"decnet\").")))
      (values type host-name obj))))

(defun connect-to-peer (network-ids)
  "returns as a multiple values the chosen network-id and a two way stream
  connected to the first peer that accept us."
  (loop with stream = nil
	for network-id in network-ids do
	(multiple-value-bind (type hostname obj)
	    (parse-network-id network-id)
	  (setf stream 
		(ignore-errors
		  (case type 
		    (:local (connect-to-local-peer hostname obj))
		    (:tcp (connect-to-tcp-peer hostname obj))
		    (:decnet (connect-to-decnet-peer hostname obj))))))
	until stream
	finally (return (values network-id stream))))

(defun make-default-versions
    (&key (major +ice-proto-major+) (minor +ice-proto-minor+))
  (declare (type card16 major minor))
  (let ((versions (make-versions 1)))
    (declare (type versions versions))
    (setf (aref versions 0) (make-version major minor))
    versions))

(defun available-authentication-protocols (proto-name network-id protocols)
  "returns an array of string containing the names of the available 
  authentication protocols, according to .ICEauthority file."
  (declare (type string proto-name network-id))
  (declare (type list protocols))
  (let ((names (list)))
    (flet ((filter (entry)
	     (with-slots ((pn protocol-name) (nid network-id) auth-name) entry
	       (when (and (string= nid network-id) (string= pn proto-name))
		 (when (member auth-name protocols :test #'string=)
		   (push auth-name names))))))
      (read-ice-auth-file :filter #'filter)
      (make-array (length names) :initial-contents names))))

;;;; ICE authentication.

;; MIT-MAGIC-COOKIE-1 authentication protocol.

(defun mit-magic-cookie-1-handler (ice-connection authentication-required-req)
  (declare (ignore authentication-required-req))
  (flet ((filter (entry)
	   (with-slots (protocol-name network-id auth-name) entry
	     (and (string= protocol-name "ICE")
		  (string= network-id (ice-connection-string ice-connection))
		  (string= auth-name "MIT-MAGIC-COOKIE-1")))))
    (let ((entry (car (read-ice-auth-file :filter #'filter))))
      (if (and entry (slot-value entry 'auth-data))
	  (with-slots (auth-data) entry
	    (let ((length (length auth-data)))
	      (post-request :authentication-reply ice-connection
		:length-of-authentication-data length
		:data (make-data length :initial-contents auth-data))))
	  (error "Can't find correct MIT-MAGIC-COOKIE-1 authentication")))))

(register-ice-authentication-protocol
  "MIT-MAGIC-COOKIE-1"
  #'mit-magic-cookie-1-handler)
