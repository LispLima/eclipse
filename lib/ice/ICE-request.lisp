;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: ICE-request.lisp,v 1.10 2005/03/25 14:43:54 ihatchondo Exp $
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

;;;; Protocol class.

(defclass request () 
  ((major-opcode
     :initarg :major-opcode
     :initform +ice-proto-minor+
     :reader request-major-opcode
     :type card8)
   (minor-opcode 
     :initarg :minor-opcode
     :type card8
     :reader request-minor-opcode))
  (:documentation "Protocol class."))
  
(defgeneric make-request (key-name &rest request-slots)
  (:documentation "Returns a newly allocated instance of class designed 
   by the request-key-name."))

(defgeneric decode-request (request-key ice-connection buffer)
  (:documentation "Returns a instance of request that represent the one
   in the input buffer."))

(defgeneric post-request (request-key ice-connection &rest request-slots)
  (:documentation "Post a request specified by the request-key and
  request-slots to the given destination ice-connection. The request-slots
  passed depend on the request type. The keyword symbols used for each request
  type are request slot names defined by the declare-request macro."))

(defgeneric request-length (request)
  (:documentation "Computes and returns the size (in bytes) of a request."))

(defun request-p (object)
  "Returns true if object is of type request; otherwise, returns nil."
  (typep object 'request))

;; request declaration.

(define-request :request-error 0)
(define-request :byte-order 1)
(define-request :connection-setup 2)
(define-request :authentication-required 3)
(define-request :authentication-reply 4)
(define-request :authentication-next-phase 5)
(define-request :connection-reply 6)
(define-request :protocol-setup 7)
(define-request :protocol-reply 8)
(define-request :ping 9)
(define-request :ping-reply 10)
(define-request :want-to-close 11)
(define-request :no-close 12)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (register-protocol +ice-proto-minor+ 
    '#(:request-error :byte-order :connection-setup :authentication-required
       :authentication-reply :authentication-next-phase :connection-reply
       :protocol-setup :protocol-reply :ping :ping-reply :want-to-close
       :no-close)))

(declare-request byte-order (request)
  ((byte-order :type ice-byte-order :pad-size 1)
   (length :type card32 :initform 0))  
  (:documentation "Both parties must send this message before sending any
  other, including errors. This message specifies the byte order that will be
  used on subsequent messages sent by this party. 
   Note: If the receiver detects an error in this message, it must be sure to
  send its own {defclass byte-order} message before sending the Error."))

(declare-request connection-setup (request)
  ((number-of-versions-offered :type card8)
   (number-of-authentication-protocol-names-offered :type card8)
   (length :type card32 :initform 0)
   (must-authenticate-p :type boolean :pad-size 7)
   (vendor-name :type string)
   (release-name :type string)
   (authentication-protocol-names
    :type strings :length number-of-authentication-protocol-names-offered)
   (version-list :type versions :length number-of-versions-offered))
  (:documentation "The party that initiates the connection (the one that does
  the ``(connect ..)'') must send this message as the second message (after
  {defclass byte-order} ) on startup. Versions gives an array of version
  (array card16 (2)), in decreasing order of preference, of the protocol
  versions this party is capable of speaking. The specification specifies
  major version 1, minor version 0. 
   If must-authenticate-p is T, the initiating party demands authentication;
  the accepting party must pick an authentication scheme and use it. In this
  case, the only valid response is {defclass authentication-required} .
   If must-authenticate-p is NIL, the accepting party may choose an
  authentication mechanism, use a host-address-based authentication scheme,
  or skip authentication. As a result, a {defclass connection-reply} or a
  {defclass authentication-required} are both valid responses. If a host
  address based authentication scheme is used, then in this case both 
  {defclass authentication-rejected} or {defclass authentication-failed}
  error messages are possible.
   If must-authenticate is True, presumably the party will offer only
  authentication mechanisms allowing mutual authentication. Vendor gives the
  name of the vendor of this ICE implementation. release-name gives the release
  identifier of this ICE implementation.
   authentication-protocol-names specifies a (or NIL, if must-authenticate-p
  is NIL) array of authentication protocols names the party is willing to
  perform."))

(declare-request authentication-required (request)
  ((authentication-protocol-index :type card8 :pad-size 1)
   (length :type card32 :initform 0)
   (length-of-authentication-data :type card16 :pad-size 6)
   (data :type data :length length-of-authentication-data))
  (:documentation "This message is sent in response to both messages 
  {defclass connection-setup} and {defclass protocol-setup} to specify that
  authentication is to be done and what authentication mechanism is to be used.
  The authentication protocol is specified by a 0-based index into the array of
  names given in the connection-setup or protocol-setup. Any protocol-specific
  data that might be required is also sent."))

(declare-request authentication-reply (request)
  ((minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (length-of-authentication-data :type card16 :pad-size 6)
   (data :type data :length length-of-authentication-data))
  (:documentation "This message is sent in response to both messages 
  {defclass authentication-required} or {defclass authentication-next-phase}
  to supply authentication data as defined by the authentication protocol
  being used.
   Note that this message is sent by the party that initiated the current
  negotiation (the party that sent the {defclass connection-setup} or the
  {defclass protocol-setup} messages).
   A {defclass authentication-next-phase} message can then be sent to indicate
  that more is to be done to complete the authentication. If the authentication
  is complete, a {defclass connection-reply} message is appropriate if the
  current authentication handshake is the result of a connection setup, but a
  {defclass protocol-reply} message is appropriate if the authentication is the
  result of a protocol setup."))

(declare-request authentication-next-phase (request)
  ((minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (length-of-authentication-data :type card16 :pad-size 6)
   (data :type data :length length-of-authentication-data))
  (:documentation "This message is sent in response to an authentication-reply
  message, to supply authentication data as defined by the authentication
  protocol being used."))

(declare-request connection-reply (request)
  ((version-index :type card8 :pad-size 1)
   (length :type card32 :initform 0)
   (vendor-name :type string)
   (release-name :type string))
  (:documentation "This message is sent in response to both messages 
  {defclass connection-setup} or {defclass authentication-reply} to indicate
  that the authentication handshake is complete.
  - version-index gives a 0-based index into the array of versions offered in
    the {defclass connection-setup} message; it specifies the version of the
    ICE protocol that both parties should speak for the duration of the
    connection.
  - vendor-name gives the name of the vendor of this ICE implementation.
  - release-name gives the release identifier of this ICE implementation."))

(declare-request protocol-setup (request)
  ((protocol-major-opcode :type card8)
   (must-authenticate-p :type boolean)
   (length :type card32 :initform 0)
   (number-of-versions-offered :type card8)
   (number-of-authentication-protocol-names-offered :type card8 :pad-size 6)
   (protocol-name :type string)
   (vendor-name :type string)
   (release-name :type string)
   (authentication-protocol-names
    :type strings :length number-of-authentication-protocol-names-offered)
   (version-list :type versions :length number-of-versions-offered))
  (:documentation "This message is used to initiate negotiation of a protocol
  and establish any authentication specific to it.
  - protocol-name gives the name of the protocol the party wishes to speak.
  - major-opcode gives the opcode that the party will use in messages it sends.
  - versions gives an array of version (array card8 (2)), in decreasing order
  of preference, that the party is willing to speak.
  - vendor-name and release-name are identification strings with semantics
  defined by the specific protocol being negotiated.
  - authentication-protocol-names specifies an (possibly nil, if
  must-authenticate is False) array of authentication protocols names the party
  is willing to perform. If must-authenticate is True, presumably the party
  will offer only authentication mechanisms allowing mutual authentication.

   If must-authenticate-p is T, the initiating party demands authentication;
  the accepting party must pick an authentication scheme and use it. In this
  case, the only valid response is authentication-required.
   If must-authenticate-p is NIL, the accepting party may choose an
  authentication mechanism, use a host-address-based authentication scheme, or
  skip authentication. When must-authenticate-p is NIL, both response messages 
  {defclass protocol-reply} and {defclass authentication-required} are valid.
  But if an host-address-based authentication scheme is used, then both 
  {defclass authentication-rejected} and {defclass authentication-failed} error
  messages are possible."))

(declare-request protocol-reply (request)
  ((version-index :type card8)
   (protocol-major-opcode :type card8)
   (length :type card32 :initform 0)
   (vendor-name :type string)
   (release-name :type string))
  (:documentation "This message is sent in response to both messages 
  {defclass protocol-setup} and {defclass authentication-reply} to indicate
  that the authentication handshake is complete.
  - major-opcode gives the opcode that this party will use in messages that it
    sends.
  - version-index gives a 0-based index into the array of versions offered in
    the {defclass protocol-setup} message; it specifies the version of the
    protocol that both parties should speak for the duration of the connection.
  - vendor-name and release-name are identification strings with semantics
    defined by the specific protocol being negotiated."))

(declare-request ping (request)
  ((minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation 
   "This message is used to test if the connection is still functioning."))

(declare-request ping-reply (request)
  ((minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "This message is sent in response to a ping message,
  indicating that the connection is still functioning."))

(declare-request want-to-close (request)
  ((minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "This message is used to initiate a possible close of the
  connection. The sending party has noticed that, as a result of mechanisms
  specific to each protocol, there are no active protocols left. There are
  four possible scenarios arising from this request:
   - The receiving side noticed too, and has already sent a want-to-close. 
     On receiving a want-to-close while already attempting to shut down,
     each party should simply close the connection.
   - The receiving side hasn't noticed, but agrees. It closes the connection.
     The receiving side has a {defclass protocol-setup} ``in flight,'' in which
     case it is to ignore want-to-close and the party sending want-to-close is
     to abandon the shutdown attempt when it receives the protocol-setup.
   - The receiving side wants the connection kept open for some reason not
     specified by the ICE protocol, in which case it sends no-close.
  
  See the state transition diagram in the ICE protocol document for additional
  information."))

(declare-request no-close (request)
  ((minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "This message is sent in response to a want-to-close message
  to indicate that the responding party does not want the connection closed at
  this time. The receiving party should not close the connection. Either party
  may again initiate {defclass want-to-close} at some future time."))

;;;; Errors.

(defclass request-error (request)
  ((class :initarg :class :type card16 :accessor request-error-class)
   (offending-minor-opcode
     :initarg :offending-minor-opcode :type card8
     :accessor request-error-offending-minor-opcode)
   (severity 
     :initarg :severity :type error-severity
     :accessor request-error-severity)
   (sequence-number-of-erroneous-message 
     :initarg :sequence-number-of-erroneous-message :type card32
     :accessor request-error-sequence-number-of-erroneous-message))
  (:documentation "Protocol class.
  This message is sent to report an error in response to a message from any
  protocol. The Error message exists in all protocol major-opcode spaces; it
  is minor-opcode zero in every protocol. The minor opcode of the message that
  caused the error is reported, as well as the sequence number of that message.
  The severity indicates the sender's behavior following the identification of
  the error. Severity can be:
   - :can-continue indicates the sender is willing to accept additional
   messages for this protocol.
   - :fatal-to-procotol indicates the sender is unwilling to accept further
   messages for this protocol but that messages for other protocols may be
   accepted.
   - :fatal-to-connection indicates the sender is unwilling to accept any
   further messages for any protocols on the connection.
  The sender is required to conform to specified severity
  conditions for generic and ICE (major opcode 0) errors; see Sections 6.1 and
  6.2. The class defines the generic class of error. Classes are specified
  separately for each protocol (numeric values can mean different things in
  different protocols). The error values, if any, and their types vary with
  the specific error class for the protocol."))

(defgeneric request-error-handler (request-error)
  (:documentation "Default ice error handler: signal an error: <error-CLASS>."))

(defun report-ice-error (condition stream)
  ;; Since the :report arg is evaluated as (function report-request-error) the
  ;; define-condition must come after the function definition.
  (with-slots (class severity offending-minor-opcode major-opcode)
      (ice-error-request-error condition)			 
    (format stream
	    "an ICE error of class ~a with severity ~a,~%~
             with offending minor opcode: ~a occured.~%"
	    (decode-error class) severity
	    (decode-ice-minor-opcode offending-minor-opcode major-opcode))))

(define-condition ice-error (error)
  ((request-error :initarg :request-error :reader ice-error-request-error))
  (:report report-ice-error))

(defun get-error-class (error-buffer)
  "Returns the error class of the given error-buffer without modifying the 
   current position of the buffer index."
  (declare (type buffer error-buffer))
  (let ((buff-pos (buffer-index error-buffer)))
    (declare (type fixnum buff-pos))
    (setf (buffer-index error-buffer) 2)
    (prog1 (buffer-read-card16 error-buffer)
      (setf (buffer-index error-buffer) buff-pos))))

(defmethod decode-request ((key (eql :request-error)) ice-conn buff)
  (declare (type buffer buff))
  (let* ((handler (ice-error-handler ice-conn))
	 (class (get-error-class buff))
	 (err (decode-request (decode-error class) ice-conn buff))
	 (severity (request-error-severity err)))
    (declare (type error-handler handler))
    (declare (type card16 class))
    ;; Close the connection if one this condition is satisfied (cf: ICE 6.1):
    ;;   severity is :fatal-to-connection.
    ;;   severity is :fatal-to-protocol and major-opcode is ICE.
    (case severity
      (:fatal-to-connection (close (connection-stream ice-conn)))
      (:fatal-to-protocol (when (= (read-major-opcode buff) +ice-proto-minor+)
			    (close (connection-stream ice-conn)))))
    (when handler 
      (funcall handler err))))

;;; ICE error classes that are common to all protocols
  
(define-error :bad-minor #x8000)
(define-error :bad-state #x8001)
(define-error :bad-length #x8002)
(define-error :bad-value #x8003)

(declare-error bad-minor () ()
  (:documentation "Received a message with an unknown minor opcode."))

(declare-error bad-state () ()
  (:documentation "Received a message with a valid minor opcode which is not
  appropriate for the current state of the protocol."))

(declare-error bad-length () ()
  (:documentation "Received a message with a bad length. The length of the
  message is longer or shorter than required to contain the data."))

(declare-error bad-value ()
  ((offset-of-offending-value :type card32)
   (length-of-offending-value :type card32)
   (offending-value :type data :length length-of-offending-value))
  (:documentation "Received a message with a bad value specified.")
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Bad value: ~a~%"
		       (slot-value request-error 'offending-value))))))


;;; ICE error classes that are specific to the ICE protocol
  
(define-error :bad-major 0)
(define-error :no-authentication 1)
(define-error :no-version 2)
(define-error :setup-failed 3)
(define-error :authentication-rejected 4)
(define-error :authentication-failed 5)
(define-error :protocol-duplicate 6)
(define-error :major-opcode-duplicate 7)
(define-error :unknown-protocol 8)

(declare-error bad-major ()
  ((values :type card8))
  (:documentation "The opcode given is not one that has been registered.")
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Bad major: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error no-authentication () ()
  (:documentation
   "None of the authentication protocols offered are available."))

(declare-error no-version () ()
  (:documentation "None of the protocol versions offered are available."))

(declare-error setup-failed ()
  ((values :type string))
  (:documentation "The sending side is unable to accept the new connection or
  new protocol for a reason other than authentication failure. Typically this
  error will be a result of inability to allocate additional resources on the
  sending side. The reason field will give a human-interpretable message
  providing further detail on the type of failure.")
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Setup failed: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error authentication-rejected ()
  ((values :type string))
  (:documentation "Authentication rejected. The peer has failed to properly
  authenticate itself. The reason field will give a human-interpretable message
  providing further detail.")
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Authentication rejected: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error authentication-failed ()
  ((values :type string))
  (:documentation "Authentication failed. authentication-failed does not imply
  that the authentication was rejected, as authentication-rejected does.
  Instead it means that the sender was unable to complete the authentication
  for some other reason. (For instance, it may have been unable to contact an
  authentication server.) The reason field will give a human-interpretable
  message providing further detail.")
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Authentication failed: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error protocol-duplicate ()
  ((values :type string))
  (:documentation "The protocol name was already registered. This is fatal to
  the ``new'' protocol being set up by protocol-setup, but it does not affect
  the existing registration.")
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Protocol duplication: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error major-opcode-duplicate ()
  ((values :type card8))
  (:documentation "The major opcode specified was already registered. This is
  fatal to the ``new'' protocol being set up by protocol-setup, but it does not
  affect the existing registration.")
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Major opcode duplication: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error unknown-protocol ()
  ((values :type string))
  (:documentation "The protocol specified is not supported.")
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Unknown protocol: ~a~%"
		       (slot-value request-error 'values))))))
