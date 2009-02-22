;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SM-LIB; -*-
;;; $Id: sm.lisp,v 1.12 2005/12/06 13:52:49 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: SM Library
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

(in-package :SM-LIB)

;;;; Constants & vars

(defvar *xsmp* nil)

;; Protocol version

(defconstant +sm-proto-major+ 1)
(defconstant +sm-proto-minor+ 0)

;; Protocol release and vendor names

(defconstant +release-name+ "CL-SM-1.0")
(defconstant +vendor-name+ "LoopFor & Mapcar corp")

#| XSMP 11.  Predefined Properties

All property values are stored in a LISTofARRAY8.  If the
type of the property is CARD8, the value is stored as a
LISTofARRAY8 with one ARRAY8 that is one byte long.  That
single byte contains the CARD8.  If the type of the property
is ARRAY8, the value is stored in the first element of a
single element LISTofARRAY8.

The required properties must be set each time a client con-
nects with the SM.  The properties must be set after the
client sends RegisterClient and before the client sends
SaveYourselfDone.  Otherwise, the behavior of the session
manager is not defined.

Clients may set, get, and delete nonstandard properties.
The lifetime of stored properties does not extend into sub-
sequent sessions.

----------------------------------------------------------
Name               Type          Posix Type     Required?
----------------------------------------------------------
CloneCommand       OS-specific   LISTofARRAY8     Yes
CurrentDirectory   OS-specific   ARRAY8           No
DiscardCommand     OS-specific   LISTofARRAY8     No*
Environment        OS-specific   LISTofARRAY8     No
ProcessID          OS-specific   ARRAY8           No
Program            OS-specific   ARRAY8           Yes
RestartCommand     OS-specific   LISTofARRAY8     Yes
ResignCommand      OS-specific   LISTofARRAY8     No
RestartStyleHint   CARD8         CARD8            No
ShutdownCommand    OS-specific   LISTofARRAY8     No
UserID             ARRAY8        ARRAY8           Yes
----------------------------------------------------------
|#

(defconstant +clone-command+ "CloneCommand")
(defconstant +current-directory+ "CurrentDirectory")
(defconstant +discard-command+ "DiscardCommand")
(defconstant +environment+ "Environment")
(defconstant +process-id+ "ProcessID")
(defconstant +program+ "Program")
(defconstant +restart-command+ "RestartCommand")
(defconstant +resign-command+ "ResignCommand")
(defconstant +restart-style-hint+ "RestartStyleHint")
(defconstant +shutdown-command+ "ShutdownCommand")
(defconstant +user-id+ "UserID")

#|
---------------------------
Name                 Value
---------------------------
RestartIfRunning       0
RestartAnyway          1
RestartImmediately     2
RestartNever           3
---------------------------
|#

(defconstant +restart-if-running+ 0)
(defconstant +restart-anyway+ 1)
(defconstant +restart-immediately+ 2)
(defconstant +restart-never+ 3)

(defconstant +list-of-array8+ "LISTofARRAY8")
(defconstant +array8+ "ARRAY8")
(defconstant +card8+ "CARD8")

;;;; Types.

(deftype interact-style () `(member :none :errors :any))
(deftype dialog-type () `(member :error :normal))
(deftype save-type () `(member :global :local :both))
(deftype array8 () `(simple-array card8 (*)))
(deftype client-id () `string)

(deftype array8s () 'list)
(deftype strings () 'list)
(deftype properties () 'list)

(defstruct property
  (name nil :type (or null string))
  (type nil :type (or null string))
  (values nil :type array8s))

(defun string->array8 (string)
  "Transforms a string into an sm-lib:array8."
  (declare (type simple-string string))
  (map 'array8 #'char-code string))

(defun array8->string (array)
  "Transforms an sm-lib:array8 into a string."
  (declare (type array8 array))
  (map 'string #'code-char array))

(defun strings->array8s (&rest strings)
  "Transforms the given strings into a list of sm-lib:array8."
  (declare (type list strings))
  (mapcar #'string->array8 strings))

(defun array8s->strings (&rest array8s)
  "Transforms the given sm-lib:array8's into a list of simple-string"
  (declare (type array8s array8s))
  (mapcar #'array8->string array8s))

;; <type>-length functions

(defun interact-style-length (value)
  (declare (ignore value))
  (declare (optimize (speed 3) (safety 0)))
  1)

(defun dialog-type-length (value)
  (declare (ignore value))
  (declare (optimize (speed 3) (safety 0)))
  1)

(defun save-type-length (value)
  (declare (ignore value))
  (declare (optimize (speed 3) (safety 0)))
  1)

(defun string-length (string)
  (declare (type (or null simple-string) string))
  (let ((length (+ 4 (if (null string) 0 (length string)))))
    (+ length (mod (- length) 8))))

(defun array8-length (array)
  (declare (type (or null array8) array))
  (let ((length (+ 4 (if (null array) 0 (length array)))))
    (+ length (mod (- length) 8))))

(defun client-id-length (client-id)
  (declare (type (or null client-id) client-id))
  (string-length client-id))

(defun strings-length (strings)
  (declare (type strings strings))
  (+ 8 (loop for s in strings sum (string-length s))))

(defun array8s-length (arrays)
  (declare (type array8s arrays))
  (+ 8 (loop for a in arrays sum (array8-length a))))

(defun property-length (property)
  (declare (type property property))
  (+ (string-length (property-name property))
     (string-length (property-type property))
     (array8s-length (property-values property))))

(defun properties-length (properties)
  (declare (type properties properties))
  (+ 8 (loop for p in properties sum (property-length p))))

;; type constructor

(defun make-array8 (len &rest args &key (initial-element 0) &allow-other-keys)
  "Creates and returns an array constructed of the most specialized type that
   can accommodate elements of type (unsigned-byte 8). For the rest of the 
   options see common-lisp:make-array."
  (declare (type fixnum len))
  (if (getf args :initial-contents)
      (remf args :initial-element)
      (setf (getf args :initial-element) initial-element))
  (apply #'make-array len :element-type 'card8 args))

;; macro accessor

(defmacro define-sequence-accessor (type element-type)
  (let ((read (sintern (format nil "BUFFER-READ-~a" element-type)))
	(write (sintern (format nil "BUFFER-WRITE-~a" element-type))))
    `(define-accessor ,type
       ((buffer)
	(with-gensym (count buff)
	  `(multiple-value-bind (,buff ,count)
	       (values ,buffer (buffer-read-card32 ,buffer))
	     (declare (type buffer ,buff))
	     (index+ ,buff 4)
	     (loop for i from 0 below ,count collect (,',read ,buff)))))
       ((sequence buffer)
	(with-gensym (seq buff)	
	  `(let ((,seq ,sequence) (,buff ,buffer))
	     (declare (type buffer ,buff))
	     (buffer-write-card32 (length ,seq) ,buff)
	     (index+ ,buff 4)
	     (loop for e in ,seq do (,',write e ,buff))))))))

;; <type>-{writer,reader} macros

(define-member8-accessor interact-style '#(:none :errors :any))

(define-member8-accessor dialog-type '#(:error :normal))

(define-member8-accessor save-type '#(:global :local :both))

(define-accessor array8
  ((buffer)
   (with-gensym (length buff)
     `(let* ((,buff ,buffer)
	     (,length (buffer-read-card32 ,buff)))
        (declare (type buffer ,buff))
	(prog1 (buffer-read-data ,buff ,length)
	  (index+ ,buff (mod (- (+ 4 ,length)) 8))))))
  ((array buffer)
   (with-gensym (length buff)
     `(let ((,length (if (null ,array) 0 (length ,array)))
	    (,buff ,buffer))
        (declare (type buffer ,buff))
        (buffer-write-card32 ,length ,buff)
        (buffer-write-data ,array ,buff)
        (index+ ,buff (mod (- (+ 4 ,length)) 8))))))

(define-accessor string
  ((buffer)
   `(map 'string #'code-char (buffer-read-array8 ,buffer)))
  ((string buffer)
   `(buffer-write-array8 (map 'array8 #'char-code ,string) ,buffer)))

(define-sequence-accessor strings string)

(define-sequence-accessor array8s array8)

(define-accessor client-id
  ((buffer) `(buffer-read-string ,buffer))
  ((id buffer) `(buffer-write-string ,id ,buffer)))

(define-accessor property 
  ((buffer)
   (with-gensym (buff)
     `(let ((,buff ,buffer))
        (declare (type buffer ,buff))
        (make-property
	    :name (buffer-read-string ,buff)
	    :type (buffer-read-string ,buff)
	    :values (buffer-read-array8s ,buff)))))
  ((property buffer)
   (with-gensym (prop buff)
     `(multiple-value-bind (,prop ,buff) (values ,property ,buffer)
        (declare (type buffer ,buff))
        (buffer-write-string (property-name ,prop) ,buff)
        (buffer-write-string (property-type ,prop) ,buff)
        (buffer-write-array8s (property-values ,prop) ,buff)))))

(define-sequence-accessor properties property)

;;;; Request declarations.

(define-request :register-client 1)
(define-request :register-client-reply 2)
(define-request :save-yourself 3)
(define-request :save-yourself-request 4)
(define-request :interact-request 5)
(define-request :interact 6)
(define-request :interact-done 7)
(define-request :save-yourself-done 8)
(define-request :die 9)
(define-request :shutdown-cancelled 10)
(define-request :connection-closed 11)
(define-request :set-properties 12)
(define-request :delete-properties 13)
(define-request :get-properties 14)
(define-request :get-properties-reply 15)
(define-request :save-yourself-phase2-request 16)
(define-request :save-yourself-phase2 17)
(define-request :save-complete 18)

(declare-request register-client (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (previous-id :type client-id))
  (:documentation "The client must send this message to the SM to register the
  client's existence. If a client is being restarted from a previous session,
  the previous-ID field must contain the client ID string from the previous
  session. For new clients, previous-ID should be NIL or the empty string. If
  previous-ID is not valid, the SM will send a BadValue error message to the
  client. At this point the SM reverts to the register state and waits for
  another register-client. The client should then send a register-client with
  a NIL previous-ID field."))

(declare-request register-client-reply (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (client-id :type client-id))
  (:documentation "The client-id specifies a unique identification string for
  this client. If the client had specified an id in the previous-ID field of
  the register-client message, client-ID will be identical to the previously
  specified id. 

   If previous-id was NIL, client-id will be a unique id freshly generated by
  the SM. The client-id format is specified in section 6 of xsmp specification.

   If the client didn't supply a previous-id field to the register-client
  message, the SM must send a save-yourself message with 
  type = :local, shutdown-p = NIL, interact-style = :none, and fast-p = NIL
  immediately after the register-client-reply. The client should respond to
  this like any other save-yourself message."))

(declare-request save-yourself (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (type :type save-type)
   (shutdown-p :type boolean)
   (interact-style :type interact-style)
   (fast-p :type boolean :pad-size 4))
  (:documentation "The SM sends this message to a client to ask it to save its
  state. The client writes a state file, if necessary, and, if necessary, uses
  set-properties to inform the SM of how to restart it and how to discard the
  saved state. During this process it can, if allowed by interact-style,
  request permission to interact with the user by sending an interact-request
  message. After the state has been saved, or if it cannot be successfully
  saved, and the properties are appropriately set, the client sends a
  save-yourself-done message. 

   If the client wants to save additional information after all the other
  clients have finished changing their own state, the client should send
  save-yourself-phase2-request instead of save-yourself-done. The client
  must then freeze interaction with the user and wait until it receives a
  save-complete, die, or a shutdown-cancelled message.

   If interact-style is :none, the client must not interact with the user while
  saving state. If the interact-style is :errors, the client may interact with
  the user only if an error condition arises. 

   If interact-style is :any, then the client may interact with the user for
  any purpose. This is done by sending an interact-request message. The SM will
  send an Interact message to each client that sent an interact-request. 

   When a client receives save-yourself and has not yet responded
  save-yourself-done to a previous save-yourself, it must send a
  save-yourself-done and may then begin responding as appropriate
  to the newly received save-yourself.

    The type slot specifies the type of information that should be saved: 
   - :local  indicates that the application must update the properties to
             reflect its current state, send a save-yourself-done and continue.
             Specifically it should save enough information to restore the
             state as seen by the user of this client. It should not affect
             the state as seen by other users.
   - :global indicates that the user wants the client to commit all of its
             data to permanent, globally-accessible storage.
   - :both   indicates that the client should do both of these. In this case,
             the client should first commit the data to permanent storage
             before updating its SM properties.

   The shutdown-p slot specifies whether the system is being shut down.

   The fast slot specifies whether or not the client should save its state as
  quickly as possible. For example, if the SM knows that power is about to
  fail, it should set the fast field to True."))

(declare-request save-yourself-request (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (type :type save-type)
   (shutdown-p :type boolean)
   (interact-style :type interact-style)
   (fast-p :type boolean)
   (global-p :type boolean :pad-size 3))
  (:documentation "An application sends this to the SM to request a checkpoint.
  When the SM receives this request it may generate a save-yourself message in
  response and it may leave the slots intact.

   If global-p is set True, then the resulting save-yourself should be sent to
  all applications. 

   If global-p is NIL, then the resulting save-yourself should be sent to the
  application that sent the save-yourself-request."))

(declare-request interact-request (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8)
   (dialog-type :type dialog-type :pad-size 1)
   (length :type card32 :initform 0))
  (:documentation "During a checkpoint or session-save operation, only one
  client at a time might be granted the privilege of interacting with the user.
  The interact-request message causes the SM to emit an Interact message at
  some later time if the shutdown is not cancelled by another client first.

   The dialog-type slot is one of :error :normal, indicating that the client
  wants to start an error dialog or normal, meaning the client wishes to start
  a non-error dialog."))

(declare-request interact (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "This message grants the client the privilege of interacting
  with the user. When the client is done interacting with the user it must send
  an interact-done message to the SM unless a shutdown cancel is received."))

(declare-request interact-done (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8)
   (cancel-shutdown-p :type boolean :pad-size 1)
   (length :type card32 :initform 0))
  (:documentation "This message is used by a client to notify the SM that it is
  done interacting. Setting the cancel-shutdown-p to True indicates that the
  user has requested that the entire shutdown be cancelled. cancel-shutdown-p
  may only be True if the corresponding save-yourself message specified True in
  the shutdown-p slot and :any or :errors in the interact-style slot.
  Otherwise, cancel-shutdown-p must be NIL."))

(declare-request save-yourself-done (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8)
   (success-p :type boolean :pad-size 1)
   (length :type card32 :initform 0))
  (:documentation "This message is sent by a client to indicate that all of the
  properties representing its state have been updated. After sending
  save-yourself-done the client must wait for a save-complete,
  shutdown-cancelled, or die message before changing its state. If the
  save-yourself operation was successful, then the client should set the
  success-p slot to T; otherwise the client should set it to NIL."))

(declare-request die (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "When the SM wants a client to die it sends a Die message.
  Before the client dies it responds by sending a connection-closed message
  and may then close its connection to the SM at any time."))

(declare-request shutdown-cancelled (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "The shutdown currently in process has been aborted. The
  client can now continue as if the shutdown had never happened. If the client
  has not sent save-yourself-done yet, the client can either abort the save
  and send save-yourself-done with the success-p slot set to Nil, or it can
  continue with the save and send a SaveYourselfDone with the success-p slot
  set to reflect the outcome of the save."))

(declare-request connection-closed (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (reason :type strings))
  (:documentation "Specifies that the client has decided to terminate. It
  should be immediately followed by closing the connection. 

   The reason slot specifies why the client is resigning from the session. It
  is encoded as alist of simple strings. If the resignation is expected by the
  user, there will typically be a NIL or a null string here. But if the client
  encountered an unexpected fatal error, the error message (which might
  otherwise be printed on *standard-error*) should be forwarded to the SM here,
  one string per line of the message. It is the responsibility of the SM to
  display this reason to the user.

   After sending this message, the client must not send any additional XSMP
  messages to the SM."))

(declare-request set-properties (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (properties :type properties))
  (:documentation "Sets the specified properties to the specified values.
  Existing properties not specified in the SetProperties message are
  unaffected. Some properties have predefined semantics. See section 11,
  ``Predefined Properties.'' The protocol specification recommends that
  property names used for properties not defined by the standard should begin
  with an underscore. To prevent conflicts among organizations, additional
  prefixes should be chosen. The organizational prefixes should be registered
  with the X Registry. The XSMP reserves all property names not beginning with
  an underscore for future use."))

(declare-request delete-properties (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (properties :type strings))
  (:documentation "Removes the named properties."))

(declare-request get-properties (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "Requests that the SM respond with the values of all the
  properties for this client."))

(declare-request get-properties-reply (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0)
   (properties :type properties))
  (:documentation "This message is sent in reply to a get-properties message
  and includes the values of all the properties."))

(declare-request save-yourself-phase2-request (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "This message is sent by a client to indicate that it needs
  to be informed when all the other clients are quiescent, so it can continue
  its state."))

(declare-request save-yourself-phase2 (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "The SM sends this message to a client that has previously
  sent a save-yourself-phase2-request message. This message informs the client
  that all other clients are in a fixed state and this client can save state
  that is associated with other clients.

   The client writes a state file, if necessary, and, if necessary, uses
  set-properties to inform the SM of how to restart it and how to discard the
  saved state. During this process it can request permission to interact with
  the user by sending an interact-request message. This should only be done if
  an error occurs that requires user interaction to resolve. After the state
  has been saved, or if it cannot be successfully saved, and the properties
  are appropriately set, the client sends a save-yourself-done message."))

(declare-request save-complete (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (length :type card32 :initform 0))
  (:documentation "When the SM is done with a checkpoint, it will send each
  of the clients a save-complete message. The client is then free to change
  its state."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;;;
;;;;                               SM Library                              ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sm-connection (ice-connection)
  ((client-id 
     :initarg :client-id :type client-id
     :accessor sm-client-id)
   (sm-release
     :initarg :sm-release :type string
     :accessor sm-release)
   (sm-vendor
     :initarg :sm-vendor :type string
     :accessor sm-vendor)
   (sm-protocol-version
     :initarg :sm-protocol-version :type card16
     :accessor sm-protocol-version)
   (sm-protocol-revision
     :initarg :sm-protocol-revision :type card16
     :accessor sm-protocol-revision)))

(defun register-xsmp-protocol (opcode)
  (register-protocol opcode 
    '#(:request-error :register-client :register-client-reply :save-yourself
       :save-yourself-request :interact-request :interact :interact-done
       :save-yourself-done :die :shutdown-cancelled :connection-closed
       :set-properties :delete-properties :get-properties :get-properties-reply
       :save-yourself-phase2-request :save-yourself-phase2 :save-complete)))

(define-condition session-manager-unavailable (error)
  ((reason 
     :initarg :reason :type string 
     :reader session-manager-unavailable-reason))
  (:report (lambda (condition stream)
	     (format stream 
		     "SM-lib: Unable to connect to session manager: ~a~%"
		     (session-manager-unavailable-reason condition)))))

(defmacro signal-sm-error (string &rest args)
  `(error 'session-manager-unavailable :reason (format nil ,string ,@args)))

(defun open-sm-connection (&key must-authenticate-p previous-id network-ids)
  "Returns an sm-connection object if it succeeds. Otherwise an error will be
   signaled. (its type will depend on the reason of the failure)
  
    :network-ids : if given, must be a list of network-id for the session
   manager. If not given, the value of the SESSION_MANAGER environment variable
   will be used. An attempt will be made to use the first network-id. If this
   fails an attempt will be made to use the second one, and so on. Each
   network-id has the following format:
    - (local | unix)/<HOST-NAME>:<PATH>
    - tcp/<HOST-NAME>:<PORT-NUMBER>
    - decnet/<HOST-NAME>::<OBJ>

    :previous-id : if the client is restarted from a previous session, should
   contain the previous client-id of that previous session. If :previous-id is
   specified, but is determined to be invalid by the session manager, we will 
   re-register the client with a previous-id set to NIL. If the client is first
   joining the session :previous-id can be NIL (default) or the empty string.

   Any authentication requirements are handled internally by the SM Library.
   The method by which authentication data is obtained is implementation
   dependent. We only use and know the default use of the ICEauthority file.
   You will need to register your own methods for other authentication methods.
   To do so see and use register-ice-authentication-protocol."
  (declare (type (or null list) network-ids))
  (declare (type (or null client-id) previous-id))
  (declare (type boolean must-authenticate-p))
  (unless network-ids
    (setf network-ids (list (get-environment-variable)))
    (when (null (car network-ids))
      (signal-sm-error "SESSION_MANAGER environment variable is undefined.")))
  (let ((sm-conn (make-instance 'sm-connection)))
    (open-connection 
     network-ids :connection sm-conn :must-authenticate-p must-authenticate-p)
    ;; Send protocol-setup request and wait for protocol-reply,then
    ;; send register-client and wait for register-client-reply.
    ;; Authentication will take place behind the scene.
    (let ((protocols 
	   (available-authentication-protocols
	    "XSMP" (ice-connection-string sm-conn) (ice-auth-proto-names)))
	  (versions (make-default-versions 
		        :major +sm-proto-major+ :minor +sm-proto-minor+)))
      (declare (type (simple-array string (*)) protocols))
      (declare (type versions versions))
      (post-request :protocol-setup sm-conn
	:protocol-name "XSMP"
	:protocol-major-opcode +sm-proto-major+
	:number-of-versions-offered (length versions)
	:must-authenticate-p must-authenticate-p
	:vendor-name +vendor-name+
	:release-name +release-name+
	:authentication-protocol-names protocols
	:version-list versions
	:number-of-authentication-protocol-names-offered (length protocols))
      (with-error-handler (sm-conn #'(lambda (x) x))
	(request-case (sm-conn :timeout nil :place request :ice-flush-p nil)
	  (authentication-required ((index authentication-protocol-index))
	    (let ((handler (get-protocol-handler (aref protocols index))))
	      (declare (type function handler))
	      (funcall handler sm-conn request))
	    (values))
	  (protocol-reply (protocol-major-opcode vendor-name release-name)
	    ;; internally register the protocol.
	    (setf *xsmp* protocol-major-opcode)
	    (register-xsmp-protocol protocol-major-opcode)
	    ;; send the register-client request.
	    (post-request :register-client sm-conn
	      :previous-id (or previous-id ""))
	    ;; collect some connection infos.
	    (with-slots (version-index) request
	      (let ((version (aref versions version-index)))
		(declare (type version version))
		(setf (sm-protocol-version sm-conn) (aref version 0))
		(setf (sm-protocol-revision sm-conn) (aref version 1))))
	    (setf (sm-release sm-conn) release-name)
	    (setf (sm-vendor sm-conn) vendor-name)
	    (values))
	  (register-client-reply (client-id)
	    (setf (sm-client-id sm-conn) client-id))
	  (request-error ((omo offending-minor-opcode) (mo major-opcode))
	    (let ((offender (decode-ice-minor-opcode omo mo)))
	      (if (and (bad-value-p request) (eq offender :register-client))
		  ;; Could not register the client because the previous ID
		  ;; was bad. So now we register the client with the
		  ;; previous ID set to empy string.
		  (post-request :register-client sm-conn :previous-id "")
		  ;; signal an error.
		  (request-error-handler request)))
	    (values))
	  ;; bad state signal an error.
	  (t (signal-sm-error "bad state during protocol setup: ~a." request))))
      sm-conn)))

(defun close-sm-connection (sm-conn &key reason)
  "Closes a connection with a session manager."
  (declare (type sm-connection sm-conn))
  (declare (type (or null string) reason))
  (ice-lib:post-request :want-to-close sm-conn)
  (ice-lib:post-request :connection-closed sm-conn :reason reason)
  (ice-flush sm-conn)
  (setf (sm-release sm-conn) nil)
  (setf (sm-vendor sm-conn) nil)
  (setf (ice-release sm-conn) nil)
  (setf (ice-vendor sm-conn) nil)
  (setf (ice-connection-string sm-conn) nil)
  (close (ice-lib:connection-stream sm-conn)))
