;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SM-LIB; -*-
;;; $Id: sm.lisp,v 1.3 2004/03/02 19:14:26 ihatchondo Exp $
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

;;;; Types.

(deftype interact-style () `(member :none :errors :any))
(deftype dialog-type () `(member :error :normal))
(deftype save-type () `(member :global :local :both))
(deftype array8 () `(simple-array card8 (*)))
(deftype client-id () `string)

(deftype array8s () 'list)
(deftype properties () 'list)

(defstruct property
  (name nil :type (or null string))
  (type nil :type (or null string))
  (values nil :type array8s))

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
       ((byte-order buffer index)
	(with-gensym (count buff)
	  `(let* ((,buff ,buffer)
		  (,count (buffer-read-card32 ,byte-order ,buffer ,index)))
	     (declare (type buffer ,buff))
	     (incf ,index 4)
	     (loop for i from 0 below ,count
	           collect (,',read ,byte-order ,buff ,index)))))
       ((sequence byte-order buffer index)
	(with-gensym (seq buff)	
	  `(let ((,seq ,sequence) (,buff ,buffer))
	     (declare (type buffer ,buff))
	     (buffer-write-card32 (length ,seq) ,byte-order ,buff ,index)
	     (incf ,index 4)
	     (loop for e in ,seq
	           do (,',write e ,byte-order ,buff ,index))))))))

;; <type>-{writer,reader} macros

(define-member8-accessor interact-style '#(:none :errors :any))

(define-member8-accessor dialog-type '#(:error :normal))

(define-member8-accessor save-type '#(:global :local :both))

(define-accessor string
  ((byte-order buffer index)
   `(map 'string #'code-char (buffer-read-array8 ,byte-order ,buffer ,index)))
  ((string byte-order buffer index)
   `(buffer-write-array8
     (map 'vector #'char-code ,string) ,byte-order ,buffer ,index)))

(define-sequence-accessor strings string)

(define-accessor array8
  ((byte-order buffer index)
   (with-gensym (length array buff)
     `(let* ((,buff ,buffer)
	     (,length (buffer-read-card32 ,byte-order ,buff ,index))
	     (,array (buffer-read-data ,byte-order ,buff ,index ,length)))
        (declare (type buffer ,buff))
	(incf ,index (mod (- (+ 4 ,length)) 8))
	,array)))
  ((array byte-order buffer index)
   (with-gensym (length buff)
     `(let ((,length (if (null ,array) 0 (length ,array)))
	    (,buff ,buffer))
        (declare (type buffer ,buff))
        (buffer-write-card32 ,length ,byte-order ,buff ,index)
        (buffer-write-data ,array ,byte-order ,buff ,index)
        (incf ,index (mod (- (+ 4 ,length)) 8))))))

(define-sequence-accessor array8s array8)

(define-accessor client-id
  ((byte-order buffer index)
   `(buffer-read-string ,byte-order ,buffer ,index))
  ((id byte-order buffer index)
   `(buffer-write-string ,id ,byte-order ,buffer ,index)))

(define-accessor property 
  ((byte-order buffer index)
   (with-gensym (buff)
     `(let ((,buff ,buffer))
        (declare (type buffer ,buff))
        (make-property
	 :name (buffer-read-string ,byte-order ,buff ,index)
	 :type (buffer-read-string ,byte-order ,buff ,index)
	 :values (buffer-read-array8s ,byte-order ,buff ,index)))))
  ((property byte-order buffer index)
   (with-gensym (prop buff)
     `(multiple-value-bind (,prop ,buff) (values ,property ,buffer)
        (declare (type buffer ,buff))
        (buffer-write-string (property-name ,prop) ,byte-order ,buff ,index)
        (buffer-write-string (property-type ,prop) ,byte-order ,buff ,index)
        (buffer-write-array8s
	 (property-values ,prop) ,byte-order ,buff ,index)))))

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
   (previous-id :type client-id)))

(declare-request register-client-reply (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (client-id :type client-id)))

(declare-request save-yourself (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (type :type save-type)
   (shutdown-p :type boolean)
   (interact-style :type interact-style)
   (fast-p :type boolean :pad-size 4)))

(declare-request save-yourself-request (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (type :type save-type)
   (shutdown-p :type boolean)
   (interact-style :type interact-style)
   (fast-p :type boolean)
   (global-p :type boolean :pad-size 3)))

(declare-request interact-request (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8)
   (dialog-type :type dialog-type :pad-size 5)))

(declare-request interact (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)))

(declare-request interact-done (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8)
   (cancel-shutdown-p :type boolean :pad-size 1)))

(declare-request save-yourself-done (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8)
   (success-p :type boolean :pad-size 1)))

(declare-request die (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)))

(declare-request shutdown-cancelled (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)))

(declare-request connection-closed (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (reason :type array8s)))

(declare-request set-properties (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (properties :type properties)))

(declare-request delete-properties (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (properties :type array8s)))

(declare-request get-properties (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)))

(declare-request get-properties-reply (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)
   (properties :type properties)))

(declare-request save-yourself-phase2-request (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)))

(declare-request save-yourself-phase2 (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)))

(declare-request save-complete (request)
  ((major-opcode :initform *xsmp* :type card8)
   (minor-opcode :type card8 :pad-size 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;;;
;;;;                               SM Library                              ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sm-connection (ice-connection)
  ((client-id 
     :initform nil :initarg :client-id :type client-id
     :accessor sm-client-id)
   (sm-release
     :initform nil :initarg :sm-release :type string 
     :accessor sm-release)
   (sm-vendor
     :initform nil :initarg :sm-vendor :type string
     :accessor sm-vendor)
   (sm-protocol-version
     :initform nil :initarg :sm-protocol-version
     :accessor sm-protocol-version)
   (sm-protocol-revision
     :initform nil :initarg :sm-protocol-revision
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
  
  - :network-ids : if given, must be a list of network-id for the session
  manager. If not given, the value of the SESSION_MANAGER environment variable
  will be used. An attempt will be made to use the first network-id. If this
  fails an attempt will be made to use the second one, and so on. Each
  network-id has the following format:
    local/<HOST-NAME>:<PATH>
    tcp/<HOST-NAME>:<PORT-NUMBER>
    decnet/<HOST-NAME>::<OBJ>

  - :previous-id : if the client is restarted from a previous session, should
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
    (let ((error-handler (ice-error-handler sm-conn))
	  (protocols 
	   (available-authentication-protocols
	    "XSMP" (ice-connection-string sm-conn) (ice-auth-proto-names)))
	  (versions (make-default-versions 
		        :major +sm-proto-major+ :minor +sm-proto-minor+)))
      (post-request :protocol-setup sm-conn
		    :protocol-name "XSMP"
		    :protocol-major-opcode +sm-proto-major+
		    :number-of-versions-offered (length versions)
		    :must-authenticate-p must-authenticate-p
		    :vendor-name +vendor-name+
		    :release-name +release-name+
		    :authentication-protocol-names protocols
		    :version-list versions
		    :number-of-authentication-protocol-names-offered
		    (length protocols))
      (setf (ice-error-handler sm-conn) (lambda (x) x))
      (request-case (sm-conn :timeout nil :place request :ice-flush-p nil)
	(authentication-required ((index authentication-protocol-index))
	  (let ((name (aref protocols index)))
	    (funcall (get-protocol-handler name) sm-conn request))
	  (values))
	(protocol-reply (protocol-major-opcode vendor-name release-name)
	  ;; internally register the protocol.
	  (setf *xsmp* protocol-major-opcode)
	  (register-xsmp-protocol protocol-major-opcode)
	  ;; send the register-client request.
	  (post-request :register-client sm-conn :previous-id previous-id)
	  ;; collect some connection infos.
	  (with-slots (version-index) request
	    (let ((version (aref versions version-index)))
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
		(signal-request-error request)))
	  (values))
	;; bad state signal an error.
	(t (signal-sm-error "bad state during protocol setup: ~a." request)))
      ;; Reset the error handler and Returns the sm-connection instance.
      (setf (ice-error-handler sm-conn) error-handler) 
      sm-conn)))

(defun close-sm-connection (sm-conn &key reason)
  "Close a connection with a session manager."
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
