;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: ICE-request.lisp,v 1.2 2004/03/02 19:14:26 ihatchondo Exp $
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

;;;; helpful stuff for the declare-request macro code generation.

(defmacro with-slot-key-args ((&rest key-args) slot-decl &body body)
  `(destructuring-bind (&key ,@key-args &allow-other-keys) (cdr ,slot-decl)
     ,@body))

(defmacro with-reply-buffer ((buffer input-byte-order) &rest slots)
  (with-gensym (index buff ibo length)
    `(let* ((,buff ,buffer) (,ibo ,input-byte-order)
	    (,index 4)
	    (,length (buffer-read-card32 ,ibo ,buff ,index)))
       (declare (type buffer ,buff))
       (declare (ignorable ,length))
       (loop for i from 0 to 3 do (setf (aref ,buff (+ i 4)) (aref ,buff i)))
       (setf ,index 4)
       ,@(unless (member 'MAJOR-OPCODE slots :key #'car)
	   `((setf major-opcode (buffer-read-card8 ,ibo ,buff ,index))))
       ,@(loop for slot-description in slots
	       for name = (car slot-description) collect	       
	       (with-slot-key-args ((type 'card8) (pad-size 0) length)
		   slot-description
		 (flet ((type-reader (type)
			  (format nil "BUFFER-READ-~a" (symbol-name type))))
		   `(setf ,(intern (symbol-name name))
		          (,(sintern (type-reader type)) ,ibo ,buff ,index
		          ,@(when length `(,(intern (symbol-name length)))))
		          ,@(unless (zerop pad-size)
			      `(,index (+ ,index ,pad-size))))))))))

(defmacro with-request-buffer ((buffer output-byte-order length) &rest slots)
  (with-gensym (index buff obo)
    `(let ((,buff ,buffer) (,obo ,output-byte-order)
	   (,index 4))
       (declare (type buffer ,buff))
       ,@(unless (member 'MAJOR-OPCODE slots :key #'car)
	   `((buffer-write-card8 major-opcode ,obo ,buff ,index)))
       ,@(loop for slot-desc in slots 
	       for name = (intern (symbol-name (car slot-desc))) collect
	       (with-slot-key-args ((type 'card8) (pad-size 0)) slot-desc
		 (flet ((type-writer (type)
			  (format nil "BUFFER-WRITE-~a" (symbol-name type))))
		   `(prog1
		        (,(sintern (type-writer type)) ,name ,obo ,buff ,index)
		      ,@(unless (zerop pad-size) `((incf ,index ,pad-size)))))))
       (loop for i from 0 to 3 do (setf (aref ,buff i) (aref ,buff (+ i 4))))
       (setf ,index 4)
       (buffer-write-card32 ,length ,obo ,buff ,index))))

(defun get-type (slot-declaration)
  "returns the type (or card8) given in a slot declaration."
  (with-slot-key-args ((type 'card8)) slot-declaration
    type))

(defun get-pad-size (slot-declaration)
  "returns the pad-size (or 0) given in a slot declaration."
  (with-slot-key-args ((pad-size 0)) slot-declaration 
    pad-size))

(defun make-request-length (slots)
  (let ((minor-slot (car (member 'MINOR-OPCODE slots :key #'car))))
    (setf slots (delete 'MAJOR-OPCODE slots :key #'car))
    (setf slots (delete 'MINOR-OPCODE slots :key #'car))
    `(+ 2 ; major-opcode + minor-opcode = 1 Byte + 1 Byte
       ,@(when minor-slot `(,(get-pad-size minor-slot)))
       ,@(loop for slot in slots 
	       for fun = (sintern (format nil "~a-LENGTH" (get-type slot)))
	       for pad = (get-pad-size slot)
	       collect `(+ (,fun ,(intern (symbol-name (car slot)))) ,pad)))))

(defun make-constructor-key-args (slots &key add-major-opcode)
  (when (and add-major-opcode (not (member 'MAJOR-OPCODE slots :key #'car)))
    (setf slots (cons (list 'MAJOR-OPCODE :initform +ice-proto-minor+) slots)))
  (loop for slot in slots
	for initform = (with-slot-key-args (initform) slot initform)
	if initform collect `(,(car slot) ,initform) else collect (car slot)))

;; Public.
;; Macro wraper around defclass to factorize some declarations, and 
;; defines the: make-ice-request method, the <class-name>-p predicate function
;; the request-length method and the post-request, decode-request method for
;; writing or reading a request over the wire.
(defmacro declare-request (name (&rest super-classes) (&rest slots) &rest doc)
  "Declares a request. Generate a class named `name' and specialized methods 
  for: make-ice-request, decode-request, post-request, request-length plus 
  a predicate function named<name>-p.
  
  The syntax is the same than in defclass. The only difference is the four
  more keyword arguments that exist for slot description:

  :prefix     symbol   (default: class-name)
    If given, then it will be use with the slots name to construct the 
    accessor method name if :accessor option was not given.

  :pad-size   fixnum   (default: 0)
    You may (or have to) indicate the number of padding bytes between two
    slots. This information will not be used in the class form, but will
    be in the encoder and decoder methods.

  :inherited  boolean  (default: NIL)
    You also need to indicate the inherited slots if any in your declaration
    (except for the two inherited from the request top level class). This
    information is needed for the {en/de}coder methods.

  :length     slot-name
    For sequence type, the decoder method will need this information to be 
    able to read the correct quantity of items of the given sequence type.
    For example: if you have a slot typed versions = (simple-array version (*))
    then the decoder will need to know how much version to read."
  (let ((slot-names (list))
	(class-key-name (kintern (symbol-name name))))
    (pushnew (list 'MINOR-OPCODE) slots :key #'car)
    (setf (getf (cdr (car (member 'MINOR-OPCODE slots :key #'car)))
		:initform)
	  (encode-ice-minor-opcode class-key-name))
    `(progn
      ;; Generate defclass form.
      (defclass ,name ,super-classes
	,(loop for slot in slots 
	       nconc
	        (destructuring-bind
		      (slot-name 
		       &key initform allocation type documentation inherited
		            (prefix name)
			    (initarg (kintern (symbol-name slot-name)))
			    (accessor 
			     (sintern (format nil "~A-~A" prefix slot-name)))
		       &allow-other-keys)
		    slot
		  (unless (eq slot-name 'MINOR-OPCODE)
		    (push (intern (symbol-name slot-name)) slot-names))
		  (unless inherited
		    `((,slot-name
		       :initarg ,initarg
		       :accessor ,accessor
		       ,@(when allocation `(:allocation ,allocation))
		       ,@(when initform `(:initform ,initform))
		       ,@(when type `(:type ,type))
		       ,@(when documentation `(:documentation ,documentation))
		       )))))
	,@doc)
      ;; Generate make-ice-request method with eql-specializer on class-name.
      (defmethod make-ice-request
	  ((name (eql ,class-key-name))
	   &key ,@(make-constructor-key-args slots) &allow-other-keys)
	(make-instance ',name
	  ,@(loop for arg in (make-constructor-key-args slots)
		  for foo = (if (consp arg) (car arg) arg)
		  collect (kintern (symbol-name foo)) collect foo)))
      ;; Generate <class-name>-p predicate function.
      (defun ,(sintern (format nil "~a-P" name)) (object)
	,(format nil "Return true if object is of type ~a; nil otherwise" name)
	(typep object ',name))
      ;; Ensure we have {major,minor}-opcode in the slot-names.
      ,@(progn
	  (pushnew (intern (symbol-name 'MINOR-OPCODE)) slot-names)
	  (pushnew (intern (symbol-name 'MAJOR-OPCODE)) slot-names)
	  (values))
      ;; Generate request-length method.
      (defmethod request-length ((request ,(intern (symbol-name name))))
	(with-slots (,@slot-names) request
	  (let ((length (+ 4 ,(make-request-length slots))))
	    (declare (type fixnum length))
	    (+ length (mod (- length) 8)))))
      ;; Generate post-request.
      (defmethod post-request
	  ((name (eql ,class-key-name)) ice-connection
	   &key ,@(make-constructor-key-args slots :add-major-opcode t)
	   &allow-other-keys)
	(with-slots (output-byte-order stream) ice-connection
	  (flet ((rlength ()
		   (let ((length (+ 4 ,(make-request-length slots))))
		     (declare (type fixnum length))
		     (+ length (mod (- length) 8)))))
	    (let* ((request-length (rlength))
		   (buffer (make-buffer request-length))
		   (length (1- (/ request-length 8))))
	      (declare (type buffer buffer))
	      (with-request-buffer (buffer output-byte-order length)
		,@slots)
	      (setf (connection-output-buffer ice-connection) buffer)
	      (write-sequence buffer stream)
	      (finish-output stream)))))
      ;; Generate request decoder.
      (defmethod decode-request
	  ((name (eql ,class-key-name)) ice-connection buffer input-byte-order)
	(declare (ignorable ice-connection))
	(declare (type buffer buffer))
	(let ((request (make-instance ',name)))
	  (with-slots (,@slot-names) request
	    (with-reply-buffer (buffer input-byte-order)
	      ,@slots))
	  request)))))

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
  
(defgeneric make-ice-request (key-name &rest request-slots)
  (:documentation "returns a newly allocated instance of class designed 
   by the request-key-name."))

(defgeneric decode-request (request-key ice-connection buffer byte-order)
  (:documentation "returns a instance of request that represent the one
   in the input buffer."))

(defgeneric post-request (request-key ice-connection &rest request-slots)
  (:documentation "post a request specified by the request-key and
  request-slots to the given destination ice-connection. The request-slots
  passed depend on the request type. The keyword symbols used for each request
  type are request slot names defined by the declare-request macro."))

(defgeneric request-length (request)
  (:documentation "compute and returns the size (in bytes) of a request."))

(defun request-p (object)
  "returns true if object is of type request; otherwise, returns nil."
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
  ((byte-order :type ice-byte-order :pad-size 1)))

(declare-request connection-setup (request)
  ((number-of-versions-offered :type card8)
   (number-of-authentication-protocol-names-offered :type card8)
   (must-authenticate-p :type boolean :pad-size 7)
   (vendor-name :type string)
   (release-name :type string)
   (authentication-protocol-names
    :type strings :length number-of-authentication-protocol-names-offered)
   (version-list :type versions :length number-of-versions-offered)))

(declare-request authentication-required (request)
  ((authentication-protocol-index :type card8 :pad-size 1)
   (length-of-authentication-data :type card16 :pad-size 6)
   (data :type data :length length-of-authentication-data)))

(declare-request authentication-reply (request)
  ((minor-opcode :type card8 :pad-size 2)
   (length-of-authentication-data :type card16 :pad-size 6)
   (data :type data :length length-of-authentication-data)))

(declare-request authentication-next-phase (request)
  ((minor-opcode :type card8 :pad-size 2)
   (length-of-authentication-data :type card16 :pad-size 6)
   (data :type data :length length-of-authentication-data)))

(declare-request connection-reply (request)
  ((version-index :type card8 :pad-size 1)
   (vendor-name :type string)
   (release-name :type string)))

(declare-request protocol-setup (request)
  ((protocol-major-opcode :type card8)
   (must-authenticate-p :type boolean)
   (number-of-versions-offered :type card8)
   (number-of-authentication-protocol-names-offered :type card8 :pad-size 6)
   (protocol-name :type string)
   (vendor-name :type string)
   (release-name :type string)
   (authentication-protocol-names
    :type strings :length number-of-authentication-protocol-names-offered)
   (version-list :type versions :length number-of-versions-offered)))

(declare-request protocol-reply (request)
  ((version-index :type card8)
   (protocol-major-opcode :type card8)
   (vendor-name :type string)
   (release-name :type string)))

(declare-request ping (request)
  ((minor-opcode :type card8 :pad-size 2)))

(declare-request ping-reply (request)
  ((minor-opcode :type card8 :pad-size 2)))

(declare-request want-to-close (request)
  ((minor-opcode :type card8 :pad-size 2)))

(declare-request no-close (request)
  ((minor-opcode :type card8 :pad-size 2)))

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
  (:documentation "Protocol class."))

(defgeneric signal-request-error (request-error)
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

(defmethod decode-request ((key (eql :request-error)) ice-conn buff byte-order)
  (let* ((index 2)
	 (handler (ice-error-handler ice-conn))
	 (class (buffer-read-card16 byte-order buff index))
	 (err (decode-request (decode-error class) ice-conn buff byte-order)))
    (when (arrayp handler)
      (setf handler (aref handler class)))
    (when handler 
      (funcall handler err))))

(defmacro declare-error (name (&rest slots) &body options)
  "Declare an ice error: creates a class named `name' and a condition named
  ice-error-<name>. The error class will be a request-error subclass which is
  a subclass of request. The condition will be a sub condition of ice-error.
  The interpretation of the slot declaration is as for declare-request. Plus
  you can pass options, such as in declare-condition, that will be pass to the
  declare-condition form.
  The generated condition has only one slot inherited from the ice-error
  condition: request-error with a reader named `ice-error-request-error'."
  `(progn
     (declare-request ,name (request-error)
       ((major-opcode :type card8 :inherited t)
	(class :type card16 :inherited t)
	(offending-minor-opcode :type card8 :inherited t)
	(severity :type error-severity :pad-size 2 :inherited t)
	(sequence-number-of-erroneous-message :type card32 :inherited t)
	,@slots)
       ,@(let ((doc (car (member :documentation options :key #'car))))
	   (when doc `(,doc))))
     (define-condition ,(sintern (format nil "ICE-ERROR-~a" name))
       (ice-error) () ,@options)
     (defmethod signal-request-error ((req ,(intern (symbol-name name))))
       (error ',(sintern (format nil "ICE-ERROR-~a" name)) 
	      :request-error req))))

;;; ICE error classes that are common to all protocols
  
(define-error :bad-minor #x8000)
(define-error :bad-state #x8001)
(define-error :bad-length #x8002)
(define-error :bad-value #x8003)

(declare-error bad-minor ())

(declare-error bad-state ())

(declare-error bad-length ())

(declare-error bad-value
  ((offset-of-offending-value :type card32)
   (length-of-offending-value :type card32)
   (offending-value :type data :length length-of-offending-value))
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

(declare-error bad-major
  ((values :type card8))
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Bad major: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error no-authentication ())

(declare-error no-version ())

(declare-error setup-failed
  ((values :type string))
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Setup failed: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error authentication-rejected
  ((values :type string))
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Authentication rejected: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error authentication-failed
  ((values :type string))
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Authentication failed: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error protocol-duplicate
  ((values :type string))
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Protocol duplication: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error major-opcode-duplicate
  ((values :type card8))
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Major opcode duplication: ~a~%"
		       (slot-value request-error 'values))))))

(declare-error unknown-protocol
  ((values :type string))
  (:report (lambda (condition stream)
	     (report-ice-error condition stream)
	     (let ((request-error (ice-error-request-error condition)))
	       (format stream "Unknown protocol: ~a~%"
		       (slot-value request-error 'values))))))
