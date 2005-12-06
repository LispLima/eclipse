;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ICE-LIB; -*-
;;; $Id: ICE-macros.lisp,v 1.6 2005/03/25 14:43:54 ihatchondo Exp $
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

(defmacro with-slot-key-args ((&rest key-args) slot-decl &body body)
  `(destructuring-bind (&key ,@key-args &allow-other-keys) (cdr ,slot-decl)
     ,@body))

(defmacro with-reply-buffer ((buffer) &rest slots)
  (flet ((type-reader (type) (format nil "BUFFER-READ-~a" (symbol-name type))))
    (with-gensym (buff)
      `(let ((,buff ,buffer))
	 (declare (type buffer ,buff))
	 (setf (buffer-index ,buff) 0)
	 ,@(loop for slot-description in slots
		 for name = (car slot-description) collect	       
		 (with-slot-key-args ((type 'card8) (pad-size 0) length)
		     slot-description
		   `(setf ,(intern (symbol-name name))
		          (,(sintern (type-reader type)) ,buff
		          ,@(when length `(,(intern (symbol-name length)))))
		          ,@(unless (zerop pad-size)
			      `((buffer-index ,buff)
				(+ (buffer-index ,buff) ,pad-size))))))))))

(defmacro with-request-buffer ((buffer) &rest slots)
  (flet ((type-writer (type) (format nil "BUFFER-WRITE-~a" (symbol-name type))))
    (with-gensym (buff)
      `(let ((,buff ,buffer))
	 (declare (type buffer ,buff))
	 ,@(loop for slot-desc in slots 
		 for name = (intern (symbol-name (car slot-desc))) collect
		 (with-slot-key-args ((type 'card8) (pad-size 0)) slot-desc
		   `(prog1 (,(sintern (type-writer type)) ,name ,buff)
		      ,@(unless (zerop pad-size)
			  `((index+ ,buff ,pad-size))))))))))

(defun make-request-length (slots &optional nb-bytes)
  `(+ ,@(when (numberp nb-bytes) `(,nb-bytes))
      ,@(loop for slot in slots 
	      for type = (with-slot-key-args ((type 'card8)) slot type)
	      for fun = (sintern (format nil "~a-LENGTH" type))
	      when (with-slot-key-args (pad-size) slot pad-size) collect it
	      collect `(,fun ,(intern (symbol-name (car slot)))))))

(defun make-constructor-key-args (slots)
  (loop for slot in slots
	for initform = (with-slot-key-args (initform) slot initform)
	if initform collect `(,(car slot) ,initform) else collect (car slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;;;
;;;;                          Public interface                             ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macro wraper around defclass to factorize some declarations, and 
;; defines the: make-request method, the <class-name>-p predicate function
;; the request-length method and the post-request, decode-request method for
;; writing or reading a request over the wire.

(defmacro defrequest (name (&rest super-classes) (&rest slots) &rest doc)
  "Declares a request. Generate a class named `name' and specialized methods 
   for: make-request, decode-request, post-request, request-length plus a
   predicate function named<name>-p.
    The syntax is the same than in defclass. The only difference is the four
   more keyword arguments that exist for slot description:

    - :prefix (symbol): If given, then it will be use with the slots name to
      construct the accessor method name if :accessor option was not given.
      If not given the class name will be used.
    - :pad-size (fixnum): You may (or have to) indicate the number of padding
       bytes between two slots. This information will not be used in the class
       form, but will be in the encoder and decoder methods. Default is: 0.
    - :inherited (boolean): You also need to indicate the inherited slots if
      any in your declaration (except for the two inherited from the request
      top level class). This information is needed for the {en/de}coder
      methods. Default: NIL.
    - :length (slot-name): For sequence type, the decoder method will need this
      information to be able to read the correct quantity of items of the given
      sequence type. For example: if you have a slot typed versions where 
      versions is a for instance (simple-array version (*)) then the decoder
      will need to know how much version to read."
  (let ((slot-names (list))
	(class-key-name (kintern (symbol-name name))))
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
		  (push (intern (symbol-name slot-name)) slot-names)
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
      ;; Generate make-request method with eql-specializer on class-name.
      (defmethod make-request
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
      ;; Generate request-length method.
      (defmethod request-length ((request ,(intern (symbol-name name))))
	(with-slots (,@slot-names) request
	  (let ((length ,(make-request-length slots)))
	    (declare (type fixnum length))
	    (+ length (mod (- length) 8)))))
      ;; Generate post-request.
      (defmethod post-request
	  ((request ,(intern (symbol-name name))) ice-connection &rest rest)
	(declare (ignore rest))
	(with-slots (output-byte-order stream) ice-connection
	  (let ((buffer
		 (make-buffer (request-length request) output-byte-order)))
	    (with-slots (,@slot-names) request
	      (with-request-buffer (buffer)
		,@slots))
	    (setf (connection-output-buffer ice-connection) buffer)
	    (buffer-write-sequence buffer stream)
	    (finish-output stream))))
      ;; Generate request decoder.
      (defmethod decode-request
	  ((name (eql ,class-key-name)) ice-connection buffer)
	(declare (ignorable ice-connection))
	(declare (type buffer buffer))
	(let ((request (make-instance ',name)))
	  (with-slots (,@slot-names) request
	    (with-reply-buffer (buffer)
	      ,@slots))
	  request)))))

(defmacro declare-request (name (&rest super-classes) (&rest slots) &rest doc)
  "Declares a request. Generate a class named `name' and specialized methods 
   for: make-request, decode-request, post-request, request-length plus 
   a predicate function named<name>-p.
    The syntax is the same than in defclass. The only difference is the four
   more keyword arguments that exist for slot description:

    - :prefix (symbol): If given, then it will be use with the slots name to
      construct the accessor method name if :accessor option was not given.
      If not given the class name will be used.
    - :pad-size (fixnum): You may (or have to) indicate the number of padding
       bytes between two slots. This information will not be used in the class
       form, but will be in the encoder and decoder methods. Default is: 0.
    - :inherited (boolean): You also need to indicate the inherited slots if
      any in your declaration (except for the two inherited from the request
      top level class). This information is needed for the {en/de}coder
      methods. Default: NIL.
    - :length (slot-name): For sequence type, the decoder method will need this
      information to be able to read the correct quantity of items of the given
      sequence type. For example: if you have a slot typed versions where 
      versions is a for instance (simple-array version (*)) then the decoder
      will need to know how much version to read."
  (let ((minor (list 'MINOR-OPCODE :type 'card8))
	(major (list 'MAJOR-OPCODE :type 'card8 :initform +ice-proto-minor+)))
    (pushnew minor slots :key #'car)
    (pushnew major slots :key #'car)
    (let ((minor-opcode-slotdef (car (member 'MINOR-OPCODE slots :key #'car))))
      (unless (getf (cdr minor-opcode-slotdef) :initform)
	(setf (getf (cdr minor-opcode-slotdef) :initform)
	      (encode-ice-minor-opcode (kintern (symbol-name name))))))
    `(progn
       (defrequest ,name ,super-classes ,slots ,@doc)
       (defmethod post-request
	   ((name (eql ,(kintern (symbol-name name)))) ice-connection
	    &key ,@(make-constructor-key-args slots)
	    &allow-other-keys)
	 (let ((req
		(make-instance ',name
		  ,@(loop for arg in (make-constructor-key-args slots)
			  for foo = (if (consp arg) (car arg) arg)
			  collect (kintern (symbol-name foo)) collect foo))))
	   (setf (slot-value req 'length) (1- (/ (request-length req) 8)))
	   (post-request req ice-connection))))))

(defmacro declare-error (name (&rest parents) (&rest slots) &body options)
  "Declare an ice error: creates a class named `name' and a condition named
  `ice-error-<name>'.
    The defined class will be a {defclass request-error} subclass if no parent
   classes are supplied.
    The condition will be a sub condition of {define-condition ice-error} if
   no parent classes are supplied. The generated condition has only one slot
   inherited from the ice-error condition: request-error with a reader named
   `ice-error-request-error'. If parent classes are supplied, then the correct
   parent conditions will be computed by concatenation of prefix `ice-error-'
   to the parents, keeping package prefix if any.
    The interpretation of the slots declaration is as for declare-request. Plus
   you can pass options, such as in define-condition, that will be pass to the
   define-condition form."
  (let ((condition (sintern (format nil "ICE-ERROR-~a" name)))
	(cparents (loop for parent in parents collect
			(intern (format nil "ICE-ERROR-~a" parent)
				(package-name (symbol-package parent))))))
    `(progn
       (declare-request ,name ,(or parents `(request-error))
	 ((major-opcode :type card8 :inherited t)
	  (minor-opcode :type card8 :initform 0)
	  (class :type card16 :inherited t)
	  (length :type card32 :initform 0)
	  (offending-minor-opcode :type card8 :inherited t)
	  (severity :type error-severity :pad-size 2 :inherited t)
	  (sequence-number-of-erroneous-message :type card32 :inherited t)
	  ,@slots)
	 ,@(let ((doc (car (member :documentation options :key #'car))))
	     (when doc `(,doc))))
       (define-condition ,condition ,(or cparents `(ice-error)) () ,@options)
       (defmethod request-error-handler ((req ,(intern (symbol-name name))))
	 (error ',condition :request-error req)))))
