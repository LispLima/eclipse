;;; -*- Mode: Lisp; Package: CLX-EXTENSIONS -*-
;;; $Id: event.lisp,v 1.3 2003/08/28 14:44:40 hatchond Exp $
;;;
;;; Add on for CLX to have some CLOS events.
;;; This file is part of Eclipse.
;;; Copyright (C) 2000, 2001, 2002 Iban HATCHONDO
;;; contact : hatchond@yahoo.fr
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(declaim (optimize (speed 3)
		   (safety 3)
		   (debug 0)
		   (compilation-speed 0)))

(in-package :clx-extensions)

;;;; Creation of all event's classes
;;; Standard generic class for all event

(defclass event ()
  ((event-window :initarg :event-window :reader event-event-window)
   (send-event-p :initform nil
		 :initarg :send-event-p
		 :reader event-send-event-p)))

;;; Mixin

(defclass window-mixin ()
  ((window :initarg :window :initform nil :reader event-window)))
(defclass coordinates-mixin ()
  ((x :initarg :x :reader event-x)
   (y :initarg :y :reader event-y)))
(defclass size-mixin ()
  ((width :initarg :width :reader event-width)
   (height :initarg :height :reader event-height)))
(defclass root-mixin ()
  ((root :initarg :root :reader event-root)
   (root-x :initarg :root-x :reader event-root-x)
   (root-y :initarg :root-y :reader event-root-y)))
(defclass opcode-mixin () ;; for exopsure's
  ((drawable :initarg :drawable :reader event-drawable)
   (major :initarg :major :reader major)
   (minor :initarg :minor :reader minor)))

;;; Keyboard and pointer events

(defclass keyboard-pointer-event (event coordinates-mixin root-mixin)
  ((code :initarg :code :reader event-code)
   (state :initarg :state :reader event-state)
   (time :initarg :time :reader event-time)
   (child :initarg :child :reader event-child)
   (same-screen-p :initarg :same-screen-p :reader event-same-screen-p)))

(defclass pointer-event (keyboard-pointer-event) ())
(defclass button-press (pointer-event) ())
(defclass button-release (pointer-event) ())
(defclass motion-notify (pointer-event)
  ((hint-p :initarg :hint-p :reader event-hint-p)))

(defclass keyboard-event (keyboard-pointer-event) ())
(defclass key-press (keyboard-event) ())
(defclass key-release (keyboard-event) ())

(defclass in-out-window-event (event coordinates-mixin root-mixin)
  ((mode :initarg :mode :reader event-mode)
   (kind :initarg :kind :reader event-kind)
   (focus-p :initarg :focus-p :reader event-focus-p)
   (state :initarg :state :reader event-state)
   (time :initarg :time :reader event-time)
   (child :initarg :child :reader event-child)
   (same-screen-p :initarg :same-screen-p :reader event-same-screen-p)))

(defclass leave-notify (in-out-window-event) ())
(defclass enter-notify (in-out-window-event) ())

;;; Input focus events

(defclass input-focus-event (event)
  ((mode :initarg :mode :reader event-mode)
   (kind :initarg :kind :reader event-kind)))

(defclass focus-in (input-focus-event) ())
(defclass focus-out (input-focus-event) ())

;;; Keyboard and pointer state events

(defclass keymap-notify (event)
  ((keymap :initarg :keymap :reader event-keymap)))
(defclass mapping-notify (event)
  ((request :initarg :request :reader event-request)
   (start :initarg :start :reader event-start)
   (count :initarg :count :reader event-count)))

;;; Exposure events

(defclass exposure (event coordinates-mixin size-mixin)
  ((count :initarg :count :reader event-count)))
(defclass no-exposure (event opcode-mixin) ())
(defclass graphics-exposure  (exposure opcode-mixin) ())

;;; Window state events

(defclass circulate-notify (event window-mixin)
  ((place :initarg :place :reader event-window-place)))
(defclass configure-notify (event coordinates-mixin size-mixin window-mixin)
  ((border-width :initarg :border-width :reader event-border-width)
   (above-sibling :initarg :above-sibling :reader event-above-sibling)
   (override-redirect-p :initarg :override-redirect-p :reader event-override-redirect-p)))
(defclass create-notify (event coordinates-mixin size-mixin window-mixin)
  ((parent :initarg :parent :reader event-parent)
   (border-width :initarg :border-width :reader event-border-width)
   (override-redirect-p :initarg :override-redirect-p :reader event-override-redirect-p)))
(defclass map-notify (event window-mixin)
  ((override-redirect-p :initarg :override-redirect-p :reader event-override-redirect-p)))
(defclass reparent-notify (event coordinates-mixin window-mixin)
  ((parent :initarg :parent :reader event-parent)
   (override-redirect-p :initarg :override-redirect-p :reader event-override-redirect-p)))
(defclass unmap-notify (event window-mixin)
  ((configure-p :initarg :configure-p :reader event-configure-p)))
(defclass visibility-notify (event)
  ((state :initarg :state :reader event-state)))
(defclass destroy-notify (event window-mixin) ())
(defclass gravity-notify (event coordinates-mixin window-mixin) ())

;;; Structure control event

(defclass colormap-notify (event)
  ((colormap :initarg :colormap :reader event-colormap)
   (new-p :initarg :new-p :reader event-new-p)
   (installed-p :initarg :installed-p :reader event-installed-p)))
(defclass configure-request (event coordinates-mixin size-mixin window-mixin)
  ((border-width :initarg :border-width  :reader event-border-width)
   (stack-mode :initarg :stack-mode :reader event-stack-mode)
   (above-sibling :initarg :above-sibling :reader event-above-sibling)
   (value-mask :initarg :value-mask :reader event-value-mask)))
(defclass map-request (event window-mixin) ())
(defclass resize-request (event size-mixin window-mixin) ())
(defclass circulate-request (circulate-notify) ())

;;; Client communications events

(defclass client-message (event)
  ((type :initarg :type :reader event-type)
   (format :initarg :format :reader event-data-format)
   (data :initarg :data :reader event-data)))
(defclass property-notify (event)
  ((atom :initarg :atom :reader event-atom)
   (state :initarg :state :reader event-state)
   (time :initarg :time :reader event-time)))
(defclass selection-clear (event)
  ((selection :initarg :selection :reader event-selection)
   (time :initarg :time :reader event-time)))
(defclass selection-notify (selection-clear)
  ((target :initarg :target :reader event-target)
   (property :initarg :property :reader event-property)))
(defclass selection-request (selection-notify)
  ((requestor :initarg :requestor :reader event-requestor)))

;;; 

;(defmacro define-event (name
;			(&rest super-classes)
;			(&rest slots)
;			(&key documentation))
;  `(progn
;     (defclass ,name ,super-classes ,slots (:documentation ,documentation))
;     (xlib:declare-event :,name
;       ,(loop for 
;     ))

;;; We use resourcing in get-next-event.

(defparameter *events* (make-hash-table))

(defun initialize-event-table ()
  (loop for event-key across
	   (the (simple-array (or null keyword) (*)) xlib::*event-key-vector*)
	for symbol = (intern (symbol-name event-key))
	when (and event-key (find-class symbol nil)) do
	  (setf (gethash event-key *events*) (make-instance symbol))))

(initialize-event-table)

(defgeneric make-event (event-key &rest args)
  (:documentation "returns an new event of type given by event-key"))

(defgeneric re-initialize-event (event-key &rest args)
  (:documentation "re-initialize the event of type given by event-key"))

(defmethod make-event (event-key &rest args)
  (declare (ignorable args))
  (values))

(defmethod re-initialize-event (event-key &rest args)
  (declare (ignorable args))
  (values))

;;; Macro constructor

(defmacro class-slots (class)
  #+allegro `(clos:class-slots ,class)
  #+clisp `(clos::class-slots ,class)
  #+cmu `(pcl::class-slots ,class)
  #+cormanlisp `(cl:class-slots ,class)
  #+lispworks `(hcl::class-slots ,class)
  #+lucid `(clos:class-slots ,class)
  #+sbcl `(sb-pcl::class-slots ,class))

(defmacro slot-initargs (slot)
  #+(and allegro (not (version>= 6))) `(clos::slotd-initargs ,slot)
  #+(and allegro (version>= 6)) `(clos:slot-definition-initargs ,slot)
  #+clisp `(clos::slotdef-initargs ,slot)
  #+cmu `(slot-value ,slot 'pcl::initargs)
  #+cormanlisp `(getf ,slot :initargs)
  #+lispworks `(hcl::slot-definition-initargs ,slot)
  #+lucid `(clos:slot-definition-initargs ,slot)
  #+sbcl `(slot-value ,slot 'sb-pcl::initargs))

(defmacro class-initargs (class)
  `(loop for slot in (class-slots ,class) collect (car (slot-initargs slot))))

(defmacro keywds->names (keywords)
  `(mapcar #'(lambda (k) (intern (symbol-name k))) ,keywords))

(macrolet ((define-make-event-function ()
  `(values
    ,@(loop for event-key across xlib::*event-key-vector*
	    for class = (find-class (intern (symbol-name event-key)) nil)
	    for initargs = (when class (class-initargs class))
	    when class
	    collect
	    `(defmethod make-event ((,(gensym) (eql ,event-key))
				    &key ,@(keywds->names initargs)
				    &allow-other-keys)
	       (make-instance
		   ',(intern (symbol-name event-key))
		   ,@(loop for key in initargs
			   for sym = (intern (symbol-name key))
			   collect key collect sym)))
	    and collect
	     `(defmethod re-initialize-event ((,(gensym) (eql ,event-key))
					      &key ,@(keywds->names initargs)
					      &allow-other-keys)
	        (let ((event (gethash ,event-key *events*)))
		  ,@(loop for slot in (keywds->names initargs) 
			  collect `(setf (slot-value event ',slot) ,slot))
		  event))))))
  (define-make-event-function))

;;;; New event-processing function

(defun clos-event-handler (&rest event-slots &key event-key &allow-other-keys)
  "This display event handler returns an event object of type given by 
   EVENT-KEY. It use only one instance of each event type, by reinitializing 
   them each time it receive an event. Returns nil if no object can be found."
  (apply #'re-initialize-event event-key event-slots))

(defun get-next-event (display &key timeout peek-p discard-p (force-output-p t))
  "If force-output-p is true, first invokes display-force-output. Invokes 
   clos-event-handler on the first queued event, and returns event object.
   If such an object can not be found (i.e.: does not have a corresponding 
   class) invokes clos-event-handler on the next queued event and so on.
   If peek-p is true, then the event is not removed from the queue.  
   If discard-p is true, then events for which event-clos-handler returns nil
   (i.e.: event who do not have a corresponding class) are removed from the 
   queue, otherwise they are left in place.
   Hangs until non-nil is generated for some event, or for the specified 
   timeout (in seconds, if given); Returns nil on timeout."
  (xlib:process-event display :handler #'clos-event-handler
		      :timeout timeout :peek-p peek-p
		      :discard-p discard-p :force-output-p force-output-p))

