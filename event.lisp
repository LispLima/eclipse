;;; -*- Mode: Lisp; Package: CL-USER -*-
;;; $Id$
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

(defclass button-press (keyboard-pointer-event) ())
(defclass button-release (keyboard-pointer-event) ())
(defclass key-press (keyboard-pointer-event) ())
(defclass key-release (keyboard-pointer-event) ())
(defclass motion-notify (keyboard-pointer-event) ())

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

(defmacro class-initargs (foo)
  `(mapcar #'(lambda (slot) (car (slot-initargs slot)))
           (class-slots (typecase ,foo
			  (class ,foo)
			  (keyword (find-class (intern (symbol-name ,foo))))
			  (symbol (find-class ,foo))
			  (t (class-of ,foo))))))

(defmacro keywds->names (keywords)
  `(mapcar #'(lambda (k) (intern (symbol-name k))) ,keywords))

(macrolet ((define-make-event-function ()
  `(values
    ,@(loop for event-key across xlib::*event-key-vector*
	    for initargs = (when event-key (class-initargs event-key))
	    when event-key
	    collect
	    `(defmethod make-event ((,(gensym) (eql ,event-key))
				    &key ,@(keywds->names initargs)
				    &allow-other-keys)
	      (make-instance
	       ',(intern (symbol-name event-key))
	       ,@(loop for key in initargs
		       for sym = (intern (symbol-name key))
		       collect key collect sym)))))))
  (define-make-event-function))

;;;; New event-processing function

(defun clos-event-handler (&rest event-slots &key event-key &allow-other-keys)
  (apply #'make-event event-key event-slots))

(defun get-next-event (display &key timeout peek-p discard-p (force-output-p t))
  (xlib:display-finish-output display)
  (xlib:process-event display :handler #'clos-event-handler
		      :timeout timeout :peek-p peek-p
		      :discard-p discard-p :force-output-p force-output-p))

