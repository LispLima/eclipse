;;; -*- Mode: Lisp; Package: ECLIPSE-INTERNALS -*-
;;; $Id: misc.lisp,v 1.1 2002/11/07 15:06:03 hatchond Exp $
;;;
;;; This file is part of Eclipse.
;;; Copyright (C) 2002 Iban HATCHONDO
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

(in-package :ECLIPSE-INTERNALS)

(defun home-subdirectory (directory)
  (concatenate
       'string
       #+:cmu "home:"
       #-:cmu (let ((homedir (user-homedir-pathname)))
		(or (when homedir (namestring homedir))
		    "~/"))
       directory))

(defmacro screen-width ()
  `(xlib:screen-width (xlib:display-default-screen *display*)))
(defmacro screen-height ()
  `(xlib:screen-height (xlib:display-default-screen *display*)))
(defmacro id->atom-name (id) `(xlib:atom-name *display* ,id))
(defmacro atom-name->id (name) `(xlib:find-atom *display* ,name))

(defmacro with-root-cursor ((new-cursor) &body body)
  `(unwind-protect
     (setf (xlib:window-cursor *root-window*) ,new-cursor)
     ,@body
     (setf (xlib:window-cursor *root-window*) (root-default-cursor *root*))))

(defmacro make-error-handler ((type &key throw return (verbose t)) &body body)
  `(defun ,(intern (with-standard-io-syntax
		       (format nil "HANDLE-~a-CONDITION" type)))
       (condition)
     (declare (ignorable condition))
     ,@(when verbose
	`((format *stderr* "error - ~A - : ~A~%" ',type condition)
	  (finish-output *stderr*)))
     ,(if return `nil `(throw ',(or throw type) ,@body))))

(make-error-handler (general-error) nil)
(make-error-handler (invalid-wm-property) nil)
(make-error-handler (end-of-file :throw end) nil)
(make-error-handler (wrong-name :verbose nil)
  (timed-message-box *root-window* "Wrong application name"))

(defun run-application (program &rest arguments)
  (lambda ()
    (catch 'wrong-name
      (handler-bind ((error #'handle-wrong-name-condition))
	(%run-program% program arguments)))))

(defun make-view-port-property ()
  (make-list (* 2 *nb-vscreen*) :initial-element 0))

(defun wm-state (window)
  (xlib:get-property window :WM_STATE))

(defsetf wm-state (window &key icon-id) (state)
  (let ((net-wm-state (gensym)))
    `(let ((,net-wm-state (netwm:net-wm-state ,window)))
       (if (or (= ,state 3) (= ,state 0))
	   (pushnew :_net_wm_state_hidden ,net-wm-state)
           (setf ,net-wm-state (delete :_net_wm_state_hidden ,net-wm-state)))
       (setf (netwm:net-wm-state ,window) ,net-wm-state)
       (xlib:change-property ,window
			     :WM_STATE
			     (list ,state ,icon-id)
			     :WM_STATE
			     32))))

(defun workspace-names (window)
  (or (netwm:net-desktop-names window) (gnome:win-workspace-names window)))

(defsetf workspace-names () (names)
  `(when ,names
     (setf (netwm:net-desktop-names *root-window*) ,names
           (gnome:win-workspace-names *root-window*) ,names)))

(defun wm-name (window)
  (or (ignore-errors (netwm:net-wm-name window))
      (ignore-errors (xlib:wm-name window))
      (format nil "incognito")))

(defun gnome-desktop-num (window)
  (or (netwm:net-wm-desktop window) (gnome:win-workspace window)))

(defsetf window-desktop-num (window) (n)
  `(setf (gnome:win-workspace ,window) ,n
	 (netwm:net-wm-desktop ,window) ,n
	 (application-desktop-number (lookup-widget ,window)) ,n))

(defun motif-wm-decoration (window)
  (let ((prop (xlib:get-property window :_MOTIF_WM_HINTS)))
    (or (and prop (logbitp 1 (first prop)) (zerop (third prop)) :OFF) :ON)))

(defun stick-p (window)
  (or (= (or (gnome-desktop-num window) -1) +any-desktop+)
      (logbitp 0 (or (gnome:win-state window :result-type t) 0))))

(defun send-wm-protocols-client-message (window atom &rest data)
  (xlib:send-event window
		   :client-message
		   nil
		   :window window
		   :type :WM_PROTOCOLS
		   :format 32
		   :data (cons (atom-name->id atom) data)))

(defun grab-root-pointer (&key cursor owner-p confine-to)
  (xlib:grab-pointer
      *root-window*
      +pointer-event-mask+
      :confine-to confine-to
      :cursor (or cursor (root-default-cursor *root*))
      :owner-p owner-p))

(defun query-application-tree ()
  (loop for window in (xlib:query-tree *root-window*)
	for obj = (lookup-widget window)
	for app = (typecase obj
		    (application window)
		    (decoration (get-child obj :application :window t)))
	when app collect app))

(defun delete-root-properties ()
  (mapc #'(lambda (prop) (xlib:delete-property *root-window* prop))
	(concatenate 'list +gnome-protocols+ +netwm-protocol+)))
