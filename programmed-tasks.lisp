;;; -*- Mode: Lisp; Package: PROGRAMMED-TASKS  -*-
;;; $Id: programmed-tasks.lisp,v 1.3 2002/11/07 14:54:27 hatchond Exp $
;;;
;;; This file is part of Eclipse.
;;; Copyright (C) 2001 Iban HATCHONDO
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

(common-lisp:in-package :common-lisp-user)

(defpackage programmed-tasks
  (:nicknames pt)
  (:use common-lisp)
  (:size 50)
  (:export
   preprogrammed-tasks
   arm-timer
   execute-preprogrammed-tasks))

(in-package :PROGRAMMED-TASKS)

(defvar preprogrammed-tasks nil 
  "A set of tasks. If nil then no task are registered.")

(defun arm-timer (delta-time lambda)
  "Arm a timer that expires in delta-time (unit is second). At expiration 
   the given lambda (with no parameter) will be executed."
  (push (cons (+ delta-time (get-universal-time)) lambda)
	preprogrammed-tasks))

(defun execute-preprogrammed-tasks ()
  "Execute all tasks that have an expired time."
  (loop with task = (get-preprogrammed-task)
	while task do
	  (funcall (cdr task))
	  (remove-preprogrammed-task task)
	  (setf task (get-preprogrammed-task))))

(defun get-preprogrammed-task ()
  "Get the next task to execute if any at this time."
  (assoc (get-universal-time) preprogrammed-tasks :test #'>=))

(defun remove-preprogrammed-task (task)
  "Remove the given task. A task is a pair (time . lambda)."
  (setf preprogrammed-tasks (remove task preprogrammed-tasks :test #'equal)))


