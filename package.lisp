;;; -*- Mode: Lisp; Package: User -*-
;;; $Id: $
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

(common-lisp:in-package :common-lisp-user)

(defpackage ECLIPSE-INTERNALS
  (:nicknames eclipse ewmi)
  (:use clx-extensions common-lisp)
  (:size 10)
  (:export 
   eclipse
   )
  (:documentation ""))

(defun start-eclipse (display)
  (eclipse:eclipse display)
  (eclipse::%quit%))
 