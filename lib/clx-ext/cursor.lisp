;;; -*- Mode: Lisp; Package: Clx-Extensions -*-
;;; $Id: cursor.lisp,v 1.4 2004/03/08 23:37:15 ihatchondo Exp $
;;;
;;; ECLIPSE. The Common Lisp Window Manager.
;;; This file is part of Eclipse. It gives an other (shortest) 
;;; way to use the predefined X cursors.
;;;
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(in-package :clx-extensions)

(defvar +cursor-font+ nil)
(defvar +clx-color-black+ (xlib:make-color :red 0 :green 0 :blue 0))
(defvar +clx-color-white+ (xlib:make-color :red 1 :green 1 :blue 1))

(defvar *cursor-cache* (make-array 128 :initial-element nil))

(declaim (type (simple-array (or null xlib:cursor) *) *cursor-cache*))

(defun cursor-key-name->cursor-font-index (name &optional (cursors +cursors+))
  "Returns the cursor font index of the specified cursor or NIL if not found.
   - name: the name of the cursor to find.
   - cursors (or list (simple-array * (*))): a cursor name collection.
     Its default value is keyboard::+cursors+
   Result is unpredictable if the type of name is not the same as the 
   collection elements one."
  (let ((position (position name cursors)))
    (and position (* 2 position))))

(defun lookup-cursor (index)
  "Returns the cursor, or NIL, of index `index' from the cache."
  (aref *cursor-cache* (ash index -1)))

(defsetf cache-cursor (index) (cursor)
  "Cache the given cursor of index `index'."
  `(setf (aref *cursor-cache* (ash ,index -1)) ,cursor))

(defun get-x-cursor (display cursor-name &key reverse (cache t))
  "Returns the cursor designed by `cursor-name' of the \"cursor\" font.
   If :cache T then returns a cached (shared) cursor. Otherwise returns
   a new allocated one (default value is T)."
  (let ((i (cursor-key-name->cursor-font-index cursor-name)))
    (unless +cursor-font+
      (setf +cursor-font+ (xlib:open-font display "cursor")))
    (flet ((make-cursor (index)
	     (xlib:create-glyph-cursor
		 :foreground (if reverse +clx-color-white+ +clx-color-black+)
		 :background (if reverse +clx-color-black+ +clx-color-white+)
	         :source-font +cursor-font+ :mask-font +cursor-font+
		 :source-char index :mask-char (1+ index))))
      (if cache
	  (or (lookup-cursor i) (setf (cache-cursor i) (make-cursor i)))
	  (make-cursor i)))))