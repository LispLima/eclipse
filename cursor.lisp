;;; -*- Mode: Lisp; Package: User -*-

;;; ECLIPSE. The Common Lisp Window Manager.
;;; This file is part of Eclipse. It gives an other (shortest) 
;;; way to use the predefined X cursors.

;;; Copyright (C) 2002 Iban HATCHONDO
;;; contact : hatchond@yahoo.fr

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(defvar +cursor-table+ (make-hash-table))
(defvar +cursor-font+ nil)
(defvar +black+ (xlib:make-color :red 0 :green 0 :blue 0))
(defvar +white+ (xlib:make-color :red 1 :green 1 :blue 1))

(defun define-glyph-cursor (cursor-key-name font-index)
  (setf (gethash cursor-key-name +cursor-table+) font-index))

(defun cursor-key-name->cursor-font-index (cursor-key-name)
  (gethash cursor-key-name +cursor-table+))

(defun get-x-cursor (display cursor-key-name &key reverse)
  (let ((index (cursor-key-name->cursor-font-index cursor-key-name)))
    (unless +cursor-font+
      (setf +cursor-font+ (xlib:open-font display "cursor")))
    (xlib:create-glyph-cursor
        :source-font +cursor-font+
	:mask-font +cursor-font+
	:source-char index
	:mask-char (1+ index)
	:foreground (if reverse +white+ +black+)
	:background (if reverse +black+ +white+))))
