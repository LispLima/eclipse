;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: COMMON-LISP; -*-
;;; $Id: package.lisp,v 1.2 2004/03/01 14:54:04 ihatchondo Exp $
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

;;; NOTE: All comments below are from the original, modified only for
;;; readability. (added for package doc).
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a 
;;; copy of this software and associated documentation files (the ``Software'')
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;; OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;; Except as contained in this notice, the name of the X Consortium shall not
;;; be used in advertising or otherwise to promote the sale, use or other
;;; dealings in this Software without prior written authorization from the X
;;; Consortium.
;;; (X Window System is a trademark of X Consortium, Inc.)

(common-lisp:in-package :common-lisp-user)

(defpackage "ICE-LIB"
  (:use common-lisp)
  (:size 200)
  (:documentation
   "Inter-Client Exchange (ICE) protocol provides a generic framework for
    building protocols on top of reliable, byte-stream transport connections.
    It provides basic mechanisms for setting up and shutting down connections,
    for performing authentication, for negotiating versions, and for reporting
    errors. The protocols running within an ICE connection are referred to here
    as subprotocols. ICE provides facilities for each subprotocol to do its own
    version negotiation, authentication, and error reporting.  In addition, if
    two parties are communicating using several different subprotocols, ICE
    will allow them to share the same transport layer connection.

    This a small implementation of the protocol specification that contains
    only the functions that are used on the client side. If you want to build a
    ICE server you'll need to implement some functionalities such as creating a
    `socket server' and implmenting an `accept' operation.

    protocol specification can be found at: 
     <ftp://ftp.xfree86.org/X.Org/pub/R6.6/xc/doc/hardcopy/ICE/ice.PS.gz>")
  (:export 
   ;; request classes.
   #:request                     ; protocol class
                                 ; minor opcode
   #:byte-order                  ; 1
   #:connection-setup            ; 2
   #:authentication-required     ; 3
   #:authentication-reply        ; 4
   #:authentication-next-phase   ; 5
   #:connection-reply            ; 6
   #:protocol-setup              ; 7
   #:protocol-reply              ; 8
   #:ping                        ; 9
   #:ping-reply                  ; 10
   #:want-to-close               ; 11
   #:no-close                    ; 12
   
   ;; error request classes.
   #:request-error               ; protocol class
                                 ; class
   #:bad-minor                   ; #x8000
   #:bad-state                   ; #x8001
   #:bad-length                  ; #x8002
   #:bad-value                   ; #x8003
   #:bad-major                   ; 0
   #:no-authentication           ; 1
   #:no-version                  ; 2
   #:setup-failed                ; 3
   #:authentication-rejected     ; 4
   #:authentication-failed       ; 5
   #:protocol-duplicate          ; 6
   #:major-opcode-duplicate      ; 7
   #:unknown-protocol 

   ;; ice lib classes 
   #:ice-authority-entry 
   #:ice-connection 

   ;; error condition
   #:ice-error 
   #:ice-error-authentication-failed 
   #:ice-error-authentication-rejected 
   #:ice-error-bad-length 
   #:ice-error-bad-major 
   #:ice-error-bad-minor 
   #:ice-error-bad-state 
   #:ice-error-bad-value 
   #:ice-error-major-opcode-duplicate 
   #:ice-error-no-authentication 
   #:ice-error-no-version 
   #:ice-error-protocol-duplicate 
   #:ice-error-setup-failed 
   #:ice-error-unknown-protocol 

   ;; generic methods
   #:decode-request 
   #:ice-flush 
   #:last-received-message 
   #:last-sended-message 
   #:make-ice-request 
   #:post-request 
   #:read-request 
   #:request-length 
   #:signal-request-error 

   ;; slots
   #:minor-opcode 
   #:major-opcode 

   ;; slot accessors.
   #:authentication-failed-values 
   #:authentication-next-phase-data 
   #:authentication-next-phase-length-of-authentication-data 
   #:authentication-rejected-values 
   #:authentication-reply-data 
   #:authentication-reply-length-of-authentication-data 
   #:authentication-required-authentication-protocol-index 
   #:authentication-required-data 
   #:authentication-required-length-of-authentication-data 
   #:bad-major-values 
   #:bad-value-length-of-offending-value 
   #:bad-value-offending-value 
   #:bad-value-offset-of-offending-value 
   #:byte-order-byte-order 
   #:connection-input-buffer 
   #:connection-input-byte-order 
   #:connection-output-buffer 
   #:connection-output-byte-order 
   #:connection-reply-release-name 
   #:connection-reply-vendor-name 
   #:connection-reply-version-index 
   #:connection-setup-authentication-protocol-names 
   #:connection-setup-must-authenticate-p 
   #:connection-setup-number-of-authentication-protocol-names-offered 
   #:connection-setup-number-of-versions-offered 
   #:connection-setup-release-name 
   #:connection-setup-vendor-name 
   #:connection-setup-version-list 
   #:connection-stream 
   #:ice-connection-string 
   #:ice-connection-wait 
   #:ice-error-handler 
   #:ice-protocol-revision 
   #:ice-protocol-version 
   #:ice-release 
   #:ice-vendor 
   #:major-opcode-duplicate-values 
   #:protocol-duplicate-values 
   #:protocol-reply-release-name 
   #:protocol-reply-vendor-name 
   #:protocol-reply-version-index 
   #:protocol-setup-authentication-protocol-names 
   #:protocol-setup-must-authenticate-p 
   #:protocol-setup-number-of-authentication-protocol-names-offered 
   #:protocol-setup-number-of-versions-offered 
   #:protocol-setup-protocol-major-opcode 
   #:protocol-setup-protocol-name 
   #:protocol-setup-release-name 
   #:protocol-setup-vendor-name 
   #:protocol-setup-version-list 
   #:request-error-class 
   #:request-error-offending-minor-opcode 
   #:request-error-sequence-number-of-erroneous-message 
   #:request-error-severity 
   #:request-major-opcode 
   #:request-minor-opcode 
   #:setup-failed-values 
   #:unknown-protocol-values 

   ;; functions   
   #:authentication-failed-p 
   #:authentication-next-phase-p 
   #:authentication-rejected-p 
   #:authentication-reply-p 
   #:authentication-required-p 
   #:bad-length-p 
   #:bad-major-p 
   #:bad-minor-p 
   #:bad-state-p 
   #:bad-value-p 
   #:byte-order-p 
   #:connection-reply-p 
   #:connection-setup-p 
   #:major-opcode-duplicate-p 
   #:no-authentication-p 
   #:no-close-p 
   #:no-version-p 
   #:ping-p 
   #:ping-reply-p 
   #:protocol-duplicate-p 
   #:protocol-reply-p 
   #:protocol-setup-p 
   #:request-p 
   #:setup-failed-p 
   #:unknown-protocol-p 
   #:want-to-close-p 

   #:boolean-length 
   #:card16-length 
   #:card24-length 
   #:card32-length 
   #:card8-length 
   #:data-length 
   #:error-severity-length 
   #:ice-byte-order-length 
   #:make-request-length 
   #:pad-length 
   #:string-length 
   #:strings-length 
   #:version-length 
   #:versions-length 
   
   #:available-authentication-protocols 
   #:decode-error 
   #:decode-ice-minor-opcode 
   #:encode-error 
   #:encode-ice-minor-opcode 
   #:get-environment-variable 
   #:get-protocol-handler 
   #:ice-auth-proto-names 
   #:ice-error-request-error 
   #:make-buffer 
   #:make-data 
   #:make-version 
   #:make-versions 
   #:open-connection 
   #:parse-iceauthority-file 
   #:process-request 
   #:read-ice-auth-file 
   #:register-protocol 
   #:report-ice-error 

   ;; macros
   #:buffer-read-boolean 
   #:buffer-read-card16 
   #:buffer-read-card32 
   #:buffer-read-card8 
   #:buffer-read-data 
   #:buffer-read-error-severity 
   #:buffer-read-ice-byte-order 
   #:buffer-read-string 
   #:buffer-read-strings 
   #:buffer-read-version 
   #:buffer-read-versions 
   #:buffer-write-boolean 
   #:buffer-write-card16 
   #:buffer-write-card32 
   #:buffer-write-card8 
   #:buffer-write-data 
   #:buffer-write-error-severity 
   #:buffer-write-ice-byte-order 
   #:buffer-write-string 
   #:buffer-write-strings 
   #:buffer-write-version 
   #:buffer-write-versions 
   #:declare-error 
   #:declare-request 
   #:define-accessor 
   #:define-error 
   #:define-member16-accessor 
   #:define-member8-accessor 
   #:define-request 
   #:read-major-opcode 
   #:read-minor-opcode 
   #:register-ice-authentication-protocol 
   #:with-gensym 
   #:request-case 

   ;; types
   #:buffer
   #:card8 #:card16 #:card24 #:card32
   #:error-severity #:ice-byte-order 
   #:strings #:version #:versions #:data 

   ;; constants
   #:+ice-proto-major+ #:+ice-proto-minor+
   ))


