;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: COMMON-LISP; -*-
;;; $Id:$
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
   "REQUEST"                    ; protocol class
                                ; minor opcode
   "BYTE-ORDER"                 ; 1
   "CONNECTION-SETUP"           ; 2
   "AUTHENTICATION-REQUIRED"    ; 3
   "AUTHENTICATION-REPLY"       ; 4
   "AUTHENTICATION-NEXT-PHASE"  ; 5
   "CONNECTION-REPLY"           ; 6
   "PROTOCOL-SETUP"             ; 7
   "PROTOCOL-REPLY"             ; 8
   "PING"                       ; 9
   "PING-REPLY"                 ; 10
   "WANT-TO-CLOSE"              ; 11
   "NO-CLOSE"                   ; 12
   
   ;; error request classes.
   "REQUEST-ERROR"              ; protocol class
                                ; class
   "BAD-MINOR"                  ; #x8000
   "BAD-STATE"                  ; #x8001
   "BAD-LENGTH"                 ; #x8002
   "BAD-VALUE"                  ; #x8003
   "BAD-MAJOR"                  ; 0
   "NO-AUTHENTICATION"          ; 1
   "NO-VERSION"                 ; 2
   "SETUP-FAILED"               ; 3
   "AUTHENTICATION-REJECTED"    ; 4
   "AUTHENTICATION-FAILED"      ; 5
   "PROTOCOL-DUPLICATE"         ; 6
   "MAJOR-OPCODE-DUPLICATE"     ; 7
   "UNKNOWN-PROTOCOL"

   ;; ice lib classes 
   "ICE-AUTHORITY-ENTRY"
   "ICE-CONNECTION"

   ;; error condition
   "ICE-ERROR"
   "ICE-ERROR-AUTHENTICATION-FAILED"
   "ICE-ERROR-AUTHENTICATION-REJECTED"
   "ICE-ERROR-BAD-LENGTH"
   "ICE-ERROR-BAD-MAJOR"
   "ICE-ERROR-BAD-MINOR"
   "ICE-ERROR-BAD-STATE"
   "ICE-ERROR-BAD-VALUE"
   "ICE-ERROR-MAJOR-OPCODE-DUPLICATE"
   "ICE-ERROR-NO-AUTHENTICATION"
   "ICE-ERROR-NO-VERSION"
   "ICE-ERROR-PROTOCOL-DUPLICATE"
   "ICE-ERROR-SETUP-FAILED"
   "ICE-ERROR-UNKNOWN-PROTOCOL"

   ;; generic methods
   "DECODE-REQUEST"
   "ICE-FLUSH"
   "LAST-RECEIVED-MESSAGE"
   "LAST-SENDED-MESSAGE"
   "MAKE-ICE-REQUEST"
   "POST-REQUEST"
   "READ-REQUEST"
   "REQUEST-LENGTH"
   "SIGNAL-REQUEST-ERROR"

   ;; slots
   "MINOR-OPCODE"
   "MAJOR-OPCODE"

   ;; slot accessors.
   "AUTHENTICATION-FAILED-VALUES"
   "AUTHENTICATION-NEXT-PHASE-DATA"
   "AUTHENTICATION-NEXT-PHASE-LENGTH-OF-AUTHENTICATION-DATA"
   "AUTHENTICATION-REJECTED-VALUES"
   "AUTHENTICATION-REPLY-DATA"
   "AUTHENTICATION-REPLY-LENGTH-OF-AUTHENTICATION-DATA"
   "AUTHENTICATION-REQUIRED-AUTHENTICATION-PROTOCOL-INDEX"
   "AUTHENTICATION-REQUIRED-DATA"
   "AUTHENTICATION-REQUIRED-LENGTH-OF-AUTHENTICATION-DATA"
   "BAD-MAJOR-VALUES"
   "BAD-VALUE-LENGTH-OF-OFFENDING-VALUE"
   "BAD-VALUE-OFFENDING-VALUE"
   "BAD-VALUE-OFFSET-OF-OFFENDING-VALUE"
   "BYTE-ORDER-BYTE-ORDER"
   "CONNECTION-INPUT-BUFFER"
   "CONNECTION-INPUT-BYTE-ORDER"
   "CONNECTION-OUTPUT-BUFFER"
   "CONNECTION-OUTPUT-BYTE-ORDER"
   "CONNECTION-REPLY-RELEASE-NAME"
   "CONNECTION-REPLY-VENDOR-NAME"
   "CONNECTION-REPLY-VERSION-INDEX"
   "CONNECTION-SETUP-AUTHENTICATION-PROTOCOL-NAMES"
   "CONNECTION-SETUP-MUST-AUTHENTICATE-P"
   "CONNECTION-SETUP-NUMBER-OF-AUTHENTICATION-PROTOCOL-NAMES-OFFERED"
   "CONNECTION-SETUP-NUMBER-OF-VERSIONS-OFFERED"
   "CONNECTION-SETUP-RELEASE-NAME"
   "CONNECTION-SETUP-VENDOR-NAME"
   "CONNECTION-SETUP-VERSION-LIST"
   "CONNECTION-STREAM"
   "ICE-CONNECTION-STRING"
   "ICE-CONNECTION-WAIT"
   "ICE-ERROR-HANDLER"
   "ICE-PROTOCOL-REVISION"
   "ICE-PROTOCOL-VERSION"
   "ICE-RELEASE"
   "ICE-VENDOR"
   "MAJOR-OPCODE-DUPLICATE-VALUES"
   "PROTOCOL-DUPLICATE-VALUES"
   "PROTOCOL-REPLY-RELEASE-NAME"
   "PROTOCOL-REPLY-VENDOR-NAME"
   "PROTOCOL-REPLY-VERSION-INDEX"
   "PROTOCOL-SETUP-AUTHENTICATION-PROTOCOL-NAMES"
   "PROTOCOL-SETUP-MUST-AUTHENTICATE-P"
   "PROTOCOL-SETUP-NUMBER-OF-AUTHENTICATION-PROTOCOL-NAMES-OFFERED"
   "PROTOCOL-SETUP-NUMBER-OF-VERSIONS-OFFERED"
   "PROTOCOL-SETUP-PROTOCOL-MAJOR-OPCODE"
   "PROTOCOL-SETUP-PROTOCOL-NAME"
   "PROTOCOL-SETUP-RELEASE-NAME"
   "PROTOCOL-SETUP-VENDOR-NAME"
   "PROTOCOL-SETUP-VERSION-LIST"
   "REQUEST-ERROR-CLASS"
   "REQUEST-ERROR-OFFENDING-MINOR-OPCODE"
   "REQUEST-ERROR-SEQUENCE-NUMBER-OF-ERRONEOUS-MESSAGE"
   "REQUEST-ERROR-SEVERITY"
   "REQUEST-MAJOR-OPCODE"
   "REQUEST-MINOR-OPCODE"
   "SETUP-FAILED-VALUES"
   "UNKNOWN-PROTOCOL-VALUES"

   ;; functions   
   "AUTHENTICATION-FAILED-P"
   "AUTHENTICATION-NEXT-PHASE-P"
   "AUTHENTICATION-REJECTED-P"
   "AUTHENTICATION-REPLY-P"
   "AUTHENTICATION-REQUIRED-P"
   "BAD-LENGTH-P"
   "BAD-MAJOR-P"
   "BAD-MINOR-P"
   "BAD-STATE-P"
   "BAD-VALUE-P"
   "BYTE-ORDER-P"
   "CONNECTION-REPLY-P"
   "CONNECTION-SETUP-P"
   "MAJOR-OPCODE-DUPLICATE-P"
   "NO-AUTHENTICATION-P"
   "NO-CLOSE-P"
   "NO-VERSION-P"
   "PING-P"
   "PING-REPLY-P"
   "PROTOCOL-DUPLICATE-P"
   "PROTOCOL-REPLY-P"
   "PROTOCOL-SETUP-P"
   "REQUEST-P"
   "SETUP-FAILED-P"
   "UNKNOWN-PROTOCOL-P"
   "WANT-TO-CLOSE-P"

   "BOOLEAN-LENGTH"
   "CARD16-LENGTH"
   "CARD24-LENGTH"
   "CARD32-LENGTH"
   "CARD8-LENGTH"
   "DATA-LENGTH"
   "ERROR-SEVERITY-LENGTH"
   "ICE-BYTE-ORDER-LENGTH"
   "MAKE-REQUEST-LENGTH"
   "PAD-LENGTH"
   "STRING-LENGTH"
   "STRINGS-LENGTH"
   "VERSION-LENGTH"
   "VERSIONS-LENGTH"
   
   "AVAILABLE-AUTHENTICATION-PROTOCOLS"
   "DECODE-ERROR"
   "DECODE-ICE-MINOR-OPCODE"
   "ENCODE-ERROR"
   "ENCODE-ICE-MINOR-OPCODE"
   "GET-ENVIRONMENT-VARIABLE"
   "GET-PROTOCOL-HANDLER"
   "ICE-AUTH-PROTO-NAMES"
   "ICE-ERROR-REQUEST-ERROR"
   "MAKE-BUFFER"
   "MAKE-DATA"
   "MAKE-VERSION"
   "MAKE-VERSIONS"
   "OPEN-CONNECTION"
   "PARSE-ICEAUTHORITY-FILE"
   "PROCESS-REQUEST"
   "READ-ICE-AUTH-FILE"
   "REGISTER-PROTOCOL"
   "REPORT-ICE-ERROR"

   ;; macros
   "BUFFER-READ-BOOLEAN"
   "BUFFER-READ-CARD16"
   "BUFFER-READ-CARD32"
   "BUFFER-READ-CARD8"
   "BUFFER-READ-DATA"
   "BUFFER-READ-ERROR-SEVERITY"
   "BUFFER-READ-ICE-BYTE-ORDER"
   "BUFFER-READ-STRING"
   "BUFFER-READ-STRINGS"
   "BUFFER-READ-VERSION"
   "BUFFER-READ-VERSIONS"
   "BUFFER-WRITE-BOOLEAN"
   "BUFFER-WRITE-CARD16"
   "BUFFER-WRITE-CARD32"
   "BUFFER-WRITE-CARD8"
   "BUFFER-WRITE-DATA"
   "BUFFER-WRITE-ERROR-SEVERITY"
   "BUFFER-WRITE-ICE-BYTE-ORDER"
   "BUFFER-WRITE-STRING"
   "BUFFER-WRITE-STRINGS"
   "BUFFER-WRITE-VERSION"
   "BUFFER-WRITE-VERSIONS"
   "DECLARE-ERROR"
   "DECLARE-REQUEST"
   "DEFINE-ACCESSOR"
   "DEFINE-ERROR"
   "DEFINE-MEMBER16-ACCESSOR"
   "DEFINE-MEMBER8-ACCESSOR"
   "DEFINE-REQUEST"
   "READ-MAJOR-OPCODE"
   "READ-MINOR-OPCODE"
   "REGISTER-ICE-AUTHENTICATION-PROTOCOL"
   "WITH-GENSYM"
   "REQUEST-CASE"

   ;; types
   "CARD8" "CARD16" "CARD24" "CARD32"
   "ERROR-SEVERITY" "ICE-BYTE-ORDER"
   "STRINGS" "VERSION" "VERSIONS" "DATA"

   ;; constants
   "+ICE-PROTO-MAJOR+" "+ICE-PROTO-MINOR+"
   ))


