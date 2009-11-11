;;;; cl-sip.asd -- ASDF package for cl-sip

;; Copyright 2009 Matt Keller
;;
;; This file is part of cl-sip.
;;
;; cl-sip is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; cl-sip is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with cl-sip.  If not, see
;; <http://www.gnu.org/licenses/>.

(defpackage #:cl-sip.asd
  (:use :cl :asdf))

(in-package :cl-sip.asd)

(defsystem cl-sip
  :name "cl-sip"
  :version "0.1.0"
  :maintainer "Matt Keller"
  :description "cl-sip"
  :long-description "Simple SIP stack in Common Lisp"
  :serial t ;; the dependencies are linear.
  :components ((:file "packages")
               (:file "util")
               (:file "msg" :depends-on ("util"))
               (:file "client" :depends-on ("msg")))
  :depends-on (:cl-ppcre :stefil))

