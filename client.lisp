;;;; client.lisp -- tiny, useless SIP client

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

(in-package :cl-sip.client)

(defun make-client (&key username proxy passwd)
  "Build a new SIP client"
  nil)

(defun call (client userb)
  "Call userb, return a new session"
  nil)

(defun answer (session)
  "Attempt to answer the new call"
  nil)
  

