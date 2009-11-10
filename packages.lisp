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


(defpackage :cl-sip.util
  (:use :common-lisp
        :cl-ppcre)
  (:export :it
           :aif
           :awhen
           :symbol-name-alist
           :trim-ws
           :join-str
           :make-keyword
           :scan-to-stringz
           :read-new-value
           :alist-push-uniq))

(defpackage :cl-sip.msg
  (:use :common-lisp
        :cl-ppcre
        :cl-sip.util
        :stefil)
  (:export :msg
           :version
           :headers
           :bodies
           :emit
           :has-header
           :add-header
           :response
           :status-code
           :sip-uri
           :scheme
           :user-info
           :host
           :ip
           :port
           :uri-parms
           :sip-parse-error
           :parse-msg
           :parse-uri))

(defpackage :cl-sip.client
  (:use :common-lisp
        :cl-sip.util
        :cl-sip.msg)
  (:export :make-client
           :call
           :answer))
