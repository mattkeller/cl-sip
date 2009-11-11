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

(defparameter sip-default-port 5060)

(defvar uac-port sip-default-port
  "Local port to listen for incoming SIP messages on")

(defparameter default-max-forwards 70)

(defvar branch-counter 0
  "Incr and add this to branch-prefix to get a globally unique branch id")

(defparameter branch-prefix "z9hG4bK")

(defclass user-proxy ()
  ((host  :initarg  :host ;; dns or ip?
          :initform (error "Must have a host")
          :reader   host)
   (port  :initarg  :port
          :initform 5060
          :reader   port)
   (proto :initform 'udp)))

(defclass subscriber ()
  ((user-name :initarg  :user-name
              :initform (error "Must have a user-name")
              :reader   user-name)
   (disp-name :initarg :disp-name
              :initform "Anonymous"
              :reader   disp-name)
   (proxy     :initarg :proxy
              :initform (error "Must have a user-proxy")
              :accessor proxy)))

(defparameter client-us-states '(offline busy online))

(defclass client-ua ()
  ((subscriber :initarg  :subscriber
               :initform (error "Must have a subscriber")
               :reader   subscriber)
   (local-port :initarg  :local-port
               :initform sip-default-port
               :reader   local-port)
   (local-ip   :initarg  :local-ip
               :initform nil
               :reader   local-ip) ;; for contact header
   sessions ;; list of sessions
   state    ;; online, offline, busy, etc
   ))

(defclass session () ;; TODO
  (call-id
   to
   from
   dialogs))

(defclass dialog () ;; TODO
  (id
   local-seq-num
   remote-seq-num
   local-uri
   remote-uri
   remote-target
   secure-p
   route-set
   early-p))

(defun new-request (method to &optional from)
  (unless from
    (setq from (concatenate'string "\"" user-display-name "\" <" user-name "@" domain ">")))
  ;; generate call-id, cseq, via, max-forwards
  ;; add contact if invite
  ;; case on method....
  nil)

(defun new-call-id ()
  "Generate a globally unique call-id"
  "nil")

(defvar example-com-proxy (make-instance 'user-proxy :host "example.com"))

(defvar bob-sub (make-instance 'subscriber
                               :user-name "bob"
                               :disp-name "Bob Jones"
                               :proxy example-com-proxy))

(defvar bob-ua (make-instance 'client-ua
                              :subscriber bob-sub
                              :local-port 5060))

;; Totally broken, but you get the idea
(defun  find-dialog (msg dialogs)
  "Match the incoming message to a dialog or session"
  (labels ((dialog-id (m)
             `((:call-id ,(call-id m)) (:remote-tag ,(to-tag m)) (:local-tag ,(from-tag m))))
           (dialog-id-eql (m1 m2)
             (let ((id1 (dialog-id m1))
                   (id2 (dialog-id m2)))
               (and (string-equal (car (assoc :call-id id1)) (car (assoc :call-id id2)))
                    (string-equal (car (assoc :remote-tag id1)) (car (assoc :local-tag id2)))
                    (string-equal (car (assoc :local-tag id1)) (car (assoc :remote-tag id2)))))))
    (find msg dialogs :test #'dialog-id-eql)))
#|
(socket-connect skt (make-inet-address "127.0.0.1") 5060)
(socket-send skt "foo" 3)

(host-ent-address (get-host-by-name "localhost")) => #(127 0 0 1)
|#

(defun make-sip-socket (addr &optional (port sip-default-port))
  (let ((skt (make-instance 'inet-socket :type :datagram :protocol :udp)))
    (socket-connect skt (make-inet-address addr) port)
    skt))

(defun send-msg (socket msg-string)
  (socket-send socket msg-string (length msg-string)))
    
(defun lookup-address (host)
  (handler-case (host-ent-address (get-host-by-name host))
    (name-service-error (e) nil)))
