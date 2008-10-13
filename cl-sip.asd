;;;; cl-sip.lisp -- ASDF package for cl-sip

(defpackage #:cl-sip.asd
  (:use :cl :asdf))

(in-package :cl-sip.asd)

(defsystem cl-sip
  :name "cl-sip"
  :version "0.0.0"
  :maintainer "Matt Keller"
  :description "cl-sip"
  :long-description "Simple SIP stack in Common Lisp"
  :serial t ;; the dependencies are linear.
  :components ((:file "packages")
               (:file "util")
               (:file "msg" :depends-on ("util"))
               (:file "client" :depends-on ("msg")))
  :depends-on (:cl-ppcre :stefil))

