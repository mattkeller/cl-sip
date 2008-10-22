
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
           :read-new-value))

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
