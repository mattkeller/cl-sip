
(defpackage :cl-sip.util
  (:use :common-lisp
        :cl-ppcre)
  (:export :it
           :aif
           :awhen
           :symbol-name-alist
           :trim-ws
           :join-str
           :make-keyword))

(defpackage :cl-sip.msg
  (:use :common-lisp
        :cl-ppcre
        :cl-sip.util)
  (:export :call-method-args
           :responses
           :headers))

(defpackage :cl-sip.client
  (:use :common-lisp
        :cl-sip.util
        :cl-sip.msg)
  (:export :make-client
           :call
           :answer))
