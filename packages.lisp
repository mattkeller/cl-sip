
(defpackage :cl-sip.util
  (:use :common-lisp
        :cl-ppcre)
  (:export :symbol-name-alist))

(defpackage :cl-sip.msg
  (:use :common-lisp
        :cl-ppcre
        :cl-sip.util)
  (:export :call-method-args
           :responses
           :headers))

