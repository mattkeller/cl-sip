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


(in-package :cl-sip.util)

(defun symbol-name-alist (symbols)
  "Give an alist of '((symbol . symbol-name) ...)"
  (mapcar (lambda (s) (cons s (symbol-name s))) symbols))

(defmacro if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (var test then &optional else)
  `(if-bind ,var ,test ,then ,else))

(defmacro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (var test &body body)
  `(when-bind ,var ,test ,@body))

(defun trim-ws (str)
  (let ((ws '(#\Space #\Tab)))
    (string-left-trim ws (string-right-trim ws str))))

(defun join-str (separator-str a b)
  (concatenate 'string a separator-str b))

(defun make-keyword (name)
  "Make a keyword with given name. Attempts to respect the current readtable case."
  (intern (case (readtable-case *readtable*)
            (:upcase (string-upcase name))
            (:downcase (string-downcase name))
            (t name))
          :keyword))

(defmacro scan-to-stringz (var-list regex str &body body)
  "Bind elements of var-list to matches in regex or nil"
  (let ((g1 (gensym))
        (g2 (gensym))
        (i -1))
  `(multiple-value-bind (,g1 ,g2) (scan-to-strings ,regex ,str)
     (declare (ignore ,g1))
     (let (,@(mapcar #'(lambda (v)
                         (setf i (1+ i))
                         `(,v (if (and ,g2 (>= (length ,g2) ,i)) (aref ,g2 ,i) nil)))
                     var-list))
       ,@body))))

(defun read-new-value ()
  (format *query-io* "~&Enter a new value (unevaluated): ")
  (force-output *query-io*)
  (multiple-value-list (read *query-io*)))

(defun alist-push-uniq (alist key value &optional &key (test #'eq))
  "Push (key . value) onto alist unless key is already in alist"
  (if (assoc key alist :test test)
      alist
      (acons key value alist)))