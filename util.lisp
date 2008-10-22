
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
  (list (read *query-io*)))
