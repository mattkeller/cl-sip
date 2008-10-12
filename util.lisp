
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

(defmacro aif (test then &optional else)
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  `(when-bind it ,test ,@body))

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
