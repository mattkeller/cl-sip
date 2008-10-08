
(in-package :cl-sip.util)

(defun symbol-name-alist (symbols)
  "Give an alist of '((symbol . symbol-name) ...)"
  (mapcar (lambda (s) (cons s (symbol-name s))) symbols))

