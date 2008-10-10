;;;; msg.lisp -- SIP msg parsing and constructing

; TODO
; * defconstant is a pain!
; * review rfc for parsing details
; * handle sips uris?
; * generic parse-msg fn
; * print-object for msg
; * parsing of specific headers
; * msg construction & "toString"
; * parse-headers does not catch a multline continuation as the first header line

(in-package :cl-sip.msg)

;;; SIP Constants ------------------------------------------------------

(defconstant +crlf+ (format nil "~a~a" #\Return #\Linefeed))

(defconstant +methods+ (symbol-name-alist '(invite ack options bye cancel register options info)))

(defun is-method (m)
  (if (assoc m +methods+) t nil))

(defun is-method-name (name)
  (aif (member name +methods+ :key #'cdr :test #'string-equal)
       (car (car cl-sip.util:it))
       nil))

;; case ignored as per rfc
(defconstant +headers+ (symbol-name-alist
                        '(accept
                          accept-encoding
                          accept-language
                          alert-info
                          allow
                          authentication-info
                          authorization
                          call-id
                          call-info
                          contact
                          content-disposition
                          content-encoding
                          content-language
                          content-length
                          content-type
                          cseq
                          date
                          error-info
                          expires
                          extension-header
                          from
                          in-reply-to
                          max-forwards
                          mime-version
                          min-expires
                          organization
                          priority
                          proxy-authenticate
                          proxy-authorization
                          proxy-require
                          record-route
                          reply-to
                          require
                          retry-after
                          route
                          server
                          subject
                          supported
                          timestamp
                          to
                          unsupported
                          user-agent
                          via
                          warning
                          www-authenticate)))

(defun is-header (h)
  (if (assoc h +headers+) h nil))

(defun is-header-name (name)
  (aif (member name +headers+ :key #'cdr :test #'string-equal)
       (car (car cl-sip.util:it))
       nil))

(defconstant +status-codes+ '((100 . "Trying")
                              (180 . "Ringing")
                              (181 . "Call Is Being Forwarded")
                              (182 . "Queued")
                              (183 . "Session Progress")
                              (200 . "Ok")
                              (300 . "Multiple Choices")
                              (301 . "Moved Permanently")
                              (302 . "Moved Temporarily")
                              (305 . "Use Proxy")
                              (380 . "Alternative Service")
                              (400 . "Bad Request")
                              (401 . "Unauthorized")
                              (402 . "Payment Required")
                              (403 . "Forbidden")
                              (404 . "Not Found")
                              (405 . "Method Not Allowed")
                              (406 . "Not Acceptable")
                              (407 . "Proxy Authentication Required")
                              (408 . "Request Timeout")
                              (410 . "Gone")
                              (413 . "Request Entity Too Large")
                              (414 . "Request-URI Too Large")
                              (415 . "Unsupported Media Type")
                              (416 . "Unsupported URI Scheme")
                              (420 . "Bad Extension")
                              (421 . "Extension Required")
                              (423 . "Interval Too Brief")
                              (480 . "Temporarily not available")
                              (481 . "Call Leg/Transaction Does Not Exist")
                              (482 . "Loop Detected")
                              (483 . "Too Many Hops")
                              (484 . "Address Incomplete")
                              (485 . "Ambiguous")
                              (486 . "Busy Here")
                              (487 . "Request Terminated")
                              (488 . "Not Acceptable Here")
                              (491 . "Request Pending")
                              (493 . "Undecipherable")
                              (500 . "Internal Server Error")
                              (501 . "Not Implemented")
                              (502 . "Bad Gateway")
                              (503 . "Service Unavailable")
                              (504 . "Server Time-out")
                              (505 . "SIP Version not supported")
                              (513 . "Message Too Large")
                              (600 . "Busy Everywhere")
                              (603 . "Decline")
                              (604 . "Does not exist anywhere")
                              (606 . "Not Acceptable")))

(defun is-status-code (r)
  (if (assoc r +status-codes+) t nil))

(defun status-code-str (r)
  (cdr (assoc r +status-codes+)))

;;; Message classes ----------------------------------------------------

(defclass msg ()
  ((version :initarg :version
            :initform nil
            :reader version)
   (headers :initarg :headers
            :initform nil
            :reader headers)
   (bodies  :initarg  :bodies
            :initform nil
            :reader bodies)))

(defclass response (msg)
  ((status-code :initarg :status-code
                :initform (error "Need a status-code")
                :reader status-code)))

(defmethod print-object ((r response) stream)
  (print-unreadable-object (r stream :identity t :type t)
    (format stream "Status-code: ~a~% Version: ~a~% Headers: ~{~a~}~%"
            (status-code r) (version r) (headers r))))

(defclass request (msg)
  ((method  :initarg :method
            :initform (error "Need a method")
            :reader meth)
   (uri     :initarg :uri
            :reader uri)))

(defmethod print-object ((r request) stream)
  (print-unreadable-object (r stream :identity t :type t)
    (format stream "Method: ~a~% Uri: ~a~% Version: ~a~% Headers: ~{~a~}~%"
            (meth r) (uri r) (version r) (headers r))))

(defmethod has-header ((m msg) header)
  (assoc header (headers m)))

;;; Parsing ------------------------------------------------------------

(define-condition sip-parse-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "SIP Parse Error: ~a" (text condition)))))

(defmacro sip-parse-error (fmt-str &rest args)
  `(error 'sip-parse-error :text (funcall #'format nil ,fmt-str ,@args)))

(defun can-parse-p (fn &rest args)
  "If (fn ..args..) does not throw a sip-parse-error, give (values t (fn ..args..)),
otherwise (values nil <sip-parse-error>)"
  (handler-case (apply fn args)
    (sip-parse-error (e) (values nil e))
    (:no-error (&rest args) (values t args))))

(defun parse-msg (str)
  "Parse the str into the proper msg class"
  (destructuring-bind (msg-lines body) (split-msg str)
    (unless (and msg-lines body)
      (sip-parse-error "Invalid msg -- no blank line included!"))
    (multiple-value-bind (req req-v) (can-parse-p #'parse-uri-line (first msg-lines))
      (cond (req (parse-request msg-lines body))
            (t
             (multiple-value-bind (resp resp-v) (can-parse-p #'parse-status-line (first msg-lines))
               (cond (resp (parse-response msg-lines body))
                     ((and req-v resp-v)
                      (sip-parse-error "Cannot determine if msg is Request or Response"))
                     (req-v (sip-parse-error (text req-v)))
                     (resp-v (sip-parse-error (text resp-v))))))))))

(defun parse-request (msg-lines body)
  (let ((uri-vals (parse-uri-line (first msg-lines)))
        (headers (parse-headers (cdr msg-lines))))
    (make-instance 'request
                   :method  (first uri-vals)
                   :uri     (second uri-vals)
                   :version (third uri-vals)
                   :headers headers
                   :bodies  (parse-bodies body))))

(defun parse-response (msg-lines body)
  (let ((status-vals (parse-status-line (first msg-lines)))
        (headers (parse-headers (cdr msg-lines))))
    (make-instance 'response
                   :status-code (second status-vals)
                   :version (first status-vals)
                   :headers headers
                   :bodies  (parse-bodies body))))

(defun split-msg (str)
  "Return list: (all msg data above the bodies split by CRLF, body section"
  (let ((fields (cl-ppcre:split (format nil "~a~a" +crlf+ +crlf+) str)))
    (if fields
        (list (cl-ppcre:split +crlf+ (first fields)) (second fields))
        nil)))

(defun parse-uri-line (line)
  "Parse the uri line from string; return (method uri version)"
  
  (let ((fields (cl-ppcre:split " +" line)))
    (if (= (length fields) 3)
      (list (parse-method (first fields))
            (parse-uri (second fields))
            (parse-version (third fields)))
      (sip-parse-error "Invalid SIP-URI line: ~a " line))))

(defun parse-status-line (line)
  "Parse first line of response msg: return '(version code reason-phrase)"
  (multiple-value-bind (whole-match fields) (scan-to-strings "([^ ]+)? (\\d{3})? (.+)" line)
    (declare (ignore whole-match))
    (if (= (length fields) 3)
        (list (parse-version (aref fields 0))
              (parse-response-code (aref fields 1))
              (aref fields 2))
        (sip-parse-error "Invalid Status-Line: ~a" line))))

(defun parse-response-code (str)
  (aif (parse-integer str :junk-allowed t)
       (if (is-status-code cl-sip.util:it)
           cl-sip.util:it
           (sip-parse-error "Invalid Status-Code: ~a" str))
       (sip-parse-error "Invalid Status-Code: ~a" str)))

(defun parse-method (m)
  (let ((msym (is-method-name m)))
    (if msym msym (sip-parse-error "Invalid method: ~a" m))))

(defclass sip-uri ()
  ((user-info :initarg :user-info
              :initform nil
              :accessor user-info)
   (hostport  :initarg :hostport
              :initform nil
              :accessor hostport)
   (uri-parms :initarg :uri-parms
              :initform nil
              :accessor uri-parms)
   (headers   :initarg :headers
              :initform nil
              :accessor headers)))

(defmethod print-object ((obj sip-uri) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    (format stream "User-info: ~a; Hostport: ~a; Parms: ~a; Headers: ~a"
            (user-info obj) (hostport obj) (uri-parms obj) (headers obj))))

(defun parse-uri (str)
  "Parse the SIP-URI line into a sip-uri object"
  (multiple-value-bind (whole-match matches) (scan-to-strings "sip:(.*@)([^;]+)(;[^\\?]*)?(\\?(.*))?" str)
    (declare (ignore whole-match))
    (cond
      (matches
       (let ((uri (make-instance 'sip-uri))
             (len (length matches)))
         (when (> len 0) (setf (user-info uri) (string-right-trim '(#\@) (aref matches 0))))
         (when (> len 1) (setf (hostport uri) (aref matches 1)))
         (when (> len 2) (parse-uri-parms uri (string-left-trim '(#\;) (aref matches 2))))
         (when (> len 4) (parse-uri-headers uri (aref matches 4)))
         uri))
      (t (sip-parse-error "Invalid SIP-URI: ~a" str)))))

(defun parse-extended-uri (sip-uri str)
  (let ((fields (split "\\?" str)))
    (when fields
      (parse-uri-parms sip-uri (first fields))
      (when (= (length fields) 2)
        (parse-uri-headers sip-uri (second fields))))
    sip-uri))

(defun parse-uri-parms (sip-uri str)
  "Add any uri-parms in str to sip-uri"
  (let ((fields (split "\;" str))
        (parms nil))
    (when fields
      (dolist (f fields)
        (multiple-value-bind (whole-match matches) (scan-to-strings "(.*)=(.*)" f)
          (declare (ignore whole-match))
          (when (= (length matches) 2)
            (setf parms (acons (aref matches 0) (aref matches 1) parms))))))
    (setf (uri-parms sip-uri) parms)))

(defun parse-uri-headers (sip-uri str)
  "Add any uri headers in str to sip-uri"
  (let ((fields (split "\&" str))
        (parms nil))
    (when fields
      (dolist (f fields)
        (multiple-value-bind (whole-match matches) (scan-to-strings "(.*)=(.*)" f)
          (declare (ignore whole-match))
          (when (= (length matches) 2)
            (setf parms (acons (aref matches 0) (aref matches 1) parms))))))
    (setf (headers sip-uri) parms)))

(defun parse-version (v)
  (if (scan "SIP/\\d\\.\\d" v)
      v
      (sip-parse-error "Invalid SIP-Version: ~a" v)))

(defun parse-bodies (str) str)

(defun parse-header-line (str)
  "Give '(hdr-symbol . hdr-value-string) if given a legal header line, otherwise nil"
  (let ((fields (split ":" str)))
    (cond
      ((and (eql (length fields) 2))
       (let ((hdr (is-header-name (trim-ws (first fields)))))
         (if hdr
             (cons hdr (trim-ws (second fields)))
             (sip-parse-error "Invalid header: ~a" (first fields)))))
      (t nil))))

(defun parse-headers (lines)
  "Return alist of header/header-value pairs.

This function works by successive filterings of lists. In the parse-line pass, the raw
header lines are transformed into an alist of hdr/value pairs. If the header line is a
multiline continuation (starts with whitespace), its hdr symbol becomes 'continuation. In
the multiline-hdr-join pass, the continutations are squashed into their preceeding alist
pairs. In the combino pass, pairs with the same car (same header) are combined with a
comma separating their values."
  (labels ((parse-line (line)
             "Parse line to either nil or '(hdr-symbol . hdr-value) cons"
             (cond
               ((string= line "") nil)
               ((scan "^[\\s+]" line) (cons 'continuation (trim-ws line)))
               (t (parse-header-line line))))
           (multiline-hdr-join (alist y)
             "Squash together cdrs when 2nd cons has car of 'continuation"
             (cond ((atom (car alist)) ; alist is a bare cons, make it a alist and try again
                    (multiline-hdr-join (list alist) y))
                   ((eq (car y) 'continuation)
                    (let* ((hdr-cons (first (last alist))))
                      (append (butlast alist) (list (cons (car hdr-cons) (join-str " " (cdr hdr-cons) (cdr y)))))))
                   (t (append alist (list y)))))
           (combino (lst &optional (acc nil))
             "Combine alist entries with eq cars to have cdrs separated by commas"
             (cond ((null lst) acc)
                   ((assoc (caar lst) acc)
                    (let ((hdr (assoc (caar lst) acc))
                          (newvalue (cdr (car lst))))
                      (rplacd hdr (join-str "," (cdr hdr) newvalue))
                      (combino (cdr lst) acc)))
                   (t (combino (cdr lst) (cons (car lst) acc))))))
    (combino
     (remove-if #'(lambda (x) (eq (car x) 'continuation)) ;; TODO: error if there is a 'continuation still
                (reduce #'multiline-hdr-join (remove-if #'null (mapcar #'parse-line lines)))))))


;;; Testing utils ------------------------------------------------------

(defun build-msg-str (hdr-lst &optional (body-lst nil))
  (declare (ignore body-lst))
  (concatenate 'string
               (reduce #'(lambda (x y) (concatenate 'string x +crlf+ y)) hdr-lst)
               +crlf+
               +crlf+
               "...fake-body..."))
