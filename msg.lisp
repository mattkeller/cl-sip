;;;; msg.lisp -- SIP msg parsing and constructing

;; Data constructs
;; * Message: type, headers, tags
;; * sip uri
;; * Dialog: session, past msgs
;; * Session: state, user
;; * Response codes: 301, ...
;; * udp endpoint: proxy, port
;; * user: username, domain, passwd, proxy
;; * proxy
;; * timers: t1, t2,

(in-package :cl-sip.msg)

(defvar crlf (format nil "~a~a" #\Return #\Linefeed))

(defvar *methods* (symbol-name-alist '(invite ack options bye cancel register options info)))

(defun is-method (m)
  (if (assoc m *methods*) t nil))

(defun is-method-name (name)
  (let ((s (member name *methods* :key #'cdr :test #'string-equal)))
    (if s (car (car s)) nil)))

(defvar *headers* (symbol-name-alist
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
  (if (assoc h *headers*) h nil))

(defun is-header-name (name)
  (let ((h (member name *headers* :key #'cdr :test #'string-equal)))
    (if h (car (car h)) nil)))

(defvar *responses* '((100 . "Trying")
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

(defun is-response (r)
  (if (assoc r *responses*) t nil))

(defun response-str (r)
  (cdr (assoc r *responses*)))

;; message = request | response

(defclass request ()
  ((method  :initarg :method
            :initform (error "Need a method")
            :reader meth)
   (uri     :initarg :uri
            :reader uri)
   (version :initarg :version
            :initform nil
            :reader version)
   (headers :initarg :headers
            :initform nil
            :reader headers)
   (bodies  :initarg  :bodies
            :initform nil
            :reader bodies)))

(defmethod print-object ((r request) stream)
  (print-unreadable-object (r stream :identity t :type t)
    (format stream "Method: ~a~% Uri: ~a~% Version: ~a~% Headers: ~{~a~}~%"
            (meth r) (uri r) (version r) (headers r))))

(defun parse-request (str)
  (multiple-value-bind (msg-lines body) (split-msg str)
    (when msg-lines
      (let ((uri-vals (parse-uri-line (first msg-lines)))
            (headers (parse-headers (cdr msg-lines))))
        (make-instance 'request
                       :method  (first uri-vals)
                       :uri     (second uri-vals)
                       :version (third uri-vals)
                       :headers headers
                       :bodies  (parse-bodies body))))))

(defmethod has-header ((r request) header)
  (assoc header (headers r)))

(defun split-msg (str)
  "Return values: all msg data above the bodies split by CRLF, body section"
  (let ((fields (cl-ppcre:split (format nil "~a~a" crlf crlf) str)))
    (if fields
        (values (cl-ppcre:split crlf (first fields)) (second fields))
        nil)))

(defun parse-uri-line (line)
  "Parse the uri line from string; return (method uri version)"
  (let ((fields (cl-ppcre:split " +" line)))
    (if (= (length fields) 3)
      (list (parse-method (first fields))
            (parse-uri (second fields))
            (parse-version (third fields)))
      (error "Invalid SIP-URI line: ~a " line))))

(defun parse-method (m)
  (let ((msym (is-method-name m)))
    (if msym msym (error "Invalid method: ~a" m))))

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
      (t (error "Invalid SIP-URI: ~a" str)))))

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
      (error "Invalid SIP-Version: ~a" v)))

(defun parse-bodies (str) str)

(defun parse-headers (lines)
  "Parser header section, return alist of header/header-value pairs; ignore unknown headers"
  (let ((headers nil))
    (dolist (line lines)
      (when (string= line "") (return-from parse-headers headers))
      (let ((fields (split ": " line)))
        (when (and (eql (length fields) 2)
                   (is-header-name (first fields)))
          (setf headers (acons
                         (is-header-name (first fields))
                         (second fields)
                         headers)))))
    headers))


;; 25.1 Basic Rules
;;
;;    The following rules are used throughout this specification to
;;    describe basic parsing constructs.  The US-ASCII coded character set
;;    is defined by ANSI X3.4-1986.
;;
;;       alphanum  =  ALPHA / DIGIT
;;
;;    Several rules are incorporated from RFC 2396 [5] but are updated to
;;    make them compliant with RFC 2234 [10].  These include:
;;
;;       reserved    =  ";" / "/" / "?" / ":" / "@" / "&" / "=" / "+"
;;                      / "$" / ","
;;       unreserved  =  alphanum / mark
;;       mark        =  "-" / "_" / "." / "!" / "~" / "*" / "'"
;;                      / "(" / ")"
;;       escaped     =  "%" HEXDIG HEXDIG
;;
;;    SIP header field values can be folded onto multiple lines if the
;;    continuation line begins with a space or horizontal tab.  All linear
;;    white space, including folding, has the same semantics as SP.  A
;;    recipient MAY replace any linear white space with a single SP before
;;    interpreting the field value or forwarding the message downstream.
;;    This is intended to behave exactly as HTTP/1.1 as described in RFC
;;    2616 [8].  The SWS construct is used when linear white space is
;;    optional, generally between tokens and separators.
;;
;;       LWS  =  [*WSP CRLF] 1*WSP ; linear whitespace
;;       SWS  =  [LWS] ; sep whitespace
;;
;;    To separate the header name from the rest of value, a colon is used,
;;    which, by the above rule, allows whitespace before, but no line
;;    break, and whitespace after, including a linebreak.  The HCOLON
;;    defines this construct.
;;
;;       HCOLON  =  *( SP / HTAB ) ":" SWS
;;
;;    The TEXT-UTF8 rule is only used for descriptive field contents and
;;    values that are not intended to be interpreted by the message parser.
;;    Words of *TEXT-UTF8 contain characters from the UTF-8 charset (RFC
;;    2279 [7]).  The TEXT-UTF8-TRIM rule is used for descriptive field
;;    contents that are n t quoted strings, where leading and trailing LWS
;;    is not meaningful.  In this regard, SIP differs from HTTP, which uses
;;    the ISO 8859-1 character set.
;;
;;       TEXT-UTF8-TRIM  =  1*TEXT-UTF8char *(*LWS TEXT-UTF8char)
;;       TEXT-UTF8char   =  %x21-7E / UTF8-NONASCII
;;       UTF8-NONASCII   =  %xC0-DF 1UTF8-CONT
;;                       /  %xE0-EF 2UTF8-CONT
;;                       /  %xF0-F7 3UTF8-CONT
;;                       /  %xF8-Fb 4UTF8-CONT
;;                       /  %xFC-FD 5UTF8-CONT
;;       UTF8-CONT       =  %x80-BF
;;
;;    A CRLF is allowed in the definition of TEXT-UTF8-TRIM only as part of
;;    a header field continuation.  It is expected that the folding LWS
;;    will be replaced with a single SP before interpretation of the TEXT-
;;    UTF8-TRIM value.
;;
;;    Hexadecimal numeric characters are used in several protocol elements.
;;    Some elements (authentication) force hex alphas to be lower case.
;;
;;       LHEX  =  DIGIT / %x61-66 ;lowercase a-f
;;
;;    Many SIP header field values consist of words separated by LWS or
;;    special characters.  Unless otherwise stated, tokens are case-
;;    insensitive.  These special characters MUST be in a quoted string to
;;    be used within a parameter value.  The word construct is used in
;;    Call-ID to allow most separators to be used.
;;
;;       token       =  1*(alphanum / "-" / "." / "!" / "%" / "*"
;;                      / "_" / "+" / "`" / "'" / "~" )
;;       separators  =  "(" / ")" / "<" / ">" / "@" /
;;                      "," / ";" / ":" / "\" / DQUOTE /
;;                      "/" / "[" / "]" / "?" / "=" /
;;                      "{" / "}" / SP / HTAB
;;       word        =  1*(alphanum / "-" / "." / "!" / "%" / "*" /
;;                      "_" / "+" / "`" / "'" / "~" /
;;                      "(" / ")" / "<" / ">" /
;;                      ":" / "\" / DQUOTE /
;;                      "/" / "[" / "]" / "?" /
;;                      "{" / "}" )
;;
;;    When tokens are used or separators are used between elements,
;;    whitespace is often allowed before or after these characters:
;;
;;       STAR    =  SWS "*" SWS ; asterisk
;;       SLASH   =  SWS "/" SWS ; slash
;;       EQUAL   =  SWS "=" SWS ; equal
;;       LPAREN  =  SWS "(" SWS ; left parenthesis
;;       RPAREN  =  SWS ")" SWS ; right parenthesis
;;       RAQUOT  =  ">" SWS ; right angle quote
;;       LAQUOT  =  SWS "<"; left angle quote
;;       COMMA   =  SWS "," SWS ; comma
;;       SEMI    =  SWS ";" SWS ; semicolon
;;       COLON   =  SWS ":" SWS ; colon
;;       LDQUOT  =  SWS DQUOTE; open double quotation mark
;;       RDQUOT  =  DQUOTE SWS ; close double quotation mark
;;
;;    Comments can be included in some SIP header fields by surrounding the
;;    comment text with parentheses.  Comments are only allowed in fields
;;    containing "comment" as part of their field value definition.  In all
;;    other fields, parentheses are considered part of the field value.
;;
;;       comment  =  LPAREN *(ctext / quoted-pair / comment) RPAREN
;;       ctext    =  %x21-27 / %x2A-5B / %x5D-7E / UTF8-NONASCII
;;                   / LWS
;;
;;    ctext includes all chars except left and right parens and backslash.
;;    A string of text is parsed as a single word if it is quoted using
;;    double-quote marks.  In quoted strings, quotation marks (") and
;;    backslashes (\) need to be escaped.
;;
;;       quoted-string  =  SWS DQUOTE *(qdtext / quoted-pair ) DQUOTE
;;       qdtext         =  LWS / %x21 / %x23-5B / %x5D-7E
;;                         / UTF8-NONASCII
;;
;;    The backslash character ("\") MAY be used as a single-character
;;    quoting mechanism only within quoted-string and comment constructs.
;;    Unlike HTTP/1.1, the characters CR and LF cannot be escaped by this
;;    mechanism to avoid conflict with line folding and header separation.
;;
;; quoted-pair  =  "\" (%x00-09 / %x0B-0C
;;                 / %x0E-7F)
;;
;; SIP-URI          =  "sip:" [ userinfo ] hostport
;;                     uri-parameters [ headers ]
;; SIPS-URI         =  "sips:" [ userinfo ] hostport
;;                     uri-parameters [ headers ]
;; userinfo         =  ( user / telephone-subscriber ) [ ":" password ] "@"
;; user             =  1*( unreserved / escaped / user-unreserved )
;; user-unreserved  =  "&" / "=" / "+" / "$" / "," / ";" / "?" / "/"
;; password         =  *( unreserved / escaped /
;;                     "&" / "=" / "+" / "$" / "," )
;; hostport         =  host [ ":" port ]
;; host             =  hostname / IPv4address / IPv6reference
;; hostname         =  *( domainlabel "." ) toplabel [ "." ]
;; domainlabel      =  alphanum
;;                     / alphanum *( alphanum / "-" ) alphanum
;; toplabel         =  ALPHA / ALPHA *( alphanum / "-" ) alphanum
;;
;; IPv4address    =  1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
;; IPv6reference  =  "[" IPv6address "]"
;; IPv6address    =  hexpart [ ":" IPv4address ]
;; hexpart        =  hexseq / hexseq "::" [ hexseq ] / "::" [ hexseq ]
;; hexseq         =  hex4 *( ":" hex4)
;; hex4           =  1*4HEXDIG
;; port           =  1*DIGIT
;;
;;    The BNF for telephone-subscriber can be found in RFC 2806 [9].  Note,
;;    however, that any characters allowed there that are not allowed in
;;    the user part of the SIP URI MUST be escaped.
;;
;; uri-parameters    =  *( ";" uri-parameter)
;; uri-parameter     =  transport-param / user-param / method-param
;;                      / ttl-param / maddr-param / lr-param / other-param
;; transport-param   =  "transport="
;;                      ( "udp" / "tcp" / "sctp" / "tls"
;;                      / other-transport)
;; other-transport   =  token
;; user-param        =  "user=" ( "phone" / "ip" / other-user)
;; other-user        =  token
;; method-param      =  "method=" Method
;; ttl-param         =  "ttl=" ttl
;; maddr-param       =  "maddr=" host
;; lr-param          =  "lr"
;; other-param       =  pname [ "=" pvalue ]
;; pname             =  1*paramchar
;; pvalue            =  1*paramchar
;; paramchar         =  param-unreserved / unreserved / escaped
;; param-unreserved  =  "[" / "]" / "/" / ":" / "&" / "+" / "$"
;;
;; headers         =  "?" header *( "&" header )
;; header          =  hname "=" hvalue
;; hname           =  1*( hnv-unreserved / unreserved / escaped )
;; hvalue          =  *( hnv-unreserved / unreserved / escaped )
;; hnv-unreserved  =  "[" / "]" / "/" / "?" / ":" / "+" / "$"
;;
;; SIP-message    =  Request / Response
;; Request        =  Request-Line
;;                   *( message-header )
;;                   CRLF
;;                   [ message-body ]
;; Request-Line   =  Method SP Request-URI SP SIP-Version CRLF
;; Request-URI    =  SIP-URI / SIPS-URI / absoluteURI
;; absoluteURI    =  scheme ":" ( hier-part / opaque-part )
;; hier-part      =  ( net-path / abs-path ) [ "?" query ]
;; net-path       =  "//" authority [ abs-path ]
;; abs-path       =  "/" path-segments
;;
;; opaque-part    =  uric-no-slash *uric
;; uric           =  reserved / unreserved / escaped
;; uric-no-slash  =  unreserved / escaped / ";" / "?" / ":" / "@"
;;                   / "&" / "=" / "+" / "$" / ","
;; path-segments  =  segment *( "/" segment )
;; segment        =  *pchar *( ";" param )
;; param          =  *pchar
;; pchar          =  unreserved / escaped /
;;                   ":" / "@" / "&" / "=" / "+" / "$" / ","
;; scheme         =  ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
;; authority      =  srvr / reg-name
;; srvr           =  [ [ userinfo "@" ] hostport ]
;; reg-name       =  1*( unreserved / escaped / "$" / ","
;;                   / ";" / ":" / "@" / "&" / "=" / "+" )
;; query          =  *uric
;; SIP-Version    =  "SIP" "/" 1*DIGIT "." 1*DIGIT
;;
;; message-header  =  (Accept
;;                 /  Accept-Encoding
;;                 /  Accept-Language
;;                 /  Alert-Info
;;                 /  Allow
;;                 /  Authentication-Info
;;                 /  Authorization
;;                 /  Call-ID
;;                 /  Call-Info
;;                 /  Contact
;;                 /  Content-Disposition
;;                 /  Content-Encoding
;;                 /  Content-Language
;;                 /  Content-Length
;;                 /  Content-Type
;;                 /  CSeq
;;                 /  Date
;;                 /  Error-Info
;;                 /  Expires
;;                 /  From
;;                 /  In-Reply-To
;;                 /  Max-Forwards
;;                 /  MIME-Version
;;                 /  Min-Expires
;;                 /  Organization
;;                 /  Priority
;;                 /  Proxy-Authenticate
;;                 /  Proxy-Authorization
;;                 /  Proxy-Require
;;                 /  Record-Route
;;                 /  Reply-To
;;
;;                 /  Require
;;                 /  Retry-After
;;                 /  Route
;;                 /  Server
;;                 /  Subject
;;                 /  Supported
;;                 /  Timestamp
;;                 /  To
;;                 /  Unsupported
;;                 /  User-Agent
;;                 /  Via
;;                 /  Warning
;;                 /  WWW-Authenticate
;;                 /  extension-header) CRLF
;;
;; INVITEm           =  %x49.4E.56.49.54.45 ; INVITE in caps
;; ACKm              =  %x41.43.4B ; ACK in caps
;; OPTIONSm          =  %x4F.50.54.49.4F.4E.53 ; OPTIONS in caps
;; BYEm              =  %x42.59.45 ; BYE in caps
;; CANCELm           =  %x43.41.4E.43.45.4C ; CANCEL in caps
;; REGISTERm         =  %x52.45.47.49.53.54.45.52 ; REGISTER in caps
;; Method            =  INVITEm / ACKm / OPTIONSm / BYEm
;;                      / CANCELm / REGISTERm
;;                      / extension-method
;; extension-method  =  token
;; Response          =  Status-Line
;;                      *( message-header )
;;                      CRLF
;;                      [ message-body ]
;;
;; Status-Line     =  SIP-Version SP Status-Code SP Reason-Phrase CRLF
;; Status-Code     =  Informational
;;                /   Redirection
;;                /   Success
;;                /   Client-Error
;;                /   Server-Error
;;                /   Global-Failure
;;                /   extension-code
;; extension-code  =  3DIGIT
;; Reason-Phrase   =  *(reserved / unreserved / escaped
;;                    / UTF8-NONASCII / UTF8-CONT / SP / HTAB)
;;
;; Informational  =  "100"  ;  Trying
;;               /   "180"  ;  Ringing
;;               /   "181"  ;  Call Is Being Forwarded
;;               /   "182"  ;  Queued
;;               /   "183"  ;  Session Progress
;;
;; Success  =  "200"  ;  OK
;;
;; Redirection  =  "300"  ;  Multiple Choices
;;             /   "301"  ;  Moved Permanently
;;             /   "302"  ;  Moved Temporarily
;;             /   "305"  ;  Use Proxy
;;             /   "380"  ;  Alternative Service
;;
;; Client-Error  =  "400"  ;  Bad Request
;;              /   "401"  ;  Unauthorized
;;              /   "402"  ;  Payment Required
;;              /   "403"  ;  Forbidden
;;              /   "404"  ;  Not Found
;;              /   "405"  ;  Method Not Allowed
;;              /   "406"  ;  Not Acceptable
;;              /   "407"  ;  Proxy Authentication Required
;;              /   "408"  ;  Request Timeout
;;              /   "410"  ;  Gone
;;              /   "413"  ;  Request Entity Too Large
;;              /   "414"  ;  Request-URI Too Large
;;              /   "415"  ;  Unsupported Media Type
;;              /   "416"  ;  Unsupported URI Scheme
;;              /   "420"  ;  Bad Extension
;;              /   "421"  ;  Extension Required
;;              /   "423"  ;  Interval Too Brief
;;              /   "480"  ;  Temporarily not available
;;              /   "481"  ;  Call Leg/Transaction Does Not Exist
;;              /   "482"  ;  Loop Detected
;;              /   "483"  ;  Too Many Hops
;;              /   "484"  ;  Address Incomplete
;;              /   "485"  ;  Ambiguous
;;              /   "486"  ;  Busy Here
;;              /   "487"  ;  Request Terminated
;;              /   "488"  ;  Not Acceptable Here
;;              /   "491"  ;  Request Pending
;;              /   "493"  ;  Undecipherable
;;
;; Server-Error  =  "500"  ;  Internal Server Error
;;              /   "501"  ;  Not Implemented
;;              /   "502"  ;  Bad Gateway
;;              /   "503"  ;  Service Unavailable
;;              /   "504"  ;  Server Time-out
;;              /   "505"  ;  SIP Version not supported
;;              /   "513"  ;  Message Too Large
;;
;; Global-Failure  =  "600"  ;  Busy Everywhere
;;                /   "603"  ;  Decline
;;                /   "604"  ;  Does not exist anywhere
;;                /   "606"  ;  Not Acceptable
;;
;; Accept         =  "Accept" HCOLON
;;                    [ accept-range *(COMMA accept-range) ]
;; accept-range   =  media-range *(SEMI accept-param)
;; media-range    =  ( "*/*"
;;                   / ( m-type SLASH "*" )
;;                   / ( m-type SLASH m-subtype )
;;                   ) *( SEMI m-parameter )
;; accept-param   =  ("q" EQUAL qvalue) / generic-param
;; qvalue         =  ( "0" [ "." 0*3DIGIT ] )
;;                   / ( "1" [ "." 0*3("0") ] )
;; generic-param  =  token [ EQUAL gen-value ]
;; gen-value      =  token / host / quoted-string
;;
;; Accept-Encoding  =  "Accept-Encoding" HCOLON
;;                      [ encoding *(COMMA encoding) ]
;; encoding         =  codings *(SEMI accept-param)
;; codings          =  content-coding / "*"
;; content-coding   =  token
;;
;; Accept-Language  =  "Accept-Language" HCOLON
;;                      [ language *(COMMA language) ]
;; language         =  language-range *(SEMI accept-param)
;; language-range   =  ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) / "*" )
;;
;; Alert-Info   =  "Alert-Info" HCOLON alert-param *(COMMA alert-param)
;; alert-param  =  LAQUOT absoluteURI RAQUOT *( SEMI generic-param )
;;
;; Allow  =  "Allow" HCOLON [Method *(COMMA Method)]
;;
;; Authorization     =  "Authorization" HCOLON credentials
;; credentials       =  ("Digest" LWS digest-response)
;;                      / other-response
;; digest-response   =  dig-resp *(COMMA dig-resp)
;; dig-resp          =  username / realm / nonce / digest-uri
;;                       / dresponse / algorithm / cnonce
;;                       / opaque / message-qop
;;                       / nonce-count / auth-param
;; username          =  "username" EQUAL username-value
;; username-value    =  quoted-string
;; digest-uri        =  "uri" EQUAL LDQUOT digest-uri-value RDQUOT
;; digest-uri-value  =  rquest-uri ; Equal to request-uri as specified
;;                      by HTTP/1.1
;; message-qop       =  "qop" EQUAL qop-value
;;
;; cnonce            =  "cnonce" EQUAL cnonce-value
;; cnonce-value      =  nonce-value
;; nonce-count       =  "nc" EQUAL nc-value
;; nc-value          =  8LHEX
;; dresponse         =  "response" EQUAL request-digest
;; request-digest    =  LDQUOT 32LHEX RDQUOT
;; auth-param        =  auth-param-name EQUAL
;;                      ( token / quoted-string )
;; auth-param-name   =  token
;; other-response    =  auth-scheme LWS auth-param
;;                      *(COMMA auth-param)
;; auth-scheme       =  token
;;
;; Authentication-Info  =  "Authentication-Info" HCOLON ainfo
;;                         *(COMMA ainfo)
;; ainfo                =  nextnonce / message-qop
;;                          / response-auth / cnonce
;;                          / nonce-count
;; nextnonce            =  "nextnonce" EQUAL nonce-value
;; response-auth        =  "rspauth" EQUAL response-digest
;; response-digest      =  LDQUOT *LHEX RDQUOT
;;
;; Call-ID  =  ( "Call-ID" / "i" ) HCOLON callid
;; callid   =  word [ "@" word ]
;;
;; Call-Info   =  "Call-Info" HCOLON info *(COMMA info)
;; info        =  LAQUOT absoluteURI RAQUOT *( SEMI info-param)
;; info-param  =  ( "purpose" EQUAL ( "icon" / "info"
;;                / "card" / token ) ) / generic-param
;;
;; Contact        =  ("Contact" / "m" ) HCOLON
;;                   ( STAR / (contact-param *(COMMA contact-param)))
;; contact-param  =  (name-addr / addr-spec) *(SEMI contact-params)
;; name-addr      =  [ display-name ] LAQUOT addr-spec RAQUOT
;; addr-spec      =  SIP-URI / SIPS-URI / absoluteURI
;; display-name   =  *(token LWS)/ quoted-string
;;
;; contact-params     =  c-p-q / c-p-expires
;;                       / contact-extension
;; c-p-q              =  "q" EQUAL qvalue
;; c-p-expires        =  "expires" EQUAL delta-seconds
;; contact-extension  =  generic-param
;; delta-seconds      =  1*DIGIT
;;
;; Content-Disposition   =  "Content-Disposition" HCOLON
;;                          disp-type *( SEMI disp-param )
;; disp-type             =  "render" / "session" / "icon" / "alert"
;;                          / disp-extension-token
;;
;; disp-param            =  handling-param / generic-param
;; handling-param        =  "handling" EQUAL
;;                          ( "optional" / "required"
;;                          / other-handling )
;; other-handling        =  token
;; disp-extension-token  =  token
;;
;; Content-Encoding  =  ( "Content-Encoding" / "e" ) HCOLON
;;                      content-coding *(COMMA content-coding)
;;
;; Content-Language  =  "Content-Language" HCOLON
;;                      language-tag *(COMMA language-tag)
;; language-tag      =  primary-tag *( "-" subtag )
;; primary-tag       =  1*8ALPHA
;; subtag            =  1*8ALPHA
;;
;; Content-Length  =  ( "Content-Length" / "l" ) HCOLON 1*DIGIT
;; Content-Type     =  ( "Content-Type" / "c" ) HCOLON media-type
;; media-type       =  m-type SLASH m-subtype *(SEMI m-parameter)
;; m-type           =  discrete-type / composite-type
;; discrete-type    =  "text" / "image" / "audio" / "video"
;;                     / "application" / extension-token
;; composite-type   =  "message" / "multipart" / extension-token
;; extension-token  =  ietf-token / x-token
;; ietf-token       =  token
;; x-token          =  "x-" token
;; m-subtype        =  extension-token / iana-token
;; iana-token       =  token
;; m-parameter      =  m-attribute EQUAL m-value
;; m-attribute      =  token
;; m-value          =  token / quoted-string
;;
;; CSeq  =  "CSeq" HCOLON 1*DIGIT LWS Method
;;
;; Date          =  "Date" HCOLON SIP-date
;; SIP-date      =  rfc1123-date
;; rfc1123-date  =  wkday "," SP date1 SP time SP "GMT"
;; date1         =  2DIGIT SP month SP 4DIGIT
;;                  ; day month year (e.g., 02 Jun 1982)
;; time          =  2DIGIT ":" 2DIGIT ":" 2DIGIT
;;                  ; 00:00:00 - 23:59:59
;; wkday         =  "Mon" / "Tue" / "Wed"
;;                  / "Thu" / "Fri" / "Sat" / "Sun"
;; month         =  "Jan" / "Feb" / "Mar" / "Apr"
;;                  / "May" / "Jun" / "Jul" / "Aug"
;;                  / "Sep" / "Oct" / "Nov" / "Dec"
;;
;; Error-Info  =  "Error-Info" HCOLON error-uri *(COMMA error-uri)
;;
;; error-uri   =  LAQUOT absoluteURI RAQUOT *( SEMI generic-param )
;;
;; Expires     =  "Expires" HCOLON delta-seconds
;; From        =  ( "From" / "f" ) HCOLON from-spec
;; from-spec   =  ( name-addr / addr-spec )
;;                *( SEMI from-param )
;; from-param  =  tag-param / generic-param
;; tag-param   =  "tag" EQUAL token
;;
;; In-Reply-To  =  "In-Reply-To" HCOLON callid *(COMMA callid)
;;
;; Max-Forwards  =  "Max-Forwards" HCOLON 1*DIGIT
;;
;; MIME-Version  =  "MIME-Version" HCOLON 1*DIGIT "." 1*DIGIT
;;
;; Min-Expires  =  "Min-Expires" HCOLON delta-seconds
;;
;; Organization  =  "Organization" HCOLON [TEXT-UTF8-TRIM]
;;
;; Priority        =  "Priority" HCOLON priority-value
;; priority-value  =  "emergency" / "urgent" / "normal"
;;                    / "non-urgent" / other-priority
;; other-priority  =  token
;;
;; Proxy-Authenticate  =  "Proxy-Authenticate" HCOLON challenge
;; challenge           =  ("Digest" LWS digest-cln *(COMMA digest-cln))
;;                        / other-challenge
;; other-challenge     =  auth-scheme LWS auth-param
;;                        *(COMMA auth-param)
;; digest-cln          =  realm / domain / nonce
;;                         / opaque / stale / algorithm
;;                         / qop-options / auth-param
;; realm               =  "realm" EQUAL realm-value
;; realm-value         =  quoted-string
;; domain              =  "domain" EQUAL LDQUOT URI
;;                        *( 1*SP URI ) RDQUOT
;; URI                 =  absoluteURI / abs-path
;; nonce               =  "nonce" EQUAL nonce-value
;; nonce-value         =  quoted-string
;; opaque              =  "opaque" EQUAL quoted-string
;; stale               =  "stale" EQUAL ( "true" / "false" )
;; algorithm           =  "algorithm" EQUAL ( "MD5" / "MD5-sess"
;;                        / token )
;; qop-options         =  "qop" EQUAL LDQUOT qop-value
;;                        *("," qop-value) RDQUOT
;; qop-value           =  "auth" / "auth-int" / token
;;
;; Proxy-Authorization  =  "Proxy-Authorization" HCOLON credentials
;;
;; Proxy-Require  =  "Proxy-Require" HCOLON option-tag
;;                   *(COMMA option-tag)
;; option-tag     =  token
;;
;; Record-Route  =  "Record-Route" HCOLON rec-route *(COMMA rec-route)
;; rec-route     =  name-addr *( SEMI rr-param )
;; rr-param      =  generic-param
;;
;; Reply-To      =  "Reply-To" HCOLON rplyto-spec
;; rplyto-spec   =  ( name-addr / addr-spec )
;;                  *( SEMI rplyto-param )
;; rplyto-param  =  generic-param
;; Require       =  "Require" HCOLON option-tag *(COMMA option-tag)
;;
;; Retry-After  =  "Retry-After" HCOLON delta-seconds
;;                 [ comment ] *( SEMI retry-param )
;;
;; retry-param  =  ("duration" EQUAL delta-seconds)
;;                 / generic-param
;;
;; Route        =  "Route" HCOLON route-param *(COMMA route-param)
;; route-param  =  name-addr *( SEMI rr-param )
;;
;; Server           =  "Server" HCOLON server-val *(LWS server-val)
;; server-val       =  product / comment
;; product          =  token [SLASH product-version]
;; product-version  =  token
;;
;; Subject  =  ( "Subject" / "s" ) HCOLON [TEXT-UTF8-TRIM]
;;
;; Supported  =  ( "Supported" / "k" ) HCOLON
;;               [option-tag *(COMMA option-tag)]
;;
;; Timestamp  =  "Timestamp" HCOLON 1*(DIGIT)
;;                [ "." *(DIGIT) ] [ LWS delay ]
;; delay      =  *(DIGIT) [ "." *(DIGIT) ]
;;
;; To        =  ( "To" / "t" ) HCOLON ( name-addr
;;              / addr-spec ) *( SEMI to-param )
;; to-param  =  tag-param / generic-param
;;
;; Unsupported  =  "Unsupported" HCOLON option-tag *(COMMA option-tag)
;; User-Agent  =  "User-Agent" HCOLON server-val *(LWS server-val)
;;
;; Via               =  ( "Via" / "v" ) HCOLON via-parm *(COMMA via-parm)
;; via-parm          =  sent-protocol LWS sent-by *( SEMI via-params )
;; via-params        =  via-ttl / via-maddr
;;                      / via-received / via-branch
;;                      / via-extension
;; via-ttl           =  "ttl" EQUAL ttl
;; via-maddr         =  "maddr" EQUAL host
;; via-received      =  "received" EQUAL (IPv4address / IPv6address)
;; via-branch        =  "branch" EQUAL token
;; via-extension     =  generic-param
;; sent-protocol     =  protocol-name SLASH protocol-version
;;                      SLASH transport
;; protocol-name     =  "SIP" / token
;; protocol-version  =  token
;; transport         =  "UDP" / "TCP" / "TLS" / "SCTP"
;;                      / other-transport
;; sent-by           =  host [ COLON port ]
;; ttl               =  1*3DIGIT ; 0 to 255
;;
;; Warning        =  "Warning" HCOLON warning-value *(COMMA warning-value)
;; warning-value  =  warn-code SP warn-agent SP warn-text
;; warn-code      =  3DIGIT
;; warn-agent     =  hostport / pseudonym
;;                   ;  the name or pseudonym of the server adding
;;                   ;  the Warning header, for use in debugging
;; warn-text      =  quoted-string
;; pseudonym      =  token
;;
;; WWW-Authenticate  =  "WWW-Authenticate" HCOLON challenge
;;
;; extension-header  =  header-name HCOLON header-value
;; header-name       =  token
;; header-value      =  *(TEXT-UTF8char / UTF8-CONT / LWS)
;; message-body  =  *OCTET
