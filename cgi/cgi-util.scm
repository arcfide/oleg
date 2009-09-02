;------------------------------------------------------------------------
;       A collection of several procedures for common CGI processing
;
; IMPORT
; A prelude appropriate for your Scheme system
;	(myenv-bigloo.scm, myenv-mit.scm, etc.)
; util.scm 
; input-parse.scm
; SFRI-12 (see srfi-12.scm) needed in CGI:exception-handler
; OS:getenv, which on most systems can be defined simply as
;            (define OS:getenv getenv)
;
; $Id: cgi-util.scm,v 3.4 2002/09/30 22:38:39 oleg Exp oleg $


; Parse the string of URL parameters, the "QUERY_STRING"
; That is, convert a string like
;	"parm1=val1&parm2=val2+val%253+val4&%7Eparm3=&parm4"
; into an assoc list
;	'((parm4) (~parm3 "")
;         (parm2 "val2" "val%3" "val4")
;         (parm1 "val1"))
;
; Parsing is done by a finite state machine, that takes into
; account the current state (looking for a parm, looking for a value,
; looking for a continuation of parm/value after %xx quoted-char),
; the action-prefix ('=', '+', '&' or '/') and the token that follows
; the action prefix (that is, a set of characters through the next
; action-prefix or the end-of-string). At the very beginning, the
; action-prefix is assumed '&'. Note, we assume '/' separator to
; act exactly like '&' (so we can use the present function to
; parse both SCRIPT_PATH and QUERY_STRINGs)
;
; test
; (CGI:url-unquote "aaa")
; (CGI:url-unquote "aaa=")
; (CGI:url-unquote "aaa=&bbb=b1&ccc=c1+c2&ddd")
; (CGI:url-unquote "aaa=/bbb=b1/ccc=c1+c2/ddd")
; (CGI:url-unquote "%7eaaa=/%25b%25bb=b%201/c%20c%7E=c1+c2/ddd%21")
;
; Also note that we skip empty string tokens, such as the leading space before
; val1 and the extra space between val1 and val2 in the following string:
;	"parm=+val1++val2+&parm2"
; That means that leading spaces from parameter values are trimmed.

(define (CGI:url-unquote parm-string)
  (let ((result '())
        (read-primitive-token
          (lambda ()
            (if (eof-object? (peek-char)) ""
              (next-token '() '(#\= #\+ #\& #\% *eof*) "URL-unquoting")))))
            
    (with-input-from-string parm-string
      (lambda ()
        (do ((action-prefix #\& (read-char)) (status 'init)
             (vals '()) (keyword #f))
           ((eq? status 'stop) result)
           
           (let
             ((token (read-primitive-token)))

             	; If #\% left on stream, read it and the following
             	; two characters (hex digits), unquote the char and
             	; append it to the rest of the token
             (do () ((not (eq? (peek-char) #\%)))
               (read-char)		; read the percent char
               (let ((quoted-char-str (make-string 2)))
                 (string-set! quoted-char-str 0 (read-char))
                 (string-set! quoted-char-str 1 (read-char))
                 (let ((quoted-char (string->number quoted-char-str 16)))
                   (set! token
                     (string-append token 
                       (if quoted-char (string (integer->char quoted-char))
                         "*INVALID-%-SEQ*")
                       (read-primitive-token))))))
             
             (if (eof-object? action-prefix)
               (set! action-prefix '*eof*))
             (set! status
               (case action-prefix 
                 ((#\& *eof* #\/)		; new parmset to follow
                   (case status
                     ((init) #t)
                     ((have-read-keyword)	; parm without any values
                       (set! result (cons (list keyword) result)))
                     ((have-read-value)
                       (set! result (cons (cons keyword (reverse vals)) result)))
                     (else (error "unexpected status " status)))
                   (set! keyword (string->symbol token))
                   (if (eq? action-prefix '*eof*) 'stop 'have-read-keyword))
                 ((#\=)
                   (case status
                     ((have-read-keyword)  ; the first value after the keyword
                       (set! vals (list token))
                       'have-read-value)
                     ((have-read-value)
                       (error "= unexpected after the first value"))
                     (else (error "unexpected status " status))))
                 ((#\+)
                   (case status
                     ((have-read-keyword)
                       (error "+ unexpected after a keyword"))
                     ((have-read-value)		; other values after the keyword
                       (if (equal? (car vals) "")
                         (set-car! vals token)	; if the previous token was empty, ditch it
                         (set! vals (cons token vals)))
                         'have-read-value)
                     (else (error "unexpected status " status))))
                 (else (error "unexpected action-prefix " action-prefix))))))))))


;------------------------------------------------------------------------
;	CGI:lookup NAME TYPE [DEFAULT-VALUE]
;
; Lookup a NAME among CGI variables. NAME is a symbol. Convert the result
; to the TYPE. If the NAME is not CGI-bound and the DEFAULT-VALUE is
; specified, return the DEFAULT-VALUE. Otherwise, signal an error.
; An error is signalled also when the conversion to the TYPE fails.
; TYPE is a symbol, one of the following: token, tokens, 
; string, io, int, number
;
; If the QUERY_STRING or a POST message contain 
;	param=
; we assume that CGI parameter 'param is not specified. That is,
; in QUERY_STRING, "param=" is equivalent to the empty string.
;
; if the QUERY_STRING or a POST message contain
;	param
; then (CGI:lookup 'param 'token) returns #f.
;      (CGI:lookup 'param 'tokens) returns '()
;      (CGI:lookup 'param 'string) returns ""
;      (CGI:lookup 'param 'io) returns '("")
;      (CGI:lookup 'param 'int) and (CGI:lookup 'param 'number) fail.
;
; If the QUERY_STRING says
;	param=val
; then (CGI:lookup 'param 'token) returns "val"
;      (CGI:lookup 'param 'tokens) returns '("val")
;      (CGI:lookup 'param 'string) returns "val"
;      (CGI:lookup 'param 'io) returns '("val")
;      (CGI:lookup 'param 'int) and (CGI:lookup 'param 'number) 
;      try to convert "val" to a number or an integer, and raise the exception
;      upon the failure.
;      
; If the QUERY_STRING says
;	param=val1+val2
; or
;	param=val1
;	param=val2
; (as 'param may be mentioned several times, for example, if it's a name
; of a check-box or a multiple-choice option)
; then (CGI:lookup 'param 'token) returns "val1"
;      (CGI:lookup 'param 'tokens) returns '("val1" "val2")
;      (CGI:lookup 'param 'string) returns "val1 val2"
;      (CGI:lookup 'param 'io) returns '("val" " " "val2")
;      (CGI:lookup 'param 'int) and (CGI:lookup 'param 'number) fail.
;
;
; There are special 'predefined' names that can be looked up:
; 	'query-string - the return value is the query-string
;			as a string
;	'query-parms  - an assoc list of query parameters, or '()
;	'self-url     - (demand-env-var "SCRIPT_NAME")
; for them, the TYPE argument must be 'token

(define CGI:lookup
  (let ((query-string #f) (query-parms #f))

		; alist is an assoc list: '((parm1 val1) (parm2 val2)...)
		; The list may contain duplicate parm's. In that case,
		; they are merged.
		; The result is an assoc list without any duplicates.
    (define (consolidate-duplicates alist)
      (let loop ((old-l alist) (new-l '()))
        (cond
          ((null? old-l) new-l)
          ((assq (caar old-l) new-l) =>
            (lambda (duplicate-assoc)
              (let last-link-loop ((link duplicate-assoc))
                (if (null? (cdr link))
                  (set-cdr! link (cdar old-l))
                  (last-link-loop (cdr link))))
              (loop (cdr old-l) new-l)))
          (else
            (loop (cdr old-l) (cons (car old-l) new-l))))))

		; Coerce the values to the correct type
    (define (coerce type name . vals)
      (case type
	((token)
	 (if (null? vals) #f (car vals)))
	((tokens) vals)
	((string)
	 (cond
	  ((null? vals) "")
	  ((null? (cdr vals)) (car vals))
	  (else
	   (apply string-append (list-intersperse vals " ")))))
	((io)
	 (cond
	  ((null? vals) '(""))
	  ((null? (cdr vals)) vals)
	  (else (list-intersperse vals " "))))
	((int number)
	 (let* ((val-raw (and (pair? vals) (null? (cdr vals)) (car vals)))
		(val-c (if (eq? type 'int)
			   (string->integer val-raw 0 (string-length val-raw))
			   (string->number val-raw))))
	   (or val-c
	       (abort
		(make-property-condition 'cgi-type-error
					 'name name
					 'type type)))))
	(else
	 (error "invalid type: " type))))


		; The lookup procedure itself
    (lambda (name type . default-value-l)
      
      (whennot query-string
        (set! query-string (CGI:get-query-string))
        (set! query-parms
	      (consolidate-duplicates
	       (reverse (CGI:url-unquote query-string)))))

			; Handle special names first
      (case name
        ((query-string) (assert (eq? type 'token)) query-string)
        ((query-parms)  (assert (eq? type 'token)) query-parms)
	((self-url)	(assert (eq? type 'token)) 
			(demand-env-var "SCRIPT_NAME"))
        (else
	 (let ((found-assoc (assq name query-parms)))
	   (cond
	    ((and found-assoc (not (equal? '("") (cdr found-assoc))))
	     (apply coerce (cons type found-assoc)))
	    ((null? default-value-l)
	     (error "CGI name " name
		    " was not found among form parameters: " query-parms))
	    (else (car default-value-l))))))
      )))


;------------------------------------------------------------------------
;		    Miscellaneous CGI-related code

; The exception handler for the CGI code
; SRFI-12 is assumed
(define (CGI:exception-handler exn)
  (let ((err-msgs
	 (if ((condition-predicate 'exn) exn)
	     (list "Runtime error:"
		   ((condition-property-accessor 'exn 'message) exn))
	     (list "Other exception:" exn))))
    (cerr err-msgs nl)
    (cout "Content-type: text/html" nl nl	; TWO nl are very important
      "<html><head><title>Application Server Error</title></head><body>" nl
      "<h1>Script " (OS:getenv "SCRIPT_NAME") " has detected a problem</h1>" nl
      "<P><b>" err-msgs "</b>" nl
      "</body></html>")
    (exit)))

; Get the name of the authenticated remote user, or '*'
; If the name of the remote user is set, the web server has already
; verified the user's credentials.
; This also means that the remote user name does not contain any
; funny and dangerous characters like quotes and newlines.
(define (CGI:get-remote-user-name)
  (or (OS:getenv "REMOTE_USER")
      (OS:getenv "SSL_CLIENT_S_DN")
      "*"))

(define (demand-env-var var-name)
  (or (OS:getenv var-name)
      (error (string-append var-name " env parameter expected"))))

(define (CGI:get-query-string)
  (if (string=? "POST" (demand-env-var "REQUEST_METHOD"))
      (let ((post-message-len 
	     (string->number (demand-env-var "CONTENT_LENGTH"))))
	(assert (string-ci=? "application/x-www-form-urlencoded"
			     (demand-env-var "CONTENT_TYPE")))
	(let ((post-message (read-string post-message-len
					 (current-input-port))))
	  (assert (= post-message-len (string-length post-message)))
	  post-message))
      (or (OS:getenv "QUERY_STRING") "")))

