;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Library for Oleg's CGI Processing
;;; 
;;; Copyright (c) 2009 Aaron W. Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(library (oleg cgi processing)
  (export 
    cgi:url-unquote 
    cgi:lookup 
    cgi:get-remote-user-name
    cgi:get-query-string
    (rename (cgi:handle-exception cgi:exception-handler)))
  (import 
    (except (rnrs base) error)
    (rnrs exceptions)
    (rnrs conditions)
    (rnrs programs)
    (rnrs io simple)
    (rnrs mutable-pairs)
    (rnrs unicode)
    (rnrs lists)
    (rnrs control)
    (rnrs mutable-strings)
    (rename 
      (only (scheme) 
        getenv 
        with-input-from-string 
        with-output-to-string
        display-condition)
      (getenv os:getenv))
    (only (srfi :13) 
      string-index-right 
      string-contains
      string-null?
      string-concatenate-reverse)
    (oleg sxml-to-html)
    (oleg prelude)
    (srfi :23)
    (srfi :0)
    (srfi private include))

(define parser-error
  (lambda (port msg . args)
    (apply error (cons msg args))))

(define abort
  (lambda (condition)
    (raise condition)))

(define make-property-condition
  (lambda (type . props)
    (condition
      (make-error)
      (make-who-condition type)
      (make-irritants-condition props))))

(define condition-property-accessor
  (lambda args
    (raise (make-implementation-restriction-violation))))

(include/resolve-ci ("oleg" "ssax" "lib") "char-encoding.scm")
(include/resolve-ci ("oleg" "ssax" "lib") "util.scm")
(include/resolve-ci ("oleg" "ssax" "lib") "input-parse.scm")
(include/resolve-ci ("oleg" "cgi") "cgi-util.scm")

(define cgi:handle-exception
  (lambda (c)
    (display "Content-type: text/html")
    (newline) (newline)
    (sxml->html
      `(html
         (head (title "Application Server Error"))
         (body
           (h1 "Error!")
           (p "Script " ,(os:getenv "SCRIPT_NAME")
             " has detected a problem.")
           (p 
             (strong 
               ,(with-output-to-string 
                  (lambda () (display-condition c))))))))
    (exit)))

)