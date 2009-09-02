;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prelude for Oleg's code for Chez Scheme 8.0
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

#!chezscheme

(library (oleg prelude)
  (export 
    inc dec -- ++
    begin0
    nl
    call-with-input-string
    cerr cout
    and-let*
    pp
    define-opt optional
    whennot)
  (import (rename (scheme) (unless whennot)))

(define inc 1+)
(define dec 1-)
(define -- 1-)
(define ++ 1+)

(define-syntax begin0
  (syntax-rules ()
    ((begin0 form form1 ... ) 
      (let ((val form)) form1 ... val))))

(define nl (string #\newline))

(define call-with-input-string
  (lambda (str proc)
    (let* ([ip (open-input-string str)]
           [val (proc ip)])
      (close-port ip)
      val)))

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x) (x (console-output-port))
		(display x (console-output-port))))
            args))

(define-syntax and-let*
  (syntax-rules ()
    ((_ ()) #t)
    ((_ claws)    ; no body
       ; re-write (and-let* ((claw ... last-claw)) ) into
       ; (and-let* ((claw ...)) body) with 'body' derived from the last-claw
     (and-let* "search-last-claw" () claws))
    ((_ "search-last-claw" first-claws ((exp)))
     (and-let* first-claws exp))	; (and-let* (... (exp)) )
    ((_ "search-last-claw" first-claws ((var exp)))
     (and-let* first-claws exp))	; (and-let* (... (var exp)) )
    ((_ "search-last-claw" first-claws (var))
     (and-let* first-claws var))	; (and-let* (... var) )
    ((_ "search-last-claw" (first-claw ...) (claw . rest))
     (and-let* "search-last-claw" (first-claw ... claw) rest))
    
    ; now 'body' is present
    ((_ () . body) (begin . body))	; (and-let* () form ...)
    ((_ ((exp) . claws) . body)		; (and-let* ( (exp) claw... ) body ...)
     (and exp (and-let* claws . body)))
    ((_ ((var exp) . claws) . body)	; (and-let* ((var exp) claw...)body...)
     (let ((var exp)) (and var (and-let* claws . body))))
    ((_ (var . claws) . body)		; (and-let* ( var claw... ) body ...)
     (and var (and-let* claws . body)))
))

(define pp pretty-print)

(define-syntax optional
  (lambda (x)
    (error #f "misplaced aux keyword ~a" (syntax->datum x))))

(define-syntax define-opt
  (syntax-rules (optional)
    ((define-opt (name . bindings) . bodies)
      (define-opt "seek-optional" bindings () ((name . bindings) . bodies)))

    ((define-opt "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
	(letrec-syntax
	  ((handle-opts
	     (syntax-rules ()
	       ((_ rest bodies (var init))
		 (let ((var (if (null? rest) init
			      (if (null? (cdr rest)) (car rest)
				(error "extra rest" rest)))))
		   . bodies))
	       ((_ rest bodies var) (handle-opts rest bodies (var #f)))
	       ((_ rest bodies (var init) . other-vars)
		 (let ((var (if (null? rest) init (car rest)))
		       (new-rest (if (null? rest) '() (cdr rest))))
		   (handle-opts new-rest bodies . other-vars)))
	       ((_ rest bodies var . other-vars)
		 (handle-opts rest bodies (var #f) . other-vars))
	       ((_ rest bodies)		; no optional args, unlikely
		 (let ((_ (or (null? rest) (error "extra rest" rest))))
		   . bodies)))))
	  (handle-opts _rest _bodies . _opt-bindings))))

    ((define-opt "seek-optional" (x . rest) (reqd ...) form)
      (define-opt "seek-optional" rest (reqd ... x) form))

    ((define-opt "seek-optional" not-a-pair reqd form)
      (define . form))			; No optional found, regular define

    ((define-opt name body)		; Just the definition for 'name',
      (define name body))		; for compatibilibility with define
))

)