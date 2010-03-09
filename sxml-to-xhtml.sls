;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapping for Oleg's SXML->HTML code
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
(library (oleg sxml-to-xhtml)
  (export
    sxml->xhtml
    enattr
    entag
    string->goodhtml)
  (import
    (except (rnrs base) error)
    (rnrs io simple)
    (rnrs mutable-pairs)
    (rnrs lists)
    (rnrs unicode)
    (only (srfi :13)
      string-index-right
      string-contains
      string-null?)
    (srfi :23)
    (oleg prelude)
    (oleg sxml-tree-trans)
    (srfi private include))

(include/resolve ("oleg" "ssax" "lib") "util.scm")

(define (sxml->xhtml tree)
 (srv:send-reply
   (pre-post-order tree
                ; Universal transformation rules. Work for every HTML,
                ; present and future
    `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) (enattr attr-key value))))
      . ,(lambda (trigger . value) (cons '@ value)))
     (*default* . ,(lambda (tag . elems) (entag tag elems)))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodhtml str) str)))
 
                ; Handle a nontraditional but convenient top-level element:
                ; (html:begin title <html-body>) element
     (html:begin . ,(lambda (tag title . elems)
        (list "Content-type: text/html"         ; HTTP headers
              nl nl                            ; two nl end the headers
              "<html><head><title>" title "</title></head>"
	      elems
              "</html>"))))
 
     )))

(define (entag tag elems)
  (if (and (pair? elems) (pair? (car elems)) (eq? '@ (caar elems)))
    (list #\newline #\< tag (cdar elems) 
      (if (pair? (cdr elems))
	  (list #\> (cdr elems) "</" tag #\>)
	  " />"))
    (list #\newline #\< tag 
      (if (pair? elems) 
	  (list #\> elems "</" tag #\>)
	  " />"))))
 
(define (enattr attr-key value)
  (if (null? value) (list #\space attr-key)
    (list #\space attr-key "=\"" value #\")))


(define string->goodhtml
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))


)
