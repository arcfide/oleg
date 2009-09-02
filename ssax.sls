;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapper for Oleg's SSAX Code
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

(library (oleg ssax)
  (export
    xml-token? xml-token-kind xml-token-head
    make-empty-attlist attlist-add
    attlist-null?
    attlist-remove-top
    attlist->alist attlist-fold
    ssax:uri-string->symbol
    ssax:skip-internal-dtd
    ssax:read-pi-body-as-string
    ssax:reverse-collect-str-drop-ws
    ssax:read-markup-token
    ssax:read-cdata-body
    ssax:read-char-ref
    ssax:read-attributes
    ssax:complete-start-tag
    ssax:read-external-id
    ssax:read-char-data
    ssax:make-parser
    ssax:xml->sxml)
  (import
    (rnrs base)
    (rnrs unicode)
    (rnrs io simple)
    (except (rnrs lists) fold-right)
    (rnrs control)
    (rnrs mutable-strings)
    (srfi :0)
    (only (srfi :13) 
      string-concatenate-reverse
      string-index
      string-concatenate/shared
      string-null?
      string-concatenate-reverse/shared)
    (oleg prelude)
    (only (scheme) warning)
    (srfi private include))

(define parser-error
  (lambda (port message . rest)
    (apply error `(ssax-parser ,message ,@rest))))

(define ssax:warn
  (lambda (port message . irritants)
    (apply warning `(ssax ,message ,@irritants))))

(include/resolve ("oleg" "ssax" "lib") "look-for-str.scm")
(include/resolve ("oleg" "ssax" "lib") "char-encoding.scm")
(include/resolve ("oleg" "ssax" "lib") "input-parse.scm")
(include/resolve ("oleg" "ssax" "lib") "SSAX-code.scm")

)
