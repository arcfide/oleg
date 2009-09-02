;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapping for SXML Tools
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

(library (oleg sxml-tools)
  (export
    sxml:attr-list-node
    sxml:attr-as-list
    sxml:aux-list-node
    sxml:aux-as-list
    sxml:find-name-separator
    sxml:error
    sxml:empty-element?
    sxml:shallow-normalized?
    sxml:normalized?
    sxml:shallow-minimized?
    sxml:minimized?
    sxml:name
    sxml:element-name
    sxml:node-name
    sxml:ncname
    sxml:name->ns-id
    sxml:content
    sxml:text
    sxml:content-raw
    sxml:attr-list-u
    sxml:aux-list
    sxml:aux-list-u
    sxml:aux-node
    sxml:aux-nodes
    sxml:attr
    sxml:attr-from-list
    sxml:num-attr
    sxml:attr-u
    sxml:ns-list
    sxml:ns-id->nodes
    sxml:ns-id->uri
    sxml:ns-uri->nodes
    sxml:ns-uri->id
    sxml:ns-id
    sxml:ns-uri
    sxml:ns-prefix
    sxml:change-content!
    sxml:change-content
    sxml:change-attrlist
    sxml:change-attrlist!
    sxml:change-name!
    sxml:change-name
    sxml:add-attr
    sxml:add-attr!
    sxml:change-attr
    sxml:change-attr!
    sxml:set-attr
    sxml:set-attr!
    sxml:add-aux
    sxml:add-aux!
    sxml:squeeze!
    sxml:squeeze
    sxml:clean
    select-first-kid
    sxml:node-parent
    sxml:add-parents
    sxml:lookup
    sxml:attr->xml
    sxml:string->xml
    sxml:sxml->xml
    sxml:attr->html
    sxml:string->html
    sxml:non-terminated-html-tag?
    sxml:sxml->html)
  (import
    (except (rnrs base) error)
    (rnrs syntax-case)
    (rnrs lists)
    (rnrs mutable-pairs)
    (rnrs io simple)
    (rnrs unicode)
    (srfi :0)
    (only (srfi :13)
      string-index-right
      string-contains
      string-null?)
    (srfi :23)
    (oleg prelude)
    (oleg sxpath library)
    (only (scheme) void exit)
    (srfi private include))

(define-syntax define-macro
  (lambda (x)
    #'(begin)))

(define-syntax sxml:find-name-separator
  (lambda (x)
    (syntax-case x ()
      [(k len)
       #`(let rpt ([pos (- len 1)])
           (cond
             [(negative? pos) #f]
             [(char=? #\: 
                (string-ref #,(datum->syntax #'k 'name) 
                  pos))
              pos]
             [else (rpt (- pos 1))]))])))

(include/resolve ("oleg" "ssax" "lib") "util.scm")
(include/resolve ("oleg" "sxml-tools") "sxml-tools.scm")

)