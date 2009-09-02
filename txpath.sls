;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapping for TXPaths
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

(library (oleg txpath)
  (export
    sxml:xpointer-runtime-error
    sxml:xpath-nodeset-filter
    sxml:arithmetic-eval
    sxml:core-last
    sxml:core-position
    sxml:core-count
    sxml:core-id
    sxml:core-local-name
    sxml:core-namespace-uri
    sxml:core-name
    sxml:core-string
    sxml:core-concat
    sxml:core-starts-with
    sxml:core-contains
    sxml:core-substring-before
    sxml:core-substring-after
    sxml:core-substring
    sxml:core-string-length
    sxml:core-normalize-space
    sxml:core-translate
    sxml:core-boolean
    sxml:core-not
    sxml:core-true
    sxml:core-false
    sxml:core-lang
    sxml:core-number
    sxml:core-sum
    sxml:core-floor
    sxml:core-ceiling
    sxml:core-round
    sxml:classic-params
    sxml:api-helper0
    sxml:classic-res
    sxml:api-helper
    sxml:xpath
    sxml:xpointer
    sxml:xpath-expr
    sxml:xpath+root+vars
    sxml:xpointer+root+vars
    sxml:xpath+root
    txpath
    sxml:api-index-helper
    sxml:xpath+index
    sxml:xpointer+index)
  (import
    (except (rnrs base) error)
    (rnrs r5rs)
    (rnrs io simple)
    (rnrs mutable-pairs)
    (rnrs unicode)
    (rnrs lists)
    (only (srfi :13)
      string-index-right
      string-contains
      string-null?
      string-prefix?
      string-prefix-ci?)
    (srfi :23)
    (only (scheme) exit)
    (oleg prelude)
    (oleg xpath parser)
    (oleg sxml-tools)
    (oleg sxpath extensions)
    (oleg sxpath library)
    (srfi private include))

(include/resolve ("oleg" "ssax" "lib") "util.scm")
(include/resolve ("oleg" "sxml-tools") "txpath.scm")

)