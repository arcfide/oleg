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

(library (oleg sxml-to-html)
  (export
    sxml->html
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
(include/resolve-ci ("oleg" "ssax" "lib") "SXML-to-HTML.scm")

)