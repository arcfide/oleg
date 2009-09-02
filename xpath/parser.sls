;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapper for XPath Parsing
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

(library (oleg xpath parser)
  (export
    txp:param-value
    txp:error?
    sxml:xpointer-parse-error
    sxml:xpointer-parse-warning
    txp:semantic-errs-detected?
    txp:signal-semantic-error
    sxml:whitespace
    sxml:delimiter
    sxml:non-first?
    sxml:skip-ws
    sxml:assert-end-of-path
    sxml:parse-check
    sxml:parse-check-sequence
    sxml:parse-assert
    sxml:parse-ncname
    sxml:parse-name
    sxml:parse-qname
    sxml:parse-natural
    sxml:parse-literal
    sxml:parse-number
    txp:resolve-ns-prefix
    txp:parameterize-parser)
  (import
    (rnrs base)
    (rnrs r5rs)
    (rnrs io simple)
    (rnrs lists)
    (rnrs unicode)
    (oleg prelude)
    (srfi private include))

(include/resolve ("oleg" "sxml-tools") "xpath-parser.scm")

)