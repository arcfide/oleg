;	Transforming SXML to SXML: Composing SXML transformations
;
; The present code tests a version of pre-post-order
; that transforms an SXML document into a _strictly conformant_ SXML
; document. That is, the result of a pre-post-order transformation can
; be queried with SXPath or transformed again with SXSLT.

; Joerg-Cyril Hoehle wrote on the SSAX-SXML mailing list about
; chaining of transformations on a SXML source:
;	SXML --transform--> SXML1 --transform--> SXML2 ... --> XML
; It's only the last transformation step that would produce XML.
; We can use a pre-post-order traversal combinator with an appropriate
; stylesheet to run each 'transform' step above. SRV:send-reply at the
; end will write out the resulting XML document.
; (see Joerg-Cyril Hoehle's messages on the SSAX-SXML list on Oct
; 21 and 22, 2003).
;
; Composing SXML transformations by feeding the result of one
; pre-post-order traversal into another works. Still, the result of
; pre-post-order is merely a tree of fragments, which is generally not
; a strictly valid SXML.  Joerg-Cyril Hoehle pointed out that, for
; example, given an SXML document
;	'(Data (repeat 3 (random-Header 3))))
; a sample transformation
; (pre-post-order sxml
;   `((repeat *macro*
;      . ,(lambda (tag count . elems)
; 	  (apply make-list count elems)))
;      (random-Header *preorder*
;        . ,(lambda (tag elems)
; 	    `(Header ,(gensym))))
;      (*text* . ,(lambda (trigger x) x))
;      (*default* . ,(lambda x x))))
;
; yields the following.
; (Data
;  ((Header VOTj)
;   (Header 0qel)
;   (Header bA97)))
;
; All (Header ...) elements are enclosed in an extra pair of
; parentheses. In general, pre-post-order may add extra nesting levels
; and insert empty lists. Both these features break the strict SXML
; specification compliance of the transformation result. Still,
; pre-post-order itself can process such a tree correctly. Therefore,
; if we use only pre-post-order for our multi-stage SXML
; transformations, no problems occur. However, if we wish to employ
; SXPath to select parts from a pre-post-order-transformed SXML
; document, we get a problem. SXPath, unlike pre-post-order, insists
; on its source document being fully SXML compliant.
;
; The problem can be rectified in pre-post-order-splice.
;
; For a pure SXML-to-XML conversion, the splicing-in seems to be an
; overkill. Therefore, it may make sense to keep both versions of
; pre-post-order. Personally I have no problem with proliferation of
; pre-post-order-like functions. I believe that it is the data
; structure/protocols that should be standardized and
; parsimonious. Each user may write processing code in his own way. Of
; course some of the processing code turns out more general than the
; other, and can be shared. Nevertheless, it's the common data
; structure, the common format that guarantees interoperability --
; rather than the common library. Code should be tailored (or even
; automatically generated) to suit circumstances.
;
; IMPORT
; The following is a Bigloo-specific module declaration.
; Other Scheme systems have something similar.
; (module sxml-to-sxml
;   (include "myenv-bigloo.scm")
;   (include "srfi-13-local.scm") ; or import from SRFI-13 if available
;   (include "util.scm")
;   )
;
; $Id: sxml-to-sxml.scm,v 1.3 2009/03/16 03:08:58 oleg Exp $


(define pre-post-order pre-post-order-splice)


; First example from Joerg-Cyril Hoehle, see above

;(require 'srfi-1)
; make-list from SRFI-1
; It is implemented here for easy reference
(define (make-list count elem)
  (if (not (positive? count)) '()
    (cons elem (make-list (dec count) elem))))

(define (transform1 sxml)
  (pre-post-order sxml
  `((repeat *macro*
     . ,(lambda (tag count . elems)
	  (apply make-list count elems)))
     (random-Header *preorder*
       . ,(lambda (tag elems)
	    `(Header ,(gensym))))
     (*text* . ,(lambda (trigger x) x))
     (*default* . ,(lambda x x)))))

; Make sure the transformation result is pure SXML, with
; no superfluous nesting levels.
; In particular, the Data node should have three children
(display "First example") (newline)
(let ((result
	(transform1 '(Data (repeat 3 (random-Header 3))))))
  (pp result)
  (assert (pair? result) (eq? 'Data (car result))
    (= 3 (length (cdr result)))))
(newline)

; The second example from Joerg-Cyril Hoehle (given in his message
; as of Oct 22, 2003). If a stylesheet handler transforms an SXML node to '(),
; that node disappears without a trace in the result tree.
(display
  "Second example: transforming a node to '() effectively eliminates it")
(newline)
(let*
  ((src-doc '(Request (Prio 1) (stuff 3)))
   (result
     (pre-post-order src-doc
       `((Prio *macro* . ,(lambda x '())) ; transforms Prio node to '()
	  (*text* . ,(lambda (trigger x) x))
	  (*default* . ,(lambda x x)))))
    )
  (pp result)
  (assert (equal? result 
	          '(Request (stuff 3)))) ; Prio node is gone now
)
(newline)

; An example of composing two transformations. One transformation
; looks for SXML nodes (count n). It inserts a node (mark) before
; count and increments 'n'. The other transformation decrements the
; count and removes the mark. Hence the composition must be the identity.

(let*
  ((ss-id				; The identity stylesheet
     `((*text* . ,(lambda (trigger x) x))
       (*default* . ,(lambda x x))))
   (ss-1				; The first transformation
     (append
       `((count . ,(lambda (tag n) `((mark) (,tag ,(+ n 1))))))
       ss-id))
   (ss-2				; The second transformation
     (append
       `((mark . ,(lambda x '()))	; remove the mark
	 (count . ,(lambda (tag n) (list tag (- n 1)))) ; decr the counter
	  )
       ss-id))

    (src-doc
      `(data (count 1) "text" (more-data (count 42) (count 43)) (br)))

    )
  (display "The source document") (newline)
  (pp src-doc)
  (let ((result (pre-post-order src-doc ss-id)))
    (display "The result of applying the identity transform") (newline)
    (pp result)
    (assert (equal? result src-doc)))

  (let* ((res1 (pre-post-order src-doc ss-1))
	 (res2 (pre-post-order res1 ss-2)))
    (display "The result after ss-1") (newline)
    (pp res1)
    (display "The result after ss-2") (newline)
    (pp res2)
    (assert (equal? res2 src-doc)))
  (newline)
)

(display "All tests passed")
(newline)
