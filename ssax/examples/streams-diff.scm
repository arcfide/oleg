;		Stream-wise processing of two SXML documents
;
; using a not-so-trivial example of detecting the differences in
; character content within the corresponding paragraphs of two SXML
; documents. We assume that two SXML documents in question are made of
; the same number of paragraphs.  The paragraphs may be enclosed in
; other elements such as `div', chapters, etc., and that enclosing may
; vary between the two documents. We disregard such differences when
; comparing the documents. Paragraphs contain text, perhaps
; interspersed with markup such as `em', etc. We compare the text
; content of the respective paragraphs from two SXML documents and
; annotate the paragraphs that differ.  We return two other SXML
; documents with such annotated paragraphs.  We compare only the
; character context, *regardless* of how that content is arranged into
; strings or enclosed into additional markup elements.  For example,
; the following two paragraphs
;
;    (p "This" " " "is a " (strong "simple") " example!")
;    (p "This is a simple " (br) (em "example") "!")
; 
; will be considered equal. Optionally we may treat (br) as a white space
; and disregard the differences in white space. 
;
; The over-arching idea is stream processing of two documents side-by-side.
; All comparison is done in-place. No strings are ever copied.
;
; We use shift/reset to convert a pre-post-order iterator into a stream
; -- or, zipper. See messages
; http://pobox.com/~oleg/ftp/Scheme/zipper-in-scheme.txt
; http://www.haskell.org/pipermail/haskell/2005-April/015769.html
; http://www.haskell.org/pipermail/haskell/2005-May/015844.html
;

; $Id: streams-diff.scm,v 1.1 2005/08/23 08:45:28 oleg Exp $

; IMPORT
; See the Makefile


; Preliminaries: declare the zipper record

(cond-expand
  (scm
    (require 'srfi-9))
  (else
    #f)
)

(cond-expand
  (chez
    (define-record zipper (curr-node k))
    (define curr-node zipper-curr-node)
    (define next zipper-k))
  (else					; Use SRFI-9
    (define-record-type *zipper*
      (make-zipper z-curr-node z-k)
      zipper?
      (z-curr-node curr-node)
      (z-k next))
))

; The following two functions are taken verbatim from sxml-to-sxml.scm
; The function map-node-concat (unlike map) has an advantage
; that it processes list elements strictly left-to-right, deterministically.


(define (map-node-concat fn lst)
  (if (null? lst) '()
    (let ((result (fn (car lst))))
      (cond
	((null? result)			; It's a null node-list, splice it in
	  (map-node-concat fn (cdr lst)))
	((and (pair? result) (not (symbol? (car result))))
		;  it's a non-null node-list
	  (append result (map-node-concat fn (cdr lst))))
	(else
	  (cons   result (map-node-concat fn (cdr lst))))))))

(define (pre-post-order tree bindings)
  (let* ((default-binding (assq '*default* bindings))
	 (text-binding (or (assq '*text* bindings) default-binding))
	 (text-handler			; Cache default and text bindings
	   (and text-binding
	     (if (procedure? (cdr text-binding))
	         (cdr text-binding) (cddr text-binding)))))
    (let loop ((tree tree))
      (cond
	((null? tree) '())
	((not (pair? tree))
	  (let ((trigger '*text*))
	    (if text-handler (text-handler trigger tree)
	      (error "Unknown binding for " trigger " and no default"))))
					; tree is a nodelist
	((not (symbol? (car tree))) (map-node-concat loop tree))
	(else				; tree is an SXML node
	  (let* ((trigger (car tree))
		 (binding (or (assq trigger bindings) default-binding)))
	    (cond
	      ((not binding) 
		(error "Unknown binding for " trigger " and no default"))
	      ((not (pair? (cdr binding)))  ; must be a procedure: handler
		(apply (cdr binding) trigger
		  (map-node-concat loop (cdr tree))))
	      ((eq? '*preorder* (cadr binding))
		(apply (cddr binding) tree))
	      ((eq? '*macro* (cadr binding))
		(loop (apply (cddr binding) tree)))
	      (else			    ; (cadr binding) is a local binding
		(apply (cddr binding) trigger 
		  (pre-post-order (cdr tree) (append (cadr binding) bindings)))
		))))))))

;------------------------------------------------------------------------

; transform enumerator to a stream
;     enum->stream :: ENUM -> ZIPPER or TREE
; TREE -- the data structure being enumerated
; ENUM is the enumerator:
;     ENUM :: TRAVERSAL-FN -> TREE
; It takes the traversal function
;     TRAVERSAL-FN :: NODE -> NODE
; applies it to the data structure in question, and returns (perhaps changed)
; data structure. The traversal function receives the current node and
; returns a replacement node (which may be the same as the input one).

(define (enum->stream enum)
  (reset (enum (lambda (node) (shift f (make-zipper node f))))))

; Example 1
; Numbering all sections in the documents straight-through.
; To each attribute node of every Section, add an attribute
; (counter n) where n is the sequential counter of the Section in the
; document order.

(define doc1
  '(*TOP*
     (Section (@)
       (p "par 1")
       (Section (@ (class "subsection"))
	 (p "par 11")))
     (Section (@) (p "par 2"))))

(define (number-sections doc)
  (let loop
    ((counter 1)
     (cursor
       (enum->stream
	 (lambda (traversal-fn)
	   (pre-post-order doc
	     `((*text* . ,(lambda (tag str) str)) ; basically, identity
		(*default* . ,(lambda x x))       ; ditto
		(Section
		  ((@ *preorder* 	; Attr node within a section
		     . ,(lambda (tag . elems) 
			  (cons tag (traversal-fn elems)))))
		  . ,(lambda x x))))))))          ; otherwise, id
    (if (zipper? cursor)
      (loop (+ 1 counter)
	((next cursor) `((counter  ,counter) . ,(curr-node cursor))))
      cursor				; not a zipper, it's the result
      )))

(cout nl "Numbering sections" nl "Original document" nl)
(pp doc1)
(cout nl "Document with sections numbered" nl)      
(pp (number-sections doc1))



; Main example: comparing character content within two documents
; We convert an SXML document into a stream made of two types of data:
; strings and 'p' elements. To be mode precise, our stream has the 
; following grammar:
;   STREAM ::= (STRING* P)*
; We convert that stream into a stream of streams of characters.

(define docl
  '(*top*
     (Section 1
       (p "This" " " "is a " (strong "simple") " example!")
       (p "Second paragraph.")
       (p (@ (id "1")) "Third paragraph.")
       )))

(define docr
  '(*top*
     (Section 1
       (p "This is a simple " (br) (em "example") "!")
       (Section 11 (p "2nd" " paragraph."))
       (Section 12 (p "Third paragraph" "."))
       )))



; Define a character content stream (ccs for short) as a record
; of the zipper and an input string port. The zipper is a backing store
; for the input string port. We maintain the invariant that the input port
; is at EOF => the current node of the zipper is not a text one.

(cond-expand
  (chez
    (define-record ccs (zipper port)))
  (else					; Use SRFI-9
    (define-record-type *ccs*
      (make-ccs zipper in-port)
      ccs?
      (zipper ccs-zipper)
      (in-port ccs-port))
))


; Overlay the character content stream over the zipper

; Open a new character stream over the string pointed to by zipper
; ccs-new:: ZIPPER -> CCS
(define (ccs-new zipper)
  (if (string? (curr-node zipper))
    (make-ccs zipper (open-input-string (curr-node zipper)))
    (make-ccs zipper (open-input-string "")))) ; EOF


; Get the current character from CCS
; ccs-get-char:: CCS -> [CHAR CCS]   -- returns two values
; on underflow, advance the zipper
(define (ccs-get-char ccs)
  (let ((c (read-char (ccs-port ccs))))
    (if (eof-object? c)
      (if (string? (curr-node (ccs-zipper ccs)))
	(ccs-get-char (ccs-new (advance (ccs-zipper ccs))))
	(values c ccs))			; otherwise, stuck at EOF
      (values c ccs))))

(define (advance zipper) ((next zipper) (curr-node zipper)))

; Flush until zipper points to the specific tree node
(define (flush-until tag zipper)
  (let ((node (curr-node zipper)))
    (if (and (pair? node) (eq? tag (car node))) zipper ; already here
      (flush-until tag (advance zipper)))))

; diff-cc :: DOC1 * DOC2 -> [DOC1' DOC2']
(define (diff-cc doc1 doc2)

  ; traverse only 'p' nodes and strings within p nodes, disregard attributes
  (define (enumerator doc)
    (lambda (traversal-fn)
      (pre-post-order doc
	`((*default* . ,(lambda x x))
	  (*text* . ,(lambda (tag str) str)) ; don't traverse strings outside p
	  (@ *preorder* . ,(lambda x x)) ; ignore attributes
	  (p 
	    ((*text* . ,(lambda (tag str) (traversal-fn str))))
	    . ,(lambda node (traversal-fn node)))))))

  ; Annotate a changed paragraph
  (define (annotate z) ((next z) `(changed ,(curr-node z))))

  ; Main loop
  (let loop 
    ((z1 (enum->stream (enumerator doc1)))
     (z2 (enum->stream (enumerator doc2))))
    ;(cout nl "loop: " z1 z2)
    (if (not (and (zipper? z1) (zipper? z2)) )
      (values z1 z2)			; both streams should finish...
      ; still traversing
      (let inner ((ccs1 (ccs-new z1)) (ccs2 (ccs-new z2)))
	(let*-values
	  (((c1 ccs1) (ccs-get-char ccs1))
	   ((c2 ccs2) (ccs-get-char ccs2)))
	  (cond
	    ((and (eof-object? c1) (eof-object? c2))
	      ; successful comparison, no differences detected
	      (loop (advance (ccs-zipper ccs1))
		    (advance (ccs-zipper ccs2))))
	    ((eqv? c1 c2)
	      (inner ccs1 ccs2))
	    (else			; detected the difference
	      (loop (annotate (flush-until 'p (ccs-zipper ccs1)))
		    (annotate (flush-until 'p (ccs-zipper ccs2)))))))))))

(cout nl "Comparing two documents and annotating their differences" nl
  "The documents are" nl)
(pp docl) (pp docr)
(let*-values (((d1 d2) (diff-cc docl docr)))
  (cout nl "Annotated documents" nl)
  (pp d1) (pp d2)
  )

(display "All tests passed")
(newline)


