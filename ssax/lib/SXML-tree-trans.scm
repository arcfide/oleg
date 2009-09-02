;		XML/HTML processing in Scheme
;		SXML expression tree transformers
;
; IMPORT
; A prelude appropriate for your Scheme system
;	(myenv-bigloo.scm, myenv-mit.scm, etc.)
;
; EXPORT
; (provide SRV:send-reply
;	   post-order pre-post-order replace-range)
;
; See vSXML-tree-trans.scm for the validation code, which also
; serves as usage examples.
;
; $Id: SXML-tree-trans.scm,v 1.8 2009/03/16 03:08:59 oleg Exp $


; procedure: SRV:send-reply FRAGMENT ...
;
; Output the 'fragments'
; The fragments are a list of strings, characters,
; numbers, thunks, #f, #t -- and other fragments.
; The function traverses the tree depth-first, writes out
; strings and characters, executes thunks, and ignores
; #f and '().
; The function returns #t if anything was written at all;
; otherwise the result is #f
; If #t occurs among the fragments, it is not written out
; but causes the result of SRV:send-reply to be #t

(define (SRV:send-reply . fragments)
  (let loop ((fragments fragments) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((eq? #t (car fragments)) (loop (cdr fragments) #t))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
        (display (car fragments))
        (loop (cdr fragments) #t)))))



; procedure: pre-post-order TREE BINDINGS
;
;	          Traversal of an SXML tree or a grove:
;			a <Node> or a <Nodelist>
;
; A <Node> and a <Nodelist> are mutually-recursive datatypes that
; underlie the SXML tree:
;	<Node> ::= (name . <Nodelist>) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodelist> ::= (<Node> ...)
; Nodelists, and Nodes other than text strings are both lists. A
; <Nodelist> however is either an empty list, or a list whose head is
; not a symbol (an atom in general). A symbol at the head of a node is
; either an XML name (in which case it's a tag of an XML element), or
; an administrative name such as '@'.
; See SXPath.scm and SSAX.scm for more information on SXML.


; Pre-Post-order traversal of a tree and creation of a new tree:
;	pre-post-order:: <tree> x <bindings> -> <new-tree>
; where
; <bindings> ::= (<binding> ...)
; <binding> ::= (<trigger-symbol> *preorder* . <handler>) |
;               (<trigger-symbol> *macro* . <handler>) |
;		(<trigger-symbol> <new-bindings> . <handler>) |
;		(<trigger-symbol> . <handler>)
; <trigger-symbol> ::= XMLname | *text* | *default*
; <handler> :: <trigger-symbol> x [<tree>] -> <new-tree>
;
; The pre-post-order function visits the nodes and nodelists
; pre-post-order (depth-first).  For each <Node> of the form (name
; <Node> ...) it looks up an association with the given 'name' among
; its <bindings>. If failed, pre-post-order tries to locate a
; *default* binding. It's an error if the latter attempt fails as
; well.  Having found a binding, the pre-post-order function first
; checks to see if the binding is of the form
;	(<trigger-symbol> *preorder* . <handler>)
; If it is, the handler is 'applied' to the current node. Otherwise,
; the pre-post-order function first calls itself recursively for each
; child of the current node, with <new-bindings> prepended to the
; <bindings> in effect. The result of these calls is passed to the
; <handler> (along with the head of the current <Node>). To be more
; precise, the handler is _applied_ to the head of the current node
; and its processed children. The result of the handler, which should
; also be a <tree>, replaces the current <Node>. If the current <Node>
; is a text string or other atom, a special binding with a symbol
; *text* is looked up.
;
; A binding can also be of a form
;	(<trigger-symbol> *macro* . <handler>)
; This is equivalent to *preorder* described above. However, the result
; is re-processed again, with the current stylesheet.

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
	((not (symbol? (car tree))) (map loop tree)) ; tree is a nodelist
	(else				; tree is an SXML node
	  (let* ((trigger (car tree))
		 (binding (or (assq trigger bindings) default-binding)))
	    (cond
	      ((not binding) 
		(error "Unknown binding for " trigger " and no default"))
	      ((not (pair? (cdr binding)))  ; must be a procedure: handler
		(apply (cdr binding) trigger (map loop (cdr tree))))
	      ((eq? '*preorder* (cadr binding))
		(apply (cddr binding) tree))
	      ((eq? '*macro* (cadr binding))
		(loop (apply (cddr binding) tree)))
	      (else			    ; (cadr binding) is a local binding
		(apply (cddr binding) trigger 
		  (pre-post-order (cdr tree) (append (cadr binding) bindings)))
		))))))))

; procedure: post-order TREE BINDINGS
; post-order is a strict subset of pre-post-order without *preorder*
; (let alone *macro*) traversals. 
; Now pre-post-order is actually faster than the old post-order.
; The function post-order is deprecated and is aliased below for
; backward compatibility.
(define post-order pre-post-order)


; A version of pre-post-order that transforms an SXML document into a
; _strictly conformant_ SXML document. That is, the result of a
; pre-post-order transformation can be queried with SXPath or
; transformed again with SXSLT.

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
; The problem can be rectified, by changing pre-post-order as shown in
; the code below. The only change is replacing the two occurrences of
; 'map' (there are only two such occurrences) with
; map-node-concat. Justification for the change: a pre-post-order
; handler can yield either a node, or a nodelist. Now, if the handler
; returns a nodelist, we _splice_ it in in the result tree. This
; operation seems to make sure that each node of a tree is a valid
; SXML node.
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

; map-node-concat FN NODELIST -> NODELIST
; Map FN to each element of NODELIST where FN is a function
;	NODE -> NODE or NODELIST
; If an application of FN yields a NODELIST (including the empty list),
; we _splice_ it in into the result. Essentially,
;	(map-node-concat fn nodelist)
; is equivalent to
; (apply append
;   (map (lambda (node)
; 	 (let ((result (fn node)))
; 	   (if (nodelist? result) result (list result))))
;     nodelist))

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

; The following is almost identical to pre-post-order
; except that the two occurrences of 'map' in that pre-post-order
; (there are only two such occurrences) are replaced with map-node-concat
; in the code below.

(define (pre-post-order-splice tree bindings)
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
;			Extended tree fold
; tree = atom | (node-name tree ...)
;
; foldts fdown fup fhere seed (Leaf str) = fhere seed str
; foldts fdown fup fhere seed (Nd kids) =
;         fup seed $ foldl (foldts fdown fup fhere) (fdown seed) kids

; procedure fhere: seed -> atom -> seed
; procedure fdown: seed -> node -> seed
; procedure fup: parent-seed -> last-kid-seed -> node -> seed
; foldts returns the final seed

(define (foldts fdown fup fhere seed tree)
  (cond
   ((null? tree) seed)
   ((not (pair? tree))		; An atom
    (fhere seed tree))
   (else
    (let loop ((kid-seed (fdown seed tree)) (kids (cdr tree)))
      (if (null? kids)
	  (fup seed kid-seed tree)
	  (loop (foldts fdown fup fhere kid-seed (car kids))
		(cdr kids)))))))

; procedure: replace-range:: BEG-PRED x END-PRED x FOREST -> FOREST
; Traverse a forest depth-first and cut/replace ranges of nodes.
;
; The nodes that define a range don't have to have the same immediate
; parent, don't have to be on the same level, and the end node of a
; range doesn't even have to exist. A replace-range procedure removes
; nodes from the beginning node of the range up to (but not including)
; the end node of the range.  In addition, the beginning node of the
; range can be replaced by a node or a list of nodes. The range of
; nodes is cut while depth-first traversing the forest. If all
; branches of the node are cut a node is cut as well.  The procedure
; can cut several non-overlapping ranges from a forest.

;	replace-range:: BEG-PRED x END-PRED x FOREST -> FOREST
; where
;	type FOREST = (NODE ...)
;	type NODE = Atom | (Name . FOREST) | FOREST
;
; The range of nodes is specified by two predicates, beg-pred and end-pred.
;	beg-pred:: NODE -> #f | FOREST
;	end-pred:: NODE -> #f | FOREST
; The beg-pred predicate decides on the beginning of the range. The node
; for which the predicate yields non-#f marks the beginning of the range
; The non-#f value of the predicate replaces the node. The value can be a
; list of nodes. The replace-range procedure then traverses the tree and skips
; all the nodes, until the end-pred yields non-#f. The value of the end-pred
; replaces the end-range node. The new end node and its brothers will be
; re-scanned.
; The predicates are evaluated pre-order. We do not descend into a node that
; is marked as the beginning of the range.

(define (replace-range beg-pred end-pred forest)

  ; loop forest keep? new-forest
  ; forest is the forest to traverse
  ; new-forest accumulates the nodes we will keep, in the reverse
  ; order
  ; If keep? is #t, keep the curr node if atomic. If the node is not atomic,
  ; traverse its children and keep those that are not in the skip range.
  ; If keep? is #f, skip the current node if atomic. Otherwise,
  ; traverse its children. If all children are skipped, skip the node
  ; as well.

  (define (loop forest keep? new-forest)
    (if (null? forest) (values (reverse new-forest) keep?)
	(let ((node (car forest)))
	  (if keep?
	      (cond			; accumulate mode
	       ((beg-pred node) =>	; see if the node starts the skip range
		(lambda (repl-branches)	; if so, skip/replace the node
		  (loop (cdr forest) #f 
			(append (reverse repl-branches) new-forest))))
	       ((not (pair? node))	; it's an atom, keep it
		(loop (cdr forest) keep? (cons node new-forest)))
	       (else
		(let*-values
		 (((node?) (symbol? (car node))) ; or is it a nodelist?
		  ((new-kids keep?)		 ; traverse its children
		   (loop (if node? (cdr node) node) #t '())))
		 (loop (cdr forest) keep?
		       (cons 
			(if node? (cons (car node) new-kids) new-kids)
			new-forest)))))
	      ; skip mode
	      (cond
	       ((end-pred node) =>	; end the skip range
		(lambda (repl-branches)	; repl-branches will be re-scanned
		  (loop (append repl-branches (cdr forest)) #t
			new-forest)))
	       ((not (pair? node))	; it's an atom, skip it
		(loop (cdr forest) keep? new-forest))
	       (else
		(let*-values
		 (((node?) (symbol? (car node)))  ; or is it a nodelist?
		  ((new-kids keep?)		  ; traverse its children
		   (loop (if node? (cdr node) node) #f '())))
		 (loop (cdr forest) keep?
		       (if (or keep? (pair? new-kids))
			   (cons
			    (if node? (cons (car node) new-kids) new-kids)
			    new-forest)
			   new-forest)		; if all kids are skipped
		       ))))))))			; skip the node too
  
  (let*-values (((new-forest keep?) (loop forest #t '())))
     new-forest))

