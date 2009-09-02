;; This extracts documentation sections from the source code
;; and generates a PLT-style doc.txt.

;; This file is written in pure R5RS Scheme.

(define *source-files* '(("SSAX.scm" . "ssax.ss")
			 ("input-parse.scm" . "input-parse.ss")
			 ("SXML-tree-trans.scm" . "sxml-tree-trans.ss")
			 ("SXML-to-HTML.scm" . "sxml-to-html.ss")
			 ("SXML-to-HTML-ext.scm" . "sxml-to-html-ext.ss")))

(define *doc-start-tags* '("procedure:" "syntax:" "value:" "procedure+:"))

(define (read-line port)
  (let loop ((l '()))
    (let ((c (read-char port)))
      (if (eof-object? c)
          c
          (if (char=? c #\newline)
              (list->string (reverse l))
              (loop (cons c l)))))))


(define (string-null? string)
  (string=? "" string))

(define (comment? line)
  (and (not (string-null? line))
       (char=? #\; (string-ref line 0))))

(define (comment-text line)
  (let loop ((line line))
    (if (comment? line)
	(loop (substring line 1 (string-length line)))
	line)))

(define (count-prefix-spaces line)
  (let ((size (string-length line)))
    (let loop ((pos 0))
      (cond
       ((= pos size) pos)
       ((char=? #\space (string-ref line pos))
	(loop (+ 1 pos)))
       (else pos)))))

(define (skip-spaces line)
  (substring line
	     (count-prefix-spaces line)
	     (string-length line)))

(define (string-prefix? maybe-prefix string)
  (let ((maybe-prefix-size (string-length maybe-prefix)))
    (and (>= (string-length string) maybe-prefix-size)
	 (string=? (substring string 0 maybe-prefix-size)
		   maybe-prefix))))

(define (any pred list)
  (let loop ((list list))
    (cond
     ((null? list) #f)
     ((pred (car list)))
     (else (loop (cdr list))))))
   
(define (doc-start-line line)
  (and (comment? line)
       (let* ((text (skip-spaces (comment-text line)))
	      (size (string-length text)))
	 (any (lambda (tag)
		(if (string-prefix? tag text)
		    (substring text (string-length tag) size)
		    #f))
	       *doc-start-tags*))))

;; eats the line following the doc
;; returns two values: list of lines, and eof? flag

(define (collect-doc first-line skip input-port)
  (let loop ((rev-lines
	      (list
	       (string-append "> "
			      (substring first-line skip (string-length first-line))))))
    (let ((line (read-line input-port)))
      (cond
       ((eof-object? line)
	(values (reverse rev-lines) #t))
       ((string-null? line)
	(loop (cons "" rev-lines)))
       ((comment? line)
	(let* ((text (comment-text line))
	       (size (string-length text)))
	  (loop (cons (if (> size skip)
			  (substring text skip size)
			  "")
		      rev-lines))))
       (else
	(values (reverse rev-lines) #f))))))
			       
(define (extract-docs input-port output-port)
  (let loop ()
    (let ((line (read-line input-port)))
      (cond
       ((eof-object? line) 'fick-dich-ins-knie)
       ((doc-start-line line)
	=> (lambda (rest)
	     (let ((skip (count-prefix-spaces (comment-text line))))
	       (call-with-values
		   (lambda () (collect-doc rest skip input-port))
		 (lambda (doc eof?)
		   (for-each (lambda (line)
			       (display line output-port) (newline output-port))
			     doc)
		   (newline output-port)
		   (if (not eof?)
		       (loop)))))))
       (else (loop))))))

(define (extract-docs-from-file input-file output-port)
  (call-with-input-file input-file
    (lambda (input-port)
      (extract-docs input-port output-port))))

(define (generate-doc.txt)
  (call-with-output-file "doc.txt"
    (lambda (output-port)
      (for-each (lambda (pair)
		  (let ((file (car pair))
			(module (cdr pair)))
		    (display "================================================================" output-port)
		    (newline output-port)
		    (display "_" output-port)
		    (display module output-port)
		    (display "_" output-port)
		    (newline output-port)
		    (display "================================================================" output-port)
		    (newline output-port)
		    (newline output-port)
		    (display "To load the module, do" output-port)
		    (newline output-port)
		    (display "(require (lib \"" output-port)
		    (display module output-port)
		    (display "\" \"ssax\"))" output-port)
		    (newline output-port)
		    (newline output-port)
		    (extract-docs-from-file file output-port)
		    (newline output-port)
		    (newline output-port)))
		*source-files*))))
