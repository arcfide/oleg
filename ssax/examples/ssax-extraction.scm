; SSAX parsing of a (large) XML document and recording only
; `interesting' parts of that document.
;
; The problem was posed by Tobias Gerdin in his message to the
; SSAX-SXML mailing list on Thu, 4 Jan 2007 19:24:02 +0100:
;
; <blockquote>
; I have a very large document essentially containing a  
; list of entries (represented as <entry> ... </entry> tags, but with  
; lots of nested tags inside). Inside each entry I want to extract two  
; #PCDATA sections, namely the content in a <keb> element and a <gloss>  
; element (in particular, I want to parse this file: ftp:// 
; ftp.monash.edu.au/pub/nihongo/JMdict.gz, more information the  
; document is found at http://www.csse.monash.edu.au/~jwb/j_jmdict.html).
;
; I've done some experimenting but haven't found any nice solution to  
; this elementary problem. Since the document is quite large (39MB) I  
; don't think I'd like the seed to contain information on the two tags  
; from all the entries, instead I'd thought that for each entry I'd  
; pass the information (the two CDATA sections) to another procedure  
; that inserts it into another data structure, and then reset the seed  
; for the next entry.
;
; So, to some it up: How do I extract CDATA sections from arbitrary  
; tags in a nice way? 
; </blockquote>
;
; $Id: ssax-extraction.scm,v 1.1 2007/01/06 12:48:05 oleg Exp $


; First we define the data structure that contains the extracted data.
; It is just a list whose elements is a list of two elements 
;   (keb-tags gloss-tags)
; An <entry> may have several keb and gloss elements, thus plural

(define empty-final-ds '())
(define (add-final-ds-entry kebs glosses ds) (cons (list kebs glosses) ds))


; define the entities that may occur in our document
; The full list is in JMDICT DTD
; http://www.csse.monash.edu.au/~jwb/jmdict_dtd_h.html
(define entities
  '((n . "noun (common) (futsuumeishi)")))


; A sample document, based roughly on
; http://www.csse.monash.edu.au/~jwb/jmdict_sample.html
; after simplifying and fixing some problems 

(define doc
  (string-concatenate/shared
    '("<JMdict>"
       "<entry>"
       "<ent_seq>1171270</ent_seq>"
       "<k_ele><keb>neko</keb><ke_pri>ichi1</ke_pri>"
       "</k_ele>"
       "<r_ele><reb>neko</reb><re_pri>ichi1</re_pri>"
       "</r_ele>"
       "<sense>"
       "<pos>&n;</pos>"
       "<gloss>cat</gloss>"
       "</sense>"
       "</entry>"

       "<entry>"
       "<ent_seq>1171271</ent_seq>"
       "<k_ele><keb>inu</keb><ke_pri>ichi1</ke_pri>"
       "</k_ele>"
       "<r_ele><reb>inu</reb><re_pri>ichi1</re_pri>"
       "</r_ele>"
       "<r_ele><reb>ken</reb>"
       "</r_ele>"
       "<sense>"
       "<pos>&n;</pos>"
       "<gloss g_lang='en'>dog</gloss>"
       "</sense>"
       "<sense>"
       "<pos>&n;</pos>"
       "<gloss>Canis (lupus) familiaris</gloss>"
       "</sense>"
       "</entry>"
       "</JMdict>")))


; The parsing process uses several seeds.
; The initial seed, which is the parent seed to the `entry'
; element, is the final-ds. It accumulates the results from parsing
; entries.
; When parsing children of an <entry>, the seed is different. It is defined
; as a record below (we use records for clarity, and we use Petite
; Chez records).
; char-data is the accumulator for character data, or #f is we skip
; char data (of elements whose content we don't care about)
; kebs is the list of char data from the kebs elements so far
; glosses is the same for glosses. All the interface is functional...

(define-record entry-seed 
  ((immutable char-data) (immutable kebs) (immutable glosses)) ())

(define new-entry-seed (make-entry-seed #f '() '()))
(define (accumulate-char-data eseed)
  (make-entry-seed '() (entry-seed-kebs eseed)
                       (entry-seed-glosses eseed)))
; Don't accumulate char data why processing an element
(define (disregard-char-data eseed)
  (make-entry-seed #f (entry-seed-kebs eseed)
                       (entry-seed-glosses eseed)))

(define (set-char-data cdata eseed)
  (make-entry-seed cdata (entry-seed-kebs eseed)
                       (entry-seed-glosses eseed)))
(define (add-kebs keb eseed)
  (make-entry-seed #f (cons keb (entry-seed-kebs eseed))
                       (entry-seed-glosses eseed)))
(define (add-glosses gloss eseed)
  (make-entry-seed #f (entry-seed-kebs eseed)
    (cons gloss (entry-seed-glosses eseed))))

    
; For clarity, the following procedure has debug output: (cout ...)
; to make it clear how the seed propagates...

; Take ssax:xml->sxml and modify it a little
; The resulting procedure takes a port and the data structure to
; store the final results in
(define custom-parser
  (ssax:make-parser
	     NEW-LEVEL-SEED 
	     (lambda (elem-gi attributes namespaces
			      expected-content seed)
	       (cout "entering: " elem-gi nl)
	       (case elem-gi
		 ((entry)		; entering an entry...
		   new-entry-seed)
		 ((keb gloss)		; switch on accum of char content
		   (accumulate-char-data seed))
		 (else seed)))
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       (cout "exiting: " elem-gi "," seed nl)
	       (case elem-gi
		 ((entry)
		   ; leaving <entry>
		   ; parent-seed is final-ds. Add data from entry-seed
		   (add-final-ds-entry 
		     (entry-seed-kebs seed)
		     (entry-seed-glosses seed)
		     parent-seed))
		 ((keb)
		   (add-kebs
		     (ssax:reverse-collect-str-drop-ws 
		       (entry-seed-char-data seed))
		     seed))
		 ((gloss)
		   (add-glosses
		     (ssax:reverse-collect-str-drop-ws 
		       (entry-seed-char-data seed))
		     seed))
		 (else seed)))

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (let ((prev-char-data
		       (entry-seed-char-data seed)))
		 (if prev-char-data
		   (set-char-data
		     (if (string-null? string2) (cons string1 prev-char-data)
		       (cons* string2 string1 prev-char-data))
		     seed)
		   seed)))		; don't accumulate char data

	     DOCTYPE
	     (lambda (port docname systemid internal-subset? seed)
	       (when internal-subset?
		     (ssax:warn port
			   "Internal DTD subset is not currently handled ")
		     (ssax:skip-internal-dtd port))
	       (ssax:warn port "DOCTYPE DECL " docname " "
		     systemid " found and skipped")
	       (values #f entities '() seed))

	     UNDECL-ROOT
	     (lambda (elem-gi seed)
	       (values #f entities '() seed))

	     PI
	     ((*DEFAULT* .
		(lambda (port pi-tag seed)
		  ; read PI's body and silently disregard it
		  (let ((dummy (ssax:read-pi-body-as-string port)))
		    seed))))
	     ))

(cout "About to parse the document" nl)
(cout "Result" nl
  (call-with-input-string doc
    (lambda (port)
      (custom-parser port empty-final-ds)))
  nl
  "Done" nl)
