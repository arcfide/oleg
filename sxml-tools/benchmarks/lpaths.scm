;; Benchmarks for location paths evaluation
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

;-------------------------------------------------
; Generators

; Generates a document of a given depth
; The elements of the document for a binary tree
(define (generate-tree depth)
  (let ((make-text
         (lambda (ordinal)
           (string-append "text" (number->string ordinal))))
        (make-el-name
         (lambda (ordinal)
           (string->symbol
            (string-append "elem" (number->string ordinal)))))
        (power2
         (lambda (num)
           (arithmetic-shift 1 num))))
    (letrec
        ((make-subtree
          (lambda (depth ordinal)
            (if (= depth 1)
                (list
                 (make-el-name ordinal)
                 (make-text (+ ordinal 1)))
                (list
                 (make-el-name ordinal)
                 (make-subtree (- depth 1) (+ ordinal 1))
                 (make-subtree (- depth 1) (+ ordinal (power2 depth) -1))
                 (make-text (+ ordinal (power2 (+ depth 1)) -3)))))))
      (list '*TOP*
            (make-subtree depth 1)))))

;; Generates random textual lpaths of a given length
;(define (generate-xpath max-steps)
;  (let* ((rnd-member
;          (lambda (lst)
;            (list-ref lst (random (length lst)))))
;         (make-rnd-step
;          (lambda ()
;            (string-append
;             (rnd-member
;              '("ancestor" "ancestor-or-self" "child" "descendant"     
;                "descendant-or-self" "following" "following-sibling"
;                "parent" "preceding" "preceding-sibling" "self"))
;             "::"
;             (rnd-member '("*" "node()" "text()"))))))    
;    (let loop ((num-steps max-steps)
;               (lpath (make-rnd-step)))
;      (if (zero? num-steps)
;          lpath
;          (loop (- num-steps 1)
;                (string-append lpath "/" (make-rnd-step)))))))

;-------------------------------------------------
; Predefined location paths

(define textual-lpaths
  '("descendant-or-self::node()/following::*"
    "descendant::text()/parent::*"
    "ancestor-or-self::node()/descendant::node()/following::node()"
    "descendant::node()/preceding-sibling::node()/following-sibling::*"
    "descendant-or-self::*/following-sibling::text()/ancestor-or-self::node()"
    "descendant-or-self::text()/preceding::text()/child::*"
    "descendant::node()/child::*/preceding::*"
    "descendant::*/descendant::node()/following-sibling::node()"
    "child::*/parent::*/descendant::*/preceding::text()"
    "ancestor-or-self::*/descendant-or-self::*/following::node()/self::text()"
    "descendant::text()/self::node()/following::text()/child::node()"
    ))

;-------------------------------------------------
; Benchmarks

; depth - depth of the source SXML document
(define (benchmark-sxpath depth lpath)
  (let ((tree (generate-tree depth)))
    (display "Tree generated")    
    (newline)
    (display "Location path: ")
    (display lpath)
    (newline)
    (let ((f-common (sxpath lpath))
          (f-with-context (sxpath-with-context lpath))
          (f-ddo (ddo:sxpath lpath)))
      (let* ((res1
              (sxml:time-apply f-common (list tree)))
             (res2
              (begin
                (display "Time required for sxpath:               ")
                (display (cdr res1))
                (newline)
                (sxml:time-apply f-with-context (list tree))))
             (res3
              (begin
                (display "Time required for sxpath-with-context:  ")
                (display (cdr res2))
                (newline)
                (sxml:time-apply f-ddo (list tree)))))
        (display "Time required for DDO SXPath:           ")
        (display (cdr res3))
        (newline)
        (newline)
        #t))))

(for-each
 (lambda (lpath) (benchmark-sxpath 8 lpath))
 textual-lpaths)
