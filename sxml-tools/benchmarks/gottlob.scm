; Reproducing benchmark expreriments suggested in paper by Gottlob:
; "Efficient Algorithms for Processing XPath Queries", VLDB 2002
; http://www.dbai.tuwien.ac.at/research/xmltaskforce/vldb2002.pdf
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

; Generates a flat document
(define (gott:doc i)
  (let loop ((i i)
             (res '()))
    (if (zero? i)
        `(*TOP* (a ,@res))
        (loop (- i 1)
              (cons '(b) res)))))

; Generate an XPath expr for experiment 1
; Query size is  1 + 2*factor
;  factor >= 1
(define (gott:xpath-expr1 factor)
  (if (= factor 1)
      "//a/b"
      (string-append (gott:xpath-expr1 (- factor 1))
                     "/parent::a/b")))

; Generates the XPath expr for experiment 2
;  factor >= 1
(define (gott:xpath-expr2 factor)
  (letrec
      ((predicates
        (lambda (factor)
          (if (= factor 0)
              ""
              (string-append "[ count(parent::a/b"
                             (predicates (- factor 1))
                             ")>1 ]")))))
    (string-append "//a/b" (predicates factor))))

; exp-no - experiment number
; xpath-gen - lambda that generates an XPath query
(define (gott:experiment doc-i exp-no lpath-gen max-factor)
  (newline)
  (display "Experiment ")
  (display exp-no)
  (newline)
  (display "Length-Factor\tTime-DDO\tTime-SXPath\tTime-Context")
  (newline)
  (let ((doc (gott:doc doc-i))
        (times 100))
    (let loop ((f 1))
      (if
       (> f max-factor)
       #t
       (let* ((lpath (gott:xpath-expr1 f))
              (impl-ddo (ddo:txpath lpath))
              (impl-conv (txpath lpath))
              (impl/c (txpath/c lpath)))
         (let* ((res1 (begin
                        (display f)
                        (display "\t\t")
                        (sxml:time-apply impl-ddo (list doc))))
                (res2 (begin
                        (display (cdr res1))
                        (display "\t\t")
                        (sxml:time-apply impl-conv (list doc))))
                (res3 (begin
                        (display (cdr res2))
                        (display "\t\t")
                        (sxml:time-apply impl/c (list doc)))))
           (display (cdr res3))
           ;(display (car res1))
           (newline)
           (loop (+ f 1))))))))

;(gott:experiment 2 1 gott:xpath-expr1 20)
(gott:experiment 2 1 gott:xpath-expr1 15)

;(gott:experiment 2 2 gott:xpath-expr2 20)
(gott:experiment 2 2 gott:xpath-expr2 15)
