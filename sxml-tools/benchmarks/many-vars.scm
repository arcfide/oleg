;; Evaluating XPath expression that contains many XPath variables
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

; Generates n variables
; Returns (listof string)
(define (gen-vars n)
  (if (zero? n)
      '()
      (cons (string-append "a" (number->string n))
            (gen-vars (- n 1)))))

; Generates XPath expression that sums variables
(define (gen-xpath-expr vars)
  (let loop ((vars vars)
             (res '()))
    (if
      (null? vars)
      (apply string-append res)
      (loop
       (cdr vars)
       (if (null? res)  ; called for first time
           (list "$" (car vars))
           (cons "$"
                 (cons (car vars)
                       (cons " + " res))))))))

; Generates var binding
(define (gen-var-binding vars)
  (map
   (lambda (str) (cons (string->symbol str) 1))
   vars))

; Repeats the function call to func for n times
(define (repeat-n n func arg-lst)
  (let loop ((k 1))
    (if
     (= k n)
     (apply func arg-lst)    
     (begin
       (apply func arg-lst)
       (loop (+ k 1))))))

(define (benchmark-vars n)
  (display n)
  (display "\t")  
  (let* ((vars (gen-vars n))
         (expr (gen-xpath-expr vars))
         (var-binding (gen-var-binding vars))
         (f-txp (sxml:xpath-expr expr))
         (f-ddo (ddo:xpath-expr expr)))
    (let* ((res1
            (sxml:time-apply f-txp (list '() var-binding)))
           (res2
            (begin
              ;(display (car res1))
              ;(display "\t")
              (display (cdr res1))
              (display "\t")
              ;(collect-garbage)              
              (sxml:time-apply f-ddo (list '() var-binding)))))
      ;(display (car res2))
      ;(display "\t")
      (display (cdr res2))
      (newline)))
  ;(collect-garbage)
  #t)

(let ((beg 1000)
      (end 10000)
      (step 1000))
  (let loop ((n beg))
    (if
     (> n end)
     #t
     (begin
       (benchmark-vars n)
       (loop (+ n step))))))
