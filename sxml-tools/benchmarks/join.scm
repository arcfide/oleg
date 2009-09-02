;; Comparing different algorithms of evaluating XPath equality comparison
; (In XPath 2.0 such a kind of comparison is called General Comparison)
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

(define (sxml:list-head lst k)
  (if (or (null? lst) (zero? k))
      '()
      (cons (car lst) (sxml:list-head (cdr lst) (- k 1)))))

; Generates 2 string sets. Each string set contains num members, each member
; is lng characters in length
; Result: (cons string-set1 string-set2)
(define (generate-string-sets num lng)
  (letrec
      ((add-to-base
        ; Generate a set of strings that start from the base string
        (lambda (base-string lng num)
          (if
           (= (string-length base-string) (- lng 1))
           ; Just one more character to add
           (let loop ((code 0)
                      (res '()))
             (if
              (or (= code 256) (= code num))  ; finish
              res  ; don't care about order
              (loop (+ code 1)
                    (cons
                     (string-append base-string
                                    (string (integer->char code)))
                     res))))
           (let rpt ((code 0)
                     (remain num)
                     (res '()))
             ;(pp remain)
             (if
              (or (= code 256) (zero? remain))
              res
              (let ((next (add-to-base
                           (string-append base-string
                                    (string (integer->char code)))
                           lng
                           remain)))
                ;(pp next)
                (rpt (+ code 1)
                     (- remain (length next))
                     (append next res)))))))))
    (let ((str-set (add-to-base
                    (make-string (- lng 3) (integer->char 0))
                    lng (* num 2))))
      (cons
       (sxml:list-head str-set num)
       (list-tail str-set num)))))

(define (join-compare num lng)
  (display "num = ")
  (display num)
  (display ";\t length = ")
  (display lng)
  (newline)
  (let* ((sets (generate-string-sets num lng))
         (set1 (car sets))
         (set2 (cdr sets)))
    (display "String sets generated")
    (newline)
    ;(collect-garbage)
    (let* (
;           (res1 (begin
;                   (display "Nested loop join:\t")
;                   (sxml:time-apply sxml:nested-loop-join
;                                    (list set1 set2 string=?))))
           (res2 (begin
;                   (display (car res1))
;                   (display "\t")
;                   (display (cdr res1))
;                   (newline)
                   ;(collect-garbage)
                   (display "Merge-sort join:\t\t")
                   (sxml:time-apply sxml:merge-sort-join
                                    (list set1 set2 string=?))))
           (res3 (begin
                   ;(display (car res2))
                   ;(display "\t")
                   (display (cdr res2))
                   (newline)
                   ;(collect-garbage)
                   (display "Radix-sort join:\t\t")
                   (sxml:time-apply sxml:radix-sort-join
                                    (list set1 set2 eq?)))))
      ;(display (car res3))
      ;(display "\t")
      (display (cdr res3))
      (newline)
      (newline))))

(for-each
 (lambda (num) (join-compare num 10))
 '(10000 20000 30000 40000 50000 60000 70000)
 )

;(for-each
; (lambda (num) (join-compare num 50))
; '(100000 200000 300000 400000 500000 600000 700000 800000 900000 1000000)
; )
