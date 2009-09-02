;; Implementation of time-apply
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin

; (lambda f argv)
; Applies procedure `f' to its argument list `argv'
; Returns:  (cons f-result exec-time)
; f-result - result produced by applying procedure `f'
; exec-time - time in milliseconds taken by `f' to execute
(cond-expand
 
 (chicken
  (define (sxml:time-apply f argv)
    (call-with-values
     (lambda () (cpu-time))
     (lambda (user-begin system-begin)
       (let ((res (apply f argv)))
         (call-with-values
          (lambda () (cpu-time))
          (lambda (user-end system-end)
            (cons res
                  (- (+ user-end system-end)
                     (+ user-begin system-begin))))))))))
 
 (plt
  (define (sxml:time-apply f argv)
    (call-with-values
     (lambda () (time-apply f argv))
     (lambda (f-res-lst t1 t2 t3)
       (cons (car f-res-lst) t2)))))
 
 (gambit
  (define (sxml:time-apply f argv)
    (let ((t-begin (time->seconds (current-time))))
      (let ((f-res (apply f argv)))
        (let ((t-end (time->seconds (current-time))))
          (cons f-res (* (- t-end t-begin) 1000)))))))
)
