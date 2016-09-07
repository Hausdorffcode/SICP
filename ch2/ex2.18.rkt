#lang planet neil/sicp
(define (reserve items)
  (define (iter items rese)
    (cond ((null? items) rese)
          (else (iter (cdr items) (cons (car items) rese)))))
  (iter items nil))
(reserve (list 2 4 5 6 8 9))