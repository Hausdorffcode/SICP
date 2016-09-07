#lang planet neil/sicp
(define (last-pair items)
  (cond ((null? (cdr items)) (car items))
        (else (last-pair (cdr items)))))
(last-pair (list 23 453 65 33))
