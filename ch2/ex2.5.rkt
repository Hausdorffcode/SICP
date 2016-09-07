#lang planet neil/sicp
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car x)
  (define (count n x)
    (cond ((= (remainder x 2) 0) (count (+ n 1) (/ x 2)))
          (else n)))
  (count 0 x))
(define (cdr x)
  (define (count n x)
    (cond ((= (remainder x 3) 0) (count (+ n 1) (/ x 3)))
          (else n)))
  (count 0 x))

;test
(cons 3 4)
(car 648)
(cdr 648)