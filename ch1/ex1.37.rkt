#lang planet neil/sicp
(define (cont-frac-re n d k)
  (define (co-cont-frac p)
    (cond ((= k p) (/ (n p) (d p)))
          (else (/ (n p) (+ (d p) (co-cont-frac (+ p 1)))))))
  (co-cont-frac 1))
      


(define (cont-frac n d k)
  (define (cf result p)
    (cond ((= p 0) result)
          (else (cf (/ (n p) (+ (d p) result)) (- p 1)))))
  (cf (/ (n k) (d k)) (- k 1)))


(/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10))
(/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11))
(/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12))
(/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           13))