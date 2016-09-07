#lang planet neil/sicp

(define (cont-frac n d k)
  (define (cf result p)
    (cond ((= p 0) result)
          (else (cf (/ (n p) (+ (d p) result)) (- p 1)))))
  (cf (/ (n k) (d k)) (- k 1)))

(+ 2.0 (cont-frac (lambda (i) 1.0)
           (lambda (i) (cond ((= 2 (remainder i 3)) (* 2 (ceiling (/ i 3))))
                             (else 1.0)))
           10))