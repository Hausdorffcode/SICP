#lang planet neil/sicp

(define (cont-frac n d k)
  (define (cf result p)
    (cond ((= p 0) result)
          (else (cf (/ (n p) (+ (d p) result)) (- p 1)))))
  (cf (/ (n k) (d k)) (- k 1)))

(define (tan-cf x k)
  (define (square y) (* y y))
  (cont-frac (lambda (i) (cond ((= i 1) x)
                               (else (- (square x)))))
             (lambda (i) (-  (* 2 i) 1))
             k))
(tan 10.0)
(tan-cf 10.0 100)
(tan 78.0)
(tan-cf 78.0 100)
