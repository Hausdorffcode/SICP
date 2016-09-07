#lang planet neil/sicp
(define a 1)
(define b 2)
(car '(a b))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))

(car ''adff)
;(car 'adffg)
;(car 'fghd'f)
