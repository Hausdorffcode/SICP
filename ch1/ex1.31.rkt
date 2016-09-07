#lang planet neil/sicp
(define (product-re term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a ) result))))
  (iter a 1))

(define (factorial n)
  (define (next x)
    (+ 1 x))
  (define (term x)
    x)
  (product term 1 next n))
(factorial 10)

(define (my-pi acurrent)
  (define (next k)
    (+ k 2))
  (define (term k)
    (/ (* (- k 1) (+ k 1)) (* k k)))
  (* 4.0 (product term 3 next acurrent)))
(my-pi 1000)
(my-pi 100000)