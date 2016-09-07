#lang planet neil/sicp
(define (sum term a next b)
  (cond ((> a b) 0)
        (else (+ (term a) (sum term (next a) next b)))))

(define (id x)
  x)
(define (inc x)
  (+ x 1))
(define (sum-integers a b)
  (sum id a inc b))
(sum-integers 1 10)

(define (cube x)
  (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)

(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))
(define (pi-next x)
  (+ x 4))
(define (pi-sum a  b)
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (define (add-dx x)
  (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)