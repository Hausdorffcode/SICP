#lang planet neil/sicp
;f(x, y) = x(1 + xy)^2 + y(1 - y) + (1 + xy)(1 - y)
;a = 1 + xy
;b = 1 - y
;f(x, y) = xa^2 + yb + ab

(define (square x)
  (* x x))

;with f-helper
(define (f1 x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
     (- 1 y)))

;with lambda
(define (f2 x y)
  ((lambda (a b)
     (+ (* x (square a))
       (* y b)
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;with let
(define (f3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;with internal definitions
(define (f4 x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
       (* y b)
       (* a b)))