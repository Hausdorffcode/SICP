#lang planet neil/sicp
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))
;((average-damp square) 10)

(define (my-sqrt1 x)
  (fixed-point (average-damp (lambda (i) (/ x i)))
               1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (i) (/ (square i) x)))
               1.0))

;Newton's Menthod
;derivative
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newton-menthod g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a x x)
       (* b x)
       c)))

(define (cubic-root a b c)
  (newton-menthod (cubic a b c) 1.0))
(cubic-root 3 2 1)
(cubic-root 2 5 5)