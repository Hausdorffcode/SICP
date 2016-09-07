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
(define (my-sqrt2 x)
  (newton-menthod (lambda (y) (- (square y) x))
                  1.0))
(sqrt 5)
(my-sqrt1 5)
(my-sqrt2 5)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (my-sqrt3 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (my-sqrt4 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(my-sqrt3 5)
(my-sqrt4 5)
