#lang planet neil/sicp

(define (iterative-improve good-enough? improve-guess)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve-guess guess)))
        (cond ((good-enough? guess next) next)
              (else (try next)))))
    (try first-guess)))

(define (my-sqrt x)
  ((iterative-improve
    (lambda (y z) (< (abs (- y z)) 0.001))
    (lambda (y) (/ (+ x (/ x y)) 2)))
   1.0))
(sqrt 5)
(my-sqrt 5)

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  ((iterative-improve
    (lambda (y z) (< (abs (- y z)) tolerance))
    f)
   first-guess))
(fixed-point cos 1.0)