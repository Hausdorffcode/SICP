#lang planet neil/sicp
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
             
             next)
            (else
             (display guess)
             (newline)
             (try next)))))
  (try first-guess))


(display "no average damping")
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2)

(display "with average damping")
(newline)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
             2)