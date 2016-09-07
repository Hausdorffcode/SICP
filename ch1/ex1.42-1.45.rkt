#lang planet neil/sicp
;ex1.42
(define (inc x)
  (+ 1 x))
(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

;ex1.43
(define (repeated-re f n)
  (cond ((= n 1) f)
        (else (compose f (repeated-re f (- n 1))))))

(define (repeated f n)
  (define (iter result n)
    (cond ((= n 1) result)
          (else (iter (compose f result) (- n 1)))))
  (iter f n))
                
((repeated square 2) 5)

;ex1.44
(define dx 0.0001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
((smooth square) 5)

(define (n-flod-smoothed f n)
  ((repeated smooth n) f))

((n-flod-smoothed square 10) 5)

;ex1.45

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
(define (average-damp g)
  (lambda (x) (/ (+ x (g x)) 2)))
(define (nth-mul x n)
  (cond ((= n 1) x)
        (else (* x (nth-mul x (- n 1))))))
(nth-mul 2 10)

(define (nth-root x n)
  (fixed-point ((repeated average-damp (ceiling (log n))) (lambda (y) (/ x (nth-mul y n))))
               1.0))
(nth-root 2 6)