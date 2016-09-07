#lang planet neil/sicp
(define  (abs-gcd a b)
  (define (gcd x y)
    (cond ((= y 0) x)
          (else (gcd y
                     (remainder x y)))))
  (gcd (abs a) (abs b)))
;(gcd 6 4)
;(gcd -4 6)
;(gcd 4 -6)
;(gcd -6 -4)
 

(define (make-rat n d) 
  (let ((g (gcd n d)))
    (cond ((< d 0) (cons (- (/ n g)) (- (/ d g))))
          (else (cons (/ n g) (/ d g))))))
    
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (rec-rat x)
  (make-rat (denom x) (numer x)))

;error -1 is not pair
;(define (sub-rat x y)
;  (add-rat x (mul-rat -1 y)))
(define (sub-rat x y)
   (make-rat (- (* (numer x) (denom y)) (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (mul-rat x (rec-rat y)))

(define (equal-rat? x y)
  (- (* (numer x) (denom y)) (* (denom x) (numer y))))

;test
(define one-half (make-rat 1 2))
(print-rat one-half)
(define minus-one-third (make-rat -1 3))
(print-rat (div-rat one-half minus-one-third))
(print-rat (mul-rat one-half minus-one-third))
(print-rat (add-rat minus-one-third minus-one-third))
(print-rat (sub-rat one-half minus-one-third))