#lang planet neil/sicp
(define (prime? n)
  (= (smallest-divisor n) n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor )))))
(define (divisor? a b)
  (= (remainder b a) 0))
(define (square x)
  (* x x))

(define (next n)
  (cond ((= n 2) 3)
        ;;((even? n) (+ n 1))
        (else (+ n 2))))
(define (continue-primes n count)
  (cond ((= count 0) (display "are primes"))
        ((prime? n) (display n)
                    (newline)
                    (continue-primes (next-odd n) (- count 1)))
        (else (continue-primes (next-odd n) count))))
(define (search-for-primes n)
  (let ((start-time (runtime)))
    (continue-primes n 3)
    (newline)
    (- (runtime) start-time)))
(define (next-odd n)
  (if (odd? n)
      (+ n 2)
      (+ n 1)))