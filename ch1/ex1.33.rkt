#lang planet neil/sicp
(define (filtered-accumulate-re combine null-value term a next b valid?)
  (if (> a b)
      null-value
      (let ((rest-terms (filtered-accumulate combine
                                             null-value
                                             term
                                             (next a)
                                             next
                                             b
                                             valid?)))
        (if (valid? a)
            (combine (term a) rest-terms)
            rest-terms))))

(define (filtered-accumulate combine null-value term a next b valid?)
  (define (iter a result)
    (if (> a b)
        result
        (if (valid? a)
            (iter (next a) (combine result (term a)))
            (iter (next a) result))))
  (iter a null-value))
        
(define (prime? n)
  (= (smallest-divisor n) n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divisor? a b)
  (= (remainder b a) 0))
(define (square x)
  (* x x))

(define (prime-sum a b)
  (define (term x)
    x)
  (define (next x)
    (+ x 1))
  (filtered-accumulate + 0 term a next b prime?))
(prime-sum 1 10)

