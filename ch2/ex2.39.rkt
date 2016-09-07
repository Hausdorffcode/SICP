#lang planet neil/sicp
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (cond ((null? sequence) initial)
        (else (op (car sequence) (fold-right op initial (cdr sequence))))))

(define (reserve sequence)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              sequence))
(define (reserve2 sequence)
  (fold-left (lambda(x y) (cons y x))
             nil
             sequence))

(define x (list 1 2 3 4 5 6))
(reserve x)
(reserve2 x)