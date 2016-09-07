#lang planet neil/sicp

(define (reserve items)
  (define (iter items rese)
    (cond ((null? items) rese)
          (else (iter (cdr items) (cons (car items) rese)))))
  (iter items nil))

(define (deep-reserve a-tree)
  (define (iter x rese)
    (cond ((null? x) rese)
          ((not (pair? x)) x)
          (else (iter (cdr x)
                      (cons (deep-reserve (car x)) rese)))))
  (iter a-tree nil))

(define x (list (list 1 2) (list 3 4)))
(equal? (deep-reserve x) (list (list 4 3) (list 2 1)))
(define y (list (list 1 2) (list 3 4) (list 5 6)))
(equal? (deep-reserve y) (list (list 6 5) (list 4 3) (list 2 1)))