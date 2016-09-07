#lang planet neil/sicp
(define (fringe a-tree)
  (cond ((null? a-tree) nil)
        ((not (pair? a-tree)) (list a-tree))
        (else (append (fringe (car a-tree))
                      (fringe (cdr a-tree))))))

(define x (list (list 1 2) (list 3 4)))
(equal? (fringe x) (list 1 2 3 4))