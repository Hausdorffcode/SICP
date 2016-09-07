#lang planet neil/sicp
(define x (cons (list 1 2 ) (list 3 4 )))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)
(count-leaves (list x x))
(define y (list (list 1 2) (list 4 5)))
(count-leaves y)
(count-leaves (list y y))
x
y

;mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* factor sub-tree)))
       tree))

(define z (list 2 (list 3 (list 4 5) 6) (list 7 8)))
(scale-tree z 10)
(scale-tree2 z 10)