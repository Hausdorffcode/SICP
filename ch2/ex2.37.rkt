#lang planet neil/sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (car-n seqs)
  (map car seqs))
(define (cdr-n seqs)
  (map cdr seqs))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (car-n seqs))
            (accumulate-n op init (cdr-n seqs)))))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list 1 2 3 4))
(define w (list 4 3 2 1))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(equal? (dot-product v w) 20)

(define (matrix-*-vector m v)
  (map (lambda (vector) (dot-product vector v))
       m))
(equal? (matrix-*-vector m v) (list 30 56 80))

(define (transpose mat)
  (accumulate-n cons nil mat))
(equal? (transpose m) (list (list 1 4 6) (list 2 5 7) (list 3 6 8) (list 4 6 9)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))
(matrix-*-matrix m (transpose m)) 