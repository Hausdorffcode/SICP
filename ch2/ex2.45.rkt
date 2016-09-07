#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define (split op1 op2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  iter)
(define right-split (split beside below))
(paint (right-split einstein 4))
(define up-split (split below beside))
(paint (up-split einstein 4))