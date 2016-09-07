#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
;(paint einstein)
;(paint (flip-vert einstein))
;(paint (number->painter 0))
;(paint diagonal-shading)
; (paint-hires  (below (beside diagonal-shading
;                        (rotate90 diagonal-shading))
;                (beside (rotate270 diagonal-shading)
;                        (rotate180 diagonal-shading))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define einstein4 (flipped-pairs einstein))
;(paint einstein4)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;(paint (right-split einstein 4))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(paint (corner-split einstein 4))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;higher-order operations
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
(define flipped-pairs2
  (square-of-four identity flip-vert identity flip-vert))
(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotare180 flip-vert)))
    (combine4 (corner-split painter n))))

;frames
;ex2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vector)
  (car vector))
(define (ycor-vect vector)
  (cdr vector))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))
(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
;ex2.47
(define (make-frames origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame a-frame)
  (car a-frame))
(define (edge1-frame a-frame)
  (cadr a-frame))
(define (edge2-frame a-frame)
  (caddr a-frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edgel-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;ex2.48
(define (make-segment start end)
  (cons start end))
(define (start-segment a-line)
  (car a-line))
(define (end-segment a-line)
  (cdr a-line))

(define (segment->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))