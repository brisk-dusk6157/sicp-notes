#lang scheme

(require racket/draw)

(define (make-segment start end) (list start end))
(define (start-segment s) (car s))
(define (end-segment s) (cadr s))

(define (make-vect xcor ycor) (list xcor ycor))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))

(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))
(define unit-square (make-frame (make-vect 0.0 0.0)
                                (make-vect 1.0 0.0)
                                (make-vect 0.0 1.0)))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v)
                (xcor-vect w))
             (+ (ycor-vect v)
                (ycor-vect w))))
(define (sub-vect v w)
  (make-vect (- (xcor-vect v)
                (xcor-vect w))
             (- (ycor-vect v)
                (ycor-vect w))))
(define (scale-vect s w)
  (make-vect (* s
                (xcor-vect w))
             (* s 
                (ycor-vect w))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (define width 1000)
    (define height 1000)
    (define target (make-bitmap width height))
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc set-background "white")
    (send dc clear)  ; applies background

    (define (draw-line start end)
      (let ((x0 (xcor-vect start))
            (y0 (ycor-vect start))
            (x1 (xcor-vect end))
            (y1 (ycor-vect end)))
        (send dc draw-line (* width x0) (* height y0) (* width x1) (* height y1))))

    (for-each
     (lambda (segment)
       (draw-line ((frame-coord-map frame) (start-segment segment))
                  ((frame-coord-map frame) (end-segment segment))))
     segment-list)
    target))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate180 (rotate90 painter)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (save target path format)
  (send target save-file path format))

(define p-0-0 (make-vect 0 0))
(define p-0-1 (make-vect 0 1))
(define p-1-1 (make-vect 1 1))
(define p-1-0 (make-vect 1 0))

(define (outline f)
  ((segments->painter
    (list (make-segment p-0-0 p-0-1)
          (make-segment p-0-1 p-1-1)
          (make-segment p-1-1 p-1-0)
          (make-segment p-1-0 p-0-0)))
   f))

(define (x-shape f)
  ((segments->painter
    (list (make-segment p-0-0 p-1-1)
          (make-segment p-0-1 p-1-0)))
   f))

(define (diamond-shape f)
    (let ((mid1 (make-vect 0 0.5))
	  (mid2 (make-vect 0.5 1))
	  (mid3 (make-vect 1 0.5))
	  (mid4 (make-vect 0.5 0)))
      ((segments->painter
	(list (make-segment mid1 mid2)
	      (make-segment mid2 mid3)
	      (make-segment mid3 mid4)
	      (make-segment mid4 mid1)))
       f)))

(save ((beside outline)
       unit-square)
      "/Users/lmg/Desktop/picture_language/beside-x-outline.jpeg"
      'jpeg)
(save ((shrink-to-upper-right x-shape)
       (make-frame (make-vect 0.0 0.0)
                   (make-vect 1.0 0.0)
                   (make-vect 0.0 0.5)))
      "/Users/lmg/Desktop/picture_language/x-35.jpeg"
      'jpeg)
