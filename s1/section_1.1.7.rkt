#lang sicp

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 4)
(sqrt 2)
(sqrt 0.0000000001)
(sqrt 10000000000000000000000000000000000000000)
