#lang sicp

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (average a b) (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess prev-guess)
  (< (abs (- prev-guess guess)) (* 0.01 guess)))

(define (sqrt x)
  (sqrt-iter 1.0 x x))

(sqrt 4)
(sqrt 9)
(sqrt 1e-40)
(sqrt 1e40)
