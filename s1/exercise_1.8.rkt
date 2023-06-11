#lang sicp

(define (cube-root-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (cube-root-iter (improve guess x) guess x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

(define (good-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) (* 0.01 guess)))

(define (cube-root x)
  (cube-root-iter 1.0 x x))

(cube-root 1)
(cube-root 4)
(cube-root 8)
(cube-root 1e-120)
(cube-root 1e120)
