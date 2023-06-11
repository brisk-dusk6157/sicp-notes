#lang sicp

(define (average a b) (/ (+ a b) 2))

(define (square a) (* a a))

(define (sqrt x)
  (define (good-enough? guess prev-guess)
    (< (* 1e4 (abs (- guess prev-guess))) guess))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter 1.0 x))

(sqrt 1e-40)
(sqrt 1e40)
(sqrt 1e2)
(sqrt 1e-2)
(sqrt 9)

