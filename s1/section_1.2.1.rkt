#lang sicp

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* product counter)
                   (+ counter 1))))
  (fact-iter 1 1))

(factorial2 6)
