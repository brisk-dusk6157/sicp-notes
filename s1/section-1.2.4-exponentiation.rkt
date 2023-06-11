#lang sicp

(define (exp-rec b n)
  (if (= n 0)
      1
      (* b (exp b (dec n)))))

(define (exp-linear b n)
  (define (exp-iter product count)
    (if (= count 0)
        product
        (exp-iter (* b product) (dec count))))
  (exp-iter 1 n))

(define (square x) (* x x))

(define (exp-fast-rec b n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp b (/ n 2))))
        (else (* b (exp b (dec n))))))

(exp 2 10)
