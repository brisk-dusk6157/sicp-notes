#lang sicp

(define (square x) (* x x))

(define (exp b n)
  (define (exp-iter p e a)
    ;; invariant a*p^e = b^n
    ;; every time (odd? e) is true the value of p is integrated into a; it always happens in the end for (= e 1)
    (cond ((= e 0) a)
          ((even? e) (exp-iter (square p) (/ e 2) a))
          (else (exp-iter p (dec e) (* a p)))))  
  (exp-iter b n 1))

(exp 3 3)
