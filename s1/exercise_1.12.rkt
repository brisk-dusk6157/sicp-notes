#lang sicp

(define (pascal i j)
  (if (or (<= j 0) (>= j i))
      1
      (+ (pascal (dec i) (dec j))
         (pascal (dec i) j))))

(pascal 4 3)
