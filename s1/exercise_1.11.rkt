#lang sicp

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f-iter n)
  (define (iter p pp ppp count)
    (if (= count 0)
        p
        (iter (+ p
                 (* 2 pp)
                 (* 3 ppp))
              p
              pp
              (dec count))))

  (if (< n 3)
      n
      (iter 2 1 0 (- n 2))))


(= (f 3) (f-iter 3))
(= (f 5) (f-iter 5))
(= (f 7) (f-iter 7))
(= (f 10) (f-iter 10))
