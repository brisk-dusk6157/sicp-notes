#lang sicp

(define (** a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))


(define (double x) (* x 2))
(define (half x) (/ x 2))
(define (multiply a b)
  (cond ((= b 1) a)
        ((even? b) (double (multiply a
                                     (half b))))
        (else (+ a (multiply a
                             (dec b))))))

(define (multiply-iter x y)
  (define (iter a b p)
    ;; a*b+p = x*y
    ;; every time b is odd, a is integrated into p
    ;; b is odd at least once, for b=1 before it gets to 0
    (cond ((= b 0) p)
          ((even? b) (iter (double a)
                           (half b)
                           p))
          (else (iter a
                      (dec b)
                      (+ p a))))
    )
  (iter x y 0))

(multiply-iter 101 12)
