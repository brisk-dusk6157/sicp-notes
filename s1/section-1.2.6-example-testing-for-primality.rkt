#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (divides? d n) (= 0 (remainder n d)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (inc test-divisor)))))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time)))
  (prime? n))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n count)
  (if (> count 0)
      (cond ((even? n)
             (search-for-primes (+ n 1)
                                count))
            ((timed-prime-test n)
             (search-for-primes (+ n 2)
                                (dec count)))
            (else
             (search-for-primes (+ n 2)
                                count)))))


(search-for-primes 10 3)
(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
(search-for-primes 1000000000 3)
