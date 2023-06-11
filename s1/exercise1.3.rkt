#lang sicp
(define (square x) (* x x))
(define (sum-of-squares a b)
  (+ (square a)
     (square b)))

(define (min a b c)
  (if (< a b)
      (if (< a c) a c)
      (if (< b c) b c)))

(define (ex13 a b c)
  (if (> a b)
      ;; a is one of the two larger numbers
      (if (> b c)
          ;; b is larger of two remaining
          (sum-of-squares a b)
          ;; c is larger of two remaining
          (sum-of-squares a c))
      ;; b is one of the two larger numbers
      (if (> a c)
          ;; a is larger of two remaining
          (sum-of-squares a b)
          ;; c is larger of two remaining
          (sum-of-squares b c))))

(ex13 1 2 3)
(ex13 1 3 2)
(ex13 2 1 3)
(ex13 2 3 1)
(ex13 3 1 2)
(ex13 3 2 1)
