#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define empty-board nil)

(define (make-pos r c)
  (cons r c))

(define (row-pos p) (car p))
(define (col-pos p) (cdr p))

(define (adjoin-position r c positions)
  (cons (make-pos r c) positions))

(define (safe? c positions)
  (define (attacked-in-col queens c)
    (display (list queens c)) (newline)
    (cond ((null? queens)
           nil)
          ((= (col-pos (car queens))
              c)
           (attacked-in-col (cdr queens) c))
          (else
           (let ((qr (row-pos (car queens)))
                 (qc (col-pos (car queens))))
             (append (list qr
                           (- qr (- c qc))
                           (+ qr (- c qc)))
                     (attacked-in-col (cdr queens) c))))))
  (define (queen-row queens)
    (if (= (col-pos (car queens))
           c)
        (car (car queens))
        (queen-row (cdr queens))))
  (define (contains? haystack needle)
    (cond ((null? haystack) #f)
          ((= (car haystack) needle) #t)
          (else (contains? (cdr haystack) needle))))
  (not (contains? (attacked-in-col positions c)
                  (queen-row positions))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)

