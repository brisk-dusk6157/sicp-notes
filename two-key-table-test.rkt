#lang sicp

(define (assoc key records)
  (cond ((null? records) false)
	((eq? key (caar records)) (car records))
	(else (assoc key (cdr records)))))


(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      false))
	false)))


(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (let* ((new-record (cons key-2 value))
			 (new-subtable (cons new-record '())))
		    (cons (cons key-1 new-subtable)
			  (cdr table))))))
  'ok)

(define (make-table)
  (cons '*table* '()))

(define t (make-table))
(insert! 'a 'a1 10 t)
(insert! 'a 'a2 11 t)
(insert! 'a 'a3 12 t)
(insert! 'a 'a4 13 t)
(insert! 'b 'b1 20 t)
(insert! 'b 'b2 21 t)
(insert! 'b 'b3 22 t)

(lookup 'a 'a1 t)
(lookup 'a 'a4 t)
(lookup 'a 'a5 t)
(lookup 'b 'b1 t)
(lookup 'b 'b3 t)
(lookup 'b 'b4 t)
(lookup 'c 'c1 t)
