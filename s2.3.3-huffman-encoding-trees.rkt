#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoint-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set)))
	 (cons x set))
	(else
	 (cons (car set) (adjoint-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoint-set (make-leaf (car pair) (cadr pair))
		     (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge node-set)
  (cond ((null? node-set)
	 (error "Empty node-set -- SUCCESSIVE-MERGE"))
	((null? (cdr node-set))
	 (car node-set))
	(else
	 (successive-merge
	  (adjoint-set (make-code-tree
			(car node-set)
			(cadr node-set))
		       (cddr node-set))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
	((contains? (symbols (left-branch tree)) symbol)
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((contains? (symbols (right-branch tree)) symbol)
	 (cons 1 (encode-symbol symbol (right-branch tree))))
	(else
	 (error "symbol has no encoding -- ENCODE-SYMBOL" symbol))))

(define (contains? haystack needle)
  (cond ((null? haystack) false)
	((eq? (car haystack) needle) true)
	(else (contains? (cdr haystack) needle))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond ((= 0 bit)
	 (left-branch tree))
	((= 1 bit)
	 (right-branch tree))
	(else
	 (error "bad bit -- CHOOSE-BRANCH" bit))))
