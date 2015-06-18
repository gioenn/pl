#lang racket

;; homework 1 

; exercise 1

(define (gcd a b)
  (if (zero? b) a
      (gcd b (modulo a b))))

; exercise 2

(define (nested-length lst)
  (if (null? lst)
      0
      (if (list? (car lst))
          (+ (nested-length (car lst)) (nested-length (cdr lst)))
          (+ 1 (nested-length (cdr lst))))))

(define (nested-length2 lst)
  (define (aux elem acc)
    (if (list? elem) (+ acc (foldl aux 0 elem)) (+ acc 1)))
  (foldl aux 0 lst)) 

; ex 3

(define (print-matrix v)
  (let row-loop ((i 0))
    (unless (= i (vector-length v))
      (let ((row (vector-ref v i)))
        (if (vector? row)
            (let col-loop ((j 0))
              (unless (= j (vector-length row))
                (display (vector-ref row j))
                (display "\t")
                (col-loop (+ j 1))))
            (display row))
        (newline)
        (row-loop (+ i 1))))))

; ex 4

(define (vector*! v1 v2 v3) 
  (let loop ((i 0))
    (unless (= i (vector-length v2))
      (vector-set! v1 i (make-vector (vector-length v3)))
      (let ((n (vector-ref v2 i)) (v (vector-ref v1 i)))
        (let mul ((j 0))
          (unless (= j (vector-length v3))
            (vector-set! v j (* n (vector-ref v3 j)))
            (mul (+ j 1)))))
      (loop (+ i 1)))))

(define v1 (make-vector 2))      
(vector*! v1 #(3 2) #(4 5 7))

; ex 5 

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2))
            (zip (cdr l1) (cdr l2)))))

(define zipped (zip '(1 2 3) '(4 5 6)))

; ex 6

(define (split lst splitter)
  (define (split-aux lst acc res)
    (cond 
      ((null? lst) (if (null? acc) res (append res (list acc))))
      ((equal? (car lst) splitter)
       (split-aux (cdr lst) '() (append res (if (null? acc) acc (list acc)))))
      (else (split-aux (cdr lst) (append acc (list (car lst))) res))))
  (split-aux lst '() '()))

(split '(0 1 0 1 0 1 0 2 0 0 4 0) 0)

; ex 7

(define (takewhile pred lst)
  (let ((elem (car lst)))
    (cond 
      ((null? lst) '())
      ((pred elem) (cons elem (takewhile pred (cdr lst))))
      (else '()))))


#! (takewhile negative? (sequence -5 10))

; ex 8

(define (flatmap f lst)
  (if (null? lst)
      '()
      (let ((elem (car lst)))
        (if (list? elem)
            (append (flatmap f elem) (flatmap f (cdr lst)))
            (cons (f elem) (flatmap f (cdr lst)))))))


; ex 9

(define-syntax on-sign
  (syntax-rules (zero: neg: pos:)
    ((on-sign val pos: pcode zero: zcode neg: ncode)
     (let ((v val))
       (cond 
         ((positive? v) pcode)
         ((zero? v) zcode)
         (else ncode))))))
