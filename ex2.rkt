#lang racket

(require "ex1.rkt")

; odd-only takes an indefinite number of arguments and returns a list containg only the odd ones

(define (odd-only . lst)
  (if (null? lst)
      '()
      (let ((head (car lst)))
        (if (odd? head)
            (cons head (apply odd-only (cdr lst)))
            (apply odd-only (cdr lst))))))

; (odd-only 2 5 1 14 6 4 7)
; (apply odd-only '(2 5 1 14 6 4 7))

;;; higher order functions

;; map

; example of application

(define (square-list lst) 
  (if (null? lst)
      '()
      (cons (square (car lst)) (square-list (cdr lst)))))

; (square-list '(1 2 3))
; (map square '(1 2 3))

(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (my-map f (cdr lst)))))

(define (my-map* f . lst)
  (map f lst))

;; filter 

; (filter odd? '(2 5 1 14 6 4 7))

(define (my-filter f lst)
  (if (null? lst)
      '()
      (if (f (car lst))
          (cons (car lst) (my-filter f (cdr lst)))
          (my-filter f (cdr lst)))))

(define (3-multiplier? n) (zero? (modulo n 3)))

; (filter 3-multiplier? '(4 30 9 27 5 0 10))

;; fold

; example of application

(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

; (sum-list '(1 2 3))
; (foldl + 0 '(1 2 3))
  
(define (factorial-fold n)
  (foldl * 1 (sequence 1 n))) 

(define (join-fold lst joiner)
  (foldl (lambda(x y) (string-append y joiner x)) (car lst) (cdr lst)))

; sort takes a list to be sorted and a function f that returns true if the first argument is less than 
; the second (return false to sort with a descending order)

(define (sort f lst)
  (define (min lst)
    (foldl (lambda(x y) (if (f x y) x y)) (car lst) (cdr lst)))
  (if (null? lst)
      '()
      (let ((m (min lst)))
        (cons m (sort f (remove m lst))))))

; (sort < '(8 10 4 6))

; list< compares two numeric lists. returns true if sum of the elements of the first list is less than the second. 
; if the two sums are equal, list< compares the length of the two lists. 

(define (list< x y)
  (let ((sum1 (foldl + 0 x)) (sum2 (foldl + 0 y)))
    (if (= sum1 sum2)
        (< (length x) (length y))
        (< sum1 sum2))))

; (sort list< '((1 10) (1 1) (2) (7) (2 5) (12 3) (2) (3 0 1)))

;; macro (to better understand how the code is transformed use the Macro Stepper tool of DrRacket)

; swap

(define-syntax swap
  (syntax-rules ()
    ((_ x y)
     (let ((temp x))
       (begin
         (set! x y)
         (set! y temp))))))

; min

(define-syntax min 
  (syntax-rules ()
    ((_ val1 val2)
       (if (< val1 val2)
           val1
           val2))))

#|
(min (begin 
         (display "A")
         (newline)
         5)
     (begin
       (display "B")
       (newline)
       3))
|# 

; min with evaluation

(define-syntax min2 
  (syntax-rules ()
    ((_ val1 val2)
     (let ((x val1) (y val2))
       (if (< x y)
           x
           y)))))

#|
(min2 (begin 
         (display "A")
         (newline)
         5)
     (begin
       (display "B")
       (newline)
       3))
|#

; repeat-until, a variant of the do-while construct

(define-syntax repeat
  (syntax-rules (until)
    ((_ body ... until cond)
     (let loop ()
       (begin
         body ...
         (when cond
           (loop)))))))

#|
(let ((x 5))
  (repeat
   (display x)
   (newline)
   (set! x (- x 1))
   until (positive? x)))
|#