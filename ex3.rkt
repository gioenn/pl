#lang racket

(require "ex1.rkt")
(require "ex2.rkt")
(provide (all-defined-out))

;; fold

; example of application

(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

; (sum-list '(1 2 3))

(define (sum-list-tail i lst)
  (if (null? lst)
      i
      (sum-list-tail (+ (car lst) i) (cdr lst))))

; (foldl + 0 '(1 2 3))
; (foldr + 0 '(1 2 3))
; (foldl cons '() '(1 2 3))  > '(3 2 1)
; (foldr cons '() '(1 2 3))  > '(1 2 3)  

(define (my-foldl f i lst) 
  (if (null? lst)
      i
      (my-foldl f (f (car lst) i) (cdr lst))))

; (foldl string-append "" '("Ping" " " "Pong"))
; (foldl string-append (string-append "Ping" "") '(" " "Pong"))
; (foldl string-append (string-append " " "Ping") '("Pong"))
; (foldl string-append (string-append "Pong" " Ping") '())
; (foldl string-append "Pong Ping" '()) > "Pong Ping"

(define (my-foldr f i lst) 
  (if (null? lst)
      i
      (f (car lst) (my-foldr f i (cdr lst)))))

; (foldr string-append "" '("Ping" " " "Pong"))
; (string-append "Ping" (foldr string-append "" '(" " "Pong"))
; (string-append " " (foldr string-append "" '("Pong"))
; (string-append "Pong" (foldr string-append "" '())

(define (map-with-fold f lst)
  (foldr (lambda(x y) (cons (f x) y)) '() lst))

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

; (sort < '(8 8 10 4 6))

; list< compares two numeric lists. returns true if the sum of the elements of the first list is less than the second. 
; if the two sums are equal, list< compares the length of the two lists. 

(define (list<? x y)
  (let ((sum1 (foldl + 0 x)) (sum2 (foldl + 0 y)))
    (if (= sum1 sum2)
        (< (length x) (length y))
        (< sum1 sum2))))

; (sort list<? '((1 10) (1 1) (2) (7) (2 5) (12 3) (2) (3 0 1)))

; (sort (lambda(x y) (not (list<? x y)) '((1 10) (1 1) (2) (7) (2 5) (12 3) (2) (3 0 1))))    ; descending order
; (sort (lambda(x y) (list<? y x)) '((1 10) (1 1) (2) (7) (2 5) (12 3) (2) (3 0 1)))          ; descending order

;; macro (to better understand how the code is transformed use the Macro Stepper tool of DrRacket)

; swap

(define-syntax swap
  (syntax-rules ()
    ((_ x y)
     (let ((temp x))
       (begin
         (set! x y)
         (set! y temp))))))

; (define x 1)
; (define y 5)
; (swap x y)

; min

(define-syntax min 
  (syntax-rules ()
    ((_ val1 val2)
     (if (< val1 val2)
         val1
         val2))))

#|
(define x 0)
(min (begin 
         (set! x (+ x 1))
         5)
     6)
|#

; min with evaluation

(define-syntax min2 
  (syntax-rules ()
    ((_ val1 val2)
     (let ((v1 val1) (v2 val2))
       (if (< v1 v2)
           v1
           v2)))))

#|
(define x 0)
(min2 (begin 
         (set! x (+ x 1))
         5)
    6)
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

; (my-let ((x 1) (y 2) (z 3)) (+ x y z))
; ((lambda (x y z) (+ x y z)) 1 2 3) 

; two variations of the for macro

; (for x in '(...)
;    code ...)

; (for y from 0 to 10
;    code ... 
;    (+ y 2))


(define-syntax for
  (syntax-rules (in from to)
    ((_ var in lst body ...)
     (for-each (lambda(var)
                 body ...) lst))
    ((_ var from start to end body ... incr)
     (let ((comp (if (> start end) >= <=)))
       (let loop ((var start))
            (when (comp var end)
              (begin
                body ...
                (loop incr))))))))


(define-syntax my-let* (syntax-rules ()
    ((_ ((var val)) body ...) 
     ((lambda (var) body ...) val )) ; double parenthesis
    ((_ ((var val) . rest) body ...) 
     ((lambda (var)
        (my-let* rest body ...)) val ))))

; (my-let* ((x 1) (y 2) (z 3)) (+ x y z))
; (lambda(x) (my-let* ((y 2) (z 3)) (+ x y z)) 1)
; (lambda(x) (lambda(y) (my-let* ((z 3)) (+ x y z)) 2) 1)                     
; ((lambda(x) ((lambda(y) ((lambda(z) (+ x y z)) 3)) 2)) 1)

(define-syntax my-let 
  (syntax-rules ()
    ((_ ((var expr) ...) body ...)
     ((lambda (var ...) body ...) expr ...))))

; (my-let ((x 1) (y 2) (z 3)) (+ x y z))
; ((lambda (x y z) (+ x y z)) 1 2 3) 
