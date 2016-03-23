#lang racket

(provide (all-defined-out))

; basics

;; defining variables

(define two 2)

;; defining a function

(define square
  (lambda(x) (* x x)))

;; defining a function (shorthand)

(define (cube x) (* x x x))

;; if expression 

(define (abs x) (if (< x 0) (- x) x))

;; basic recursion

(define (pow a n)
  (if (zero? n)
      1
      (* a (pow a (- n 1)))))

;; tail recursion 

(define (pow-tail a n)
  (define (pow-aux n acc)
    (if (zero? n)
        acc
        (pow-aux (- n 1) (* acc a))))
  (if (or (>= n 0) (zero? a))
      (pow-aux (abs n) 1)
      (/ 1 (pow-aux (abs n) 1))))

;; bindings

; (simple-binding 3 4)  > 7
(define (simple-binding x y)
  (let ((x y) (y x))
    (+ x y))) 

; (simple-binding* 3 4)  > 8 
(define (simple-binding* x y) 
  (let* ((x y) (y x))
    (+ x y)))

;; list and accessors

(define mylist '(1 2 3))
; (cons 1 2)  > '(1 . 2)
; (list? (cons 1 2))  > #f
; (pair? mylist)  > #t
; (cons 1 (cons 2 (cons 3 '())))  > '(1 2 3)
; (car mylist)  > 1
; (car (cdr (cdr mylist)))  > 3
; (cadr mylist) alias (car (cdr l))  > 2
; (caddr mylist) alias (car (cdr (cdr l)))  > 3
; (equal? mylist (list 1 2 3))  > #t

;; quoting: quote (') prevents code evaluation, quasiquote evaluates only expressions prefixed with a comma

(define (pow-list a n)
  (quasiquote (,a ^ ,n = ,(pow-tail a n))))

;; homoiconicity

(define data/code '(+ 1 2))
; data/code
; (eval data/code)

;; defining a symbol

; (define y 0)
; (define z "y")
; (define z 'y)

;; sequence returns a list of numbers between lo and hi (included)
;; e.g. (sequence 0 5) > '(0 1 2 3 4 5)

(define (sequence lo hi)
  (cond ((> lo hi) (error "lo > hi"))
        ((< lo hi) (cons lo (sequence (+ 1 lo) hi)))
        (else (list lo))))

;; len returns the number of elements contained in a list (implemented with the 'named let' construct)

(define (len lst)
  (let loop ((l lst))
    (if (null? l)
        0
        (+ 1 (loop (cdr l))))))

(define (len-tail lst)
  (let loop ((l lst) (acc 0))
    (if (null? l)
        acc
        (loop (cdr l) (+ acc 1)))))

; join takes a list of strings and a joiner string. it appends all the strings inside the list and puts the joiner string
; between each element.
; e.g. (join '("cat" "dog" "monkey") ",") > "cat,dog,monkey"

(define (join lst joiner)
  (if (null? lst)
      ""
      (let loop ((l (cdr lst)) (acc (car lst)))
        (if (null? l)
            acc
            (loop (cdr l) (string-append acc joiner (car l)))))))


; join-display: like join but also displays the result incrementally

(define (join-display lst joiner)
  (if (null? lst)
      ""
      (begin
        (display (car lst))
        (let loop ((l (cdr lst)) (acc (car lst)))
          (if (null? l)
              (begin
                (newline)
                acc)
              (begin
                (display joiner)
                (display (car l))
                (loop (cdr l) (string-append acc joiner (car l)))))))))