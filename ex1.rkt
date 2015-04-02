#lang racket

(provide (all-defined-out))

; basics

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

(define (simple-binding x y)
  (let ((x y) (y x))
    (+ x y))) 

(define (simple-binding* x y)
  (let* ((x y) (y x))
    (+ x y)))

;; list and accessors

(define mylist '(1 2 3))
; (cons 1 2)
; (cons 1 (cons 2 (cons 3 '())))
; (car mylist)
; (car (cdr (cdr mylist)))
; (cadr mylist) alias (car (cdr l))
; (caddr mylist) alias (car (cdr (cdr l)))
; (equal? mylist (list 1 2 3)) 

;; quoting: quote (') prevents code evaluation, quasiquote evaluates only expressions prefixed with a comma

(define (cube-list x)
  (quasiquote (,x ^ 3 = ,(cube x))))

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

; join takes a list of strings and a joiner string. it appends all the strings inside the list and puts
; between each element the joiner string.
; e.g. (join '("apple" "google" "microsoft" "banana") ",") > "apple,google,microsoft,banana"

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


; vector-replace takes a mutable vector v and two values a and b. it replaces each occurence of a with b.
; warning: you can't use this function with immutable vectors as literal vectors
; e.g. (define v1 (vector 'a 'b 'c))
;      (vector-replace! v1 'a 1) -> after this call v1 will be equals to #(1 b c)

(define (vector-replace! v a b)
  (for-each (lambda(pos) 
              (when (equal? (vector-ref v pos) a)
                (vector-set! v pos b))) (sequence 0 (- (vector-length v) 1))))

; useless! sets the parameter e to false. this function has no side side effects outside its scope ('call by object sharing' evaluation strategy)

(define (useless! e) 
  (set! e #f))

; struct

(struct player
  (name
   strength))

(struct stricker player
  ((goals #:mutable)))

(struct goalkeeper player ())

(define (player-show p)
  (display (player-name p))
  (display "[")
  (display (player-strength p))
  (display "]"))

(define (stricker-show f)
  (player-show f)
  (display "(S)")
  (display " goals: ")
  (display (stricker-goals f)))

(define (goalkeeper-show f)
  (player-show f)
  (display "(GK)"))

(define (player-print p)
  (cond ((stricker? p) (stricker-show p))
        ((goalkeeper? p) (goalkeeper-show p))
        ((player? p) (player-show p))
        (else (error "Not a player")))
  (newline))

(define (penalty! s gk)
  (cond 
    ((not (stricker? s)) (error "not a stricker"))
    ((not (goalkeeper? gk))  (error "not a goalkeeper"))
    (else (let ((p (if (>= (player-strength s) (player-strength gk)) 2 4)))
            (if (= (random p) 0)
                (begin 
                  (display (player-name s))
                  (display " goal")
                  (newline)
                  (set-stricker-goals! s (+ 1 (stricker-goals s))))
                (begin
                  (display (player-name s))
                  (display " missed")
                  (newline)))))))

(define luigi (stricker "Luigi" 74 0))
(define andrea (stricker "Andrea" 81 0))
(define luca (goalkeeper "Luca" 78))

(struct shootout 
  (gk
   stricker1
   stricker2))

(define (shootout-play! c)
  (let ((s1 (shootout-stricker1 c)) (s2 (shootout-stricker2 c)) (gk (shootout-gk c)))
    (begin
      (set-stricker-goals! s1 0)
      (set-stricker-goals! s2 0)
      (newline)
      (let loop ((i 0))
        (when (or (< i 5) (= (stricker-goals s1) (stricker-goals s2)))
          (display "Penalty ")
          (display (+ i 1))
          (newline)
          (penalty! s1 gk)
          (penalty! s2 gk)
          (loop (+ i 1))))
      (display "The winner is ")
      (if (> (stricker-goals s1) (stricker-goals s2))
          (display (player-name s1))
          (display (player-name s2)))
      (newline)
      (player-print s1)
      (player-print s2)
      (newline))))

(define competition (shootout luca andrea luigi))
