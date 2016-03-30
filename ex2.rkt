#lang racket

(require "ex1.rkt")
(provide (all-defined-out))

; vector-replace takes a mutable vector v and two values a and b. it replaces each occurence of a with b.
; warning: you can't use this function with immutable vectors as literal vectors
; e.g. (define v1 (vector 'a 'b 'c))
;      (vector-replace! v1 'a 1) -> after this call v1 will be equals to #(1 b c)

(define (vector-replace! v a b)
  (for-each (lambda(pos) 
              (when (equal? (vector-ref v pos) a)
                (vector-set! v pos b))) (sequence 0 (- (vector-length v) 1))))

; (define v1 (vector 1 2 3 1))
; (define v2 (make-vector 4 0))
; (vector-replace! v1 1 'a)  -> v1 = #(a 2 3 a) 
; (vector-replace! #(1 2 3 1) 1 'a)  > error

; useless! sets the parameter e to false. this function has no side side effects outside its scope ('call by object sharing' evaluation strategy)

(define (useless! e) 
  (set! e #f))

; void 

(define (void* x) (void))

; struct

(struct player
  (name
   strength))

; (player "John" 60)
; (define john (player "John" 60))
; (player-name john)
; (player-strength john)
; (player? john)
; (set-player-name! john "New John") > error

(struct stricker player
  ((goals #:mutable)))

; (define john (stricker "John" 60 0))
; (set-stricker-goals! john 2)
; (stricker-goals john)

(struct goalkeeper player ())

(define (player-show p)
  (display (player-name p))
  (display "[")
  (display (player-strength p))
  (display "]"))

(define (stricker-show s)
  (player-show s)
  (display "(S)")
  (display " goals: ")
  (display (stricker-goals s)))

(define (goalkeeper-show g)
  (player-show g)
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
      (player-print s2))))

; (define competition (shootout luca andrea luigi))
; (shootout-play! competition)
; (shootout-play! competition)


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

(define (reverse-map f lst)
  (let loop ((l lst) (acc '()))
    (if (null? l)
      acc
      (loop (cdr l) (cons (f (car l)) acc)))))

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
; (filter (lambda(n) (zero? (modulo n 3))))