#lang racket

(require "ex1.rkt")
(require "ex2.rkt")
(require "ex3.rkt")
(provide (all-defined-out))


; closures 

#| (let ((x 5))
     (lambda(y) 
       (+ x y))) |#

(define (make-greets greet)
  (lambda(who)
    (string-append greet " " who)))

(define (find hm)
  (lambda(k)
    (hash-ref hm k)))

(define capital-of (find 
                    (hash "Italy" "Rome" "France" 
                          "Paris" "Germany" "Berlin" "Japan" "Tokyo")))

(define (shootout-play-clousure! c)
  (let ((s1 (shootout-stricker1 c)) (s2 (shootout-stricker2 c)) (gk (shootout-gk c))
                                    (i 0))
    (begin
      (set-stricker-goals! s1 0)
      (set-stricker-goals! s2 0)
      (lambda()
        (if (or (< i 5) (= (stricker-goals s1) (stricker-goals s2)))
            (begin
              (display "Penalty ")
              (display (+ i 1))
              (newline)
              (penalty! s1 gk)
              (penalty! s2 gk)
              (set! i (+ i 1)))
            (begin
              (display "The winner is ")
              (if (> (stricker-goals s1) (stricker-goals s2))
                  (display (player-name s1))
                  (display (player-name s2)))
              (newline)
              (player-print s1)
              (player-print s2)
              'end))))))

; (define competition (shootout luca andrea luigi)) 
; (set! competition (shootout-play-clousure! competition))
; (competition) (competition) ...

; currying 

(define ((make-greets-curried greet) who)
  (string-append greet " " who))

; eval and homoiconicity + closures

(define ((eval-formula formula) x y)
  (eval (quasiquote (let ((x ,x) (y ,y)) ,formula))))

; (define f (eval-formula '(square (+ (- x y) 1))))
; (f 1 1) > 1
; (f 2 1) > 4

; continuations

(define (break-test p lst)
  (call/cc (lambda (break)
             (for-each 
              (lambda(i)
                (if (p i)
                    (break)
                    (begin 
                      (display i)
                      (newline)))) lst)))
  (display "end"))

; (break-test negative? '(1 5 3 -1 4 -2 5))
; (break-test positive? '(1 5 3 -1 4 -2 5))

(define (continue-test p lst)
  (for-each
   (lambda(i)
     (call/cc (lambda(continue)
                (if (p i)
                    (continue)
                    (begin 
                      (display i)
                      (newline)))))) lst)
  (display "end"))

; (continue-test negative? '(1 5 3 -1 4 -2 5))
; (continue-test positive? '(1 5 3 -1 4 -2 5))

(define (return-test p lst)
  (call/cc 
   (lambda(return)
     (for-each
      (lambda(i)
        (if (p i)
            (return)
            (begin 
              (display i)
              (newline)))) lst)
     (display "end"))))

; (return-test negative? '(1 5 3 -1 4 -2 5))
; (return-test negative? '(1 5 3 4 5))

(define-syntax for-each-b
  (syntax-rules (break:)
    ((_ lam lst break: sym)
     (call/cc (lambda(sym)
                (for-each lam lst))))))

#| (for-each-b (lambda(x)
              (if (positive? x)
                  (displayln x)
                  (bye))) '(1 2 3 -4 5 6 7) break: bye) |#

; coroutines with continuations

(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))

(define (fork proc)
  (call/cc (lambda(k)
             (enqueue k)
             (proc))))

(define (yield)
  (call/cc (lambda(k)
             (enqueue k)
             ((dequeue)))))

(define (c-exit)
  (if (empty-queue?)
      (exit)
      ((dequeue))))

(define ((do-stuff-n-print str max))
  (let loop ((n 0))
    (display str)
    (display " ")
    (display n)
    (newline)
    (yield)
    (if (< n max)
        (loop (+ 1 n))
        (c-exit))))

(define (main)
  (begin
    (fork (do-stuff-n-print "This is A" 3))
    (fork (do-stuff-n-print "This is B" 4))
    (displayln "End")
    (c-exit)))
