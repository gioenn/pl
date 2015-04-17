#lang racket

(require "ex1.rkt")
(require "ex2.rkt")

; (for x in '(...)
;    code ...)

; (for y from 0 to 10
;    code ... 
;    (+ y 2)

; two variations of the for macro

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

; closures 

#| (let ((x 5))
     (lambda(y) 
       (+ x y))) |#

(define (make-greets greet)
  (lambda(who)
    (string-append greet " " who)))

(define (find hm)
  (lambda(x)
    (hash-ref hm x)))

(define capital-of (find 
                    (hash "Italy" "Rome" "France" 
                          "Paris" "Germany" "Berlin" "Japan" "Tokio")))

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

; currying 

(define (((make-greets2 greet) who) postfix)
    (string-append greet " " who postfix))
  
(define (make-greet3 greet)
  (lambda(who)
    (lambda(postfix)
       (string-append greet " " who postfix))))

; eval 

(define ns (make-base-namespace))

(define (eval-formula formula)
  (eval (quasiquote 
         (let ((x 3) (y 2))
           ,formula)) ns))

; continuations

(define (break-test)
  (call/cc (lambda (break)
             (for-each 
              (lambda(i)
                (if (negative? i)
                    (break)
                    (begin 
                      (display i)
                      (newline)))) '(1 2 3 4 4 5 6))))
  (display "end"))

(define (continue-test)
  (for-each
   (lambda(i)
     (call/cc (lambda(continue)
                (if (negative? i)
                    (continue)
                    (begin 
                      (display i)
                      (newline)))))) '(1 2 3 -4 4 -5 6))
  (display "end"))

(define (return-aux return)
  (for-each
   (lambda(i)
     (if (negative? i)
         (return)
         (begin 
           (display i)
           (newline)))) '(1 2 3 -4 4 5 6))
  (display "end"))

(define (return-test)
  (call/cc (lambda(k)
             (return-aux k)))
  (display "---"))

(define-syntax for-each-b
  (syntax-rules (break:)
    ((_ lam lst break: sym)
     (call/cc (lambda(sym)
                (for-each lam lst))))))

#| (for-each-r (lambda(x)
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

(define (coroutines-test)
  (begin
    (fork (do-stuff-n-print "This is A" 3))
    (fork (do-stuff-n-print "This is B" 4))
    (displayln "End")
    (c-exit)))

     
           


              
                


  