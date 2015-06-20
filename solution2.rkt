#lang racket

; ex 1

(define (unzip lst)    
  (if (null? lst) 
      '(() ())
      (let ((res (unzip (cdr lst))))
        (list (cons (caar lst) (car res))  (cons (cadar lst) (cadr res))))))


(define (unzip-values lst)    
  (if (null? lst) 
      (values '() '())
      (let-values (((l r) (unzip-values (cdr lst))))
        (values (cons (caar lst) l)  (cons (cadar lst) r)))))

; ex 2

(define (zip* . ls)
  (if (and (not (null? ls)) (empty? (filter null? ls)))
      (cons (apply list (map car ls))
            (apply zip* (map cdr ls)))
      '()))

(define (exists pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) #t)
    (else (exists pred (cdr lst))))) 

(define (zip2* . ls)
  (if (or (null? ls) (exists null? ls))
      '()
      (cons (apply list (map car ls))
            (apply zip2* (map cdr ls)))))

; ex 3

(define (make-iterator lst)
  (let ((_lst lst))
    (define (has-next?) (not (null? _lst)))
    (define (next) (let ((head (car _lst))) (set! _lst (cdr _lst)) head))
    (lambda (message . args) 
      (apply (case message
               ((has-next?) has-next?)
               ((next) next)
               (else (error "Unknown Method!")))
             args))))

; ex 4

(define-syntax iter0
  (syntax-rules (->)
    ((_ it -> var code ...)
     (let loop ()
       (when (it 'has-next?)
            (let ((var (it 'next)))
              code ...
              (loop)))))))


; ex 5

(define (list-iter-cc lst)
  (call/cc 
   (lambda (return) 
     (for-each               
      (lambda (x)
        (call/cc (lambda (next-step)
                   (return (cons x next-step))))) 
      lst)
     'end)))


(define-syntax iter
  (syntax-rules (-> in)
    ((_ it -> var code ...)
     (let loop ()
       (when (it 'has-next?)
            (let ((var (it 'next)))
              code ...
              (loop)))))
    ((_ var in lst code ...)
     (let loop ((head (list-iter-cc lst)))
       (unless (eq? head 'end)
         (let ((var (car head)))
           code ... 
           (loop ((cdr head)))))))))
