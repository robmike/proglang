#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

(define sequence 
  (lambda (low high stride)
    (if (<= low high)
        (cons low (sequence (+ stride low) high stride))
        '())))

(define string-append-map
  (lambda (xs suffix)
    (map (lambda (x) (string-append x suffix)) xs)))

(define list-n-mod
  (lambda (xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(= 0 (length xs)) (error "list-nth-mod: empty list")]
          [(car (list-tail xs (remainder n (length xs))))])))

(define (stream-for-n-steps s n)
  (define (helper s n sofar)
    (define x (s))
    (cond [(or (= n 0) (empty? x)) sofar]
          [(helper (cdr x) (- n 1)
                   (cons (car x) sofar))]))
  (helper s n '()))

(define (stream-for-n-steps s n)
  (define x (s))
  (cond [(or (= n 0) (empty? x)) '()]
        [(cons (car x) (stream-for-n-steps (cdr x) (- n 1)))]))


(define (funny-number-stream)
  (define (helper i)
    (cons (if (= 0 (remainder i 5)) (- 0 i) i)
          (lambda ()
            (helper (+ 1 i)))))
  (helper 1))

(define (dan-then-dog)
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    (cons "dan.jpg" dog)))

(define ones (lambda ()
               (cons 1 ones)))

(define nats (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))]) (lambda () (f 1))))