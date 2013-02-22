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

(define ones (lambda ()
               (cons 1 ones)))
