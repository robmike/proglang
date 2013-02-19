#lang racket

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

