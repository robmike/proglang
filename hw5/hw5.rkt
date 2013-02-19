#lang racket

(define sequence 
  (lambda (low high stride)
    (if (<= low high)
        (cons low (sequence (+ stride low) high stride))
        '())))

(define string-append-map
  (lambda (xs suffix)
    (map (lambda (x) (string-append x suffix)) xs)))