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

(define (stream-add-zero s)
  (define x (s))
  (lambda () (cons (cons 0 (car x)) (stream-add-zero (cdr x)))))

(define (cycle-lists s t)
  (define (helper s t ss tt)
    (cons (cons (car ss) (car tt)) (lambda ()
                                     (helper s t (if (empty? (cdr ss)) s (cdr ss))
                                             (if (empty? (cdr tt)) t (cdr tt))))))
  (lambda () (helper s t s t)))

(define (vector-assoc v vec)
  (define (helper v vec i)
    (cond [(< i (vector-length vec))
           (define el (vector-ref vec i))

           (if (and (= 2 (length el))
                    (equal? (car el)
                            v))
               (cdr el)
               (helper v vec (+ 1 i)))]
          [else #f]))
  (helper v vec 0))

(define ones (lambda ()
               (cons 1 ones)))

(define nats (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))]) (lambda () (f 1))))