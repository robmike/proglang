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

;;; (vector-assoc "c" #(("a" 1) ("x") ("b"  2)))
(define (vector-assoc v vec)
  (define (helper v vec i)
    (cond [(< i (vector-length vec))
           (define el (vector-ref vec i))

           (if (and (pair? el)
                    (equal? (car el)
                            v))
               el
               (helper v vec (+ 1 i)))]
          [else #f]))
  (helper v vec 0))

(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define idx 0)
  (lambda (val)
    (define c (vector-assoc val cache))
    (cond [c c]
          [else (define res (assoc val xs))
                (vector-set! cache idx res)
                (set! idx (modulo (+ 1 idx) n))
              res])))

(define-syntax-rule (while-less e1 do e2)
  (begin (define x e1)
         (define (helper)
           (cond [(< e2 x) (helper)]
                 [else #t]))
         (helper)))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (begin (define lim e1)
            (define (helper)
              (cond [(< e2 lim) (helper)]
                    [else #t]))
            (helper))]))
