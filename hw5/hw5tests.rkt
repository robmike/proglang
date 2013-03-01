#lang racket

(require "hw5.rkt")

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

(racketlist->mupllist (list 3 4 5 (list 7 8)))

(mupllist->racketlist (racketlist->mupllist (list 3 4 5 (list 7 8))))
(eval-under-env (apair (int 5) (int 3)) null)

(eval-under-env (isaunit (aunit)) null)
(eval-under-env (isaunit (int 5)) null)

(eval-under-env (var "x") (add-to-env "x" 5 '()))
(eval-under-env (mlet "x" (add (int 5) (int 2)) (var "x")) null)