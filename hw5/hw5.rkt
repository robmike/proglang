;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      '()
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (null? xs)
      '()
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (add-to-env name value env)
  (cons (cons name value) env))

(define (eval-under-env e env)
  ;; (displayln "evaluating")
  ;; (displayln e)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let* ([ex (fst-e e)]
                         [val (eval-under-env ex env)])
                    (if (apair? val)
                        (apair-e1 val)
                        (error "MUPL fst applied to non-pair")))]
        [(snd? e) (let* ([ex (snd-e e)]
                         [val (eval-under-env ex env)])
                    (if (apair? val)
                        (apair-e2 val)
                        (error "MUPL snd applied to non-pair")))]
        [(aunit? e) e]
        [(isaunit? e) (if (aunit?
                           (eval-under-env (isaunit-e e) env))
                          (int 1) (int 0))]
        [(mlet? e) (let ([name (mlet-var e)]
                         [value (eval-under-env (mlet-e e) env)]
                         [body (mlet-body e)])
                     (if (string? name)
                         (eval-under-env body (add-to-env name value env))
                         (error "MUPL let expression applied to non-string")))]
        [(ifgreater? e) (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
                              [v2 (eval-under-env (ifgreater-e2 e) env)])
                          (if (and (int? v1)
                                   (int? v2))
                              (if (> (int-num v1) (int-num v2))
                                  (eval-under-env (ifgreater-e3 e) env)
                                  (eval-under-env (ifgreater-e4 e) env))
                              (error "MUPL ifgreater arguments applied to non-integers")))]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(call? e) (let ([funexpv (eval-under-env (call-funexp e) env)]
                         [actualv (eval-under-env (call-actual e) env)])
                     (if (not (closure? funexpv))
                         (error "MUPL call argument appied to non-closure")
                         (let* ([f (closure-fun funexpv)]
                                [fname (fun-nameopt f)]
                                [fformal (fun-formal f)]
                                [fbody (fun-body f)]
                                [newenv (add-to-env fformal actualv (closure-env funexpv))])
                           ;; (print newenv)
                           ;; (displayln ":")
                           (eval-under-env fbody (if fname
                                                     (add-to-env fname funexpv newenv)
                                                     newenv)))))]
        [#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2 
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_y" e2 (mlet "_x" e1
                      (ifgreater
                       (var "_x") (var "_y") e3
                       (ifgreater
                        (var "_y") (var "_x") e3 e4)))))

;; Problem 4

(define mupl-map (fun "map" "f"
                      (fun "_map" "xs"
                           (ifaunit (var "xs")
                                    (aunit) (apair (call (var "f") (fst (var "xs")))
                                                   (call (var "_map") (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))


(eval-under-env (call (call mupl-map (fun "add2" "x" (add (int 2) (var "x")))) (apair (int 3) (aunit))) null)