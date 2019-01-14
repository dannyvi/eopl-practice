;;(define zero (lambda () '()))
;;(define is-zero? (lambda (n) (null? n)))
;;(define successor (lambda (n) (cons #t n)))
;;(define predecessor (lambda (n) (cdr n)))


;; 2.1

(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))

(define successor
  (let ((base 10))
    (lambda (n)
      (cond
       [(null? n) (cons 1 '())]
       [(< (car n) (- base 1)) (cons (+ 1 (car n)) (cdr n))]
       [else (cons 0 (successor (cdr n)))]))))

(define predecessor
  (let ((base 10))
    (lambda (n)
      (cond
       [(and (eqv? (car n) 1) (null? (cdr n))) '()]
       [(> (car n) 0) (cons (- (car n) 1) (cdr n))]
       [else (cons (- base 1) (predecessor (cdr n)))]))))

(define plus
  (lambda (m n)
    (if (is-zero? n)
        m
        (plus (successor m) (predecessor n)))))

(define mult-help
  (lambda (base m n)
    (if (is-zero? n)
        base
        (mult-help (plus base m) m (predecessor n)))))

(define multiply
  (lambda (m n)
    (mult-help (zero) m n)))

(define fact-help
  (lambda (base m)
    (if (is-zero? m)
        base
        (fact-help (multiply base m) (predecessor m)))))

(define factorial
  (lambda (m)
    (fact-help '(1) m)))


;; 2.3
(define one (lambda () '(one)))
(define is-one? (lambda (n) (and
                             (list? n)
                             (eqv? (car n) 'one)
                             (eqv? (length n) 1))))

(define zero
  (lambda ()
    '(diff (one) (one))))

(define interp
  (lambda (n)
    (if (is-one? n)
        1
        (- (interp (cadr n))
           (interp (caddr n))))))

(define is-zero?
  (lambda (n)
    (eqv? (interp n ) 0)))

(define predecessor
  (lambda (n)
    (list 'diff n '(one))))

(define successor
  (lambda (n)
    (list 'diff n (predecessor (zero) ))))

(define diff-tree-plus
  (lambda (m n)
    (list 'diff m (list 'diff '(diff (one) (one)) n))))

;; Env

;; empty-env : () -> Env
(define empty-env
  (lambda () (list 'empty-env)))

;; extend-env : Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;; apply-env : Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
     [(eqv? (car env) 'empty-env) (report-no-binding-found search-var)]
     [(eqv? (car env) 'extend-env)
      (let ((saved-var (cadr env))
            (saved-val (caddr env))
            (saved-env (cadddr env)))
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var)))]
     [else (report-invalid-env env)])))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

;; 2.5

(define empty--env (lambda () '()))
;; 2.8
(define empty--env? (lambda (env) (null? env)))


(define extend--env*
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply--env
  (lambda (env search-var)
    (cond
     [(null? env) (report-no-binding-found search-var)]
     [(index-of (caar env) search-var)
      (list-ref (cadar env) (index-of (caar env) search-var))]
     [else (apply--env (cdr env) search-var)])))

;; (define apply--env
;;  (lambda (env search-var )
;;    (cond
;;     [(null? env) (report-no-binding-found search-var)]
;;     [(eqv? (caar env) search-var) (cadar env)]
;;     [else (apply--env  (cdr env) search-var)])))

;; 2.6
;; 1. pair: () -> (a . 10)
;; 2. double-list: (() ()) -> ((a b c) (1 2 3))




