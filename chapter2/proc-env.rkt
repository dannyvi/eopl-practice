#lang eopl

;; procedural Representation
;; empty-env : () -> Env
;; exercise 2.13 2.14 ...

(define report-no-binding-found
      (lambda (search-var)
        (eopl:error 'apply-env "No binding for ~s" search-var)))

(define empty-env
  (lambda ()
    (lambda (command)
      (cond
       [(eqv? command 'apply-env)
        (lambda (search-var)
          (report-no-binding-found search-var))]
       [(eqv? command 'empty-env?)  #t]
       [(eqv? command 'has-binding?) (lambda (search-var) #f)]))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (command)
      (cond
      [(eqv? command 'apply-env)
       (lambda (search-var)
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var)))]
      [(eqv? command 'empty-env?)  #f]
      [(eqv? command 'has-binding?)
       (lambda (search-var)
         (if (eqv? search-var saved-var)
             #t
             (has-binding? saved-env search-var)))]))))

(define has-binding?
  (lambda (env search-var)
    ((env 'has-binding?) search-var)))

(define apply-env
  (lambda (env search-var)
    ((env 'apply-env) search-var)))

(define empty-env?
  (lambda (env)
    (env 'empty-env?)))





