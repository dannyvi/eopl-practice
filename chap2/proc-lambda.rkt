
;; Lc-exp ::= Identifier
;;        ::= (lambda (Identifier) Lc-exp)
;;        ::= (Lc-exp Lc-exp)


;; constructor
;; var-exp : Var -> Lc-exp
(define var-exp
  (lambda (var)
    (lambda (command)
      (cond
       [(eqv? command 'var-exp?) #t]
       [(eqv? command 'lambda-exp?) #f]
       [(eqv? command 'app-exp?) #f]
       [(eqv? command 'var-exp->var) var]))))

(define lambda-exp
  (lambda (identifier lc-exp)
    (lambda (command)
      (cond
       [(eqv? command 'var-exp?) #f]
       [(eqv? command 'lambda-exp?) #t]
       [(eqv? command 'app-exp?) #f]
       [(eqv? command 'lambda-exp->bound-var) identifier]
       [(eqv? command 'lambda-exp->body) lc-exp]))))

(define lc-exp
  (lambda (exp1 exp2)
    (lambda (command)
      (cond
       [(eqv? command 'var-exp?) #f]
       [(eqv? command 'lambda-exp?) #f]
       [(eqv? command 'app-exp?) #t]
       [(eqv? command 'app-exp->rator) exp1]
       [(eqv? command 'app-exp->rand) exp2]))))

(define var-exp? (lambda (exp) (exp 'var-exp?)))
(define lambda-exp? (lambda (exp) (exp 'lambda-exp?)))
(define app-exp? (lambda (exp) (exp 'app-exp?)))
(define var-exp->var (lambda (exp) (exp 'var-exp->var)))
(define lambda-exp->bound-var (lambda (exp) (exp 'lambda-exp->bound-var)))
(define lambda-exp->body (lambda (exp) (exp 'lambda-exp->body)))
(define app-exp->rator (lambda (exp) (exp 'app-exp->rator)))
(define app-exp->rand (lambda (exp) (exp 'app-exp->rand)))
