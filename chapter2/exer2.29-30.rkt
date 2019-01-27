
;;
;; Lc-exp ::= Identifier
;;        ::= (lambda ({Identifier}*) Lc-exp)
;;        ::= (Lc-exp {Lc-exp}*)
;;
(require eopl)

(define ident?
  (λ (var)
    (cond [(eqv? 'lambda v)
           (eopl:error 'ident? "lambda not allowed to be an ident")]
          [(symbol? v) #t]
          [else (eopl:error 'ident? "~s is not an ident" var)])))

(define err
  (λ (var)
    (eopl:error 'lc-exp "does not have form of ~s" var)))

(define-datatype lc-exp lc-exp?
  (var-exp (var ident?))
  (lambda-exp (bound-vars (list-of ident?)) (body lc-exp?))
  (app-exp (rator lc-exp?) (rand (list-of lc-exp?))))

(define list-of
  (λ (pred)
    (λ (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define parse-expression
  (λ (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (if (eqv? (length datum) 3)
               (lambda-exp (cadr datum)
                           (parse-expression (caddr datum)))
               (eopl:error 'lambda-exp
                           "expected (lambda (bound) body) , got: ~s"
                           datum))
           (app-exp (parse-expression (car datum))
                    (map parse-expression (cdr datum)))))
      (else (eopl:error 'parse-expression
                        "illegal form of input: ~s "
                        datum)))))



