#lang eopl

(define lambda-exp
  (lambda (var lc-exp) `(lambda (,var) ,lc-exp)))

;; 2.22

(define ident? (lambda (v) (and (symbol? v) (not (eqv? 'lambda v)))))

(define-datatype lc-exp lc-exp?
  (var-exp (var ident?))
  (lambda-exp
   (bound-var ident?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
        (and
         (not (eqv? search-var bound-var))
         (occurs-free? search-var body)))
      (app-exp (rator rand)
        (or
         (occurs-free? search-var rator)
         (occurs-free? search-var rand))))))

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp (sym symbol?))
  (s-list-s-exp (slst s-list?)))


(define parse-expression
  (λ (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (report-invalid-concrete-syntax datum)))))

(define unparse-lc-exp
  (λ (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
        (list 'lambda (list bound-var)
              (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list (unparse-lc-exp rator) (unparse-lc-exp rand))))))




