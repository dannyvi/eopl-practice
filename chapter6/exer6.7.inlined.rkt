#lang eopl

;; Grammar.

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc procedure?]])

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))

(define expval->num
  (lambda (v)
    (cases expval v
      [num-val (num) num]
      [else (expval-extractor-error 'num v)])))

(define expval->bool
  (lambda (v)
    (cases expval v
      [bool-val (bool) bool]
      [else (expval-extractor-error 'bool v)])))

(define expval->proc
  (lambda (v)
    (cases expval v
      [proc-val (proc) proc]
      [else (expval-extractor-error 'proc v)])))

(define-datatype environment environment?
  [empty-env]
  [extend-env
    [bvar symbol?]
    [bval expval?]
    [saved-env environment?]]
  [extend-env-rec
    [p-name symbol?]
    [b-vars (list-of symbol?)]
    [p-body expression?]
    [saved-env environment?]])

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env ()
        (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (var val saved-env)
        (if (eqv? search-sym var)
            val
            (apply-env saved-env search-sym))]
      [extend-env-rec (p-name b-vars p-body saved-env)
        (if (eqv? search-sym p-name)
            (proc-val (procedure b-vars p-body env))
            (apply-env saved-env search-sym))])))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (cont (num-val num)))
        (var-exp (var) (cont (apply-env env var)))
        (proc-exp (var body) (cont (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body (extend-env-rec p-name b-var p-body env) cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env (lambda (val) (cont (bool-val (zero? (expval->num val)))))))
        (let-exp (var exp1 body)
          (value-of/k exp1 env (lambda (val)
            (value-of/k body (extend-env var val env) cont))))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env (lambda (val)
            (if (expval->bool val)
                (value-of/k exp2 env cont)
                (value-of/k exp3 env cont)))))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env (lambda (val1)
            (value-of/k exp2 env (lambda (val2)
              (cont (num-val (- (expval->num val1) (expval->num val2)))))))))
        (call-exp (rator rand)
          (value-of/k rator env (lambda (proc)
            (value-of/k rand env (lambda (arg)
               (apply-procedure/k (expval->proc proc) arg cont)))))))))

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 arg cont)
    (proc1 arg cont)))

(define procedure
  (lambda (var body env)
    (lambda (arg cont)
      (value-of/k body (extend-env var arg env) cont))))

(define (init-env) (empty-env))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of/k exp1 (init-env) (lambda (x) x))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
