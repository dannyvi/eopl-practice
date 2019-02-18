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



(define proc? procedure?)

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]]
  [emptylist-val]
  [pair-val [car expval?]
            [cdr expval?]])

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
  [extend-env [bvar symbol?]
              [bval expval?]
              [saved-env environment?]]
  [extend-env-rec [p-name symbol?]
                  [b-vars (list-of symbol?)]
                  [p-body expression?]
                  [saved-env environment?]])

(define identifier? symbol?)


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
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (apply-cont cont
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))
        (call-exp (rator rand)
          (value-of/k rator env
            (rator-cont rand env cont)))
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cont val)))

(define (end-cont) (lambda (val) (eopl:printf "End of computation.~%") val))
(define (zero1-cont s-cont) (lambda (val)
  (apply-cont s-cont (bool-val (zero? (expval->num val))))))
(define (let-exp-cont var body saved-env saved-cont) (lambda (val)
  (value-of/k body (extend-env var val saved-env) saved-cont)))
(define (if-test-cont exp2 exp3 saved-env saved-cont) (lambda (val)
  (if (expval->bool val)
      (value-of/k exp2 saved-env saved-cont)
      (value-of/k exp3 saved-env saved-cont))))
(define (diff1-cont exp2 saved-env saved-cont) (lambda (val)
  (value-of/k exp2 saved-env (diff2-cont val saved-cont))))
(define (diff2-cont val1 saved-cont) (lambda (val)
  (let ((num1 (expval->num val1))
        (num2 (expval->num val)))
    (apply-cont saved-cont (num-val (- num1 num2))))))
(define (rator-cont rand saved-env saved-cont) (lambda (val)
  (value-of/k rand saved-env (rand-cont val saved-cont))))
(define (rand-cont val1 saved-cont) (lambda (val)
  (let ((proc (expval->proc val1)))
    (apply-procedure/k proc val saved-cont))))


;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 arg cont)
    (proc1 arg cont)))

(define procedure
  (lambda (var body env)
    (lambda (arg cont)
      (value-of/k body (extend-env var arg env) cont))))

(define (init-env)
  (empty-env))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of/k exp1 (init-env) (end-cont))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
