#lang eopl
;; Grammar.
(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [id (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define the-grammar
  '((program (expr) a-program)
    (expr (number) const-exp)
    (expr ("-" "(" expr "," expr ")") diff-exp)
    (expr ("zero?" "(" expr ")") zero?-exp)
    (expr ("if" expr "then" expr "else" expr) if-exp)
    (expr (id) var-exp)
    (expr ("let" id "=" expr "in" expr) let-exp)
    (expr ("proc" "(" id ")" expr) proc-exp)
    (expr ("(" expr expr ")") call-exp)
    (expr ("letrec" id "(" id ")" "=" expr "in" expr) letrec-exp)))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)
(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expr exp
      (const-exp (num) (cont num))
      (var-exp (var) (cont (env var)))
      (proc-exp (var body) (cont (procedure var body env)))
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k letrec-body (lambda (sym)
          (if (eqv? sym p-name) (procedure b-var p-body env) (env sym))) cont))
      (zero?-exp (exp1)
        (value-of/k exp1 env (lambda (val) (cont (zero? val)))))
      (let-exp (var exp1 body)
        (value-of/k exp1 env (lambda (val)
          (value-of/k body (lambda (sym) (if (eqv? sym var) val (env sym))) cont))))
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env (lambda (val)
          (if val (value-of/k exp2 env cont) (value-of/k exp3 env cont)))))
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env (lambda (val1)
          (value-of/k exp2 env (lambda (val2) (cont (-  val1 val2)))))))
      (call-exp (rator rand)
        (value-of/k rator env (lambda (proc)
          (value-of/k rand env (lambda (arg) (proc arg cont)))))))))

(define procedure (lambda (var body env) (lambda (arg cont)
  (value-of/k body (lambda (sym) (if (eqv? sym var) arg (env sym))) cont))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of/k exp1
        (lambda (search-sym) (eopl:error 'apply-env "No binding for ~s" search-sym))
        (lambda (x) x))])))

(define run (lambda (string) (value-of-program (scan&parse string))))
