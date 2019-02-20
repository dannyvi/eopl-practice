#lang eopl

(require (only-in racket foldr))

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" (separated-list expression ",") ")") sum-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression
      ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
        "in" expression) letrec-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; tail-form? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define simple-exp?
  (lambda (expresn)
    (cases expression expresn
      (const-exp (num) #t)
      (var-exp (var) #t)
      (diff-exp (exp1 exp2) (and (simple-exp? exp1) (simple-exp? exp2)))
      (sum-exp (exps)
        (foldr (lambda (val initial) (and initial (simple-exp? val))) #t exps))
      (zero?-exp (exp1) (simple-exp? exp1))
      (proc-exp (var body) (tail-form-exp? body))
      (else #f))))

(define tail-form-exp?
  (lambda (expresn)
    (cases expression expresn
      (const-exp (num) #t)
      (var-exp (var) #t)
      (diff-exp (exp1 exp2) (and (simple-exp? exp1) (simple-exp? exp2)))
      (sum-exp (exps)
        (foldr (lambda (val initial) (and initial (simple-exp? val))) #t exps))
      (zero?-exp (exp1) (simple-exp? exp1))
      (if-exp (exp1 exp2 exp3) (and (simple-exp? exp1)
                                    (tail-form-exp? exp2)
                                    (tail-form-exp? exp3)))
      (letrec-exp (p-name b-vars p-body letrec-body) (and (tail-form-exp? p-body)
                                                          (tail-form-exp? letrec-body)))
      (let-exp (var exp1 body) (and (tail-form-exp? exp1)
                                    (tail-form-exp? body)))
      (proc-exp (vars body) (tail-form-exp? body))
      (call-exp (expr exps)
        (and (simple-exp? expr)
             (foldr (lambda (val initial) (and initial (simple-exp? val))) #t exps)))
      )))

(define tail-form?
  (lambda (string)
    (let ((prog (scan&parse string)))
      (cases program prog
        (a-program (exp) (tail-form-exp? exp))))))
