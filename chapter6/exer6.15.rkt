#lang eopl

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val
      (proc proc?)))

;;; extractors:

(define expval->num (lambda (v)
 (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
  (define-datatype proc proc?
    (procedure
      (vars (list-of symbol?))
      (body tfexp?)
      (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;;; represent environment as a list of bindings.
;;; binding ::= ('let    (list-of id) (list-of expval))
;;;           | ('letrec (list-of id) (list-of bvar) (list-of expression))

;;; The first binding for extend-env*, the second is for
;;; extend-env-rec**.

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

  (define empty-env
    (lambda ()
      '()))

  (define empty-env?
    (lambda (x) (null? x)))

  (define extend-env*
    (lambda (syms vals old-env)
      (cons (list 'let syms vals) old-env)))

  (define extend-env-rec**
    (lambda (p-names b-varss p-bodies saved-env)
      (cons
        (list 'letrec p-names b-varss p-bodies)
        saved-env)))

  (define apply-env
    (lambda (env search-sym)
      (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (saved-env (cdr env)))
          (let ((pos (list-index search-sym (cadr binding))))
            (if pos
              (case (car binding)
                ((let)
                 (list-ref (caddr binding) pos))
                ((letrec)
                 (let ((bvars (caddr binding))
                       (bodies (cadddr binding)))
                     (proc-val
                       (procedure
                         (list-ref bvars pos)
                         (list-ref bodies pos)
                         env)))))
              (apply-env saved-env search-sym)))))))

  ;; returns position of sym in los, else #f
  (define list-index
    (lambda (sym los)
      (let loop ((pos 0) (los los))
        ;; los is at position pos of the original los
        (cond
          ((null? los) #f)
          ((eqv? sym (car los)) pos)
          (else (loop (+ pos 1) (cdr los)))))))

;; not precise, but will do.
  (define environment?
    (list-of
      (lambda (p)
        (and
          (pair? p)
          (or (eqv? (car p) 'let) (eqv? (car p) 'letrec))))))

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

 
  ;; exercise:  Improve this code by getting rid of extend-env1.

  (define cps-out-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

 (define cps-out-grammar

    '((cps-out-program (tfexp) cps-a-program)
      (simple-expression (number) cps-const-exp)
      (simple-expression (identifier) cps-var-exp)
      (simple-expression
       ("-" "(" simple-expression "," simple-expression ")") cps-diff-exp)
      (simple-expression ("zero?" "(" simple-expression ")") cps-zero?-exp)
      (simple-expression
       ("+" "(" (separated-list simple-expression ",") ")") cps-sum-exp)
      (simple-expression
       ("proc" "(" (arbno identifier) ")" tfexp) cps-proc-exp)
      (tfexp (simple-expression) simple-exp->exp)
      (tfexp ("let" identifier "=" simple-expression "in" tfexp) cps-let-exp)
      (tfexp
       ("letrec" (arbno identifier "(" (arbno identifier) ")"
                 "=" tfexp) "in" tfexp) cps-letrec-exp)
      (tfexp ("if" simple-expression "then" tfexp "else" tfexp) cps-if-exp)
      (tfexp ("(" simple-expression (arbno simple-expression) ")") cps-call-exp)
      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

  (sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

  (define cps-show-the-datatypes
    (lambda ()
      (sllgen:list-define-datatypes cps-out-lexical-spec cps-out-grammar)))

  (define cps-out-scan&parse
    (sllgen:make-string-parser cps-out-lexical-spec cps-out-grammar))

  (define cps-out-just-scan
    (sllgen:make-string-scanner cps-out-lexical-spec cps-out-grammar))

  ;;;;;;;;;;;;;;;; a primitive pretty-printer ;;;;;;;;;;;;;;;;

  ;; exercise: Write a pretty-printer for programs in CPS-OUT.

;;   (define cps-program->string
;;     (lambda (pgm)
;;       (cases cps-out-program pgm
;;         (cps-a-program (exp1) (tfexp->string exp1 0)))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal

  (define value-of-program
    (lambda (pgm)
      (cases cps-out-program pgm
        (cps-a-program (exp1)
          (value-of/k exp1 (empty-env) )))))

  (define value-of-simple-exp
    (lambda (exp env)
      (cases simple-expression exp
        (cps-const-exp (num) (num-val num))
        (cps-var-exp (var) (apply-env env var))
        (cps-diff-exp (exp1 exp2)
          (let ((val1 (expval->num (value-of-simple-exp exp1 env)))
                (val2 (expval->num (value-of-simple-exp exp2 env))))
            (num-val (- val1 val2))))
        (cps-zero?-exp (exp1)
          (bool-val (zero? (expval->num (value-of-simple-exp exp1 env)))))
        (cps-sum-exp (exps)
          (let ((nums (map (lambda (exp)
                    (expval->num (value-of-simple-exp exp env))) exps)))
            (num-val (let sum-loop ((nums nums))
               (if (null? nums) 0 (+ (car nums) (sum-loop (cdr nums))))))))
        (cps-proc-exp (vars body)
                      (proc-val (procedure vars body env))))))

  ;; value-of/k : TfExp * Env * Cont -> FinalAnswer
  ;; Page: 209
  (define value-of/k
    (lambda (exp env)
      (cases tfexp exp
        (simple-exp->exp (simple) (value-of-simple-exp simple env))
        (cps-let-exp (var rhs body)
           (let ((val (value-of-simple-exp rhs env)))
            (value-of/k body (extend-env* (list var) (list val) env))))
        (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
          (value-of/k letrec-body
            (extend-env-rec** p-names b-varss p-bodies env)))
        (cps-if-exp (simple1 body1 body2)
          (if (expval->bool (value-of-simple-exp simple1 env))
            (value-of/k body1 env)
            (value-of/k body2 env)))
        (cps-call-exp (rator rands)
          (let ((rator-proc
                  (expval->proc
                    (value-of-simple-exp rator env)))
                (rand-vals
                  (map
                    (lambda (simple)
                      (value-of-simple-exp simple env))
                    rands)))
            (apply-procedure/k rator-proc rand-vals))))))

  ;; apply-cont : Cont * ExpVal -> Final-ExpVal
  ;; there's only one continuation, and it only gets invoked once, at
  ;; the end of the computation.


  ;; apply-procedure/k : Proc * ExpVal * Cont -> ExpVal
  ;; Page: 209
  (define apply-procedure/k
    (lambda (proc1 args )
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of/k body
            (extend-env* vars args saved-env))))))

  
  ;; trace has to be in the module where the procedure is defined.
  ;; (trace value-of/k apply-cont)

(define run
  (lambda (string)
    (value-of-program (cps-out-scan&parse string))))
