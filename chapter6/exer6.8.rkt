#lang eopl
(require (only-in racket index-of))
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression
                          "in" expression)
                letrec-exp)
    ;; Lists.  We will have lists of literal numbers only.
    (expression ("list" "(" (separated-list number ",") ")") const-list-exp)
    (expression (unary-op "(" expression ")") unop-exp)
    (expression ("try" expression "catch" "(" identifier ")" expression)
                try-exp)
    (expression ("raise" expression) raise-exp)
    (unary-op ("null?") null?-unop)
    (unary-op ("car")   car-unop)
    (unary-op ("cdr" )  cdr-unop)
    (unary-op ("zero?") zero?-unop)
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

(define environment?
  (list-of (lambda (p) (and  (pair? p) (symbol? (car p))))))

(define empty-env (lambda () '()))

(define empty-env? (lambda (x) (null? x)))

(define extend-env (lambda (sym val old-env) (cons (list sym val) old-env)))

(define extend-env-rec
  (lambda (p-name b-var p-body saved-env)
    (cons 
     (list p-name b-var p-body)
     saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env) 
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (id (list-ref binding 0))
               (expval-or-bvar (list-ref binding 1)))
          (cond
            ((not (eqv? search-sym id))
             (apply-env (cdr env) search-sym))
            ((not (symbol? expval-or-bvar))
             ;; this was built by extend-env
             expval-or-bvar)
            (else
             ;; this was built by extend-env-rec
             (let ((bvar (cadr binding))
                   (body (caddr binding)))
               (proc-val (procedure bvar body env)))))))))

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (list-val
   (lst (list-of expval?))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

(define expval->num
  (lambda (v)
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

(define expval->list
  (lambda (v)
    (cases expval v
           (list-val (lst) lst)
           (else (expval-extractor-error 'list v)))))

(define apply-cont (lambda (cont val) (cont 'cont val)))
(define apply-handler (lambda (cont val) (cont 'exn val)))

(define-syntax dispatch
  (syntax-rules ()
    ((_ (sym body) ...) (lambda (disp val)
                          (let* ((num (index-of (list sym ...) disp))
                                 (exp (list-ref (list body ...) num)))
                            (exp val))))))

(define end-cont
  (lambda ()
  (dispatch
   ('cont (lambda (val) (eopl:printf "End of computation.~%") val))
   ('exn (lambda (val) (eopl:error 'apply-handler "Uncaught exception!"))))))

(define diff1-cont
  (lambda (exp2 env cont)
    (dispatch
     ('cont (lambda (val) (value-of/k exp2 env (diff2-cont val cont))))
     ('exn (lambda (val) (apply-handler cont val))))))

(define diff2-cont
  (lambda (val1 cont)
    (dispatch
     ('cont (lambda (val) (apply-cont cont (num-val (- (expval->num val1)
                                                       (expval->num val))))))
     ('exn (lambda (val) (apply-handler cont val))))))

(define unop-arg-cont
  (lambda (unop cont)
    (dispatch
     ('cont (lambda (val) (apply-cont cont (apply-unop unop val))))
     ('exn (lambda (val) (apply-handler cont val))))))

(define if-test-cont
  (lambda (exp2 exp3 env cont)
    (dispatch
     ('cont (lambda (val) (if (expval->bool val) (value-of/k exp2 env cont)
                              (value-of/k exp3 env cont))))
     ('exn (lambda (val) (apply-handler cont val))))))

(define rator-cont
  (lambda (rand env cont)
    (dispatch
     ('cont (lambda (val)  (value-of/k rand env (rand-cont val cont))))
     ('exn (lambda (val) (apply-handler cont val))))))

(define rand-cont
  (lambda (val1 cont)
    (dispatch
     ('cont (lambda (val) (apply-procedure (expval->proc val1) val cont)))
     ('exn (lambda (val) (apply-handler cont val))))))

(define raise1-cont
  (lambda (cont)
    (dispatch
     ('cont (lambda (val) (apply-handler cont val)))
     ('exn (lambda (val) (apply-handler cont val))))))

(define try-cont
  (lambda (var handler-exp env cont)
    (dispatch
     ('cont (lambda (val) (apply-cont cont val)))
     ('exn (lambda (val) (value-of/k handler-exp (extend-env var val env) cont))))))


(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (const-list-exp (nums)
        (apply-cont cont (list-val (map num-val nums))))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env (diff1-cont exp2 env cont)))
      (unop-exp (unop exp1)
        (value-of/k exp1 env (unop-arg-cont unop cont)))
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
      (proc-exp (var body)
        (apply-cont cont (proc-val (procedure var body env))))
      (call-exp (rator rand)
        (value-of/k rator env (rator-cont rand env cont)))
      ;; make let a macro, because I'm too lazy to add the extra
      ;; continuation
      (let-exp (var exp1 body)
        (value-of/k (call-exp (proc-exp var body) exp1) env cont))
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k letrec-body (extend-env-rec p-name b-var p-body env) cont))
      (try-exp (exp1 var handler-exp)
        (value-of/k exp1 env (try-cont var handler-exp env cont)))
      (raise-exp (exp1)
                 (value-of/k exp1 env (raise1-cont cont))))))

(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
           (null?-unop () (bool-val (null? (expval->list val))))
           (car-unop () (car (expval->list val)))
           (cdr-unop () (list-val (cdr (expval->list val))))
           (zero?-unop () (bool-val (zero? (expval->num val)))))))

(define apply-procedure
  (lambda (proc1 arg cont)
    (cases proc proc1
           (procedure (var body saved-env)
                      (value-of/k body (extend-env var arg saved-env) cont)))))

(define value-of-program 
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (value-of/k body (empty-env) (end-cont))))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
