#lang eopl

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
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("letrec" identifier "("(arbno identifier) ")" "=" expression
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

(define extend-env*
  (lambda (vars vals env)
    (if (null? vars) env
        (extend-env* (cdr vars) (cdr vals)
                     (extend-env (car vars) (car vals) env)))))

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
            ((not ((list-of symbol?) expval-or-bvar))
             ;; this was built by extend-env
             expval-or-bvar)
            (else
             ;; this was built by extend-env-rec
             (let ((bvar (cadr binding))
                   (body (caddr binding)))
               (proc-val (procedure bvar body env)))))))))

(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
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

(define-datatype exception exception?
  (end-except)
  (base-except (var symbol?)
               (exp1 expression?)
               (env environment?)
               (cont continuation?)
               (excpt exception?))
  (arg-except (vars (list-of expval?))))

(define-datatype continuation continuation?
  (end-cont)
  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val1 expval?)
   (cont continuation?))
  (unop-arg-cont
   (unop unary-op?)
   (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (rator-cont
   (rands (list-of expression?))
   (env environment?)
   (cont continuation?))
  (rand-cont
   (rator-val expval?)
   (rands (list-of expression?))
   (rands-val (list-of expval?))
   (env environment?)
   (cont continuation?))
  ;(try-cont
  ; (var symbol?)
  ; (handler-exp expression?)
  ; (env environment?)
  ; (cont continuation?))
  (raise1-cont
   (excpt exception?))
  )

(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
           (null?-unop () (bool-val (null? (expval->list val))))
           (car-unop () (car (expval->list val)))
           (cdr-unop () (list-val (cdr (expval->list val))))
           (zero?-unop () (bool-val (zero? (expval->num val)))))))

(define apply-procedure
  (lambda (proc1 args cont excpt)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (if (not (eqv? (length args) (length vars)))
                          (apply-handler vars
                                         (arg-except args))
                      (value-of/k body (extend-env* vars args saved-env) cont excpt))))))

(define apply-handler
  (lambda (val excpt)
    (cases exception excpt
           (end-except () (eopl:error 'apply-handler "uncaught exception"))
           (base-except (var exp1 saved-env saved-cont saved-excpt)
                        (value-of/k exp1
                                    (extend-env var val saved-env)
                                    saved-cont
                                    saved-excpt))
           (arg-except (args)
                       (eopl:error
                        'arg-except "parameters ~s does not match arguments: ~s"
                        val args)))))

(define apply-cont
  (lambda (cont excpt val)
    (cases continuation cont
      (end-cont () val)
      (diff1-cont (exp2 saved-env saved-cont)
        (value-of/k exp2 saved-env (diff2-cont val saved-cont) excpt))
      (diff2-cont (val1 saved-cont)
        (let ((n1 (expval->num val1))
              (n2 (expval->num val)))
          (apply-cont saved-cont excpt
            (num-val (- n1 n2)))))
      (unop-arg-cont (unop cont)
        (apply-cont cont excpt
          (apply-unop unop val)))
      (if-test-cont (exp2 exp3 env cont)
        (if (expval->bool val)
          (value-of/k exp2 env cont excpt)
          (value-of/k exp3 env cont excpt)))
      (rator-cont (rands saved-env saved-cont)
        (value-of/k (car rands) saved-env
          (rand-cont val (cdr rands) '() saved-env saved-cont) excpt))
      (rand-cont (rator-val rands rand-vals saved-env saved-cont)
        (if (null? rands)
            (apply-procedure (expval->proc rator-val)
                             (cons val rand-vals) saved-cont excpt)
            (value-of/k (car rands)
                        saved-env
                        (rand-cont rator-val
                                   (cdr rands) (cons val rand-vals) saved-env saved-cont)
                        excpt)))

       ;; (let ((proc (expval->proc rator-val)))
       ;;   (apply-procedure proc val saved-cont excpt)))
      ;; the body of the try finished normally-- don't evaluate the handler
      ;;(try-cont (var handler-exp saved-env saved-cont)
      ;;  (apply-cont saved-cont val))
      ;; val is the value of the argument to raise
      (raise1-cont (saved-excpt)
        ;; we put the short argument first to make the trace more readable.
        (apply-handler val saved-excpt))
      )))

(define value-of/k
  (lambda (exp env cont excpt)
    (cases expression exp
      (const-exp (num) (apply-cont cont excpt (num-val num)))
      (const-list-exp (nums)
        (apply-cont cont excpt (list-val (map num-val nums))))
      (var-exp (var) (apply-cont cont excpt (apply-env env var)))
      (diff-exp (exp1 exp2)
        (value-of/k exp1 env (diff1-cont exp2 env cont) excpt))
      (unop-exp (unop exp1)
        (value-of/k exp1 env (unop-arg-cont unop cont) excpt))
      (if-exp (exp1 exp2 exp3)
        (value-of/k exp1 env (if-test-cont exp2 exp3 env cont) excpt))
      (proc-exp (var body)
        (apply-cont cont excpt (proc-val (procedure var body env))))
      (call-exp (rator rands)
        (value-of/k rator env (rator-cont (reverse rands) env cont) excpt))
      ;; make let a macro, because I'm too lazy to add the extra
      ;; continuation
      (let-exp (vars exps body)
        (value-of/k (call-exp (proc-exp vars body) exps) env cont excpt))
      (letrec-exp (p-name b-var p-body letrec-body)
        (value-of/k letrec-body (extend-env-rec p-name b-var p-body env) cont excpt))
      (try-exp (exp1 var handler-exp)
        (value-of/k exp1 env cont (base-except var handler-exp env cont excpt)))
      (raise-exp (exp1)
                 (value-of/k exp1 env (raise1-cont excpt) excpt)))))


(define value-of-program 
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (value-of/k body (empty-env) (end-cont) (end-except))))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))




