(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the LETREC language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  
  (require (only-in racket last ))


  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 83
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))

        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))

        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (proc*-exp (vars body)
          (def-proc* vars body env))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (call*-exp (rator-exp rands)
          (let ((rator (value-of rator-exp env)))
            (cases expval rator
              (proc-val (proc1)
                (let ((args (map (lambda (rand) (value-of rand env)) rands)))
                  (apply-proc* proc1 (reverse args))))
              (else (expval-extractor-error 'proc rator)))))

        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of letrec-body
                    (extend-env-rec p-name b-var p-body env)))

        (letrec-mutual-exp (p-names b-vars p-bodys letrec-body)
                    (value-of letrec-body
                              (extend-env-rec-mutual p-names b-vars p-bodys env)))

        (letrec*-exp (p-name b-vars p-body letrec-body)
          (value-of letrec-body
                    (extend-env-rec* p-name b-vars p-body env)))

        (letrec*-mutual-exp (p-names b-vars p-bodys letrec-body)
                           (value-of letrec-body
                                     (extend-env-rec*-mutual p-names b-vars p-bodys env)))
        )))


;  (define extend-env-rec-mutual
;    (lambda (p-names b-vars p-bodys env)
;      (if (null? p-names) env
;          (extend-env-rec-mutual (cdr p-names) (cdr b-vars) (cdr p-bodys)
;            (extend-env-rec (car p-names) (car b-vars) (car p-bodys) env)))))

  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
                   (value-of body (extend-env var arg saved-env)))
        (trace-procedure (var body saved-env)
                         (let ((result (value-of body (extend-env var arg saved-env))))
                           (begin
                             (eopl:printf
                              "~s \nvar: ~s ~s \nbody: ~s ~s \nenv: ~s ~n~s ~n"
                              'tracing=proc======================================
                              var
                              (expval->val arg)
                              body
                              (expval->val result)
                              (extend-env var arg saved-env)
                              'ending============================================)
                             result)))
        )))

  (define def-proc*
    (lambda (vars body env)
      (if (null? vars)
          (value-of body env)
          (def-proc* (cdr vars)
            (proc-exp (car vars) body) env))))

  (define apply-proc*
    (lambda (proc1 args)
      (if (null? (cdr args))
          (apply-procedure proc1 (car args))
          (apply-proc* (expval->proc (apply-procedure proc1 (car args)))
                       (cdr args)))))

  (define extend-env-rec*
    (lambda (p-name b-vars p-body env)
      (if (null? (cdr b-vars))
          (extend-env-rec p-name (car b-vars) p-body env)
          (extend-env-rec* p-name (cdr b-vars)
                           (proc-exp (car b-vars) p-body) env))))
  (define currying
    (lambda (vars body)
      (if (null? (cdr vars)) body
          (currying (cdr vars)
                    (proc-exp (car vars) body)))))

  (define extend-env-rec*-mutual
    (lambda (p-names b-var-lists p-bodys env)
      (extend-env-rec-mutual
       p-names
       (map last b-var-lists)
       (map currying b-var-lists p-bodys)
       env)
      )
    )


  )




