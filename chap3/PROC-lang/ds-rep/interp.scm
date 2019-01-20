(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        (prim-exp (prim) (prim-val (eval prim)))

        ;\commentbox{\diffspec}
        ;        (diff-exp (exp1 exp2)
        ;          (let ((val1 (value-of exp1 env))
        ;                (val2 (value-of exp2 env)))
        ;            (let ((num1 (expval->num val1))
        ;                  (num2 (expval->num val2)))
        ;              (num-val
        ;                (- num1 num2)))))

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
          (let ((local-env (init-local-env
                            (free-vars-in (list var) body) env)))
            (proc-val (procedure var body local-env))))

        (traceproc-exp (var body)
          (let ((local-env (init-local-env
                            (free-vars-in (list var) body) env)))
            (proc-val (trace-procedure var
                                 body local-env))))

        (letproc-exp (id1 id2 body)
          (extend-env id1 (proc-val (procedure id2 body env)) env))

        (proc*-exp (vars body)
            (apply-proc* vars body env))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (call*-exp (ratorexp rands)
                  ;; (if (prim-val? ratorexp)
          (let ((rator (value-of ratorexp env)))
          (cases expval rator
            (prim-val (proc1) (num-val (proc1
                               (expval->num (value-of (car rands) env))
                               (expval->num (value-of (cadr rands) env))) ))
            (proc-val (proc1)
                      (let ((args
                             (map
                              (lambda (rand)
                                (value-of rand env)) rands)))
                        (eval-call* proc1 (reverse args))))
            (else (expval-extractor-error 'proc rator))
            )))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        (procedure (var body saved-env)
                   (value-of body (extend-env var val saved-env)))
        (trace-procedure (var body saved-env)
          (begin
            (display "start proc !" )
            (let ((result (value-of body (extend-env var val saved-env))))
              (begin (display "ending") result))
            )))))

  (define apply-proc*
    (lambda (vars body env)
      (if (null? vars)
          (value-of body env)
          (apply-proc* (cdr vars)
                      (proc-exp (car vars) body) env)
          )))

  (define eval-call*
    (lambda (proc1 args)
      (if (null? (cdr args))
          (apply-procedure proc1 (car args))
          (eval-call* (expval->proc (apply-procedure proc1 (car args)))
                      (cdr args)))))

  ;; exercise 3.26
  ;; build local environment
  (define free-vars-in
    (lambda (bounds body)
      (cases expression body
        (var-exp (var) (if (member var bounds) '() (list var)))
        (zero?-exp (exp1) (free-vars-in bounds exp1))
        (if-exp (exp1 exp2 exp3)
                (append (free-vars-in bounds exp1)
                        (free-vars-in bounds exp2)
                        (free-vars-in bounds exp3)))
        (let-exp (var exp1 body)
                 (append (free-vars-in bounds exp1)
                         (free-vars-in (cons var bounds) body)))
        (proc-exp (var body) (free-vars-in (cons var bounds) body))
        (letproc-exp (id1 id2 body) (free-vars-in (cons id2 bounds) body))
        (proc*-exp (vars body) (free-vars-in (append vars bounds) body))
        (call-exp (rator rand)
                  (append (free-vars-in bounds rator)
                          (free-vars-in bounds rand)))
        (call*-exp (rator-exp rands)
          (apply append (cons (free-vars-in bounds rator-exp)
                              (map (lambda (rand)
                                     (free-vars-in bounds rand)) rands))))
        (else '()))))

  (define init-local-env
    (lambda (free-vars env)
      (if (null? free-vars)
          (empty-env)
          (bind-value (car free-vars) (init-local-env (cdr free-vars) env) env))))

  (define bind-value
    (lambda (search-sym local-env env)
      (if (empty-env? env)
          local-env
          (let ((sym (extended-env-record->sym env))
                (val (extended-env-record->val env))
                (old-env (extended-env-record->old-env env)))
            (if (eqv? search-sym sym)
                (extend-env sym val local-env)
                (bind-value search-sym local-env old-env))))))

)
