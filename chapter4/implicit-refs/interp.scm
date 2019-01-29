(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require (only-in racket last))

  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #t))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (stmt) (result-of stmt (init-env))))))
          ;(value-of stmt (init-env))))))

  (define result-of
    (lambda (sttmt env)
      (cases statement sttmt
        (assign-stmt (var expr)
          (let ((ref (apply-env env var)))
            (setref! ref (value-of expr env))
            ))
        (if-stmt (expr true-stmt false-stmt)
          (let ((val (value-of expr env)))
            (if (expval->bool val)
                (result-of true-stmt env)
                (result-of false-stmt env))))
        (block-stmt (stmts)
          (if (null? stmts) 'succeed
              (begin
                (result-of (car stmts) env)
                (result-of (block-stmt (cdr stmts)) env))))
        (print-stmt (expr) (eopl:printf "~s" (value-of expr env)))
        (while-stmt (expr stmt)
          (if (expval->bool (value-of expr env))
              (begin
                (result-of stmt env)
                (result-of (while-stmt expr stmt) env)
                )
              'end
              ))
        (decl-stmt (vars stmt)
          (if (null? vars)
              (result-of stmt env)
              (result-of (decl-stmt (cdr vars) stmt)
                         (extend-env (car vars)
                                     (newref (num-val 0)) env))))
        (read-stmt (var)
          (let ((val (num-val (read)))
                (ref (apply-env env var)))
            (setref! ref val)
            (pretty-print (get-store-as-list))
                    ))
        )))


  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r)
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (let ((val (apply-env env var)))
                         (if (reference? val) (deref val) val)))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
               (- num1 num2)))))

        (add-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (+ num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))

        (not-exp (exp1)
          (let ((val1 (expval->bool (value-of exp1 env))))
            (if val1 (bool-val #f) (bool-val #t))))

        ;\commentbox{\ma{\theifspec}}
;        (if-exp (exp1 exp2 exp3)
;          (let ((val1 (value-of exp1 env)))
;            (if (expval->bool val1)
;              (value-of exp2 env)
;              (value-of exp3 env))))

        (let-exp (vars exps body)
          (let ((v1 (map (lambda (exp1) (value-of exp1 env)) exps)))
            (value-of body
                      (extend-env* vars v1 env))))
        ;\commentbox{\ma{\theletspecsplit}}
        (letmutable-exp (vars exps body)
          (let ((v1 (map (lambda (exp1) (newref (value-of exp1 env))) exps)))
            (value-of body
              (extend-env* vars v1 env))))

        (setdynamic-exp (var exp1 body)
          (let* ((ref (apply-env env var))
                 (original-val (deref ref))
                 (dynamic-val (value-of exp1 env)))
            (begin
              (setref! ref dynamic-val)
              (let ((result (value-of body env)))
                (setref! ref original-val)
                result
                )
              )))

        (proc-exp (vars body)
           (proc-val (procedure vars body env))
           )

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env)) rands)))
            (apply-procedure proc args)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

        )))

  (define extend-env-rec*
    (lambda (p-names b-vars p-bodies env)
      (let ((p-vals (map (lambda (name) (newref 0)) p-names)))
        (let ((new-env (extend-env* p-names p-vals env)))
        (map
         (lambda (ref vars body)
           (setref! ref (proc-val (procedure vars body new-env))))
         p-vals b-vars p-bodies)
        new-env))
      ))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((r (map (lambda (arg) (newref arg)) args)))
            (let ((new-env (extend-env* vars r saved-env)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    vars)
                  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (get-store-as-list))
                  ;(pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))

  (define extend-env*
    (lambda (vars vals env)
      (if (eqv? (length vars) (length vals))
          (if (null?  vars) env
              (extend-env* (cdr vars) (cdr vals)
                           (extend-env (car vars) (car vals) env)))
          (eopl:error 'apply "expect ~s args, give ~s ~n ~s --> ~s"
                      (length vars) (length vals) vars vals))))
  ;; store->readable : Listof(List(Ref,Expval))
  ;;                    -> Listof(List(Ref,Something-Readable))
  
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))



  )




