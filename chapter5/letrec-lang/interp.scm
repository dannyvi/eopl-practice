(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure
  ;; representation of continuations (Figure 5.3).

  ;; exercise: rewrite this using the procedural representation of
  ;; continuations (Figure 5.2).

  ;; exercise: rewrite this using a trampoline (page 159).

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))  

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
        (letm-exp (vars exprs body)
          (let ((exps (reverse exprs)))
            (value-of/k (car exps) env
              (letm-cont vars '() (cdr exps) body env cont))))
        (let2-exp (var1 exp1 var2 exp2 body)
          (value-of/k exp1 env
            (let2-1-cont var1 var2 exp2 body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))        
        (call-exp (rator rand) 
          (value-of/k rator env
                      (rator-cont rand env cont)))

        (emptylist-exp () (apply-cont cont (list-val (empty-leest))))
        (cons-exp (exp1 exp2) (value-of/k exp1 env (cons-cont exp2 env cont)))
        (car-exp (exp1) (value-of/k exp1 env (car-cont cont)))
        (cdr-exp (exp1) (value-of/k exp1 env (cdr-cont cont)))
        (null?-exp (exp1) (value-of/k exp1 env  (null?-cont cont)))
        (list-exp (exprs)
          (let ((exps (reverse exprs)))
            (if (null? exps)
              (apply-cont cont (list-val (empty-leest)))
              (value-of/k (car exps) env
                (list-cont (empty-leest) (cdr exps) env cont)))))
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          (begin
            (eopl:printf
              "End of computation.~%")
            val))
        ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
        (zero1-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k body
                      (extend-env var val saved-env) saved-cont))

        (letm-cont (vars vals exps body saved-env saved-cont)
          (if (null? exps)
              (value-of/k body
                          (extend-env* vars (cons val vals) saved-env)
                          saved-cont)
              (value-of/k (car exps) saved-env
                (letm-cont vars (cons val vals)
                           (cdr exps) body saved-env saved-cont))
              ))

        (let2-1-cont (var1 var2 exp2 body s-env saved-cont)
          (value-of/k exp2 s-env
           (let2-2-cont var1 val var2 body s-env saved-cont)))

        (let2-2-cont (var1 val1 var2 body s-env saved-cont)
          (value-of/k body
            (extend-env var1 val1
              (extend-env var2 val s-env)) saved-cont))

        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (value-of/k exp2 saved-env saved-cont)
             (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont)))
        (cons-cont (exp2 saved-env saved-cont)
          (value-of/k exp2 saved-env (cons2-cont val saved-cont)))
        (cons2-cont (val1 saved-cont)
          (apply-cont saved-cont
            (list-val (cons-leest val1 (expval->leest val)))))
        (car-cont (saved-cont)
            (apply-cont saved-cont (car-leest (expval->leest val))))
        (cdr-cont (saved-cont)
            (apply-cont saved-cont (cdr-leest (expval->leest val))))
        (null?-cont (saved-cont)
          (apply-cont saved-cont
                      (bool-val (null?-leest (expval->leest val)))))
        (list-cont (vals exps saved-env saved-cont)
          (if (null? exps)
              (apply-cont saved-cont (list-val (cons-leest val vals)))
              (value-of/k (car exps) saved-env
                (list-cont (cons-leest val vals)
                           (cdr exps) saved-env saved-cont))))

        )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var arg saved-env)
            cont)))))

  (define extend-env*
    (lambda (vars vals env)
      (if (null? vars) env
          (extend-env* (cdr vars) (cdr vals)
            (extend-env (car vars) (car vals) env)))))

  )




