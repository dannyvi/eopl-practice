(module interp (lib "eopl.ss" "eopl")
  
  ;; cps interpreter for the LETREC language, using the data structure
  ;; representation of continuations (Figure 5.3).

  ;; exercise: rewrite this using the procedural representation of
  ;; continuations (Figure 5.2).

  ;; exercise: rewrite this using a trampoline (page 159).

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "store.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require (only-in racket void))

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define instrument-all (make-parameter #f))
  (define instrument-stat (make-parameter #f))

  (define current-cont 1)
  (define max-cont 1)
  (define (stat func)
    (set! current-cont (func current-cont 1))
    (if (> current-cont max-cont) (set! max-cont current-cont) (void))
    )
;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (set! current-cont 1)
      (set! max-cont 1)
      (cases program pgm
        (a-program (exp1)
          ;(trampoline
           (value-of/k exp1 (init-env) (end-cont))))))   ;)

;  (define-datatype tramp tramp?
;    (tramp-value (bounce expval?) )
;    (tramp-proc (bounce tramp?)))
;
;  (define trampoline
;    (lambda (b)
;      (cases tramp b
;             (tramp-value (bounce) bounce)
;             (tramp-proc  (bounce) (trampoline bounce)))))
;      ;(if (expval? bounce) bounce (trampoline (bounce)))))
  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (when (instrument-all)
            (eopl:printf "~n~n(value-of/k ~n ~a ~n ~a ~n ~a)" exp env cont))
      
      (cases expression exp
        (const-exp (num)
          (stat -)
          (apply-cont cont (num-val num) env))
        (var-exp (var)
          (stat -)
          (apply-cont cont (deref (apply-env env var)) env))
        (proc-exp (vars body)
          (stat -)
          (apply-cont cont 
            (proc-val (procedure vars body env)) env))
        (letrec-exp (p-name b-vars p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-vars p-body env)
            cont))
        (zero?-exp (exp1)
          (stat +)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (stat +)
          (value-of/k exp1 env
                      (let-exp-cont var body cont)))
        (letm-exp (vars exprs body)
          (stat +)
          (let ((exps (reverse exprs)))
            (value-of/k (car exps) env
              (letm-cont vars '() (cdr exps) body cont))))
        (let2-exp (var1 exp1 var2 exp2 body)
          (stat +)
          (value-of/k exp1 env
            (let2-1-cont var1 var2 exp2 body cont)))
        (if-exp (exp1 exp2 exp3)
          (stat +)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 cont)))
        (diff-exp (exp1 exp2)
          (stat +)
          (value-of/k exp1 env
                      (diff1-cont exp2 cont)))
        (multiply-exp (exp1 exp2)
          (stat +)
          (value-of/k exp1 env (multiply1-cont exp2 cont)))
        (call-exp (rator rands)
          (stat +)
          (value-of/k rator env
                      (rator-cont (reverse rands) cont)))

        (emptylist-exp ()
          (stat -)
          (apply-cont cont (list-val (empty-leest)) env))

        (cons-exp (exp1 exp2)
          (stat +)
          (value-of/k exp1 env (cons-cont exp2 cont)))
        (car-exp (exp1)
          (stat +)
          (value-of/k exp1 env (car-cont cont)))
        (cdr-exp (exp1)
          (stat +)
          (value-of/k exp1 env (cdr-cont cont)))
        (null?-exp (exp1)
          (stat +)
          (value-of/k exp1 env  (null?-cont cont)))
        (list-exp (exprs)
          (let ((exps (reverse exprs)))
            (if (null? exps)
             (begin (stat -) (apply-cont cont (list-val (empty-leest)) env))
             (begin (stat +)
               (value-of/k (car exps) env
                 (list-cont (empty-leest) (cdr exps) cont))))))
        (begin-exp (exp1 exps)
          (stat +)
          (value-of/k exp1 env (begin-cont exps cont)))
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val saved-env)
      
      (when (instrument-all)
            (eopl:printf "~n~n(apply-cont ~n ~a ~n ~a)" cont val))
      (cases continuation cont 
        (end-cont () 
          (begin
            (stat -)
            (eopl:printf
             "End of computation.~%")
            (when (instrument-stat)
                  (eopl:printf "max count ~a ~n" max-cont)
                  )
            val))
        ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
        (zero1-cont (saved-cont)
          (stat -)
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val))) saved-env))
        (let-exp-cont (var body  saved-cont)
          (value-of/k body
                      (extend-env var
                                  (newref val) saved-env) saved-cont))

        (letm-cont (vars vals exps body  saved-cont)
          (if (null? exps)
              (value-of/k body
                          (extend-env* vars (cons (newref val) vals) saved-env)
                          saved-cont)
              (begin (stat +)
              (value-of/k (car exps) saved-env
                (letm-cont vars (cons (newref val) vals)
                           (cdr exps) body  saved-cont)))
              ))

        (let2-1-cont (var1 var2 exp2 body saved-cont)
          (stat +)
          (value-of/k exp2 saved-env
           (let2-2-cont var1 (newref val) var2 body saved-cont)))

        (let2-2-cont (var1 val1 var2 body saved-cont)
          (value-of/k body
            (extend-env var1 val1
              (extend-env var2 (newref val) saved-env)) saved-cont))

        (if-test-cont (exp2 exp3  saved-cont)
          (if (expval->bool val)
             (value-of/k exp2 saved-env saved-cont)
             (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-cont)
          (stat +)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (stat -)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
                        (num-val (- num1 num2)) saved-env)))
        (multiply1-cont (exp2 saved-cont)
           (stat +)
           (value-of/k exp2
             saved-env (multiply2-cont val saved-cont)))
        (multiply2-cont (val1 saved-cont)
           (stat -)
           (let ((num1 (expval->num val1))
                          (num2 (expval->num val)))
                      (apply-cont saved-cont
                                  (num-val (* num1 num2)) saved-env)))
        (rator-cont (rands saved-cont)
          (stat +)
          (value-of/k (car rands) saved-env
            (rand-cont val (cdr rands) '() saved-cont)))
        (rand-cont (rator-val rands rand-vals saved-cont)
          (if (null? rands)
              ;(tramp-proc
              (apply-procedure/k (expval->proc rator-val)
                                 (cons val rand-vals)
                                 saved-cont);)
              (begin (stat +)
                (value-of/k (car rands) saved-env
                (rand-cont rator-val
                           (cdr rands) (cons val rand-vals)
                            saved-cont))))
                   )
          ;(let ((proc (expval->proc val1)))
          ;  (apply-procedure/k proc val saved-cont)))
        (cons-cont (exp2  saved-cont)
          (stat +)
          (value-of/k exp2 saved-env (cons2-cont val saved-cont)))
        (cons2-cont (val1 saved-cont)
          (stat -)
          (apply-cont saved-cont
            (list-val (cons-leest val1 (expval->leest val)))
            saved-env))
        (car-cont (saved-cont)
          (stat -)
            (apply-cont saved-cont (car-leest (expval->leest val))
                        saved-env))
        (cdr-cont (saved-cont)
          (stat -)
            (apply-cont saved-cont (cdr-leest (expval->leest val))
                              saved-env))
        (null?-cont (saved-cont)
          (stat -)
          (apply-cont saved-cont
                      (bool-val (null?-leest (expval->leest val)))
                      saved-env))
        (list-cont (vals exps saved-cont)
          (if (null? exps)
              (begin (stat -)
               (apply-cont saved-cont
                          (list-val (cons-leest val vals)) saved-env))
              (begin (stat +)
              (value-of/k (car exps) saved-env
                (list-cont (cons-leest val vals)
                           (cdr exps) saved-cont)))))
        (begin-cont (exps cont)
          (stat +)
          (if (null? exps) val
              (value-of/k (car exps) saved-env
                          (begin-cont (cdr exps) cont))))

        )))


;  (define apply-cont
;    (lambda (cont val saved-env)
;      (if (null? cont) val
;        (let ((op (caar cont))
;              (args (cdar cont))
;              (saved-cont (cdr cont)))
;          (cond
;           [(eqv? op 'zero1-cont)
;            (apply-cont saved-cont
;                        (bool-val
;                         (zero? (expval->num val))) saved-env)]
;           [(eqv? op 'let-exp-cont)
;            (value-of/k (cadr args) (extend-env (car args) (newref val) saved-env) saved-cont)]
;           [(eqv? op 'letm-cont)
;            (if (null? (third args))
;                (value-of/k
;                 (fourth args)
;                 (extend-env*
;                  (first args)
;                  (cons (newref val)
;                        (second args)) saved-env) saved-cont)
;                (letm-cont
;                 (first args)
;                 (cons (newref val) (second args))
;                 (cdr (third args)) (fourth args) saved-cont))]
;           [(eqv? op 'let2-1-cont )]
;           )
;          ))
;      )
;    )
  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 args cont)
      ;(tramp-value 
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of/k body
            (extend-env* vars (map newref args) saved-env)
            cont)))));)

  (define extend-env*
    (lambda (vars vals env)
      (if (null? vars) env
          (extend-env* (cdr vars) (cdr vals)
            (extend-env (car vars) (car vals) env)))))

  )




