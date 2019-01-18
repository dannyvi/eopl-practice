(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))


  (define value-of-boolexp
    (lambda (boolean env)
      (cases boolexp boolean
        (bool-exp (exp1)
                  (if (not
                       (eqv?
                        (expval->val (value-of exp1 env)))  0)
                      #t #f)))))
  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))
        [minus-exp (exp1)
                   (num-val (- (expval->num (value-of exp1 env) )))]
        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (op-exp (operator exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                ((eval operator) num1 num2)))))
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (num-val 1)
                (num-val 0)))))
        [equal?-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (if (eqv? num1 num2) (num-val 1) (num-val 0))))]
        [greater?-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (if (> num1 num2) (num-val 1) (num-val 0))))]
        [less?-exp (exp1 exp2)
                   (let ((val1 (value-of exp1 env))
                         (val2 (value-of exp2 env)))
                     (let ((num1 (expval->num val1))
                           (num2 (expval->num val2)))
                       (if (< num1 num2) (num-val 1) (num-val 0))))]
        ;\commentbox{\ma{\theifspec}}
        (if-exp (boolexp1 exp2 exp3)
          (let ((boolean (value-of-boolexp boolexp1 env)))
            (if boolean
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        [let-exp (vars exps body)
                 ;(let ((val1 (value-of exp1 env)))
                 ;  (value-of body
                 ;    (extend-env var val1 env))))
          (eval-let vars exps body env)]
        [let*-exp (vars exps body)
          (eval-let* vars exps body env)]
        [unpack-exp (vars expr body)
          (eval-unpack vars expr body env)]
        [emptylist-exp () (list-val '())]
        [cons-exp (carexp cdrexp)
          (let ((carobj (expval->val (value-of carexp env)))
                (cdrobj (expval->val (value-of cdrexp env))))
            (list-val (cons carobj cdrobj)))]
        [car-exp (exp1) (list-val (car (expval->list exp1)))]
        [cdr-exp (exp1) (list-val (cdr (expval->list exp1)))]
        [null?-exp (exp1) (null? (expval->list exp1))]
        [list-exp (params) (list-val params)]
        [cond-exp (conditions actions)
                  (eval-cond conditions actions env)]
        )))

  (define (extends-env vars vals env)
    (if (null? vars) env
        (extends-env (cdr vars)
                     (cdr vals)
                     (extend-env (car vars) (car vals) env))))

  (define (eval-let vars exps body env)
    (let ((vals (map (lambda (a) (value-of a env)) exps)))
      (value-of body (extends-env vars vals env))))

  (define (eval-let* vars exps body env)
    (if (null? vars) (value-of body env)
        (let [(new-env (extend-env
                        (car vars)
                        (value-of (car exps) env)
                        env))]
          (eval-let* (cdr vars) (cdr exps) body new-env))))

  (define (eval-unpack vars expr body env)
    (let ((vals (map (lambda (x) (const-exp x))
                     (expval->val (value-of expr env)))))
      (eval-let vars vals body env)))

  (define (eval-cond conditions actions env)
    (cond ((null? conditions)
           (num-val 0))
          ( (value-of-boolexp (car conditions) env)
           (value-of (car actions) env))
          (else
           (eval-cond (cdr conditions) (cdr actions) env))))

  )

