(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the LEXADDR language.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-translation value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-translation : Nameless-program -> ExpVal

  (define value-of-translation
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-nameless-env))))))

  ;; value-of-translation : Nameless-program -> ExpVal
  ;; Page: 100
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-nameless-env))))))

  ;; value-of : Nameless-exp * Nameless-env -> ExpVal
  (define value-of
    (lambda (exp nameless-env)
      (cases expression exp
        (const-exp (num) (num-val num))
        (emptylist-exp () (list-val '()))
        (cons-exp (car-exp cdr-exp)
          (list-val (cons (value-of car-exp nameless-env)
                          (expval->list (value-of cdr-exp nameless-env)))))
                       ;   (let ((tail (value-of cdr-exp nameless-env)))
                       ;     (if (emptylist? tail) '() tail)))))
        (car-exp (exp1) (car (expval->list exp1)))
        (cdr-exp (exp1) (list-val (cdr (expval->list exp1))))
        (null?-exp (exp1) (null? (expval->list exp1)))
        (list-exp (params)
          (list-val
           (map
            (lambda (num) (value-of num nameless-env))
            params)))

        (diff-exp (exp1 exp2)
          (let ((val1
                 (expval->num
		    (value-of exp1 nameless-env)))
                (val2
		  (expval->num
		    (value-of exp2 nameless-env))))
            (num-val
	      (- val1 val2))))

        (zero?-exp (exp1)
	  (let ((val1 (expval->num (value-of exp1 nameless-env))))
	    (if (zero? val1)
	      (bool-val #t)
	      (bool-val #f))))

        (if-exp (exp0 exp1 exp2)
          (if (expval->bool (value-of exp0 nameless-env))
            (value-of exp1 nameless-env)
            (value-of exp2 nameless-env)))
        (cond-exp (conditions actions)
          (eval-cond conditions actions nameless-env))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator nameless-env)))
                (arg (value-of rand nameless-env)))
            (display proc)
            (display "\n arg :")
            (display arg)
            (display "\n")
	    (apply-procedure proc arg)))

        (nameless-var-exp (n)
          (apply-nameless-env nameless-env n))

        (nameless-let-exp (exp1 body)
          (let ((val (value-of exp1 nameless-env)))
            (value-of body
              (extend-nameless-env val nameless-env))))

        (nameless-letrec-var-exp (expl body)
          ;(let ((env (extend-nameless-env-rec expl nameless-env)))
          ;  (let ((val (value-of expl env)))
          ;    (value-of body (extend-nameless-env val env)))))

          (value-of body (extend-nameless-env-rec expl nameless-env)))


        (nameless-proc-exp (body)
          (proc-val
           (procedure body nameless-env)))

        (nameless-unpack-exp (expr body)
          (let ((val (reverse (expval->list (value-of expr nameless-env)))))
            (value-of body (unpack-extend-nameless-env val nameless-env))))

        (else
         (eopl:error 'value-of
	    "Illegal expression in translated code: ~s" exp))

        )))

  (define extend-nameless-env-rec
    (lambda (p-body nameless-env )
      (let ((vec-p-name (make-vector 1))
            (vec-b-var (make-vector 1)))
        (let ((new-env
               (extend-nameless-env vec-b-var
               (extend-nameless-env vec-p-name nameless-env))))
          (vector-set! vec-p-name 0
                       (proc-val (procedure p-body  new-env)))
          (vector-set! vec-b-var 0
                       (proc-val (procedure p-body new-env)))
          new-env))))

  (define unpack-extend-nameless-env
    (lambda (lst env)
      (if (null? lst)
          env
          (unpack-extend-nameless-env
           (cdr lst)
           (extend-nameless-env (car lst) env)))))

  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (body saved-env)
          (value-of body (extend-nameless-env arg saved-env))))))

  (define (eval-cond conditions actions env)
    (cond ((null? conditions)
           (bool-val #f))
          ((value-of (car conditions) env)
            (value-of (car actions) env))
          (else
           (eval-cond (cdr conditions) (cdr actions) env))))

  )
