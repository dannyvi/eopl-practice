(module translator (lib "eopl.ss" "eopl")

  (require "lang.scm")

  (provide translation-of-program)
  ;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

  ;; translation-of-program : Program -> Nameless-program
  ;; Page: 96
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program
            (translation-of exp1 (init-senv)))))))

  ;; translation-of : Exp * Senv -> Nameless-exp
  ;; Page 97
  (define translation-of
    (lambda (exp senv)
      (cases expression exp
        (const-exp (num) (const-exp num))
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)))
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 senv)))
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)
            (translation-of exp3 senv)))
        (var-exp (var)
          (nameless-var-exp
            (apply-senv senv var)))
        (let-exp (var exp1 body)
          (translation-of (substitute-let-var var exp1 body) senv))
          ;(nameless-let-exp
          ;  (translation-of exp1 senv)
          ;  (translation-of body
          ;    (extend-senv var senv))))
        (proc-exp (var body)
          (let ((free-vars (free-vars-in (list var) body senv)))
            (display free-vars)
            (nameless-proc-exp
             (translation-of body (cons var (map car free-vars)))
             (map cadr free-vars))))
            ;(translation-of body
            ;  (extend-senv var senv)))))
        (call-exp (rator rand)
          (call-exp
            (translation-of rator senv)
            (translation-of rand senv)))
        (else (report-invalid-source-expression exp))
        )))

  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of
        "Illegal expression in source code: ~s" exp)))

   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

  ;;; Senv = Listof(Sym)
  ;;; Lexaddr = N

  ;; empty-senv : () -> Senv
  ;; Page: 95
  (define empty-senv
    (lambda ()
      '()))

  ;; extend-senv : Var * Senv -> Senv
  ;; Page: 95
  (define extend-senv
    (lambda (var senv)
      (cons var senv)))

  ;; apply-senv : Senv * Var -> Lexaddr
  ;; Page: 95
  (define apply-senv
    (lambda (senv var)
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
          (+ 1 (apply-senv (cdr senv) var))))))

  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable in code: ~s" var)))

  ;; init-senv : () -> Senv
  ;; Page: 96
  (define init-senv
    (lambda ()
      (extend-senv 'i
        (extend-senv 'v
          (extend-senv 'x
            (empty-senv))))))

  ;; apply-procedure : Proc * ExpVal -> ExpVal

  
  ;; exercise 3.26
  ;; build local environment
  (define free-vars-in
    (lambda (bounds body senv)
      (cases expression body
        (const-exp (num) '())
        (var-exp (var)
          (if (member var bounds)
              '()
              (list (list var (- (apply-senv senv var) 0)))))
        (zero?-exp (exp1) (free-vars-in bounds exp1 senv))
        (diff-exp (exp1 exp2) (append (free-vars-in bounds exp1 senv)
                                      (free-vars-in bounds exp2 senv)))
        (if-exp (exp1 exp2 exp3)
                (append (free-vars-in bounds exp1 senv)
                        (free-vars-in bounds exp2 senv)
                        (free-vars-in bounds exp3 senv)))
        (let-exp (var exp1 l-body)
                 (append (free-vars-in bounds exp1 senv)
                         (free-vars-in (cons var bounds) l-body senv)))
        (proc-exp (var p-body) (free-vars-in (cons var bounds) p-body senv))
        (call-exp (rator rand)
                  (append (free-vars-in bounds rator senv)
                          (free-vars-in bounds rand senv)))
        (else
         (eopl:error 'translation-of "exp type mistake ~s" body)))))


  ;; exercises
  ;; 3.43 substitute var for expression in the body of a let-expression
  ;;      to get a direct proc that eliminates the environment lookup.

  (define substitute-let-var
    (lambda (l-var l-exp l-body )
      (cases expression l-body
        (const-exp (num) (const-exp num))
        (var-exp (var) (if (eqv? var l-var) l-exp (var-exp var)))
        (zero?-exp (exp1) (zero?-exp (substitute-let-var l-var l-exp exp1)))
        (diff-exp (exp1 exp2) (diff-exp
                               (substitute-let-var l-var l-exp exp1)
                               (substitute-let-var l-var l-exp exp2)))
        (if-exp (exp1 exp2 exp3) (if-exp
                                  (substitute-let-var l-var l-exp exp1)
                                  (substitute-let-var l-var l-exp exp2)
                                  (substitute-let-var l-var l-exp exp3)))
        (let-exp (var exp1 body)
          (substitute-let-var
            var (substitute-let-var l-var l-exp exp1) body))
        (proc-exp (var p-body)
          (if (eqv? var l-var)
              (proc-exp var p-body)
              (proc-exp var (substitute-let-var l-var l-exp p-body))))
        (call-exp (rator rand)
          (call-exp (substitute-let-var l-var l-exp rator)
                    (substitute-let-var l-var l-exp rand)))
        (else
         (eopl:error "wrong expression of ~s" l-body))
        )))

  )
