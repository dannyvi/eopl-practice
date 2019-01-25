(module translator (lib "eopl.ss" "eopl")

  (require "lang.scm")
  (require (only-in racket index-of))
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
        (let-exp (vars exps body)                 ;;;;;;;
          (nameless-let-exp
            (map (lambda (exp1) (translation-of exp1 senv)) exps)
            (translation-of body
              (extend-senv vars senv))))
        (proc-exp (vars body)                     ;;;;;;;;
          (nameless-proc-exp
            (translation-of body
              (extend-senv vars senv))))
        (call-exp (rator rands)                   ;;;;;;;;;
          (call-exp
            (translation-of rator senv)
           (map (lambda (rand) (translation-of rand senv)) rands)))
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
  ;;  (define apply-senv
  ;;    (lambda (senv var)
  ;;      (cond
  ;;        ((null? senv) (report-unbound-var var))
  ;;        ((eqv? var (car senv))
  ;;         0)
  ;;        (else
  ;;          (+ 1 (apply-senv (cdr senv) var))))))


  (define apply-senv
    (lambda (senv var)
      (cond
       ((null? senv) (report-unbound-var var))
       ((index-of (car senv) var) (list 0 (index-of (car senv) var)))
       (else
        ((lambda (lst) (cons (+ 1 (car lst)) (cdr lst)))
         (apply-senv (cdr senv) var) )))))

  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable in code: ~s" var)))

  ;; init-senv : () -> Senv
  ;; Page: 96
  (define init-senv
    (lambda ()
      (extend-senv '(i)
        (extend-senv '(v)
          (extend-senv '(x)
            (empty-senv))))))

  )
