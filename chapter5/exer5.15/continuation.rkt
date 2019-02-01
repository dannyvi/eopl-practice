(module continuation (lib "eopl.ss" "eopl")
  (provide (all-defined-out))

  (define continuation?
    (lambda (cont) (list? cont)))

  (define end-cont
    (lambda () '()))

  (define zero1-cont
    (lambda (cont)
      (cons (list 'zero1-cont) cont)))

  (define let-exp-cont
    (lambda (var body env cont)
      (cons (list 'let-exp-cont var body env) cont)))


  (define let2-2-cont
    (lambda (var1 val1 var2 body cont)
      (cons (list 'let2-2-cont var1 val1 var2 body) cont)))

  (define if-test-cont
    (lambda (exp2 exp3 cont)
      (cons (list 'if-test-cont exp2 exp3) cont)))

  (define diff1-cont
    (lambda (exp2 cont)
      (cons (list 'diff1-cont exp2) cont)))

  (define diff2-cont
    (lambda (val1 cont)
      (cons (list 'diff2-cont val1) cont)))

  (define multiply1-cont
    (lambda (exp2 cont)
      (cons (list 'multiply-cont exp2) cont)))

  (define rator-cont
    (lambda (rands cont)
      (cons (list 'rator-cont rands) cont)))

  (define rand-cont
    (lambda (val1 rands rands-val cont)
      (cons (list 'rand-cont val1 rands rands-val ) cont)))

  (define cons-cont
    (lambda (exp2 cont)
      (cons (list 'cons-cont exp2) cont)))

  (define cons2-cont
    (lambda (val1 cont)
      (cons (list 'cons2-cont val1) cont)))

  (define car-cont
    (lambda (cont)
      (cons (list 'car-cont) cont)))

  (define cdr-cont
    (lambda (cont)
      (cons (list 'cdr-cont) cont)))

  (define null?-cont
    (lambda (cont)
      (cons (list 'null?-cont) cont)))

  (define list-cont
    (lambda (vals exps cont)
      (cons (list 'list-cont vals exps) cont)))

  (define begin-cont
    (lambda (exps cont)
      (cons (list 'begin-cont exps) cont)))


)
