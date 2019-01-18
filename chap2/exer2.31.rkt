;; Prefix-list ::= (Prefix-exp)
;; Prefix-exp  ::= Int
;;             ::= - Prefix-exp Prefix-exp
;;

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define parse-prefix-exp
  (λ (datum)
    (cond
      [(number? datum) (const-exp datum)]
      [(eqv? (car datum) '-)
       (let ((lst (accum-second (accum-first (cdr datum)))))
         (diff-exp (car lst) (cadr lst)))])))

(define accum-first
  (λ (datum)
    (cond
      [(number? (car datum))
       (cons (const-exp (car datum)) (cdr datum))]
      [(eqv? (car datum) '-)
       (let ((lst (accum-second (accum-first (cdr datum)))))
         (cons (diff-exp (car lst) (cadr lst)) (cddr lst)))])))

(define accum-second
  (λ (datum)
    (let ((first-exp (car datum))
          (rests (cdr datum)))
      (cond
        [(number? (car rests))
         (cons first-exp (cons (const-exp (car rests)) (cdr rests)))]
        [(eqv? (car rests) '-)
         (let ((lst (accum-second (accum-first (cdr rests)))))
           (cons first-exp
            (cons (diff-exp (car lst) (cadr lst))
                  (cddr lst))))]))))



