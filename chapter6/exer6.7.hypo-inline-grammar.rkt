#lang eopl
;; This program does not really run. Since we need a 'make-define-procedural' proc to generate 
;; the procedural program rather than data-structure.

(define the-lexical-spec
  '([whitespace (whitespace) skip]
    [comment ("%" (arbno (not #\newline))) skip]
    [ID (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

; E datatype-expression ; ID datatype-identifier ; n env ; e exp ; k cont ; v val ; a var ; d body ; pm p-name ;
(define the-grammar
  '((program (E)                  (λ (e1) (λ (n k) (e1 n k))))
    (E (number)                   (λ (b) (λ (n k) (k b))))
    (E ("-" "(" E "," E ")")      (λ (e1 e2) (λ (n k) (e1 n (λ (v1) (e2 n (λ (v2) (k (- v1 v2)))))))))
    (E ("zero?" "(" E ")")        (λ (e1) (λ (n k) (e1 n (λ (v) (k (zero? v)))))))
    (E ("if" E "then" E "else" E) (λ (e1 e2 e3) (λ (n k) (e1 n (λ (v1) (if v1 (e2 n k) (e3 n k)))))))
    (E (ID)                       (λ (a) (λ (n k) (k (n a)))))
    (E ("let" ID "=" E "in" E)    (λ (a e1 d) (λ (n k) (e1 n (λ (v1) (d (λ (a1) (if (eqv? a1 a) v1 (n a1))) k))))))
    (E ("proc" "(" ID ")" E)      (λ (a d) (λ (n k) (k (λ (v k1) (d (λ (a1) (if (eqv? a1 a) v (n a1))) k1))))))
    (E ("(" E E ")")              (λ (e1 e2) (λ (n k) (e1 n (λ (p) (e2 n (λ (v) (p v k))))))))
    (E ("letrec" ID "(" ID ")" "=" E "in" E)
       (λ (pm br pd ld) (λ (n k) (ld (λ (a) (if (eqv? a pm) (λ (v k1) (pd (λ (a1) (if (eqv? a1 br) v (n a1))) k1)) (n a))) k))))))

;; require a procedure
;;(define make-string-parse ...)
(define scan&parse (make-string-parser the-lexical-spec the-grammar))

(define run (λ (string)
  ((scan&parse string) (λ (search-sym) (eopl:error 'apply-n "No binding for ~s" search-sym)) (λ (x) x))))
