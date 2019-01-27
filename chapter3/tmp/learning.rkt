;; =============================Specifications of===============================

;; Syntax

;; Program ::= Exp                     a-program (expl)
;; Exp ::= Number                      const-exp (num)
;; Exp ::= -(Exp , Exp)                diff-exp (exp1 exp2)
;; Exp ::= zero? (Exp)                 zero?-exp (exp1)
;; Exp ::= if Exp then Exp else Exp    if-exp (exp1 exp2 exp3)
;; Exp ::= Identifer                   var-exp (var)
;; Exp ::= let Identifier = Exp in Exp let-exp (var exp1 body)


;; Values

;; ExpVal = Int + Bool                 expressed value
;; DenVal = Int + Bool                 denoted value

;; interface

;; num-val  : Int -> ExpVal
;; bool-val : Bool -> ExpVal
;; expval->num : ExpVal -> Int
;; expval->bool: Expval -> Bool


;; Environments

;; p [] [var=val]p [var1=val1, var2=val2]p [var1=val1, var2=val2,...]


;; Behavior of Exp

;; constructors:

;; const-exp : Int -> Exp
;; zero?-exp : Exp -> Exp
;; if-exp    : Exp x Exp x Exp -> Exp
;; diff-exp  : Exp x Exp -> Exp
;; var-exp   : Var -> Exp
;; let-exp   : Var x Exp x Exp -> Exp

;; observer:

;; value-of  : Exp x Env -> ExpVal

(require eopl "proc-env.rkt" "lang.scm")

;; datatypes

(define-datatype program program?
  (a-program
   (expl expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (minus-exp
   (exp1 expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (quotient-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  (emptylist)
  (cons-exp
   (carexp expression?)
   (cdrexp list?))
  (car-exp
   (exp1 list?))
  (cdr-exp
   (exp1 list?))
  (null?-exp
   (exp1 list?))
  )


;; Env implementation

;; init-env : () -> Env
;; usage    : (init-env) = [i=|1|, v=|5|, x=|10|]


(define init-env
  (λ ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

(define report-expval-extractor-error
  (λ (sym val)
    (eopl:error sym "Type error of : ~s" val)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

;; expval->num : ExpVal -> Int

(define expval->num
  (λ (val)
    (cases expval val
           (num-val (num) num)
           (else (report-expval-extractor-error 'num val)))))

;; expval->bool : ExpVal -> Bool

(define expval->bool
  (λ (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (report-expval-extractor-error 'bool val)))))

;; run          : String -> ExpVal

(define run
  (λ (str)
    (value-of-program (scan&parse str))))

;; value-of-program : Program -> ExpVal

(define value-of-program
  (λ (pgm)
    (cases progra pgm
      (a--program (expl)
         (value-of expl (init-env))))))

;; value-of : Exp x Exp -> ExpVal

(define value-of
  (λ (expr env)
    (cases expression expr
      [const-exp (num) (num-val num)]
      [minus-exp (exp1)
        (num-val (- (expval->num (value-of exp1 env) )))]
      [var-exp (var) (apply-env env var)]
      [diff-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (- num1 num2))))]
      [add-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val (+ num1 num2))))]
      [mult-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                     (let ((num1 (expval->num val1))
                           (num2 (expval->num val2)))
                       (num-val (* num1 num2))))]
      [quotient-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                     (let ((num1 (expval->num val1))
                           (num2 (expval->num val2)))
                       (num-val (quotient num1 num2))))]
      [zero?-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->num val1)))
            (if (zero? num1) (bool-val #t) (bool-val #f))))]
      [equal?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (if (eqv? num1 num2) (bool-val #t) (bool-val #f))))]
      [greater?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (if (> num1 num2) (bool-val #t) (bool-val #f))))]
      [less?-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (if (< num1 num2) (bool-val #t) (bool-val #f))))]
      [if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env)))]
      [let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body
                    (extend-env var val1 env)))]
      [emptylist () '()]
      [cons-exp (carexp cdrexp) (cons carexp cdrexp)]
      [car-exp (exp1) (car exp1)]
      [cdr-exp (exp1) (cdr exp1)]
      [null?-exp (exp1) (null? exp1)])))

