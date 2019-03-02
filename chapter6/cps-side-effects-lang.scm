#lang eopl

(require (only-in racket last))
  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val
      (proc proc?))
     (ref-val
      (ref reference?))
     )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

  ;; the interpreter is tail-recursive, so it really doesn't do
  ;; anything with the continuation.  So all we need is one
  ;; continuation value.

  (define-datatype continuation continuation?
    (end-cont)
    )

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (vars (list-of symbol?))
      (body tfexp?)
      (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;;; represent environment as a list of bindings.
;;; binding ::= ('let    (list-of id) (list-of expval))
;;;           | ('letrec (list-of id) (list-of bvar) (list-of tfexp))

;;; The first binding for extend-env, the second is for
;;; extend-env-rec**.

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

  (define empty-env
    (lambda ()
      '()))

  (define empty-env?
    (lambda (x) (null? x)))

  (define extend-env*
    (lambda (syms vals old-env)
      (cons (list 'let syms vals) old-env)))

  (define extend-env-rec**
    (lambda (p-names b-varss p-bodies saved-env)
      (cons
        (list 'letrec p-names b-varss p-bodies)
        saved-env)))

  (define apply-env
    (lambda (env search-sym)
      ;; returns position of sym in los, else #f
      (define list-index
        (lambda (sym los)
          (let loop ((pos 0) (los los))
            ;; los is at position pos of the original los
            (cond
              ((null? los) #f)
              ((eqv? sym (car los)) pos)
              (else (loop (+ pos 1) (cdr los)))))))
      (if (null? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (saved-env (cdr env)))
          (let ((pos (list-index search-sym (cadr binding))))
            (if pos
              (case (car binding)
                ((let)
                 (list-ref (caddr binding) pos))
                ((letrec)
                 (let ((bvars (caddr binding))
                       (bodies (cadddr binding)))
                     (proc-val
                       (procedure
                         (list-ref bvars pos)
                         (list-ref bodies pos)
                         env)))))
              (apply-env saved-env search-sym)))))))

;; not precise, but will do.
  (define environment?
    (list-of
      (lambda (p)
        (and
          (pair? p)
          (or (eqv? (car p) 'let) (eqv? (car p) 'letrec))))))


;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

  ;; init-env : () -> environment

  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.

  (define init-env
    (let ((extend-env1
            (lambda (sym val env)
              (extend-env* (list sym) (list val) env))))
      (lambda ()
        (extend-env1
          'i (num-val 1)
          (extend-env1
            'v (num-val 5)
            (extend-env1
              'x (num-val 10)
              (empty-env)))))))

  ;; exercise:  Improve this code by getting rid of extend-env1.

  (define instrument-newref (make-parameter #f))


  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.

  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  (define the-store 'uninitialized)

  ;; empty-store : () -> Sto
  ;; Page: 111
  (define empty-store
    (lambda () '()))

  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;; Page 111
  (define initialize-store!
    (lambda ()
      (set! the-store (empty-store))))

  ;; get-store : () -> Sto
  ;; Page: 111
  ;; This is obsolete.  Replaced by get-store-as-list below
  (define get-store
    (lambda () the-store))

  ;; reference? : SchemeVal -> Bool
  ;; Page: 111
  (define reference?
    (lambda (v)
      (integer? v)))

  ;; newref : ExpVal -> Ref
  ;; Page: 111
  (define newref
    (lambda (val)
      (let ((next-ref (length the-store)))
        (set! the-store
              (append the-store (list val)))
        (if (instrument-newref)
            (eopl:printf
             "newref: allocating location ~s with initial contents ~s~%"
             next-ref val)
            #f)
        next-ref)))

  ;; deref : Ref -> ExpVal
  ;; Page 111
  (define deref
    (lambda (ref)
      (list-ref the-store ref)))

  ;; setref! : Ref * ExpVal -> Unspecified
  ;; Page: 112
  (define setref!
    (lambda (ref val)
      (set! the-store
        (letrec
          ((setref-inner
             ;; returns a list like store1, except that position ref1
             ;; contains val.
             (lambda (store1 ref1)
               (cond
                 ((null? store1)
                  (report-invalid-reference ref the-store))
                 ((zero? ref1)
                  (cons val (cdr store1)))
                 (else
                   (cons
                     (car store1)
                     (setref-inner
                       (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref)))))

  (define report-invalid-reference
    (lambda (ref the-store)
      (eopl:error 'setref
        "illegal reference ~s in store ~s"
        ref the-store)))

  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;; Exports the current state of the store as a scheme list.
  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;;   where foo, bar, and baz are expvals.
  ;; If the store were represented in a different way, this would be
  ;; replaced by something cleverer.
  ;; Replaces get-store (p. 111)
   (define get-store-as-list
     (lambda ()
       (letrec
         ((inner-loop
            ;; convert sto to list as if its car was location n
            (lambda (sto n)
              (if (null? sto)
                '()
                (cons
                  (list n (car sto))
                  (inner-loop (cdr sto) (+ n 1)))))))
         (inner-loop the-store 0))))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '((program (expression) a-program)
      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      (expression
        ("+" "(" (separated-list expression ",") ")")
        sum-exp)
      (expression ("zero?" "(" expression ")") zero?-exp)
      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)
      (expression
        ("letrec"
          (arbno identifier "(" (arbno identifier) ")"
            "=" expression)
          "in"
          expression)
        letrec-exp)
      (expression (identifier) var-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)
      (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
      (expression ("(" expression (arbno expression) ")") call-exp)
      (expression ("print" "(" expression ")") print-exp)
      (expression ("begin" "(" (arbno expression) ")") begin-exp)
      (expression ("newref" "(" expression ")") newref-exp)
      (expression ("deref" "(" expression ")") deref-exp)
      (expression ("setref" "(" expression "," expression ")") setref-exp)
      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

  (sllgen:make-define-datatypes the-lexical-spec the-grammar)

  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))

  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))


  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

  (define cps-out-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

 (define cps-out-grammar
    '((cps-out-program (tfexp) cps-a-program)
      (simple-expression (number) cps-const-exp)
      (simple-expression (identifier) cps-var-exp)
      (simple-expression
        ("-" "(" simple-expression "," simple-expression ")")
        cps-diff-exp)
      (simple-expression
       ("zero?" "(" simple-expression ")")
       cps-zero?-exp)
      (simple-expression
        ("+" "(" (separated-list simple-expression ",") ")")
        cps-sum-exp)
      (simple-expression
       ("proc" "(" (arbno identifier) ")" tfexp)
       cps-proc-exp)
      (tfexp
        (simple-expression)
        simple-exp->exp)
      (tfexp
       ("let" identifier "=" simple-expression "in" tfexp)
       cps-let-exp)
      (tfexp
        ("letrec"
          (arbno identifier "(" (arbno identifier) ")"
            "=" tfexp)
          "in"
          tfexp)
        cps-letrec-exp)
      (tfexp
       ("if" simple-expression "then" tfexp "else" tfexp)
       cps-if-exp)
      (tfexp
       ("(" simple-expression (arbno simple-expression) ")")
       cps-call-exp)
      (tfexp
        ("printk" "(" simple-expression ")" ";" tfexp)
        cps-printk-exp)
      (tfexp
        ("newrefk" "(" simple-expression "," simple-expression ")")
        cps-newrefk-exp)
      (tfexp
        ("derefk" "(" simple-expression  "," simple-expression ")")
        cps-derefk-exp)
      (tfexp
        ("setrefk"
          "(" simple-expression "," simple-expression ")" ";"
          tfexp )
        cps-setrefk-exp)
      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

  (sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

  (define cps-out-show-the-datatypes
    (lambda () (sllgen:list-define-datatypes cps-out-lexical-spec cps-out-grammar)))

  (define cps-out-scan&parse
    (sllgen:make-string-parser cps-out-lexical-spec cps-out-grammar))

  (define cps-just-scan
    (sllgen:make-string-scanner cps-out-lexical-spec cps-out-grammar))

  ;; cps-of-program : InpExp -> TfExp
  ;; Page: 224
  (define cps-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (cps-a-program
            (cps-of-exps (list exp1)
              (lambda (new-args)
                (simple-exp->exp (car new-args)))))))))

  ;; cps-of-exp : Exp * SimpleExp -> TfExp
  ;; Page: 222, 228, 231
  (define cps-of-exp
    (lambda (exp k-exp)
      (cases expression exp
        (const-exp (num) (make-send-to-cont k-exp (cps-const-exp num)))
        (var-exp (var) (make-send-to-cont k-exp (cps-var-exp var)))
        (proc-exp (vars body)
          (make-send-to-cont k-exp
            (cps-proc-exp (append vars (list 'k%00))
              (cps-of-exp body (cps-var-exp 'k%00)))))
        (zero?-exp (exp1)
          (cps-of-zero?-exp exp1 k-exp))
        (diff-exp (exp1 exp2)
          (cps-of-diff-exp exp1 exp2 k-exp))
        (sum-exp (exps)
          (cps-of-sum-exp exps k-exp))
        (if-exp (exp1 exp2 exp3)
          (cps-of-if-exp exp1 exp2 exp3 k-exp))
        (let-exp (var exp1 body)
          (cps-of-let-exp var exp1 body k-exp))
        (letrec-exp (ids bidss proc-bodies body)
          (cps-of-letrec-exp ids bidss proc-bodies body k-exp))
        (call-exp (rator rands)
          (cps-of-call-exp rator rands k-exp))
        ;; new for cps-side-effects-lang
        ;; Page: 228
        (print-exp (rator)
          (cps-of-exps (list rator)
            (lambda (simples)
              (cps-printk-exp
                (car simples)
                (make-send-to-cont k-exp (cps-const-exp 38))))))
        (begin-exp (exps)
           (cps-of-exps exps
             (lambda (simples) (make-send-to-cont k-exp (last simples)))))
        ;; Page 231
        (newref-exp (exp1)
          (cps-of-exps (list exp1)
            (lambda (simples)
              (cps-newrefk-exp (car simples) k-exp))))
        (deref-exp (exp1)
          (cps-of-exps (list exp1)
            (lambda (simples)
              (cps-derefk-exp (car simples) k-exp))))
        (setref-exp (exp1 exp2)
          (cps-of-exps (list exp1 exp2)
            (lambda (simples)
              (cps-setrefk-exp
                (car simples)
                (cadr simples)
                ;; the third argument will be evaluated tail-recursively.
                ;; returns 23, just like in explicit-refs
                (make-send-to-cont k-exp (cps-const-exp 23))))))
        )))

  ;; cps-of-exps : (list-of expression) *
  ;;                      ((list-of cps-simple-expression) -> cps-expression)
  ;;                      -> cps-expression
  ;; Page: 219
  ;; usage:
  ;;   -- assume e_i's are non-simple, b_i's are simple
  ;;   -- then
  ;;        (cps-of-exps '(b1 b2 e1 b3 e2 e3) F) ==
  ;;        [e1](\v1.[e2](\v2.[e3](\v3.(F `(,<b1> ,<b2> ,v1 ,<b3> ,v2 ,v3)))))
  ;;      where <b> is cps-of-simple-exp of b.
  (define cps-of-exps
    (lambda (exps builder)
      (let cps-of-rest ((exps exps))
        ;; cps-of-rest : Listof(InpExp) -> TfExp
        (let ((pos (list-index
                     (lambda (exp)
                       (not (inp-exp-simple? exp)))
                     exps)))
          (if (not pos)
            (builder (map cps-of-simple-exp exps))
            (let ((var (fresh-identifier 'var)))
              (cps-of-exp
                (list-ref exps pos)
                (cps-proc-exp (list var)
                  (cps-of-rest
                    (list-set exps pos (var-exp var)))))))))))

  ;; inp-exp-simple? : InpExp -> Bool
  ;; returns #t or #f, depending on whether exp would be a
  ;; simple-exp if reparsed using the CPS-OUT language.
  (define inp-exp-simple?
    (lambda (exp)
      (cases expression exp
        (const-exp (num) #t)
        (var-exp (var) #t)
        (diff-exp (exp1 exp2)
          (and
            (inp-exp-simple? exp1)
            (inp-exp-simple? exp2)))
        (zero?-exp (exp1)
          (inp-exp-simple? exp1))
        (proc-exp (ids exp) #t)
        (sum-exp (exps)
          (all-simple? exps))
        (else #f))))

  (define all-simple?
    (lambda (exps)
      (if (null? exps)
        #t
        (and (inp-exp-simple? (car exps))
          (all-simple? (cdr exps))))))


  ;; takes a list of expressions and finds the position of the first
  ;; one that is not a simple-exp, else returns #f
  (define index-of-first-non-simple
    (lambda (exps)
      (cond
        ((null? exps) #f)
        ((inp-exp-simple? (car exps))
         (let ((pos (index-of-first-non-simple (cdr exps))))
           (if pos
             (+ pos 1) #f)))
        (else 0))))

  ;; cps-of-simple-exp : InpExp -> SimpleExp
  ;; Page: 220
  ;; assumes (inp-exp-simple? exp).
  (define cps-of-simple-exp
    (lambda (exp)
      (cases expression exp
        (const-exp (num) (cps-const-exp num))
        (var-exp (var) (cps-var-exp var))
        (diff-exp (exp1 exp2)
          (cps-diff-exp
            (cps-of-simple-exp exp1)
            (cps-of-simple-exp exp2)))
        (zero?-exp (exp1)
          (cps-zero?-exp
            (cps-of-simple-exp exp1)))
        (proc-exp (ids exp)
          (cps-proc-exp (append ids (list 'k%00))
            (cps-of-exp exp (cps-var-exp 'k%00))))
        (sum-exp (exps)
          (cps-sum-exp
            (map cps-of-simple-exp exps)))
        (else
          (report-invalid-exp-to-cps-of-simple-exp exp)))))

  (define report-invalid-exp-to-cps-of-simple-exp
    (lambda (exp)
      (eopl:error 'cps-simple-of-exp
        "non-simple expression to cps-of-simple-exp: ~s"
        exp)))

  ;; make-send-to-cont : SimpleExp * SimpleExp -> TfExp
  ;; Page: 214
  (define make-send-to-cont
    (lambda (cont bexp)
      (cps-call-exp cont (list bexp))))


  ;; cps-of-zero?-exp : InpExp * SimpleExp -> TfExp
  ;; Page: 222
  (define cps-of-zero?-exp
    (lambda (exp1 k-exp)
      (cps-of-exps (list exp1)
        (lambda (new-rands)
          (make-send-to-cont
            k-exp
            (cps-zero?-exp
              (car new-rands)))))))

  ;; cps-of-sum-exp : Listof (InpExp) * SimpleExp -> TfExp
  ;; Page: 219
  (define cps-of-sum-exp
    (lambda (exps k-exp)
      (cps-of-exps exps
        (lambda (new-rands)
          (make-send-to-cont
            k-exp
            (cps-sum-exp new-rands))))))

  ;; cps-of-diff-exp : InpExp * InpExp * SimpleExp -> TfExp
  ;; Page: 223
  (define cps-of-diff-exp
    (lambda (exp1 exp2 k-exp)
      (cps-of-exps
        (list exp1 exp2)
        (lambda (new-rands)
          (make-send-to-cont
            k-exp
            (cps-diff-exp
              (car new-rands)
              (cadr new-rands)))))))


  ;; cps-of-if-exp : InpExp * InpExp * InpExp * SimpleExp -> TfExp
  ;; Page: 223
  (define cps-of-if-exp
    (lambda (exp1 exp2 exp3 k-exp)
      (cps-of-exps (list exp1)
        (lambda (new-rands)
          (cps-if-exp (car new-rands)
            (cps-of-exp exp2 k-exp)
            (cps-of-exp exp3 k-exp))))))

  ;; cps-of-let-exp : Var * InpExp * InpExp * SimpleExp -> TfExp
  ;; Page: 222
  ;; This is wrong because it puts `k-exp` iside the scope of `id`
  ;; (define cps-of-let-exp
  ;;   (lambda (id rhs body k-exp)
  ;;     (cps-of-exps (list rhs)
  ;;       (lambda (new-rands)
  ;;         (cps-let-exp id
  ;;           (car new-rands)
  ;;           (cps-of-exp body k-exp))))))

  ;; Correct solution: translate the 'let' into the immediate
  ;; application of a lambda expression.  Then
  ;; cps-of-exps will make the needed fresh variables

  (define cps-of-let-exp
    (lambda (id rhs body k-exp)
      (cps-of-exp
       (call-exp
        (proc-exp (list id) body)
        (list rhs))
       k-exp)))

  ;; cps-of-letrec-exp :
  ;; Listof(Listof(Var)) * Listof(InpExp) * InpExp * SimpleExp -> TfExp
  ;; Page: 223
  (define cps-of-letrec-exp
    (lambda (proc-names idss proc-bodies body k-exp)
      (cps-letrec-exp
        proc-names
        (map
          (lambda (ids) (append ids (list 'k%00)))
          idss)
        (map
          (lambda (exp) (cps-of-exp exp (cps-var-exp 'k%00)))
          proc-bodies)
        (cps-of-exp body k-exp))))

  ;; cps-of-call-exp : InpExp * Listof(InpExp) * SimpleExp -> TfExp
  ;; Page: 220
  (define cps-of-call-exp
    (lambda (rator rands k-exp)
      (cps-of-exps (cons rator rands)
        (lambda (new-rands)
          (cps-call-exp
            (car new-rands)
            (append (cdr new-rands) (list k-exp)))))))

 ;;;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;;;

  (define fresh-identifier
    (let ((sn 0))
      (lambda (identifier)
        (set! sn (+ sn 1))
        (string->symbol
          (string-append
            (symbol->string identifier)
            "%"             ; this can't appear in an input identifier
            (number->string sn))))))

  ;; list-set : SchemeList * Int * SchemeVal -> SchemeList
  ;; returns a list lst1 that is just like lst, except that
  ;; (listref lst1 n) = val.
  (define list-set
    (lambda (lst n val)
      (cond
        ((null? lst) (eopl:error 'list-set "ran off end"))
        ((zero? n) (cons val (cdr lst)))
        (else (cons (car lst) (list-set (cdr lst) (- n 1) val))))))

  ;; list-index : (SchemeVal -> Bool) * SchemeList -> Maybe(Int)
  ;; returns the smallest number n such that (pred (listref lst n))
  ;; is true.  If pred is false on every element of lst, then returns
  ;; #f.
  (define list-index
    (lambda (pred lst)
      (cond
        ((null? lst) #f)
        ((pred (car lst)) 0)
        ((list-index pred (cdr lst)) => (lambda (n) (+ n 1)))
        (else #f))))




;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program
    (lambda (pgm)
      (initialize-store!)
      (cases cps-out-program pgm
        (cps-a-program (body)
          (value-of/k body (init-env) (end-cont))))))

  (define value-of-simple-exp
    (lambda (exp env)

      (cases simple-expression exp
        (cps-const-exp (num) (num-val num))
        (cps-var-exp (var) (apply-env env var))
        (cps-diff-exp (exp1 exp2)
          (let ((val1
		  (expval->num
		    (value-of-simple-exp exp1 env)))
                (val2
		  (expval->num
		    (value-of-simple-exp exp2 env))))
            (num-val
	      (- val1 val2))))
        (cps-zero?-exp (exp1)
          (bool-val
            (zero?
              (expval->num
                (value-of-simple-exp exp1 env)))))
        (cps-sum-exp (exps)
          (let ((nums (map
                        (lambda (exp)
                          (expval->num
                            (value-of-simple-exp exp env)))
                        exps)))
            (num-val
              (let sum-loop ((nums nums))
                (if (null? nums) 0
                  (+ (car nums) (sum-loop (cdr nums))))))))
        (cps-proc-exp (vars body)
          (proc-val
            (procedure vars body env)))
        )))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 228 and 230-231
  (define value-of/k
    (lambda (exp env cont)
      (cases tfexp exp
        (simple-exp->exp (bexp)
          (apply-cont cont
            (value-of-simple-exp bexp env)))
        (cps-let-exp (var exp1 body)
          (let ((val (value-of-simple-exp exp1 env)))
            (value-of/k body
              (extend-env* (list var) (list val) env)
              cont)))
        (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
          (value-of/k letrec-body
            (extend-env-rec** p-names b-varss p-bodies env)
            cont))
        (cps-if-exp (exp1 exp2 exp3)
          (value-of/k
            (if (expval->bool (value-of-simple-exp exp1 env))
              exp2
              exp3)
            env
            cont))
        (cps-call-exp (rator rands)
          (let ((rator-proc (expval->proc (value-of-simple-exp rator env)))
                (rand-vals (map
                             (lambda (bexp) (value-of-simple-exp bexp
                                              env))
                             rands)))
            (apply-procedure/k rator-proc rand-vals cont)))

        (cps-printk-exp (simple body)
          (begin
            (eopl:printf "~s~%" (value-of-simple-exp simple env))
            (value-of/k body env cont)))

        (cps-newrefk-exp (simple1 simple2)
          (let ((val1 (value-of-simple-exp simple1 env))
                (val2 (value-of-simple-exp simple2 env)))
            (let ((newval (ref-val (newref val1))))
              (apply-procedure/k
                (expval->proc val2)
                (list newval)
                cont))))

        (cps-derefk-exp (simple1 simple2)
          (apply-procedure/k
            (expval->proc (value-of-simple-exp simple2 env))
            (list
              (deref
                (expval->ref
                  (value-of-simple-exp simple1 env))))
            cont))

       (cps-setrefk-exp (simple1 simple2 body)
          (let ((ref (expval->ref (value-of-simple-exp simple1 env)))
                (val (value-of-simple-exp simple2 env)))
             (begin
                (setref! ref val)
                (value-of/k body env cont))))
        )))

  ;; apply-cont : Cont * ExpVal -> Final-ExpVal
  ;; there's only one continuation, and it only gets invoked once, at
  ;; the end of the computation.
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () val))))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> ExpVal
  ;; Page: 209
  (define apply-procedure/k
    (lambda (proc1 args cont)
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of/k body
            (extend-env* vars args saved-env)
            cont)))))

(define instrument-cps (make-parameter #f))

(define run
  (lambda (string)
    (let ((cpsed-pgm
           (cps-of-program (scan&parse string))))
      ;(when (instrument-cps) (pretty-print cpsed-pgm))
      (value-of-program cpsed-pgm))))
