#lang eopl

(define instrument-newref (make-parameter #f))
(define trace-interp (make-parameter #f))


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
      ;; like list(n1,...,nk) in exceptions language. Sorry about that.
      (expression ("[" (separated-list number ",") "]") const-list-exp)
      (expression (identifier) var-exp)   
      (expression ("-" "(" expression "," expression ")") diff-exp)
      (expression ("if" expression "then" expression "else" expression) if-exp)
      (expression ("proc" "(" identifier ")" expression) proc-exp)
      (expression ("(" expression expression ")") call-exp)
      (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
      (expression ("let" identifier "=" expression "in" expression) let-exp)
      (expression 
        ;; arbitrary number of unary procedures
       ("letrec" (arbno identifier "(" identifier ")" "=" expression)
                 "in" expression)
        letrec-exp)
      (expression ("set" identifier "=" expression) set-exp)
      (expression ("spawn" "(" expression ")") spawn-exp)
      (expression  ("yield" "(" ")") yield-exp)
      (expression ("mutex" "(" ")") mutex-exp)
      (expression ("wait" "(" expression ")") wait-exp)
      (expression ("signal" "(" expression ")") signal-exp)
      ;; other unary operators
      (expression (unop "(" expression ")") unop-exp)

      (unop ("car") car-unop)
      (unop ("cdr") cdr-unop)
      (unop ("null?") null?-unop)
      (unop ("zero?") zero?-unop)
      (unop ("print") print-unop)
      ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))



  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (list-val
      (lst (list-of expval?)))
    (mutex-val
      (mutex mutex?))
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

  (define expval->list
    (lambda (v)
      (cases expval v
	(list-val (lst) lst)
	(else (expval-extractor-error 'list v)))))

  (define expval->mutex
    (lambda (v)
      (cases expval v
        (mutex-val (l) l)
	(else (expval-extractor-error 'mutex v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))
  
;;;;;;;;;;;;;;;; mutexes ;;;;;;;;;;;;;;;;

  (define-datatype mutex mutex?
    (a-mutex
      (ref-to-closed?    reference?)    ; ref to bool
      (ref-to-wait-queue reference?)))  ; ref to (listof thread)
  
;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

  ;; used by begin-exp
  (define fresh-identifier
    (let ((sn 0))
      (lambda (identifier)  
        (set! sn (+ sn 1))
        (string->symbol
          (string-append
            (symbol->string identifier)
            "%"             ; this can't appear in an input identifier
            (number->string sn))))))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


  (define-datatype continuation continuation?

    (end-main-thread-cont)           
    (end-subthread-cont)

    (diff1-cont                       ; cont[(- [] (value-of e2 env))]
      (exp2 expression?)
      (env environment?)
      (cont continuation?))
    (diff2-cont                         ; cont[(- val1 [])]
      (val1 expval?)
      (cont continuation?))
    (if-test-cont
      (exp2 expression?)
      (exp3 expression?)
      (env environment?)
      (cont continuation?))
    (rator-cont            ; cont[(apply-proc [] (value-of rand env))]
      (rand expression?)
      (env environment?)
      (cont continuation?))
    (rand-cont                          ; cont[(apply-proc val1 [])]
      (val1 expval?)
      (cont continuation?))
    (set-rhs-cont
      (loc reference?)
      (cont continuation?))

    (spawn-cont 
      (saved-cont continuation?))
    (wait-cont 
      (saved-cont continuation?))
    (signal-cont 
      (saved-cont continuation?))

    (unop-arg-cont
      (unop1 unop?)
      (cont continuation?))
    )
  
  ;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

;;; represent environment as a list of bindings.
;;; binding ::= (id expval) 
;;;           | ((list-of id) (list-of bvar) (list-of expression))

;;; The first binding for extend-env, the second is for
;;; extend-env-rec. 

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

;;; This should probably be factored out into a module called
;;; environments.scm, like it is in most of the other interpreters.

  (define empty-env
    (lambda ()
      '()))
  
  (define empty-env? 
    (lambda (x) (null? x)))

  (define extend-env
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))

  (define extend-env-rec*
    (lambda (p-names b-vars p-bodies saved-env)
      (cons 
        (list p-names b-vars p-bodies)
        saved-env)))

  (define apply-env
    (lambda (env search-sym)
      (if (null? env) 
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (saved-env (cdr env)))
          (if (symbol? (car binding))
            ;; ok, this is an extend-env
            (if (eqv? search-sym (car binding))
              (cadr binding)
              (apply-env saved-env search-sym))
            ;; no, this is an extend-env-rec
            (let ((pos (locate search-sym (car binding)))
                  (b-vars (cadr binding))
                  (p-bodies (caddr binding)))
              (if pos
                (newref 
                  (proc-val
                    (procedure
                      (list-ref b-vars pos)
                      (list-ref p-bodies pos)
                      env)))
                (apply-env saved-env search-sym))))))))

  ;; returns position of sym in los, else #f
  (define locate
    (lambda (sym los)
      (let loop ((pos 0) (los los))
        ;; los is at position pos of the original los
        (cond
          ((null? los) #f)
          ((eqv? sym (car los)) pos)
          (else (loop (+ pos 1) (cdr los)))))))

  (define init-env
    (lambda ()
      (letrec
        ((make-init-env
           ;; entry ::= (id expval)
           (lambda (entries)
             (if (null? entries)
               (empty-env)
               (extend-env 
                 (car (car entries))
                 (newref (cadr (car entries)))
                 (make-init-env (cdr entries)))))))
        (make-init-env
          (list
            (list 'i (num-val 1))
            (list 'v (num-val 5))
            (list 'x (num-val 10)))))))

;; not precise, but will do.
  (define environment?
    (list-of
      (lambda (p)
        (and 
          (pair? p)
          (or
            (symbol? (car p))
            ((list-of symbol?) (car p)))))))



(define empty-queue
  (lambda ()
    '()))

(define empty? null?)

(define enqueue
  (lambda (q val)
    (append q (list val))))

(define dequeue
  (lambda (q f)
    (f (car q) (cdr q))))

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
        (when (instrument-newref)
            (eopl:printf 
             "newref: allocating location ~s with initial contents ~s~%"
             next-ref val))                     
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


  
  ;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;
  
  ;; components of the scheduler state:
  
  (define the-ready-queue   'uninitialized)         
  (define the-final-answer  'uninitialized)
  
  (define the-max-time-slice    'uninitialized)
  (define the-time-remaining    'uninitialized)

  ;; initialize-scheduler! : Int -> Unspecified
  (define initialize-scheduler!
    (lambda (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer 'uninitialized)
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice) 
      ))
  
  ;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

  ;; place-on-ready-queue! : Thread -> Unspecified
  ;; Page: 184  
  (define place-on-ready-queue!
    (lambda (th)
      (set! the-ready-queue
        (enqueue the-ready-queue th))))

  ;; run-next-thread : () -> FinalAnswer
  ;; Page: 184    
  (define run-next-thread
    (lambda ()
      (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
          (lambda (first-ready-thread other-ready-threads)
            (set! the-ready-queue other-ready-threads)            
            (set! the-time-remaining the-max-time-slice) 
            (first-ready-thread)
            )))))

  ;; set-final-answer! : ExpVal -> Unspecified
  ;; Page: 184    
  (define set-final-answer!
    (lambda (val)
      (set! the-final-answer val)))

  ;; time-expired? : () -> Bool
  ;; Page: 184    
  (define time-expired?
    (lambda ()
      (zero? the-time-remaining)))

  ;; decrement-timer! : () -> Unspecified
  ;; Page: 184    
  (define decrement-timer!
    (lambda ()
      (set! the-time-remaining (- the-time-remaining 1))))


  ;; new-mutex () -> Mutex
  ;; Page: 188
  (define new-mutex
    (lambda ()
      (a-mutex
        (newref #f)                     
        (newref '()))))                 

  ; wait queue, initially empty

  ;; wait-for-mutex : Mutex * Thread -> FinalAnswer
  ;; waits for mutex to be open, then closes it.
  ;; Page: 190
  (define wait-for-mutex
    (lambda (m th)
      (cases mutex m
        (a-mutex (ref-to-closed? ref-to-wait-queue)
          (cond
            ((deref ref-to-closed?)                  
             (setref! ref-to-wait-queue
               (enqueue (deref ref-to-wait-queue) th))
             (run-next-thread))
            (else
              (setref! ref-to-closed? #t)
              (th)))))))

  ;; signal-mutex : Mutex * Thread -> FinalAnswer
  ;; Page 190
  (define signal-mutex
    (lambda (m th)
      (cases mutex m
        (a-mutex (ref-to-closed? ref-to-wait-queue)
          (let ((closed? (deref ref-to-closed?))
                (wait-queue (deref ref-to-wait-queue)))
            (when closed?
              (if (empty? wait-queue)
                (setref! ref-to-closed? #f)
                (dequeue wait-queue
                  (lambda (first-waiting-th other-waiting-ths)
                    (place-on-ready-queue!
                      first-waiting-th)
                    (setref!
                      ref-to-wait-queue
                      other-waiting-ths)))))
            (th))))))

(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)
(define arg 'uninitialized)


  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page 182
  (define value-of/k                    
    (lambda ()

      (when (trace-interp)
        (eopl:printf "value-of/k: ~s~%" exp))

      (cases expression exp

        (const-exp (num)
           (set! val (num-val num))
           (apply-cont))

        (const-list-exp (nums)
          (set! val (list-val (map num-val nums)))
          (apply-cont))

        (var-exp (var)
          (set! val (deref (apply-env env var)))
          (apply-cont))
  
        (diff-exp (exp1 exp2)
          (set! exp exp1)
          (set! cont (diff1-cont exp2 env cont))
          (value-of/k))

        (if-exp (exp1 exp2 exp3)
          (set! exp exp1)
          (set! cont (if-test-cont exp2 exp3 env cont))
          (value-of/k))

        (proc-exp (var body)
          (set! val (proc-val (procedure var body env)))
          (apply-cont))

        (call-exp (rator rand)
          (set! exp rator)
          (set! cont (rator-cont rand env cont))
          (value-of/k))

        (let-exp (var exp1 body)          ; implemented like a macro!
          (set! exp (call-exp (proc-exp var body) exp1))
          (value-of/k))

        (begin-exp (exp1 exps)           ; this one, too
          (if (null? exps)
              (begin (set! exp exp1) (value-of/k))
              (begin (set! exp (call-exp
                          (proc-exp
                           (fresh-identifier 'dummy)
                           (begin-exp (car exps) (cdr exps)))
                          exp1))
               (value-of/k))))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (set! exp letrec-body)
          (set! env (extend-env-rec* p-names b-vars p-bodies env))
          (value-of/k))

        (set-exp (id exp1)
          (set! exp exp1)
          (set! cont (set-rhs-cont (apply-env env id) cont))
          (value-of/k))

        (spawn-exp (exp1)
          (set! exp exp1)
          (set! cont (spawn-cont cont))
          (value-of/k))

        (yield-exp ()
          (place-on-ready-queue!
           (lambda ()
             (set! cont cont)
             (set! val (num-val 99))
             (apply-cont)
             ))
          (run-next-thread))

        (mutex-exp ()
          (set! val (mutex-val (new-mutex)))
          (apply-cont))

        (wait-exp (exp1)
          (set! exp exp1)
          (set! cont (wait-cont cont))
          (value-of/k))

        (signal-exp (exp1)
          (set! exp exp1)
          (set! cont (signal-cont cont))
          (value-of/k))

        (unop-exp (unop1 exp1)
          (set! exp exp1)
          (set! cont (unop-arg-cont unop1 cont))
          (value-of/k))

        )))

  ;; apply-cont : Cont * Exp -> FinalAnswer
  ;; Page: 182 and 186
  (define apply-cont                    
    (lambda ()
      (if (time-expired?)
        (begin
          (place-on-ready-queue!
           (lambda ()
             (set! cont cont)
             (set! val val)
             (apply-cont)))
          (run-next-thread))
        (begin

          (decrement-timer!)

          (cases continuation cont

            (end-main-thread-cont ()
              (set-final-answer! val)
              (run-next-thread))

            (end-subthread-cont ()
              (run-next-thread))

            (diff1-cont (exp2 saved-env saved-cont)
              (set! exp exp2)
              (set! env saved-env)
              (set! cont (diff2-cont val saved-cont))
              (value-of/k))

            (diff2-cont (val1 saved-cont)
              (let ((n1 (expval->num val1))
                    (n2 (expval->num val)))
                (set! cont saved-cont)
                (set! val (num-val (- n1 n2)))
                (apply-cont)))

            (if-test-cont (exp2 exp3 saved-env saved-cont)
              (set! env saved-env)
              (set! cont saved-cont)
              (if (expval->bool val)
                  (set! exp exp2)
                  (set! exp exp3))
              (value-of/k))

            (rator-cont (rand saved-env saved-cont)
              (set! exp rand)
              (set! env saved-env)
              (set! cont (rand-cont val saved-cont))
              (value-of/k))

            (rand-cont (val1 saved-cont)
              (let ((proc (expval->proc val1)))
                (set! proc1 proc)
                (set! cont saved-cont)
                (set! arg val)
                (apply-procedure)))

            (set-rhs-cont (loc saved-cont)
              (begin
                (setref! loc val)
                (set! val (num-val 26))
                (set! cont saved-cont)
                (apply-cont)))

            (spawn-cont (saved-cont)
              (let ((proc2 (expval->proc val)))
                (place-on-ready-queue!
                 (lambda ()
                   (set! proc1 proc2)
                   (set! arg (num-val 28))
                   (set! cont (end-subthread-cont))
                   (apply-procedure)))
                (set! cont saved-cont)
                (set! val (num-val 73))
                (apply-cont)))

            (wait-cont (saved-cont)
              (wait-for-mutex
                (expval->mutex val)
                (lambda ()
                  (set! cont saved-cont)
                  (set! val (num-val 52))
                  (apply-cont))))

            (signal-cont (saved-cont)
              (signal-mutex
                (expval->mutex val)
                (lambda ()
                  (set! cont saved-cont)
                  (set! val (num-val 53))
                  (apply-cont))))

            (unop-arg-cont (unop1 cont)
              (apply-unop unop1 val cont))

            )))))

  (define apply-procedure
    (lambda ()
      (cases proc proc1
        (procedure (var body saved-env)
          (set! exp body)
          (set! env (extend-env var (newref arg) saved-env))
          (value-of/k)))))

  (define apply-unop
    (lambda (unop1 arg saved-cont)
      (cases unop unop1

        (zero?-unop ()
          (set! cont saved-cont)
          (set! val (bool-val (zero? (expval->num arg))))
          (apply-cont))

        (car-unop ()
          (let ((lst (expval->list arg)))
            (set! cont saved-cont)
            (set! val (car lst))
            (apply-cont)))

        (cdr-unop ()
          (let ((lst (expval->list arg)))
            (set! cont saved-cont)
            (set! val (list-val (cdr lst)))
            (apply-cont)))

        ;(apply-cont cont (list-val (cdr lst)))))

        (null?-unop ()
          (set! cont saved-cont)
          (set! val (bool-val (null? (expval->list arg))))
          (apply-cont))

        (print-unop ()
          (set! cont saved-cont)
          (set! val (num-val 1))
          (begin
            (eopl:printf "~a~%" (expval->num arg))
            (apply-cont)))

        )))

  

(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
           (a-program (exp1)
                      (set! exp exp1)
                      (set! env (init-env))
                      (set! cont (end-main-thread-cont))
                      (value-of/k)))))

(define run
  (lambda (timeslice string)
    (value-of-program timeslice (scan&parse string))))
