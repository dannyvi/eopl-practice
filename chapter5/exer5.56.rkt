#lang eopl

(require (only-in racket filter foldr remove))

(define instrument-newref (make-parameter #f))
(define trace-interp (make-parameter #f))
(define trace-running-thread (make-parameter #t))

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
      (expression ("kill" "(" expression ")") kill-exp)
      (expression ("send" "(" expression "," expression")") send-exp)
      (expression ("recv" "("  ")") recv-exp)
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
    (kill-cont (saved-cont continuation?))
    (send1-cont (exp2 expression?)
                (saved-env environment?)
                (saved-cont continuation?))
    (send2-cont (val expval?)
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

  ;;;;;;;;;;;;;;;; thread ;;;;;;;;;;;;;;;;;;

(define-datatype a-thread a-thread?
  (the-thread (id expval?)
              (proc procedure?)))


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


;;;;;;; thread management ;;;;;;;;
(define the-current-thread-id 'uninitialized)
(define global-thread-id 'uninitialized)
(define message-queue 'uninitialized)
(define initialize-thread
  (lambda ()
    (set! global-thread-id (num-val 0))
    (set! the-current-thread-id (num-val 0))
    (set! global-mutexes '())
    (set! message-queue '(((num-val 0) ())))))

(define new-thread-id
  (lambda () 
    (set! global-thread-id (num-val (+ 1 (expval->num global-thread-id))))
    global-thread-id))

(define global-mutexes 'uninitialized)

(define is-thread-id?
  (lambda (thrd thid)
    (cases a-thread thrd
           (the-thread (id proc) (equal? thid id)))))

(define remove-thread-from-queue

  (lambda (thid)
    (let ((q (filter
              (lambda (th)
                (cases a-thread th
                  (the-thread (id proc) (not (equal? thid id)))))
              the-ready-queue)))
      (let ((result (bool-val (foldr (lambda (l r)
                             (or r (is-thread-id? l thid))) #f the-ready-queue))))
        (set! the-ready-queue q)
        (eopl:printf "result: ~a" result)
        result))))
;;    (foldr (lambda (l r) (or (is-thread-id? r thid))) #f the-ready-queue)))

(define (init-message-queue! id)
  (set! message-queue (append message-queue (list (list id (list)))))
  )

(define (destroy-message-queue! id)
  (set! message-queue (filter (lambda (q) (not (equal? (car q) id))) message-queue))
  )

(define (send-message id msg)
  (define (add-msg id msg msg-queue)
    (cond [(null? msg-queue) '()]
          [(equal? (caar msg-queue) id)
           (let* ((queue (cadar msg-queue))
                  (m-queue (append queue (list msg)))
                  (thrd-queue (list id m-queue)))
             (cons thrd-queue (cdr msg-queue)))]
          [else (cons (car msg-queue) (add-msg id msg (cdr msg-queue)))]))
  (set! message-queue (add-msg id msg message-queue))
  )
  ;;(set! message-queue (append message-queue (list (list id msg)))))

(define (receive-message id)
  (let ((result #f))
    (define (remove-first-msg id msg-queue)
      (cond [(null? msg-queue) '()]
            [(equal? (caar msg-queue) id)
             (let* ((queue (cadar msg-queue)))
               (when (> (length queue) 0)
                 (set! result (car queue))
                 (set! queue (cdr queue)))
               (cons (list id queue) (cdr msg-queue)))]
            [else (cons (car msg-queue) (remove-first-msg id (cdr msg-queue)))]))
    (set! message-queue (remove-first-msg id message-queue))
    result))


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
            (cases a-thread first-ready-thread
              (the-thread (id proc)
                (set! the-current-thread-id id)
                (when (trace-running-thread)
                  (eopl:printf "the current thread: ~a~n" (expval->num id)))
                (proc)))

            ;(first-ready-thread)
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




  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page 182
  (define value-of/k                    
    (lambda (exp env cont)

      (when (trace-interp)
        (eopl:printf "value-of/k: ~s~%" exp))

      (cases expression exp

        (const-exp (num) (apply-cont cont (num-val num)))

        (const-list-exp (nums)
          (apply-cont cont
            (list-val (map num-val nums))))

        (var-exp (var) (apply-cont cont (deref (apply-env env var))))
  
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))

        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))

        (proc-exp (var body)
          (apply-cont cont
            (proc-val
              (procedure var body env))))

        (call-exp (rator rand)
          (value-of/k rator env
            (rator-cont rand env cont)))

        (let-exp (var exp1 body)          ; implemented like a macro!
          (value-of/k
            (call-exp
              (proc-exp var body)
              exp1)
            env
            cont))
        
        (begin-exp (exp exps)           ; this one, too
          (if (null? exps)
            (value-of/k exp env cont)
            (value-of/k
              (call-exp
                (proc-exp 
                  (fresh-identifier 'dummy)
                  (begin-exp (car exps) (cdr exps)))
                exp)
              env
              cont)))
        
        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of/k
            letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)
            cont))

        (set-exp (id exp)
          (value-of/k exp env
            (set-rhs-cont (apply-env env id) cont)))

        (spawn-exp (exp)
          (value-of/k exp env
            (spawn-cont cont)))

        (yield-exp ()
          (place-on-ready-queue!
           (the-thread the-current-thread-id
                       (lambda () (apply-cont cont (num-val 99)))))
          (run-next-thread))

        (kill-exp (exp)
                  (value-of/k exp env (kill-cont cont)))

        (send-exp (exp1 exp2)
                  (value-of/k exp1 env (send1-cont exp2 env cont)))

        (recv-exp ()
          (let ((val (receive-message the-current-thread-id)))
            (when (trace-running-thread)
            (eopl:printf "the message: ~s~n receiving: ~a~n" message-queue val))
             (if val (apply-cont cont val)
                 (begin
                   (place-on-ready-queue!
                    (the-thread the-current-thread-id
                                (lambda () (value-of/k (recv-exp) env cont))))
                   (run-next-thread))
                 )))

        (unop-exp (unop1 exp)
          (value-of/k exp env
            (unop-arg-cont unop1 cont)))

        )))

  ;; apply-cont : Cont * Exp -> FinalAnswer
  ;; Page: 182 and 186
  (define apply-cont                    
    (lambda (cont val)
      (if (time-expired?)
        (begin
          (place-on-ready-queue!
            (the-thread the-current-thread-id (lambda () (apply-cont cont val))))
          (run-next-thread))
        (begin

          (decrement-timer!)

          (cases continuation cont

            (end-main-thread-cont ()
              (set-final-answer! val)
              (destroy-message-queue! the-current-thread-id)
              (run-next-thread))
  
            (end-subthread-cont ()
              (destroy-message-queue! the-current-thread-id)
              (run-next-thread))
               
            (diff1-cont (exp2 saved-env saved-cont)
              (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
            (diff2-cont (val1 saved-cont)
              (let ((n1 (expval->num val1))
                    (n2 (expval->num val)))
                (apply-cont saved-cont
                  (num-val (- n1 n2)))))
            (if-test-cont (exp2 exp3 env cont)
              (if (expval->bool val)
                (value-of/k exp2 env cont)
                (value-of/k exp3 env cont)))
            (rator-cont (rand saved-env saved-cont)
              (value-of/k rand saved-env
                (rand-cont val saved-cont)))
            (rand-cont (val1 saved-cont)
              (let ((proc (expval->proc val1)))
                (apply-procedure proc val saved-cont)))
            (set-rhs-cont (loc cont)
              (begin
                (setref! loc val)
                (apply-cont cont (num-val 26))))

            (spawn-cont (saved-cont)
              (let ((proc1 (expval->proc val)))
                (let ((id (new-thread-id)))
                  (init-message-queue! id)
                  (place-on-ready-queue!
                   (the-thread
                    id
                    (lambda ()
                      (apply-procedure proc1
                                       id
                                       (end-subthread-cont)))))
                  (apply-cont saved-cont (num-val 73)))))

            (kill-cont (saved-cont)
                       (remove-thread-from-queue val)
                       (destroy-message-queue! the-current-thread-id)
                       (apply-cont saved-cont val))

            (send1-cont (exp2 saved-env saved-cont)
                        (value-of/k exp2 saved-env (send2-cont val saved-cont)))

            (send2-cont (thrd-id saved-cont)
                        (when (trace-running-thread)
                          (eopl:printf "sending message to thread ~a ~a~n"
                                       (expval->num thrd-id) val))
                        (send-message thrd-id val)
                        (apply-cont saved-cont (num-val 1000)))

            (unop-arg-cont (unop1 cont)
              (apply-unop unop1 val cont))

            )))))

  (define apply-procedure
    (lambda (proc1 arg cont)
      (cases proc proc1
        (procedure (var body saved-env)
          (value-of/k body
            (extend-env var (newref arg) saved-env)
            cont)))))

  (define apply-unop
    (lambda (unop1 arg cont)
      (cases unop unop1

        (zero?-unop ()
          (apply-cont cont
            (bool-val
              (zero? (expval->num arg)))))

        (car-unop ()
          (let ((lst (expval->list arg)))
            (apply-cont cont (car lst))))
        (cdr-unop ()
          (let ((lst (expval->list arg)))
            (apply-cont cont (list-val (cdr lst)))))

        (null?-unop ()
          (apply-cont cont 
            (bool-val (null? (expval->list arg)))))

        (print-unop ()
          (begin
            (eopl:printf "~a~%" (expval->num arg))
            (apply-cont cont (num-val 1))))

        )))

  

(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (initialize-thread)
    ;;(init-message-queue! (num-val 0))
    (cases program pgm
           (a-program (exp1)
                      (value-of/k
                       exp1
                       (init-env)
                       (end-main-thread-cont))))))

(define run
  (lambda (timeslice string)
    (value-of-program timeslice (scan&parse string))))
