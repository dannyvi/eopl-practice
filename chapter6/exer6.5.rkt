#lang eopl

;;  (apply-cont (end-cont) val)
;;  = (begin
;;       (eopl:printf "End of computation.~%")
;;       (eopl:printf "This sentence should appear only once.~%") val)

;; remove-first (section 1.2.3)




;;======================================================================

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

;;;;;;;;;;; data-structure ;;;;;;;;;


(define-datatype continuation continuation?
  (end-cont)
  (remove-first-cont (s symbol?)
                     (cont continuation?)))

(define remove-first/k
  (lambda (s los cont)
    (cond
      [(null? los) (apply-cont cont '())]
      [(eqv? s (car los)) (apply-cont cont (cdr los))]
      [else (remove-first/k s (cdr los) (remove-first-cont (car los) cont))]
      )))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont () (begin
                          (eopl:printf "End of computation.~%")
                          (eopl:printf "This sentence should appear only once.~%")
                          val))
      (remove-first-cont (s saved-cont) (apply-cont saved-cont (cons s val))))))

(define remove-first
  (lambda (s los)
    (remove-first/k s los (end-cont))))


;;;;;;;;;;; procedural ;;;;;;;;;;;;;

(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val)
      )))

(define remove-first-cont
  (lambda (s cont)
    (lambda (val) (apply-cont cont (cons s val)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define remove-first/k
  (lambda (s los cont)
    (cond
      [(null? los) (apply-cont cont '())]
      [(eqv? s (car los)) (apply-cont cont (cdr los))]
      [else (remove-first/k s (cdr los) (remove-first-cont (car los) cont))]
      )))

(define remove-first
  (lambda (s los)
    (remove-first/k s los (end-cont))))


;;;;;;;;;;; register ;;;;;;;;;;;;;;;

(define s 'uninitialized)
(define los 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define-datatype continuation continuation?
  (end-cont)
  (remove-first-cont (sym symbol?)
                     (cont continuation?)))

(define remove-first
  (lambda (arg-s arg-los)
    (set! cont (end-cont))
    (set! s arg-s)
    (set! los arg-los)
    (remove-first/k)
    val))

(define remove-first/k
  (lambda ()
    (cond
      [(null? los) (begin
                     (set! val '())
                     (apply-cont))]
      [(eqv? s (car los)) (begin
                            (set! val (cdr los))
                            (apply-cont))]
      [else (begin
              (set! cont (remove-first-cont (car los) cont))
              (set! los (cdr los))
              (remove-first/k))])))

(define apply-cont
  (lambda ()
    (cases continuation cont
           (end-cont ()
              (begin
                (eopl:printf "End of computation.~%")
                (eopl:printf "This sentence should appear only once.~%")
                val))
      (remove-first-cont (sym saved-cont)
                         (set! cont saved-cont)
                         (set! val (cons sym val))
                         (apply-cont)))))

;;;;;;;;;;; trampoline ;;;;;;;;;;;;;


(define s 'uninitialized)
(define los 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define pc 'uninitialzed)

(define-datatype continuation continuation?
  (end-cont)
  (remove-first-cont (sym symbol?)
                     (cont continuation?)))

(define trampoline!
  (lambda ()
    (when pc
        (begin (pc)
               (trampoline!)))))

(define remove-first
  (lambda (arg-s arg-los)
    (set! cont (end-cont))
    (set! s arg-s)
    (set! los arg-los)
    (set! pc remove-first/k)
    (trampoline!)
    val))

(define remove-first/k
  (lambda ()
    (cond
      [(null? los) (begin
                     (set! val '())
                     (set! pc apply-cont)
                     )]
      [(eqv? s (car los)) (begin
                            (set! val (cdr los))
                            (set! pc apply-cont)
                            )]
      [else (begin
              (set! cont (remove-first-cont (car los) cont))
              (set! los (cdr los))
              )])))

(define apply-cont
  (lambda ()
    (cases continuation cont
           (end-cont ()
              (begin
                (eopl:printf "End of computation.~%")
                (eopl:printf "This sentence should appear only once.~%")
                (set! pc #f)
                ))
      (remove-first-cont (sym saved-cont)
                         (set! cont saved-cont)
                         (set! val (cons sym val))
                         ))))


;;;;;;;;;;; inlined ;;;;;;;;;;;;;;;;

(define remove-first/k
  (lambda (s los k)
    (if (null? los) (k '())
        (if (eqv? (car los) s) (k (cdr los))
            (remove-first/k s (cdr los) (lambda (v0) (k (cons (car los) v0))))))))

(define remove-first
  (lambda (s los)
    (remove-first/k s los
     (lambda (x)
       (begin
         (eopl:printf "End of computation.~%")
         (eopl:printf "This sentence should appear only once.~%")
         x)))))


;;======================================================================

;; list-sum     (section 1.3)

;; list-sum : Listof(Int) -> Int

(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

;;;;;;;;;;; data-structure ;;;;;;;;;

(define-datatype continuation continuation?
  (end-cont)
  (list-sum-cont (i integer?)
                 (cont continuation?)))

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
        (apply-cont cont 0)
        (list-sum/k (cdr loi) (list-sum-cont (car loi) cont)))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val)
      (list-sum-cont (i saved-cont)
                     (apply-cont saved-cont (+ i val))))))

(define list-sum
  (lambda (loi)
    (list-sum/k loi (end-cont))))


;;;;;;;;;;; procedural ;;;;;;;;;;;;;;;;;

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
        (apply-cont cont 0)
        (list-sum/k (cdr loi) (list-sum-cont (car loi) cont)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

(define list-sum-cont
  (lambda (i cont)
    (lambda (val)
      (apply-cont cont (+ i val)))))

(define list-sum
  (lambda (loi)
    (list-sum/k loi (end-cont))))


;;;;;;;;;;; registerize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define loi 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)


(define-datatype continuation continuation?
  (end-cont)
  (list-sum-cont (i integer?)
                 (cont continuation?)))

(define list-sum/k
  (lambda ()
    (if (null? loi)
        (begin
          (set! val 0)
          (apply-cont))
        (begin
          (set! cont (list-sum-cont (car loi) cont))
          (set! loi (cdr loi))
          (list-sum/k)))))

(define apply-cont
  (lambda ()
    (cases continuation cont
           (end-cont ()
                     (eopl:printf "End of computation.~%")
                     (eopl:printf "This sentence should appear only once.~%"))
           (list-sum-cont (i saved-cont)
                          (set! val (+ i val))
                          (set! cont saved-cont)
                          (apply-cont)))))


(define list-sum
  (lambda (loi-arg)
    (set! loi loi-arg)
    (set! cont (end-cont))
    (list-sum/k)
    val))




;;;;;;;;;;; inlined ;;;;;;;;;;;;;;;;

(define list-sum
  (lambda (loi)
    (list-sum/k loi
      (lambda (x) (eopl:printf "End of computation.~%")
              (eopl:printf "This sentence should appear only once.~%")
              x))))

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
        (cont 0)
        (list-sum/k (cdr loi) (lambda (v) (cont (+ (car loi) v)))))))


;;======================================================================

;; occurs-free? (section 1.2.4)

(define occurs-free?
  (lambda (var exp)
    (cond
      [(symbol? exp) (eqv? var exp)]
      [(eqv? (car exp) 'lambda)
       (and
         (not (eqv? var (car (cadr exp))))
         (occurs-free? var (caddr exp)))]
      [else
       (or
        (occurs-free? var (car exp))
        (occurs-free? var (cadr exp)))])))

;;;;;;;;;;;;;;;;; data-structure ;;;;;;;;;;;;;;;;;;;;;


;; map-or : listof(test-proc) -> elem -> Bool
;; test whether elem satisfy any one of the test-proc
(define map-or
  (lambda procs
    (lambda (elem)
      (foldr (lambda (proc l) (or (proc elem) l)) #f procs))))

(define-datatype continuation continuation?
  (end-cont)
  (occurs-free-cont1 (cond1 boolean?)
                     (cont continuation?))
  (occurs-free-cont2 (var symbol?)
                     (exp (map-or list? symbol?))
                     (cont continuation?))
  (occurs-free-cont3 (val boolean?)
                     (cont continuation?)))


(define occurs-free?/k
  (lambda (var exp cont)
    (cond
      [(symbol? exp) (apply-cont cont (eqv? var exp))]
      [(eqv? (car exp) 'lambda)
       (occurs-free?/k var (caddr exp)
                       (occurs-free-cont1 (not (eqv? var (car (cadr exp)))) cont))]
      [else
       (occurs-free?/k var (cadr exp) (occurs-free-cont2 var (car exp) cont))])))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
         (eopl:printf "End of computation.~%")
         (eopl:printf "This sentence should appear only once.~%")
         val)
      (occurs-free-cont1 (cond1 saved-cont)
                         (apply-cont saved-cont (and cond1 val)))
      (occurs-free-cont2 (var exp saved-cont)
                         (occurs-free?/k var exp (occurs-free-cont3 val saved-cont)))
      (occurs-free-cont3 (val1 saved-cont)
                         (apply-cont saved-cont (or val1 val))))))

(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp (end-cont))))

;;;;;;;;;;; procedurual ;;;;;;;;;;;;;;;;


(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp (end-cont))))


(define occurs-free?/k
  (lambda (var exp cont)
    (cond
      [(symbol? exp) (apply-cont cont (eqv? var exp))]
      [(eqv? (car exp) 'lambda)
       (occurs-free?/k var (caddr exp)
                       (occurs-free-cont1 (not (eqv? var (car (cadr exp)))) cont))]
      [else
       (occurs-free?/k var (cadr exp) (occurs-free-cont2 var (car exp) cont))])))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

(define occurs-free-cont1
  (lambda (cond1 cont)
    (lambda (val)
      (apply-cont cont (and cond1 val)))))

(define occurs-free-cont2
  (lambda (var exp cont)
    (lambda (val)
      (occurs-free?/k var exp (occurs-free-cont3 val cont)))))

(define occurs-free-cont3
  (lambda (val1 cont)
    (lambda (val)
      (apply-cont cont (or val val1)))))

;;;;;;;;;;; registerized ;;;;;;;;;;;;;;;

(define var 'uninitialized)
(define exp 'uninitialized)
(define cont 'uninitialzed)
(define val 'uninitialzed)

(define map-or
  (lambda procs
    (lambda (elem)
      (foldr (lambda (proc l) (or (proc elem) l)) #f procs))))

(define-datatype continuation continuation?
  (end-cont)
  (occurs-free-cont1 (cond1 boolean?)
                     (cont continuation?))
  (occurs-free-cont2 (var symbol?)
                     (exp (map-or list? symbol?))
                     (cont continuation?))
  (occurs-free-cont3 (val boolean?)
                     (cont continuation?)))


(define occurs-free?/k
  (lambda ()     ;var exp cont)
    (cond
      [(symbol? exp)
              (set! val (eqv? var exp))
              (apply-cont)]
      ;(apply-cont cont (eqv? var exp))]
      [(eqv? (car exp) 'lambda)
         (set! cont (occurs-free-cont1 (not (eqv? var (car (cadr exp)))) cont))
         (set! exp (caddr exp))
         (occurs-free?/k)]
       ;(occurs-free?/k var (caddr exp)
       ;                (occurs-free-cont1 (not (eqv? var (car (cadr exp)))) cont))]
      [else
         (set! cont (occurs-free-cont2 var (car exp) cont))
         (set! exp (cadr exp))
         (occurs-free?/k)])))
       ;;(occurs-free?/k var (car exp) (occurs-free-cont2 var (cadr exp) cont))])))

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont ()
         (eopl:printf "End of computation.~%")
         (eopl:printf "This sentence should appear only once.~%"))
      (occurs-free-cont1 (cond1 saved-cont)
         (set! val (and cond1 val))
         (set! cont saved-cont)
         (apply-cont))
                         ;;(apply-cont saved-cont (and cond1 val)))
      (occurs-free-cont2 (var-arg exp-arg saved-cont)
         (set! var var-arg)
         (set! exp exp-arg)
         (set! cont (occurs-free-cont3 val saved-cont))
         (occurs-free?/k))
                     ;;(occurs-free?/k var-arg exp-arg (occurs-free-cont3 val saved-cont)))
      (occurs-free-cont3 (val1 saved-cont)
         (set! cont saved-cont)
         (set! val (or val val1))
         (apply-cont)))))
      ;(apply-cont saved-cont (or val val1))))))

(define occurs-free?
  (lambda (var-arg exp-arg)
    (set! var var-arg)
    (set! exp exp-arg)
    (set! cont (end-cont))
    (occurs-free?/k)
    val))


;;;;;;;;;;; inlined ;;;;;;;;;;;;;;;;

(define occurs-free?/k
  (lambda (var exp k)
    (cond
      [(symbol? exp) (k (eqv? var exp))]
      [(eqv? (car exp) 'lambda)
       (occurs-free?/k var (caddr exp)
         (lambda (v) (k (and (not (eqv? var (car (cadr exp)))) v)))) ]  ;; the continuation
      [else
        (k (occurs-free?/k var (car exp)
          (lambda (v0)
            (occurs-free?/k var (cadr exp)
              (lambda (v1) (or v0 v1))))))])))

(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp
      (lambda (x)
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        x))))



;;======================================================================

;; subst        (section 1.2.5)


;; subst : Sym x Sym x S-list -> S-list
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons (subst-in-s-exp new old (car slist))
              (subst new old (cdr slist))))))


;; subst-in-s-exp : Sym x Sym x S-exp -> S-exp
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))

;;;;;;;;;;;;;;;;; data-structure ;;;;;;;;;;;;;;;;;

(define-datatype continuation continuation?
  (end-cont)
  (subst-cont (new symbol?)
              (old symbol?)
              (sexp (map-or list? symbol?))
              (cont continuation?))
  (subst-in-s-exp-cont (val list?)
                       (cont continuation?))
  )

(define subst/k
  (lambda (new old slist cont)
    (if (null? slist) (apply-cont cont '())
        (subst/k new old (cdr slist) (subst-cont new old (car slist) cont)))))

(define subst-in-s-exp/k
  (lambda (new old sexp cont)
    (if (symbol? sexp)
        (if (eqv? sexp old) (apply-cont cont new) (apply-cont cont sexp))
        (subst/k new old sexp cont))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val)
      (subst-cont (new old sexp saved-cont)
                  (subst-in-s-exp/k new old sexp (subst-in-s-exp-cont val saved-cont)))
      (subst-in-s-exp-cont (val1 saved-cont)
         (apply-cont saved-cont (cons val val1))))))

(define subst
  (lambda (new old slist)
    (subst/k new old slist (end-cont))))

;;;;;;;;;;;;;;;;; procedurual ;;;;;;;;;;;;;;;;;;;;


(define subst/k
  (lambda (new old slist cont)
    (if (null? slist) (apply-cont cont '())
        (subst/k new old (cdr slist) (subst-cont new old (car slist) cont)))))

(define subst-in-s-exp/k
  (lambda (new old sexp cont)
    (if (symbol? sexp)
        (if (eqv? sexp old) (apply-cont cont new) (apply-cont cont sexp))
        (subst/k new old sexp cont))))

(define apply-cont
  (lambda (cont val)
    (cont val)))
    #| (cases continuation cont
      (end-cont ()
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val)
      (subst-cont (new old sexp saved-cont)
                  (subst-in-s-exp/k new old sexp (subst-in-s-exp-cont val saved-cont)))
      (subst-in-s-exp-cont (val1 saved-cont)
                           (apply-cont saved-cont (cons val val1)))))) |#

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

(define subst-cont
  (lambda (new old sexp cont)
    (lambda (val)
      (subst-in-s-exp/k new old sexp (subst-in-s-exp-cont val cont)))))

(define subst-in-s-exp-cont
  (lambda (val1 cont)
    (lambda (val)
      (apply-cont cont (cons val val1)))))

(define subst
  (lambda (new old slist)
    (subst/k new old slist (end-cont))))


;;;;;;;;;;;;;;;;; registerize ;;;;;;;;;;;;;;;;;;;;

(define new 'uninitialized)
(define old 'uninitialized)
(define sexp 'uninitialized)
(define slist 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)


;; map-or : listof(test-proc) -> elem -> Bool
;; test whether elem satisfy any one of the test-proc
(define map-or
  (lambda procs
    (lambda (elem)
      (foldr (lambda (proc l) (or (proc elem) l)) #f procs))))


(define-datatype continuation continuation?
  (end-cont)
  (subst-cont (new symbol?)
              (old symbol?)
              (sexp (map-or list? symbol?))
              (cont continuation?))
  (subst-in-s-exp-cont (val list?)
                       (cont continuation?))
  )

(define subst/k
  (lambda ()
    (if (null? slist)
        (begin
          (set! val '())
          (apply-cont))
        (begin
          (set! cont (subst-cont new old (car slist) cont))
          (set! slist (cdr slist))
          (subst/k)))))

        ;(subst/k new old (cdr slist) (subst-cont new old (car slist) cont)))))

(define subst-in-s-exp/k
  (lambda ()             ;(new old sexp cont)
    (if (symbol? sexp)
        (if (eqv? sexp old)
            (begin (set! val new) (apply-cont)) (begin (set! val sexp) (apply-cont)))
        ;(apply-cont cont new) (apply-cont cont sexp))
        (begin (set! slist sexp)
               (subst/k)))))
              ;(subst/k new old sexp cont))))

(define apply-cont
  (lambda ()                 ;(cont val)
    (cases continuation cont
      (end-cont ()
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        )
      (subst-cont (new-arg old-arg sexp-arg saved-cont)
        (set! new new-arg)
        (set! old old-arg)
        (set! sexp sexp-arg)
        (set! cont (subst-in-s-exp-cont val saved-cont))
        (subst-in-s-exp/k))
         ;(subst-in-s-exp/k new old sexp (subst-in-s-exp-cont val saved-cont)))
      (subst-in-s-exp-cont (val1 saved-cont)
        (set! val (cons val val1))
        (set! cont saved-cont)
        (apply-cont)))))
        ;(apply-cont saved-cont (cons val val1))))))

(define subst
  (lambda (new-arg old-arg slist-arg)
    (set! new new-arg)
    (set! old old-arg)
    (set! slist slist-arg)
    (set! cont (end-cont))
    (subst/k)
    val))
    ;(subst/k new old slist (end-cont))))



;;;;;;;;;;;;;;;;; inline ;;;;;;;;;;;;;;;;;;;;;;;;;

(define subst/k
  (lambda (new old slist k)
    (if (null? slist) (k '())
        (subst/k new old (cdr slist)
          (lambda (v0)
            (subst-in-s-exp/k new old (car slist) (lambda (v1) (k (cons v1 v0)))))))))

(define subst-in-s-exp/k
  (lambda (new old sexp k)
    (if (symbol? sexp)
        (if (eqv? sexp old) (k new) (k sexp))
        (subst/k new old sexp k))))

(define subst
  (lambda (new old slist)
    (subst/k new old slist (lambda (x) x))))


