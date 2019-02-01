(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?

  (require "store.scm")

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
     (boolean boolean?))
    (list-val
     (leest leest?))
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

  (define expval->leest
    (lambda (v)
      (cases expval v
             (list-val (bool) bool)
             (else (expval-extractor-error 'leest v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
             (ref-val (ref) ref)
             (else (expval-extractor-error 'reference v)))))
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

  ;; Page: 148
  (define identifier? symbol?)

  (define-datatype continuation continuation?
    (end-cont)                 
    (zero1-cont
      (saved-cont continuation?))
    (let-exp-cont
      (var identifier?)
      (body expression?)
      (saved-cont continuation?))
    (letm-cont
      (vars (list-of identifier?))
      (vals list?)
      (exps (list-of expression?))
      (body expression?)
      (saved-cont continuation?))
    (let2-1-cont
      (var1 identifier?)
      (var2 identifier?)
      (exp2 expression?)
      (body expression?)
      (saved-cont continuation?))
    (let2-2-cont
      (var1 identifier?)
      (val1 reference?)
      (var2 identifier?)
      (body expression?)
      (saved-cont continuation?))
    (if-test-cont 
      (exp2 expression?)
      (exp3 expression?)
      (saved-cont continuation?))
    (diff1-cont                
      (exp2 expression?)
      (saved-cont continuation?))
    (diff2-cont                
      (val1 expval?)
      (saved-cont continuation?))
    (multiply1-cont                
     (exp2 expression?)
     (saved-cont continuation?))
    (multiply2-cont                
     (val1 expval?)
     (saved-cont continuation?))
    (rator-cont            
      (rands (list-of expression?))
      (saved-cont continuation?))
    (rand-cont             
      (val1 expval?)
      (rands list?)
      (rands-val (list-of expval?))
      (saved-cont continuation?))
    (cons-cont
      (exp2 expression?)
      (saved-cont continuation?))
    (cons2-cont
      (val1 expval?)
      (saved-cont continuation?))
    (car-cont
      (saved-cont continuation?))
    (cdr-cont
      (saved-cont continuation?))
    (null?-cont
      (saved-cont continuation?))
    (list-cont
      (vals leest?)
      (exps (list-of expression?))
      (cont continuation?))
    (begin-cont
      (exps (list-of expression?))
      (cont continuation?))
    )

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvars (list-of symbol?))
      (body expression?)
      (env environment?)))
  
;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval reference?)
      (saved-env environment?))
    (extend-env-rec
      (p-name symbol?)
      (b-vars (list-of symbol?))
      (p-body expression?)
      (saved-env environment?)))



;;

  (define-datatype leest leest?
    (empty-leest)
    (cons-leest
     (car-val expval?)
     (cdr-val leest?)))
  
                                        ;(define cons-leest
                                        ;  (lambda (val1 val2)
                                        ;    (cons-leest val1 val2)))
  
  (define car-leest
    (lambda (val)
      (cases leest val
             (empty-leest () (eopl:error 'car "emptylist!"))
             (cons-leest (car-val cdr-val) car-val))))
  
  (define cdr-leest
    (lambda (val)
      (cases leest val
             (empty-leest () (eopl:error 'car "emptylist!"))
             (cons-leest (car-val cdr-val) cdr-val))))
  
  (define null?-leest
    (lambda (val)
      (cases leest val
           (empty-leest () #t)
           (cons-leest (carv cdrv) #f))))

)
