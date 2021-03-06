(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val
      (proc proc?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->val
    (lambda (v)
      (cases expval v
             (num-val (num) num)
             (bool-val (bool) bool)
             (proc-val (proc) proc)
             (else (eopl:error "Not valid ~s" v))
             )))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?))
    (trace-procedure
     (var symbol?)
     (body expression?)
     (env environment?)))

  ;; Page: 86
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval (lambda (x) (or (expval? x) (vector? x))))
      (saved-env environment?))
    ;;(extend-env-rec
    ;;  (id symbol?)
    ;;  (bvar symbol?)
    ;;  (body expression?)
    ;;  (saved-env environment?))
    (extend-env-rec-mutual
     (ids (list-of symbol?))
     (bvars (list-of symbol?))
     (bodys (list-of expression?))
     (saved-env environment?))


    )

  (define extend-env-rec
    (lambda (p-name b-var body saved-env)
      (let ((vec (make-vector 1)))
        (let ((new-env (extend-env p-name vec saved-env)))
          (vector-set! vec 0
           (proc-val (procedure b-var body new-env)))
          new-env))))

  (define list-of
    (lambda (pred)
      (lambda (val)
        (or (null? val)
            (and (pair? val)
                 (pred (car val))
                 ((list-of pred) (cdr val)))))))


)
