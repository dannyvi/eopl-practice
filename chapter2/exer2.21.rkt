;; 2.21

(define-datatype Env Env?
  (empty-env)
  (extend-env (variable symbol?)
                 (value (lambda (v) #t))
                 (previous Env?)))

(define (has-binding? env search-var)
  (cases Env env
    (empty-env () #f)
    (extend-env (variable value previous)
                (if (eqv? variable search-var) #t
                    (has-binding? previous search-var)))))


;; 2.22

(define-datatype Stack Stack?
  (empty-stack)
  (non-empty-stack (value (lambda (v) #t))
                   (prev-stack Stack?)))

(define (push val stack)
  (cases Stack stack
    (empty-stack () (non-empty-stack val stack))
    (non-empty-stack (value prev-stack)
                     (non-empty-stack val stack))))

(define (pop stack)
  (cases Stack stack
    (empty-stack () (eopl:error 'pop "Empty-stack! ~s " stack))
    (non-empty-stack (value prev-stack) prev-stack)))

(define (top stack)
  (cases Stack stack
         (empty-stack () (eopl:error 'pop "Empty-stack! ~s " stack))
         (non-empty-stack (value prev-stack) value)))

