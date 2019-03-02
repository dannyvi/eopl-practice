#lang eopl

(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fact-it n)
  (define (fact-iter n iter)
    (if (= n 0) iter (fact-iter (- n 1) (* iter n))))
  (fact-iter n 1)
  )

;;;;;;; data-structure ;;;;;;;;;;;;;;;;;;;;;;;;

(define fact/ds
  (lambda (n cont)
    (if (= n 0)
        (apply-cont cont 1)
        (fact/ds (- n 1) (fact-cont n cont)))))

(define-datatype continuation continuation?
  (end-cont)
  (fact-cont (n number?) (cont continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont () val)
      (fact-cont (n cont) (apply-cont cont (* n val))))))
;;;;;;;;; register ;;;;;;;;;;;;;

(define val 'uninitialized)
(define n 'uninitialized)
(define contr 'uninitialized)

(define fact/r
  (lambda ()
    (if (= n 0)
        (begin (set! val 1) (apply-cont-r))
        (begin (set! contr (fact-cont n contr)) (set! n (- n 1)) (fact/r)))))

(define apply-cont-r
  (lambda ()
    (cases continuation contr
           (end-cont () val)
           (fact-cont (nr cont)
                      (set! contr cont)
                      (set! val (* val nr))
                      (apply-cont-r)))))

;;;;;;;;; procedural ;;;;;;;;;;;;;;;;;;;;;;;;

(define fact/proc
  (lambda (n k)
    (if (= n 0)
      (apply-cont-p k 1)
      (fact/proc (- n 1) (fact-cont-p n k)))))

(define fact-cont-p
  (lambda (n k)
    (lambda (val)
      (apply-cont-p k (* n val)))))

(define end-cont-p
  (lambda () (lambda (val) val)))

(define apply-cont-p
  (lambda (k val) (k val)))

;;;;;;;;;;;;;;;;;;;;;;;;;; inline ;;;;;;;;;;;;;;;;;;;;

(define fact/inline
  (lambda (n k)
    (if (= n 0) (k 1) (fact/inline (- n 1) (lambda (val) (k (* val n)))))))

(define fact/p
  (lambda (n k) (if (= n 0) (k 1) (fact/p (- n 1) (f-cont n k)))))
(define f-cont (lambda (n k) (lambda (v) (k (* n v)))))
(define fact (lambda (n) (fact/p n (lambda (id) id))))


;;;;;;;;;;;;;;;;;;;;;; ds ;;;;;;;;;;;;;;;;;;;;;;;;;

(define fact/ds (λ (n k) (if (= n 0) (apply-k k 1) (fact/ds (- n 1) (fact-k n k)))))
(define-datatype kont kont? (end-k) (fact-k (n number?) (k kont?)))
(define apply-k 
  (λ (k v) (cases kont k (end-k () v) (fact-k (n k1) (apply-k k1 (* n v))))))
(define fact (λ (n) (fact/ds n (end-k))))
