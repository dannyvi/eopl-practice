#lang racket

(define in-S?
  (lambda (n)
    (if (zero? n)
        #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))

(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
                "List too short by ~s elements.~%" (+ n 1))))


(define nth-elem
  (lambda (lst nth)
    (define n-elem
      (lambda (l n)
        (if (null? l)
            (report-list-error lst nth)
            (if (zero? n)
                (car l)
                (n-elem (cdr l) (- n 1))))))
    (n-elem lst nth)
    ))

(define report-list-error
  (lambda (lst nth)
    (eopl:error 'nth-elem
                "~s does not have ~s elems" lst nth)))

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

(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))

(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))

;; partial-vector-sum : ...
;; usage:

(define duple
  (lambda (n x)
    (if (<= n 0)
        '()
        (cons x (duple (- n 1) x)))))

(define shift-lst
  (lambda (old new)
    (if (null? old)
        new
        (shift-lst (cdr old) (cons (car old) new)))))

(define invert-2-lst
  (lambda (lst)
    (map (lambda (elem) (shift-lst elem '())) lst)))


(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) '())
              (down (cdr lst))))))

(define swapper-lst
  (lambda (s1 s2 slist)
    (cond
     [(null? slist) '()]
     [(eqv? (car slist) s1) (cons s2 (swapper s1 s2 (cdr slist)))]
     [(eqv? (car slist) s2) (cons s1 (swapper s1 s2 (cdr slist)))]
     [else (cons (car slist) (swapper s1 s2 (cdr slist)))])))

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons (swap-s-exp s1 s2 (car slist))
              (swapper s1 s2 (cdr slist))))))

(define swap-s-exp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
    (cond
     [(eqv? sexp s1) s2]
     [(eqv? sexp s2) s1]
     [else sexp])
    (swapper s1 s2 sexp)
    )))



