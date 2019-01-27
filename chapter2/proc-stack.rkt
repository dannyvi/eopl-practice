#lang eopl

(define report-empty-stack
  (lambda ()
    (eopl:error 'pop "Empty stack")))

(define empty-stack
  (lambda ()
    (lambda (command)
      (cond [(eqv? command 'empty?) #t]))))

(define push
  (lambda (stack val)
    (lambda (command)
      (cond
       [(eqv? command 'empty?) #f]
       [(eqv? command 'pop) stack]
       [(eqv? command 'top) val]))))

(define pop
  (lambda (stack)
    (stack 'pop)))

(define top
  (lambda (stack)
    (stack 'top)))

(define empty?
  (lambda (stack)
    (stack 'empty?)))



