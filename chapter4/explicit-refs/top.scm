(module top (lib "eopl.ss" "eopl")

  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "lang.scm")             ; for scan&parse
  (require "interp.scm")           ; for value-of-program
  (require "tests.scm")            ; for test-list

  (provide run run-all)

   ;;; interface for book test ;;;
  (provide test-all)
  (define (test-all)
    (run-all))

  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

  ;; run : String -> ExpVal

  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  ;; run-all : () -> Unspecified

  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))

  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))

  (define sloppy->expval
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))

  ;; run-one : Sym -> ExpVal

  ;; (run-one sym) runs the test whose name is sym

  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))

  ;; (run-all)

  )

;; exercise 4.2
;; specification for zero?-exp
;;
;;           (value-of exp p o0) = (val1 , o0)
;;--------------------------------------------------------------------------
;; (value-of (zero?-exp exp) p o0) = ((expval->bool val1) , o0)


;; exercise 4.3
;; specification for call-exp
;;
;;        (value-of rator p o0) = (proc-val (procedure var body p o1))
;;--------------------------------------------------------------------------
;; (value-of (call-exp rator rand) p o0) = (value-of body [var=rand]p o1)


;; exercise 4.4
;; specification for begin exp
;;
;;               (value-of exp p o0) = (val1, o1)
;;--------------------------------------------------------------------------
;;  (value-of (begin exp exps) p o0) =
;;         { (val1 , o1)    if (null? exps)
;;         { (value-of (begin exp1 exps1) p o1) if exps = (cons exp1 exps1)


;; exercise 4.5
;; specification for list
;;
;;              (value-of exp p o0) = (val1 o1)
;;--------------------------------------------------------------------------
;; (value-of (list exp exps) p o0)
;; = ((cons val1 '()) , o1)  #if null? exps
;; : ((cons val1 (value-of (list exp1 exps1) p o1)), o1)
;;                           #if exps = (cons exp1 exps1)


;; exercise 4.8
;; Show exactly where in our implementation of the store these operations
;; take linear time rather than constant time.
;;
;; setref! takes linear time of the length of the-store at worst condition.
