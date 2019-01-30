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

;; 4.28

;;   (value-of exp1 p o0) = (val1, o1) , (value-of exp2 p o1) = (val2 ,o2)
;; -------------------------------------------------------------------------
;;  (value-of (newpair exp1 exp2) p o0) =
;;  ((mutpair-val l) , [l+1 = val2][l = val1]o1)

;; (value-of exp1 p o0) = ((mutpair-val n) , [n+1 = val2][n = val1]o0)
;; -------------------------------------------------------------------------
;; (value-of (left-exp exp1) p o1) = (val1 , o0)
;; (value-of (right-exp exp1) p o1) = (val2 , o0)
;; (value-of (setleft-exp exp1 exp2)) =
;;        ( , [n+1 = val2][n = (value-of exp2 p o0)]o0 )
;; (value-of (setright-exp exp1 exp2)) =
;;        ( , [n+1 = (value-of exp2 p o0)][n = val1]o0 )




