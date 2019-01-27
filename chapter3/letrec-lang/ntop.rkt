(module ntop (lib "eopl.ss" "eopl")
  (require "interp.scm")
  (require "lang.scm")

  (define run
    (lambda (str)
      (value-of-program (scan&parse str))))
  )

;; exercise 3.30
;; What is the purpose of the call to proc-val on the next-to-last line of apply-env?
;; To be evaluated by value-of.

