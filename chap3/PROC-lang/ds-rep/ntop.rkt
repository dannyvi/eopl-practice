(module ntop (lib "eopl.ss" "eopl")
  (require "interp.scm")
  (require "lang.scm")


  (define run
    (lambda (str)
      (value-of-program (scan&parse str))))
)


;; exercise 3.20 Currying
;; (run "let add = proc (x) proc (y) -(x, -(0, y)) in ((add 4) 5)")


