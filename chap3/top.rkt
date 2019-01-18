(require "interp.scm")

(require "lang.scm")

(define run
  (lambda (str)
    (value-of-program (scan&parse str))))
