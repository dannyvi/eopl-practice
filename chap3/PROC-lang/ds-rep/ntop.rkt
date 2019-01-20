(module ntop (lib "eopl.ss" "eopl")
  (require "interp.scm")
  (require "lang.scm")

  (define run
    (lambda (str)
      (value-of-program (scan&parse str))))
)


;; exercise 3.20 Currying
;; (run "let add = proc (x) proc (y) -(x, -(0, y)) in ((add 4) 5)")



;; exercise 3.23 combinator

;; (let* ((m (lambda (maker) (lambda (x) (if (zero? x) 0 (- ((maker maker) (- x 1)) -4)))))  (times4 (lambda (x) ((m m) x)))) (times4 1))

;;  (run "let makemult = proc (maker) proc (x) if zero?(x) then 0 else [- ((maker maker) [- x 1] ) -4] in let times4 = proc (x) ((makemult makemult) x) in (times4 3)")

;; factorial
;; (run "let makemult = proc (maker) proc (x) if zero?(x) then 1 else [* ((maker maker) [- x 1] ) x] in let factorial = proc (x) ((makemult makemult) x) in (factorial 5)")

;; exercise 3.25
;; (λfn.f (λxz.f(x x)z λxz.f(x x)z) n) λgx.if(0?x) x (- ((g (-x 1)) -4))
;;         -----------------------
;; ( λz.f(d d)z ) n

