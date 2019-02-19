(define list-sum
  (lambda (loi)
    (list-sum/k loi
                (lambda (x) (eopl:printf "End of computation.~%")
                        (eopl:printf "This sentence should appear only once.~%")
                        x))))

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
        (cont 0)
        (list-sum/k (cdr loi) (lambda (v) (cont (+ (car loi) v)))))))

(define list-sum/c
  (lambda (loi start)
    (if (null? loi) start (list-sum/c (cdr loi) (+ (car loi) start)))))

