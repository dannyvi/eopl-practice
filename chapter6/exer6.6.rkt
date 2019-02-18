
;;;; -------------   6   --------------

(lambda (x y) (+ (f (g x)) (h (j y))))

(lambda (x y cont)
  (g x (lambda (v0)
    (f v0 (lambda (v1)
      (j y (lambda (v2)
        (h v2 (lambda (v3) (+ v1 v3))))))))))

(lambda (x y cont)
  (g x (lambda (v0)
    (j y (lambda (v1)
      (f v0 (lambda (v2)
        (h v1 (lambda (v3) (+ v2 v3))))))))))

(lambda (x y cont)
  (g x (lambda (v0)
    (j y (lambda (v1)
      (h v1 (lambda (v2)
        (f v0 (lambda (v3) (+ v3 v2))))))))))

(lambda (x y cont)
  (j y ...)
  (h v0 ...)
  (g x ...)
  (f v2 ...))

(lambda (x y cont)
  (j y ...)
  (g x ...)
  (h v0 ...)
  (f v1 ...))

(lambda (x y cont)
  (j y ...)
  (g x ...)
  (f v1 ...)
  (h v0 ...))

