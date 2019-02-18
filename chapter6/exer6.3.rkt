;; 1

(lambda (x y) (p (+ 8 x) (q y)))

(lambda (x y cont) (q y (lambda (v1) (p (+ 8 x) v1 cont))))

;; 2

(lambda (x y u v) (+ 1 (f (g x y) (+ u v))))

(lambda (x y u v cont)
  (g x y (lambda (v0) (f v0 (+ u v) (lambda (v1) (+ 1 v1))))))

;; 3

(+ 1 (f (g x y) (+ u (h v))))

(g x y (lambda (v0) (+ 1 (f v0 (+ u (h v))))))
(g x y (lambda (v0) (h v (lambda (v1) (+ 1 (f v0 (+ u v1)))))))
(g x y (lambda (v0) (h v (lambda (v1) (f v0 (+ u v1) (lambda (v2) (+ 1 v2)))))))

;; 4

(zero? (if a (p x) (p y)))

(p x (lambda (v0) (zero? (if a v0 (p y)))))
(p x (lambda (v0) (p y (lambda (v1) (zero? (if a v0 v1))))))

;; 5

(zero? (if (f a) (p x) (p y)))

(f a (lambda (v0) (p x (lambda (v1) (p y  (lambda (v2) (zero? (if v0 v1 v2))))))))

;; 6

(let ((x (let ((y 8)) (p y)))) x)

(let ((y 8)) (p y))
(let ((y 8)) (p y cont))

(let ((x (let ((y 8)) (p y (lambda (a) a)))))  x)

;; 7

(let ((x (if a (p x) (p y)))) x)

(let ((x (p x (lambda (v0) (p y (lambda (v1) (if a v0 v1))))))) x)
