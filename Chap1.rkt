
(define in-S?
  (lambda (n)
    (if (zero? n)
        #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))

(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
                "List too short by ~s elements.~%" (+ n 1))))


(define nth-elem
  (lambda (lst nth)
    (define n-elem
      (lambda (l n)
        (if (null? l)
            (report-list-error lst nth)
            (if (zero? n)
                (car l)
                (n-elem (cdr l) (- n 1))))))
    (n-elem lst nth)))

(define report-list-error
  (lambda (lst nth)
    (eopl:error 'nth-elem
                "~s does not have ~s elems" lst nth)))

(define occurs-free?
  (lambda (var exp)
    (cond
     [(symbol? exp) (eqv? var exp)]
     [(eqv? (car exp) 'lambda)
      (and
       (not (eqv? var (car (cadr exp))))
       (occurs-free? var (caddr exp)))]
     [else
      (or
       (occurs-free? var (car exp))
       (occurs-free? var (cadr exp)))])))

(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))

(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))

;; 1.15

(define duple
  (lambda (n x)
    (if (<= n 0)
        '()
        (cons x (duple (- n 1) x)))))

(define shift-lst
  (lambda (old new)
    (if (null? old)
        new
        (shift-lst (cdr old) (cons (car old) new)))))

;; 1.16

(define invert-2-lst
  (lambda (lst)
    (map (lambda (elem) (shift-lst elem '())) lst)))


;; 1.17

(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) '())
              (down (cdr lst))))))

;; 1.18

(define swapper-lst
  (lambda (s1 s2 slist)
    (cond
     [(null? slist) '()]
     [(eqv? (car slist) s1) (cons s2 (swapper s1 s2 (cdr slist)))]
     [(eqv? (car slist) s2) (cons s1 (swapper s1 s2 (cdr slist)))]
     [else (cons (car slist) (swapper s1 s2 (cdr slist)))])))

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons (swap-s-exp s1 s2 (car slist))
              (swapper s1 s2 (cdr slist))))))

(define swap-s-exp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
        (cond
         [(eqv? sexp s1) s2]
         [(eqv? sexp s2) s1]
         [else sexp])
        (swapper s1 s2 sexp))))

;; 1.19

(define list-set
  (lambda (lst nth x)
    (define lst-set
      (lambda (l n x)
        (cond
         [(and (>= n 0) (null? l)) (report-list-error lst nth)]
         [(eqv? n 0) (cons x (cdr l))]
         [else (cons (car l) (lst-set (cdr l) (- n 1) x))])))
    (lst-set lst nth x)))

;; 1.20

(define partial-count-s-exp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? s sexp) 1 0)
        (count-occurrences s sexp))))

(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (partial-count-s-exp s (car slist))
           (count-occurrences s (cdr slist))))))

;; 1.21

(define prod-s-exp-lst
  (lambda (s-exp lst)
    (if (null? lst)
        '()
        (cons (cons s-exp (cons (car lst) '()))
              (prod-s-exp-lst s-exp (cdr lst))))))

(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (cons (prod-s-exp-lst (car sos1) sos2)
              (product (cdr sos1) sos2)))))

;; 1.22

(define filter-in
  (lambda (pred lst)
    (cond
     [(null? lst) '()]
     [(pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))]
     [else (filter-in pred (cdr lst))])))


;; 1.23

(define partial-index
  (lambda (n pred lst)
    (cond
     [(null? lst) #f]
     [(pred (car lst)) n]
     [else (partial-index (+ n 1) pred (cdr lst))])))

(define list-index
  (lambda (pred lst)
    (partial-index 0 pred lst)))

;; 1.24

(define every?
  (lambda (pred lst)
    (cond
     [(null? lst) #t]
     [(pred (car lst)) (every? pred (cdr lst))]
     [else #f])))

;; 1.25

(define exists?
  (lambda (pred lst)
    (cond
     [(null? lst) #f]
     [(pred (car lst)) #t]
     [else (exists? pred (cdr lst))])))

;; 1.26

(define join
  (lambda (lst1 lst2)
    (if (null?  lst1)
        lst2
        (cons (car lst1) (join (cdr lst1) lst2)))))

(define up
  (lambda (lst)
    (cond
     [(null? lst) '()]
     [(list? (car lst)) (join (car lst) (up (cdr lst)))]
     [else (cons (car lst) (up (cdr lst)))])))

;; 1.27

(define flatten-s-exp
  (lambda (s-exp)
    (if (symbol? s-exp)
        (cons s-exp '())
        (flatten s-exp))))

(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (join (flatten-s-exp (car slist)) (flatten (cdr slist))))
    ))

;; 1.28

(define merge
  (lambda (loi1 loi2)
    (cond [(null? loi1) loi2]
          [(null? loi2) loi1]
          [(< (car loi1) (car loi2))
           (cons (car loi1) (merge (cdr loi1) loi2))]
          [else (cons (car loi2) (merge loi1 (cdr loi2)))])))

;; 1.29

(define pred-of-lst
  (lambda (pred value lst)
    (cond [(null? lst) value]
          [(pred (car lst) value) (pred-of-lst pred (car lst) (cdr lst))]
          [else (pred-of-lst pred value (cdr lst))])))

(define rest-of-lst
  (lambda (choice lst)
    (cond
     [(null? lst) '()]
     [(eqv? (car lst) choice)
      (cdr lst)]
     [else
      (cons (car lst) (rest-of-lst choice (cdr lst)))])))

(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (let ((minimal (pred-of-lst < (car loi) loi)))
          (cons minimal (sort (rest-of-lst minimal loi)))))))

;; 1.30

(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
        (let ((choice (pred-of-lst pred (car loi) loi)))
          (cons choice (sort/predicate pred (rest-of-lst choice loi)))))))

;; 1.31
;; binary tree

(define leaf
  (lambda (int) int))

(define interior-node
  (lambda (sym lson rson)
    (list sym lson rson)))

(define leaf?
  (lambda (node)
    (number? node)))

(define lson
  (lambda (binary)
    (cadr binary)))

(define rson
  (lambda (binary)
    (caddr binary)))

(define contents-of
  (lambda (binary)
    (if (leaf? binary)
        binary
        (list (contents-of (lson binary)) (contents-of (rson binary))))))

;; 1.32 

(define double-tree
  (lambda (binary)
    (if (leaf? binary)
        (* 2 binary)
        (interior-node (car binary)
                       (double-tree (lson binary))
                       (double-tree (rson binary))))))

;; 1.33

(define depth-mark
  (lambda (depth binary)
    (cond
     [(leaf? binary) depth]
     [(eqv? (car binary) 'red)
      (interior-node 'red
                     (depth-mark (+ 1 depth) (lson binary))
                     (depth-mark (+ 1 depth) (rson binary)))]
     [else
      (interior-node (car binary)
                     (depth-mark depth (lson binary))
                     (depth-mark depth (rson binary)))])))

(define mark-leaves-with-red-depth
  (lambda (binary)
    (depth-mark 0 binary)))

;; 1.34
(define in-binary-search-tree?
  (lambda (integer binary-search-tree)
    (cond
     [(null? binary-search-tree) #f]
     [(eqv? (car binary-search-tree) integer) #t]
     [else (or
            (in-binary-search-tree? integer (cadr binary-search-tree))
            (in-binary-search-tree? integer (caddr binary-search-tree)))])))

(define path
  (lambda (integer binary-search-tree)
    (cond
     [(eqv? (car binary-search-tree) integer) '()]
     [(in-binary-search-tree? integer (cadr binary-search-tree))
      (cons 'left (path integer (cadr binary-search-tree)))]
     [(in-binary-search-tree? integer (caddr binary-search-tree))
      (cons 'right (path integer (caddr binary-search-tree)))]
     [else #f])))

;; 1.35

;; side-effect

(define number-leaves
  (let ((num 0) (v 0))
    (lambda (binary)
      (cond
       [(leaf? binary) (begin (set! v num) (set! num (+ num 1)) v)]
       [else (interior-node (car binary)
                            (number-leaves (cadr binary))
                            (number-leaves (caddr binary)))]))))

(define count-leaf
  (lambda (binary)
    (if (leaf? binary)
        1
        (+ (count-leaf (lson binary))
           (count-leaf (rson binary))))))

(define number-leaves-num
  (lambda (num binary)
    (if (leaf? binary)
        (leaf num)
        (interior-node (car binary)
                       (number-leaves-num
                        num
                        (lson binary))
                       (number-leaves-num
                        (+ num (count-leaf (lson binary)))
                        (rson binary))))))

(define number-leaves-2
  (lambda (binary)
    (number-leaves-num 0 binary)))

;; 1.36


(define g
  (lambda (head tail)
    (cons head
          (map (lambda (item)
                 (list (+ (car item) 1) (cadr item)))
               tail))))

(define number-elements-2
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements-2 (cdr lst))))))

