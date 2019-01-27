
;; Bintree ::= () | (Int Bintree Bintree)

(define number->bintree
  (lambda (number)
    (list number '() '())))

(define current-element
  (lambda (bin-tree)
    (car bin-tree)))

(define move-to-left-son (lambda (bin-tree) (cadr bin-tree)))
(define move-to-right-son (lambda (bin-tree) (caddr bin-tree)))
(define at-leaf? (lambda (bin-tree) (null? bin-tree)))

(define insert-to-left
  (lambda (number bin-tree)
    (list (current-element bin-tree)
          (list number (move-to-left-son bin-tree) '())
          (move-to-right-son bin-tree))))

(define insert-to-right
  (lambda (number bin-tree)
    (list (current-element bin-tree)
          (move-to-left-son bin-tree)
          (list number (move-to-right-son bin-tree) '()))))

