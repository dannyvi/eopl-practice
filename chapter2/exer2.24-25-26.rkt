#lang eopl
;; 2.24

(define-datatype bintree bintree?
  (leaf-node (number integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

(define bintree-to-list
  (lambda (btree)
    (cases bintree btree
      (leaf-node (num) `(leaf-node ,num))
      (interior-node (key left right)
         (list 'interior-node
               key
               (bintree-to-list left)
               (bintree-to-list right))))))

;; 2.25
(define sum-bintree
  (lambda (btree)
    (cases bintree btree
      (leaf-node (number) number)
      (interior-node (key left right)
        (+ (sum-bintree left) (sum-bintree right))))))

(define max-name-value
  (lambda (parent-pair left-pair right-pair)
    (let ((max-var
           (apply max (map cadr (list parent-pair left-pair right-pair)))))
    (cond
     [(eqv? max-var (cadr parent-pair)) parent-pair]
     [(eqv? max-var (cadr left-pair)) left-pair]
     [(eqv? max-var (cadr right-pair)) right-pair]))))

(define max-interior-with-value
  (lambda (btree)
    (cases bintree btree
      (leaf-node (number) `(leaf ,number))
      (interior-node (key left right)
        (max-name-value (list key (sum-bintree btree))
                        (max-interior-with-value left)
                        (max-interior-with-value right))))))

;;
;; 2.26 Red-blue-tree
;; Grammar
;; Red-blue-tree ::= Red-blue-subtree
;; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;;                  ::= (blue-node {Red-blue-subtree}*)
;;                  ::= (leaf-node Int)
;;


(define-datatype red-blue-tree red-blue-tree?
  (leaf-node (num integer?))
  (red-node (left red-blue-tree?)
            (right red-blue-tree?))
  (blue-node (nodes (list-of red-blue-tree?))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define distance-tree-num
  (λ (number rb-tree)
    (cases red-blue-tree rb-tree
           (leaf-node (num) (leaf-node number))
           (red-node (left right)
                     (red-node (distance-tree-num (+ number 1) left)
                               (distance-tree-num (+ number 1) right)))
           (blue-node (nodes)
                      (blue-node
                       (map (λ (node)
                              (distance-tree-num number node)) nodes))))))

(define distance-tree
  (λ (rb-tree) (distance-tree-num 0 rb-tree)))



