
;; Bintree ::= () | (Number Parent Son Son)
;; Parent  ::= () | (Parent Number-or-son Number-or-son)
;; Son     ::= () | (Number Son Son)
;; Number-or-son    ::= Number | Son

(define report-number-sequence-error
  (lambda (proc msg number-seq)
    (eopl:error proc msg number-seq)))

(define number->bintree
  (lambda (number)
    (list number '() '() '())))

(define current-element (lambda (bin-tree) (car bin-tree)))

(define at-left-end?
  (lambda (bin-tree) (null? (caddr bin-tree))))

(define at-right-end?
  (lambda (bin-tree) (null? (cadddr bin-tree))))

(define move-to-left-son
  (lambda (bin-tree)
    (let ((current (caddr bin-tree)))
      (if (at-left-end? bin-tree)
          (report-number-sequence-error
           'move-to-left-son
           "Null lson ~s !"
           bin-tree)
          (let* ((right (cadddr bin-tree))
                 (parent (list (cadr bin-tree)
                               (current-element bin-tree)
                               right)))
            (list (current-element current)
                  parent
                  (cadr current)
                  (caddr current)))))))

(define move-to-right-son
  (lambda (bin-tree)
    (let ((current (cadddr bin-tree)))
      (if (at-right-end? bin-tree)
          (report-number-sequence-error
           'move-to-right-son
           "Null rson ~s !"
           bin-tree)
          (let* ((left (caddr bin-tree))
                 (parent (list (cadr bin-tree)
                               left
                               (current-element bin-tree))))
            (list (current-element current)
                  parent
                  (cadr current)
                  (caddr current)))))))

(define insert-to-left-son
  (lambda (number bin-tree)
    (list (current-element bin-tree)
          (cadr bin-tree)
          (list number (caddr bin-tree) '())
          (cadddr bin-tree))))

(define insert-to-right-son
  (lambda (number bin-tree)
    (list (current-element bin-tree)
          (cadr bin-tree)
          (caddr bin-tree)
          (list number (cadddr bin-tree) '()))))

(define left-son (lambda (bin-tree) (caddr bin-tree)))
(define right-son (lambda (bin-tree) (cadddr bin-tree)))
(define at-root? (lambda (bin-tree) (null? (cadr bin-tree))))
(define at-leaf?
  (lambda (bin-tree)
    (or
     (null? (left-son bin-tree)
            (right-son bin-tree)))))

(define move-up
  (lambda (bin-tree)
    (if
     (at-root? bin-tree)
     (report-number-sequence-error 'move-up "At root of ~s !" bin-tree)
     (let* [(current (cadr bin-tree))
            (parent (car current))
            (son (list (current-element bin-tree)
                       (left-son bin-tree)
                       (right-son bin-tree)))]
       (if (number? (cadr current))
           (list (cadr current) parent son (caddr current))
           (list (caddr current) parent (cadr current) son))))))
