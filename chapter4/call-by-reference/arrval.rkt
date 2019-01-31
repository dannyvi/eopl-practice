(module arrval (lib "eopl.ss" "eopl")
  (require "drscheme-init.scm")
  (require "store.scm")

  (provide (all-defined-out))

  ;;

  (define-datatype array array?
    (an-array (locs (list-of reference?)))
    )

  (define make-array
    (lambda (vals)
      (an-array (map newref vals))))

  (define array-ref
    (lambda (arr num)
      (cases array arr
             (an-array (refs) (deref (list-ref refs num))))))

  (define array-set
    (lambda (arr num val)
      (cases array arr
             (an-array (refs) (setref! (list-ref refs num)  val)))))

  (define array-length
    (lambda (arr)
      (cases array arr
             (an-array (refs) (length refs)))))

  )
