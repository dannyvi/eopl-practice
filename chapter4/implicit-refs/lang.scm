(module lang (lib "eopl.ss" "eopl")

  ;; language for IMPLICIT-REFS

  (require "drscheme-init.scm")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '((program (statement) a-program)

      (statement (identifier "=" expression) assign-stmt)
      (statement
       ("if" expression  statement statement)
       if-stmt)

      (statement
       ("{" (separated-list statement ";" ) "}")
       block-stmt)

      (statement
       ("print" expression)
       print-stmt)

      (statement
       ("while" expression statement)
       while-stmt)

      (statement
       ("var" (separated-list identifier "," ) ";" statement)
       decl-stmt)

      (statement
       ("read" identifier)
       read-stmt)

      (expression (number) const-exp)

      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)

      (expression
       ("+" "(" expression "," expression ")")
       add-exp)

      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("not" "(" expression ")")
       not-exp)

      (expression (identifier) var-exp)

      (expression
       ("letimmut" (arbno identifier "=" expression) "in" expression)
       let-exp)

      (expression
       ("proc" "(" (arbno identifier) ")" expression)
       proc-exp)

      (expression
       ("(" expression (arbno expression) ")")
       call-exp)

      (expression
        ("letrec"
          (arbno identifier "(" (arbno identifier) ")" "=" expression)
           "in" expression)
        letrec-exp)

      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      ;; new for implicit-refs

      (expression
        ("set" identifier "=" expression)
        assign-exp)

      (expression
       ("setdynamic" identifier "=" expression "during" expression)
       setdynamic-exp)

      (expression
       ("let" (arbno identifier "=" expression) "in" expression)
       letmutable-exp)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

  (sllgen:make-define-datatypes the-lexical-spec the-grammar)

  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))

  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

  )
