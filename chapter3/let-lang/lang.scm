(module lang

  ;; grammar for the LET language

  (lib "eopl.ss" "eopl")

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
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
       ("-" "(" expression "," expression ")")
       diff-exp)

      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" (arbno identifier "=" expression) "in" expression)
       let-exp)

      ;; ex 3.6
      (expression
       ("minus" "(" expression ")")
       minus-exp)

      ;; ex 3.7
      (expression
       ("quotient" "(" expression "," expression ")")
       quotient-exp)

      ;; ex 3.8
      (expression
       ("equal?" "(" expression "," expression ")")
       equal?-exp)

      ;; ex 3.9
      (expression ("emptylist") emptylist-exp)

      (expression
       ("cons" "(" expression "," expression ")")
       cons-exp)

      (expression
       ("car" "(" expression ")")
       car-exp)

      (expression
       ("cdr" "(" expression ")")
       cdr-exp)

      (expression
       ("null?" "(" expression ")")
       null?-exp)

      ;; ex 3.10
      (expression
       ("list" "(" (separated-list expression ",") ")")
       list-exp)

      ;; ex 3.12
      (expression
       ("cond" (arbno expression "==>" expression) "end")
       cond-exp)

      ;; ex 3.15

      ;; It's not expressible because our language of rules so far doesn't
      ;; have a way to address side effects like printing.
      (expression
       ("print" "(" expression ")")
       print-exp)

      ;; ex 3.17
      (expression
       ("let*" (arbno identifier "=" expression) "in" expression)
       let*-exp)

      ;; ex 3.18
      (expression
       ("unpack" (arbno identifier) "=" expression "in" expression)
       unpack-exp)

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
