(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "helper.rkt")

  (provide value-of-program value-of)

  ;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (num-val
                       (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
                   (let ((val1 (value-of exp1 env)))
                     (let ((num1 (expval->num val1)))
                       (if (zero? num1)
                           (bool-val #t)
                           (bool-val #f)))))

        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
                (let ((val1 (value-of exp1 env)))
                  (if (expval->bool val1)
                      (value-of exp2 env)
                      (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        ;; ex 3.16
        (let-exp (vars0 exps0 body)
                 (define (loop vars exps env-accu)
                   (if (null? exps)
                       env-accu
                       (loop
                        (cdr vars)
                        (cdr exps)
                        (extend-env (car vars) (value-of (car exps) env) env-accu))))
                 (value-of body (loop vars0 exps0 env)))

        ;; ex 3.6
        (minus-exp (exp)
                   (num-val (- 0 (expval->num (value-of exp env)))))

        ;; ex 3.7
        (quotient-exp
         (e1 e2)
         (let ([v1 (value-of e1 env)]
               [v2 (value-of e2 env)])
           (let ([n1 (expval->num v1)]
                 [n2 (expval->num v2)])
             (num-val (quotient n1 n2)))))

        ;; ex 3.8
        (equal?-exp
         (e1 e2)
         (let ([v1 (value-of e1 env)]
               [v2 (value-of e2 env)])
           (let ([n1 (expval->num v1)]
                 [n2 (expval->num v2)])
             (bool-val (= n1 n2)))))

        ;; ex 3.9

        ;; I was wondering why car(cdr(cons(1, cons(2, emptylist))))
        ;; works, because cdr returns an expressed value yet car needs
        ;; an expression. Then I realized that the returned value is
        ;; not used by car but by expval->list instead.
        (emptylist-exp () (list-val '()))

        (cons-exp
         (head tail)
         (let ([head-val (value-of head env)]
               [tail-val (value-of tail env)])
           (list-val (cons head-val (expval->list tail-val)))))

        (car-exp
         (xs)
         (car (expval->list (value-of xs env))))

        (cdr-exp
         (xs)
         (list-val (cdr (expval->list (value-of xs env)))))

        (null?-exp
         (xs)
         (bool-val (null? (expval->list (value-of xs env)))))

        ;; ex 3.10
        (list-exp
         (xs)
         (list-val (map (lambda (x) (value-of x env)) xs)))

        ;; ex 3.12
        (cond-exp
         (predicates consequents)
         (define (loop ps cs)
           (cond [(null? ps) (eopl:error "Uh oh")]
                 [(expval->bool (value-of (car ps) env))
                  (value-of (car cs) env)]
                 [else  (loop (cdr ps) (cdr cs))]))
         (loop predicates consequents))

        ;; ex 3.15
        (print-exp
         (exp1)
         (display (value-of exp1 env))
         (num-val 1))

        ;; ex 3.17
        (let*-exp
         (vars0 exps0 body)

         (define (loop vars exps env-accu)
           (if (null? exps)
               env-accu
               (loop
                (cdr vars)
                (cdr exps)
                (extend-env (car vars) (value-of (car exps) env-accu) env-accu))))

         (value-of body (loop vars0 exps0 env)))

        ;; ex 3.18
        (unpack-exp
         (vars0 exp body)

         (define (loop vars lst env-accu)
           (if (null? vars)
               env-accu
               (loop
                (cdr vars)
                (cdr lst)
                (extend-env (car vars) (car lst) env-accu))))

         (let ([lst (expval->list (value-of exp env))])
           (cond [(not (list? lst)) (eopl:error "Not a list")]
                 [(not (= (length vars0) (length lst))) (eopl:error "Arity mismatch")]
                 [else (value-of body (loop vars0 lst env))])))

        )))
  )
