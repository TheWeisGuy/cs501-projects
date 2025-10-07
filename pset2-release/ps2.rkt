#lang plai-typed

(require "ps2-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;
;; See ps2-ast.rkt and README.md for more information.

;;

;parser
(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s)  (valC (numV (s-exp->number s)))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-boolean? s)(valC(boolV(s-exp->boolean s)))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (case (s-exp->symbol (first l))
         [(+) (plusC (parse (second l)) (parse (third l)))]
         [(*) (timesC (parse (second l)) (parse (third l)))]
         [(natrec) (let* ([e1 (parse (second l))]
                          [e2 (parse (third l))]
                          [rec-spec (s-exp->list (fourth l))]
                          [x (s-exp->symbol (first rec-spec))]
                          [y (s-exp->symbol (second rec-spec))]
                          [e3 (parse (third rec-spec))])
                     (natrecC e1 e2 x y e3))]
         [(equal?)(equal?C (parse (second l))(parse (third l)))]
         [(if)(ifC (parse (second l))(parse(third l))(parse(fourth l)))]
         [(list)(listC (map parse (rest l)))]
         [(cons)(consC (parse (second l))(parse (third l)))]
         [(first)(firstC (parse (second l)))]
         [(rest)(restC (parse (second l)))]
         [(listrec)(let* ([e1 (parse (second l))]
                          [e2 (parse (third l))]
                          [rec-spec (s-exp->list (fourth l))] ; '(x y e3)
                          [hd (s-exp->symbol (first rec-spec))]
                          [rest (s-exp->symbol (second rec-spec))]
                          [res (s-exp->symbol (third rec-spec))]
                          [e3 (parse (fourth rec-spec))])
                     (listrecC e1 e2 hd rest res e3))]
         [(let)
          (let* ([bindings (s-exp->list (second l))]
                 [parsed-bindings
                  (map (lambda (b)
                         (let ([pair-list (s-exp->list b)])
                           (pair (s-exp->symbol (first pair-list))
                                 (parse (second pair-list)))))
                       bindings)])
            (letC parsed-bindings (parse (third l))))]
         
         [(let*)
          (let* ([bindings (s-exp->list (second l))]
                 [parsed-bindings
                  (map (lambda (b)
                         (let ([pair-list (s-exp->list b)])
                           (pair (s-exp->symbol (first pair-list))
                                 (parse (second pair-list)))))
                       bindings)])
            (let*C parsed-bindings (parse (third l))))]
         [(unpack) 
          (let* ([vars-list (s-exp->list (second l))]
                 [vars (map s-exp->symbol vars-list)]
                 [e1   (parse (third l))]
                 [e2   (parse (fourth l))])
            (unpackC vars e1 e2))]
         )
       )]
    )
  )

(define (num-val v)
  (type-case Value v
    [numV (n) n]
    [else (error 'num-val "expected numeric value")]))


(define empty-env empty)
(define extend-env cons)

;lookup method modified slightly from class to avoid type issues
;used chatgpt for debugging many of the recursive methods

(define (lookup (x : symbol) (env : (listof (symbol * Value)))) : Value
  (cond
    [(cons? env)
     (let* ([binding (first env)]   
            [key (fst binding)]    
            [val (snd binding)])  
       (if (equal? key x)
           val
           (lookup x (rest env))))]
    [else (error 'lookup "No binding found")]))

;boolean type
(define (bool-val v)
  (type-case Value v
    [boolV (b) b]
    [else (error 'bool-val "Expected boolean value")]))

;needed to define a new pointer for racket rest for use in listrecC
(define racket-rest rest)

;A wrapper that supplies empty-env if no env is provided
(define (eval (e : Expr)) : Value
  (eval-internal e empty-env))

;main eval function
(define (eval-internal (e : Expr) (env : (listof(symbol * Value)))) : Value
  (type-case Expr e
    [valC (n) n]
    [plusC (e1 e2) (numV (+ (numV-n (eval-internal e1 env)) (numV-n (eval-internal e2 env))))]

    [timesC (e1 e2)
            (numV (* (num-val (eval-internal e1 env))
                     (num-val (eval-internal e2 env))))]

    [idC (x) (lookup x env)]
    
    [natrecC (e1 e2 x y e3)
             (let ([n (num-val (eval-internal e1 env))])   
               (if (= n 0)
                   (eval-internal e2 env)                 
                   (let* ([recVal (eval-internal (natrecC (valC (numV (- n 1))) e2 x y e3) env)]
                          [new-env (cons (pair x (numV (- n 1)))
                                         (cons (pair y recVal) env))])
                     (eval-internal e3 new-env))))]
             
    [equal?C (e1 e2)
             (boolV (equal? (eval-internal e1 env) (eval-internal e2 env)))]
    [ifC (guard e1 e2)
         (if (bool-val (eval-internal guard env))
             (eval-internal e1 env)
             (eval-internal e2 env))]
    [listC (es)
           (listV (map (lambda (e) (eval-internal e env)) es))]
    [consC (e1 e2)
           (let ([v1 (eval-internal e1 env)]
                 [v2 (eval-internal e2 env)])
             (type-case Value v2
               [listV (vs) (listV (cons v1 vs))]
               [else (error 'eval "cons expects second argument to be a list")]))]
    [firstC (e)
            (let ([v (eval-internal e env)])
              (type-case Value v
                [listV (vs)
                       (if (empty? vs)
                           (error 'eval "first called on empty list")
                           (first vs))]
                [else (error 'eval "first expects a list")]))]
    [restC (e)
           (let ([v (eval-internal e env)])
             (type-case Value v
               [listV (vs)
                      (if (empty? vs)
                          (error 'eval "rest called on empty list")
                          (listV (rest vs)))]
               [else (error 'eval "rest expects a list")]))]

    [listrecC (e1 e2 hd rest res e3)
              (let ([v (eval-internal e1 env)])
                (type-case Value v
                  [listV (vs)
                         (if (empty? vs)
                             (eval-internal e2 env)
                             (let* ([hdVal (first vs)]
                                    [restVal (listV (racket-rest vs))]
                                    [recVal (eval-internal (listrecC (listC (map valC (racket-rest vs)))
                                                                     e2 hd rest res e3)
                                                           env)]
                                    [new-env (cons (pair hd hdVal)
                                                   (cons (pair rest restVal)
                                                         (cons (pair res recVal) env)))])
                               (eval-internal e3 new-env)))]
                  [else (error 'eval "listrec expects a list")]))]

    [letC (bindings body)
          (let ([extend-env
                 (append
                  (map (lambda (b)
                         (let ([x (fst b)]
                               [rhs (snd b)])
                           (pair x (eval-internal rhs env)))) ; evaluate RHS
                       bindings)
                  env)])
            (eval-internal body extend-env))]
    
    [let*C (bindings body)
           (letrec ([extend-env-seq
                     (lambda (bs env)
                       (if (empty? bs)
                           env
                           (let* ([b (first bs)]
                                  [x (fst b)]
                                  [rhs (snd b)]
                                  [v (eval-internal rhs env)]
                                  [env-extend (cons (pair x v) env)])
                             (extend-env-seq (rest bs) env-extend))))])
             (let ([new-env (extend-env-seq bindings env)])
               (eval-internal body new-env)))]
    
    [unpackC (vars e1 e2)
             (let ([v (eval-internal e1 env)])
               (type-case Value v
                 [listV (vs)
                        (if (= (length vs) (length vars))
                            (let ([new-env
                                   (append
                                    (map (lambda (i)
                                           (pair (list-ref vars i) (list-ref vs i)))
                                         (build-list (length vars) identity))
                                    env)])
                              (eval-internal e2 new-env))
                            (error 'eval "unpack: wrong number of values"))]
                 [else (error 'eval "unpack expects a list")]))]
    )
  
  )
