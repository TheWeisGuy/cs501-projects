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

(define (eval (e : Expr)) : Value
  (error 'eval "Not yet implemented.")
  )
