#lang plai-typed
(require "ps5-ast.rkt")

(define (parse-ty (s : s-expression)) : Type
  (cond
    [(s-exp-symbol? s)
     (case (s-exp->symbol s)
       [(boolT) (boolT)]
       [(voidT) (voidT)]
       [(numT) (numT)])]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(funT) (funT (parse-ty (second l)) (parse-ty (third l)))]
            [(pairT) (pairT (parse-ty (second l)) (parse-ty (third l)))]
            [(boxT) (boxT (parse-ty (second l)))]
            [(listT) (listT (parse-ty (second l)))]
            )]))]))

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(pair) (pairC (parse (second l)) (parse (third l)))]
            [(equal?) (equal?C (parse (second l)) (parse (third l)))]
            [(cons) (consC (parse (second l)) (parse (third l)))]
            [(is-empty?) (is-empty?C (parse (second l)))]
            [(empty) (emptyC (parse-ty (second l)))]
            [(first) (firstC (parse (second l)))]
            [(rest) (restC (parse (second l)))]
            [(fst) (fstC (parse (second l)))]
            [(snd) (sndC (parse (second l)))]
            [(box) (boxC (parse (second l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(set-box!) (set-box!C (parse (second l)) (parse (third l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse-ty (third l)) (parse (fourth l)))]
            [(rec) (recC (s-exp->symbol (second l)) (s-exp->symbol (third l)) (parse-ty (fourth l))
                         (parse-ty (list-ref l 4)) (parse (list-ref l 5)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
       ))]
    ))

(define (type-equal? (t1 : Type) (t2 : Type)) : boolean
  (equal? t1 t2))


(define-type (Binding 'a)
  [bind (name : symbol) (val : 'a)])

(define-type-alias TyEnv (listof (Binding Type)))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : (listof (Binding 'a)))) : 'a
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))


; TODO: you must implement this.
; It if e has type t under environment env, then
; (tc-env env e) should return t.
; Otherwise, if e is not well-typed (i.e. does not type check), tc-env should raise an exception
; of some form using the 'error' construct in plai-typed.

(define (tc-env (env : TyEnv) (e : Expr)) : Type
[type-case Expr e
[numC (n)
  (numT)]
  
[voidC ()
  (voidT)]
  
[boolC (b)
  (boolT)]

  [pairC (e1 e2)
  (pairT (tc-env env e1)
         (tc-env env e2))]

[fstC (e1)
  (let ([t (tc-env env e1)])
    (type-case Type t
      [pairT (t1 t2) t1]
      [else (error 'tc-env "fst applied to non-pair")]))]

  [sndC (e1)
  (let ([t (tc-env env e1)])
    (type-case Type t
      [pairT (t1 t2) t2]
      [else (error 'tc-env "snd applied to non-pair")]))]
  
[plusC (e1 e2)
  (let ([t1 (tc-env env e1)]
        [t2 (tc-env env e2)])
    (if (and (type-equal? t1 (numT))
             (type-equal? t2 (numT)))
        (numT)
        (error 'tc-env "+ expects numT numT")))]

[timesC (e1 e2)
  (let ([t1 (tc-env env e1)]
        [t2 (tc-env env e2)])
    (if (and (type-equal? t1 (numT))
             (type-equal? t2 (numT)))
        (numT)
        (error 'tc-env "* expects numT numT")))]

  [equal?C (e1 e2)
  (let ([t1 (tc-env env e1)]
        [t2 (tc-env env e2)])
    (if (type-equal? t1 t2)
        (boolT)
        (error 'tc-env "equal? arguments must have same type")))]

  [letC (x e1 e2)
  (let* ([t1 (tc-env env e1)]
         [env2 (extend-env (bind x t1) env)])
    (tc-env env2 e2))]

  [lambdaC (x argT body)
  (let* ([env2 (extend-env (bind x argT) env)]
         [retT (tc-env env2 body)])
    (funT argT retT))]

  [appC (e1 e2)
  (let ([t1 (tc-env env e1)]
        [t2 (tc-env env e2)])
    (type-case Type t1
      [funT (argT retT)
        (if (type-equal? argT t2)
            retT
            (error 'tc-env "function argument type mismatch"))]
      [else (error 'tc-env "application of non-function")]))]

  [idC (x)
  (lookup x env)]


  [ifC (test thenE elseE)
  (let ([t-test (tc-env env test)])
    (if (type-equal? t-test (boolT))
        (let ([t-then (tc-env env thenE)]
              [t-else (tc-env env elseE)])
          (if (type-equal? t-then t-else)
              t-then
              (error 'tc-env "branches of if must have same type")))
        (error 'tc-env "if condition must be boolT")))]


  [emptyC (t)
  (listT t)]

  [consC (e1 e2)
  (let ([t1 (tc-env env e1)]
        [t2 (tc-env env e2)])
    (type-case Type t2
      [listT (elemT)
        (if (type-equal? t1 elemT)
            (listT elemT)
            (error 'tc-env "cons element type mismatch"))]
      [else (error 'tc-env "cons second argument must be a list")]))]


  [firstC (e1)
  (let ([t (tc-env env e1)])
    (type-case Type t
      [listT (elemT) elemT]
      [else (error 'tc-env "first expects a list")]))]


  [restC (e1)
  (let ([t (tc-env env e1)])
    (type-case Type t
      [listT (elemT) (listT elemT)]
      [else (error 'tc-env "rest expects a list")]))]


  [is-empty?C (e1)
  (let ([t (tc-env env e1)])
    (type-case Type t
      [listT (elemT) (boolT)]
      [else (error 'tc-env "is-empty? expects a list")]))]

  [recC (f x argT retT body)
  (let* ([funTy (funT argT retT)]
         [env2  (extend-env (bind f funTy)
                            (extend-env (bind x argT) env))]
         [bodyT (tc-env env2 body)])
    (if (type-equal? bodyT retT)
        funTy
        (error 'tc-env "recursive function body type does not match retT")))]

  [boxC (e1)
  (boxT (tc-env env e1))]

  [unboxC (e1)
  (let ([t (tc-env env e1)])
    (type-case Type t
      [boxT (innerT) innerT]
      [else (error 'tc-env "unbox expects a box")]))]

[set-box!C (e1 e2)
  (let ([t1 (tc-env env e1)]
        [t2 (tc-env env e2)])
    (type-case Type t1
      [boxT (innerT)
        (if (type-equal? innerT t2)
            (voidT)
            (error 'tc-env "set-box! value type mismatch"))]
      [else (error 'tc-env "set-box! first argument must be a box")]))]

  

  ]

  

  

  )

(define (tc (e : Expr))
  (tc-env empty-env e))

