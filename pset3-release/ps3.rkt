#lang plai-typed
(require "ps3-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps3-ast.rkt and README.md for more information.

;; Note that as in lecture 6, you probably want to implement a version
;; of eval that returns a result that can be an arbitrary value (not just
;; a BaseValue) and also returns a store.  Your eval-base would then be a
;; wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be.
;;
;; For grading, the test cases all result in values that can be converted to base values.

(define (vector-copy (v : (vectorof Value))) : (vectorof Value)
  (let ([len (vector-length v)]
        [newv (make-vector (vector-length v) (numV 0))])
    (local [(define (copy (i : number)) : (vectorof Value)
              (if (= i len)
                  newv
                  (begin
                    (vector-set! newv i (vector-ref v i))
                    (copy (+ i 1)))))]
      (copy 0))))

(define (override-store (c : Storage) (sto : Store)) : Store
  (cond
    [(empty? sto)
     (list c)]
    [(= (cell-location (first sto)) (cell-location c))
     (cons c (rest sto))]
    [else
     (cons (first sto)
           (override-store c (rest sto)))]))


(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-boolean? s)
     (boolC (s-exp->boolean s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(box) (boxC (parse (second l)))]
            [(set-box!) (setboxC (parse (second l)) (parse (third l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(begin) (beginC (map parse (rest l)))]
            [(pair)
             (pairC (parse (second l)) (parse (third l)))]
            [(vector)
             (vectorC (map parse (rest l)))]
            [(vector-make)
             (vector-makeC (parse (second l)) (parse (third l)))]

            [(if)
             (ifC (parse (second l))
                  (parse (third l))
                  (parse (fourth l)))]
            [(equal?)
             (equal?C (parse (second l)) (parse (third l)))]
            [(fst) 
             (fstC (parse (second l)))]
            [(snd) 
             (sndC (parse (second l)))]
            [(vector-length)
             (vector-lengthC (parse (second l)))]
            [(vector-ref)
             (vector-refC (parse (second l)) (parse (third l)))]
            [(vector-set!)
             (vector-set!C (parse (second l))
                           (parse (third l))
                           (parse (fourth l)))]
            [(subvector)
             (subvectorC (parse (second l))
                         (parse (third l))
                         (parse (fourth l)))]
           
            [(transact)
             (transactC (parse (second l)))]

            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
         ))]
    ))
(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [pairV (v1 : Value) (v2 : Value)]
  [closV (env : Env) (x : symbol) (body : Expr)]
  [boxV (l : Location)]
  [vectorV (vec : (vectorof Value))]
  [subvectorV (base : Location) (offset : number) (len : number)])


(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define-type-alias Location number)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define (fetch (l : Location) (sto : Store)) : Value
  (cond
    [(cons? sto)
     (if (equal? (cell-location (first sto)) l)
         (cell-val (first sto))
         (fetch l (rest sto)))]
    [else (error 'fetch "No location found")]))

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))


(define-type-alias Store (listof Storage))
(define-type (Result 'a)
  [res (v : 'a) (s : Store)])

(define-type-alias (Stateful 'a)
  (Store -> (Result 'a)))

(define (mret (v : 'a)) : (Stateful 'a)
  (lambda (s) (res v s)))

(define (mbind (m : (Stateful 'a)) (f : ('a -> (Stateful 'b)))) : (Stateful 'b)
  (lambda (s1) 
    (type-case (Result 'a) (m s1)
      [res (v s2) ((f v) s2)])))

(define (mseq (m1 : (Stateful 'a)) (m2 : (Stateful 'b))) : (Stateful 'b)
  (mbind m1
         (lambda (v) m2)))

(define (moverride (l : Location) (v : Value)) : (Stateful Value)
  (lambda (s)
    (res v (override-store (cell l v) s))))

(define (mfetch (l : Location)) : (Stateful Value)
  (lambda (s)
    (res (fetch l s) s)))

(define new-loc
  (let ([counter (box 0)])
    (lambda () 
      (let ([l (unbox counter)])
        (begin (set-box! counter (+ 1 l))
               l)))))

(define (eval-list (env : Env) (es : (listof Expr))) : (Stateful (listof Value))
  (cond
    [(empty? es) (mret empty)]
    [else
     (mbind (eval-env env (first es))
            (lambda (v1)
              (mbind (eval-list env (rest es))
                     (lambda (vs)
                       (mret (cons v1 vs))))))]))

(define (eval-seq (env : Env) (es : (listof Expr))) : (Stateful Value)
  (cond
    [(empty? es)
     (error 'eval "begin: empty sequence")]
    [(empty? (rest es))
     (eval-env env (first es))]
    [else
     (mseq (eval-env env (first es))
           (eval-seq env (rest es)))]))


(define (make-value-vector (len : number)) : (vectorof Value)
  (make-vector len (numV 0)))  

(define (fill-vector (v : (vectorof Value))
                     (i : number)
                     (lst : (listof Value))) : (vectorof Value)
  (if (empty? lst)
      v
      (begin
        (vector-set! v i (first lst))
        (fill-vector v (+ i 1) (rest lst)))))

(define (list->vector (vs : (listof Value))) : (vectorof Value)
  (let ([v (make-value-vector (length vs))])
    (fill-vector v 0 vs)))


(define (is-equal? (v1 : Value) (v2 : Value)) : boolean
  (type-case Value v1
    [numV (n1)
          (type-case Value v2
            [numV (n2) (= n1 n2)]
            [else #f])]
    [boolV (b1)
           (type-case Value v2
             [boolV (b2) (equal? b1 b2)]
             [else #f])]
    [pairV (a1 a2)
           (type-case Value v2
             [pairV (b1 b2)
                    (and (is-equal? a1 b1)
                         (is-equal? a2 b2))]
             [else #f])]
    [else(error 'eval "invalid value given")]
    ))

(define (subvector (v : (vectorof Value)) (start : number) (end : number)) : (vectorof Value)
  (let* ([len (- end start)]
         [new (make-vector len (numV 0))]) 
    (local [(define (copy (i : number)) : (vectorof Value)
              (if (= i len)
                  new
                  (begin
                    (vector-set! new i (vector-ref v (+ start i)))
                    (copy (+ i 1)))))]
      (copy 0))))

(define (eval-env (env : Env) (e : Expr)) : (Stateful Value)
  (type-case Expr e
    [numC (n) (mret (numV n))]
    [boolC (n) (mret(boolV n))]
    [lambdaC (x body)
             (mret (closV env x body))]

    [plusC (e1 e2)
           (mbind (eval-env env e1)
                  (lambda (v1)
                    (mbind (eval-env env e2)
                           (lambda (v2)
                             (mret (numV (+ (numV-n v1) (numV-n v2))))))))]
    [timesC (e1 e2)
            (mbind (eval-env env e1)
                   (lambda (v1)
                     (mbind (eval-env env e2)
                            (lambda (v2)
                              (mret (numV (* (numV-n v1) (numV-n v2))))))))]
    [letC (x e1 e2)
          (mbind (eval-env env e1)
                 (lambda (v1)
                   (eval-env (extend-env (bind x v1) env) e2)))]

    [appC (e1 e2)
          (mbind (eval-env env e1)
                 (lambda (v1)
                   (mbind (eval-env env e2)
                          (lambda (v2)
                            (eval-env
                             (extend-env (bind (closV-x v1) v2)
                                         (closV-env v1))
                             (closV-body v1))))))]
   
    [idC (x)   
         (mret (lookup x env))]
    [boxC (a) (mbind (eval-env env a)
                     (lambda (v)
                       (let [(l (new-loc))]
                         (mseq (moverride l v)
                               (mret (boxV l))))))]
    [unboxC (a) (mbind (eval-env env a)
                       (lambda (v)
                         (mfetch (boxV-l v))))]
    [setboxC (e1 e2)
             (mbind (eval-env env e1)
                    (lambda (v1)
                      (mbind (eval-env env e2)
                             (lambda (v2)
                               (moverride (boxV-l v1) v2)))))]
    [pairC (e1 e2)
           (mbind (eval-env env e1)
                  (lambda (v1)
                    (mbind (eval-env env e2)
                           (lambda (v2)
                             (mret (pairV v1 v2))))))]

    [fstC (e1)
          (mbind (eval-env env e1)
                 (lambda (v)
                   (type-case Value v
                     [pairV (v1 v2) (mret v1)]
                     [else (error 'eval "fst expects a pair")])))]
    [sndC (e1)
          (mbind (eval-env env e1)
                 (lambda (v)
                   (type-case Value v
                     [pairV (v1 v2) (mret v2)]
                     [else (error 'eval "snd expects a pair")])))]
    
    [equal?C (e1 e2)
             (mbind (eval-env env e1)
                    (lambda (v1)
                      (mbind (eval-env env e2)
                             (lambda (v2)
                               (mret (boolV (is-equal? v1 v2)))))))]
    
    [ifC (guard e1 e2)
         (mbind (eval-env env guard)
                (lambda (v)
                  (type-case Value v
                    [boolV (b)
                           (if b
                               (eval-env env e1)
                               (eval-env env e2))]
                    [else (error 'eval "if: guard must evaluate to a boolean")])))]
    [vectorC (es)
             (mbind (eval-list env es)
                    (lambda (vs)
                      (let ([v (list->vector vs)]
                            [l (new-loc)])
                        (mseq (moverride l (vectorV v))
                              (mret (boxV l))))))]

    [vector-lengthC (e1)
                    (mbind (eval-env env e1)
                           (lambda (v)
                             (type-case Value v
                               [boxV (loc)
                                     (mbind (mfetch loc)
                                            (lambda (inner)
                                              (type-case Value inner
                                                [vectorV (vec)      (mret (numV (vector-length vec)))]
                                                [subvectorV (base off len) (mret (numV len))]
                                                [else (error 'eval "vector-length expects a vector")])))]
                               [vectorV (vec)                  (mret (numV (vector-length vec)))]
                               [subvectorV (base off len)      (mret (numV len))]
                               [else (error 'eval "vector-length expects a vector")])))]


    [vector-refC (e1 e2)
                 (mbind (eval-env env e1)
                        (lambda (v1)
                          (type-case Value v1
                            [boxV (loc)
                                  (mbind (mfetch loc)
                                         (lambda (inner)
                                           (type-case Value inner
                                             [vectorV (vec)
                                                      (mbind (eval-env env e2)
                                                             (lambda (v2)
                                                               (type-case Value v2
                                                                 [numV (n)
                                                                       (if (and (<= 0 n) (< n (vector-length vec)))
                                                                           (mret (vector-ref vec n))
                                                                           (error 'eval "vector-ref: index out of range"))]
                                                                 [else (error 'eval "vector-ref: expected numeric index")])))]
                                             [subvectorV (base off len)
                                                         (mbind (eval-env env e2)
                                                                (lambda (v2)
                                                                  (type-case Value v2
                                                                    [numV (n)
                                                                          (if (and (<= 0 n) (< n len))
                                                                              (mbind (mfetch base)
                                                                                     (lambda (binner)
                                                                                       (type-case Value binner
                                                                                         [vectorV (bvec)
                                                                                                  (mret (vector-ref bvec (+ off n)))]
                                                                                         [else (error 'eval "vector-ref: corrupted base vector")])))
                                                                              (error 'eval "vector-ref: index out of range"))]
                                                                    [else (error 'eval "vector-ref: expected numeric index")])))]
                                             [else (error 'eval "vector-ref: expected vector")])))]
                            [else (error 'eval "vector-ref: expected box (vector)")]))) ]

    [vector-set!C (e1 e2 e3)
                  (mbind (eval-env env e1)
                         (lambda (v1)
                           (type-case Value v1
                             [boxV (loc)
                                   (mbind (mfetch loc)
                                          (lambda (inner)
                                            (type-case Value inner
                                              [vectorV (vec)
                                                       (mbind (eval-env env e2)
                                                              (lambda (v2)
                                                                (type-case Value v2
                                                                  [numV (n)
                                                                        (if (and (<= 0 n) (< n (vector-length vec)))
                                                                            (mbind (eval-env env e3)
                                                                                   (lambda (v3)
                                                                                     (let ([newv (vector-copy vec)])
                                                                                       (begin
                                                                                         (vector-set! newv n v3)
                                                                                         (mseq (moverride loc (vectorV newv))
                                                                                               (mret v3))))))
                                                                            (error 'eval "vector-set!: index out of range"))]
                                                                  [else (error 'eval "vector-set!: expected numeric index")])))]
                       
                                              [subvectorV (base off len)
                                                          (mbind (eval-env env e2)
                                                                 (lambda (v2)
                                                                   (type-case Value v2
                                                                     [numV (n)
                                                                           (if (and (<= 0 n) (< n len))
                                                                               (mbind (eval-env env e3)
                                                                                      (lambda (v3)
                                                                                        (mbind (mfetch base)
                                                                                               (lambda (binner)
                                                                                                 (type-case Value binner
                                                                                                   [vectorV (bvec)
                                                                                                            (let ([newv (vector-copy bvec)])
                                                                                                              (begin
                                                                                                                (vector-set! newv (+ off n) v3)
                                                                                                                (mseq (moverride base (vectorV newv))
                                                                                                                      (mret v3))))]
                                                                                                   [else (error 'eval "vector-set!: corrupted base vector")])))))
                                                                               (error 'eval "vector-set!: index out of range"))]

                                                                     [else (error 'eval "vector-set!: expected numeric index")])))]
                                              [else (error 'eval "vector-set!: expected vector")])))]
                             [else (error 'eval "vector-set!: expected box (vector)")]))) ]

    [vector-makeC (e1 e2)
                  (mbind (eval-env env e1)
                         (lambda (v1)
                           (type-case Value v1
                             [numV (n)
                                   (if (>= n 0)
                                       (mbind (eval-env env e2)
                                              (lambda (v2)
                                                (let ([v (make-vector n v2)]
                                                      [l (new-loc)])
                                                  (mseq (moverride l (vectorV v))
                                                        (mret (boxV l))))))
                                       (error 'eval "vector-make: expected nonnegative length"))]
                             [else (error 'eval "vector-make: expected numeric length")])))]



    [subvectorC (e offset len)
                (mbind (eval-env env e)
                       (lambda (v1)
                         (type-case Value v1
                           [boxV (loc)
                                 (mbind (mfetch loc)
                                        (lambda (inner)
                                          (type-case Value inner
                                            [vectorV (vec)
                                                     (mbind (eval-env env offset)
                                                            (lambda (v2)
                                                              (type-case Value v2
                                                                [numV (n)
                                                                      (mbind (eval-env env len)
                                                                             (lambda (v3)
                                                                               (type-case Value v3
                                                                                 [numV (l)
                                                                                       (if (and (>= n 0)
                                                                                                (>= l 0)
                                                                                                (<= (+ n l) (vector-length vec)))
                                                                                           (let ([l2 (new-loc)])
                                                                                             (mseq (moverride l2 (subvectorV loc n l))
                                                                                                   (mret (boxV l2))))
                                                                                           (error 'eval "subvector: invalid offset/length"))]
                                                                                 [else (error 'eval "subvector: expected numeric len")])))]
                                                                [else (error 'eval "subvector: expected numeric offset")])))]
                                            [else (error 'eval "subvector: expected vector")])))]
                           [else (error 'eval "subvector: expected vector")])))]

    [beginC (es)
            (eval-seq env es)]

    [transactC (e)
               (let ([saved-env env])                  
                 (lambda (s0)
                   (type-case (Result Value) ((eval-env saved-env e) s0)
                     [res (v1 s1)
                          (type-case Value v1
                            [pairV (vbool vval)
                                   (type-case Value vbool
                                     [boolV (b)
                                            (if b
                                                (res vval s1)          
                                                (res vval s0))]       
                                     [else (error 'eval "transact: first component must be a boolean")])]
                            [else (error 'eval "transact: expected a pair")])])))]

    ))

(define (value->base (v : Value)) : BaseValue
  (type-case Value v
    [numV (n) (numBV n)]
    [boolV (b) (boolBV b)]
    [pairV (v1 v2)
           (pairBV (value->base v1)
                   (value->base v2))]
    [else (error 'eval-base "Cannot convert non-base value to BaseValue")]))


(define (eval-base (e : Expr)) : BaseValue
  (type-case (Result Value) ((eval-env empty-env e) empty)
    [res (v s)
         (begin
           (value->base v))]))

