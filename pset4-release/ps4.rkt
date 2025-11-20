#lang plai-typed
(require "ps4-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps4-ast.rkt and README.md for more information.

;; Note that as in the previous problem set you probably want to implement a version
;; of eval that can return more general values and takes an environment / store as arguments.
;; Your eval-base would then be a wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be converted.
;;
;; For grading, the test cases all result in values that can be converted to base values.





(define-type-alias Location number)
 
(define-type Storage
  [cell (location : Location) (val : Value)])
 
(define-type-alias Store (listof Storage))
(define empty-store empty)
(define override-store cons)



(define-type Value
  [numV (n : number)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [boxV (l : Location)]
  [boolV (b : boolean)]
  [objV (fields : (listof Field))
        (methods : (listof MethodDecl))
        (delegate : (optionof Value))]
  )

(define (parse-field f)
  (let* ([fname (s-exp->symbol (first f))]
         [fexpr (parse (second f))])
    (pair fname fexpr)))    

(define (parse-method m)
  (let* ([name (s-exp->symbol (first m))]
         [args (map s-exp->symbol
                    (s-exp->list (second m)))]
         [body (parse (third m))])
    (method-decl name args body)))


(define (parse s) : Expr
  (cond
    ;; number
    [(s-exp-number? s)
     (numC (s-exp->number s))]

    ;; symbol or boolean
    [(s-exp-symbol? s)
     (let ([sym (s-exp->symbol s)])
       (cond
         [(equal? sym 'true)  (boolC #t)]
         [(equal? sym 'false) (boolC #f)]
         [else (idC sym)]))]

    [(s-exp-boolean? s)
     (boolC (s-exp->boolean s))]
    ;; list form
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))

   
            [(+) (plusC (parse (second l))
                        (parse (third l)))]
            [(*) (timesC (parse (second l))
                         (parse (third l)))]
            [(lambda)
             (lambdaC (s-exp->symbol (second l))
                      (parse (third l)))]
            [(let)
             (letC (s-exp->symbol (second l))
                   (parse (third l))
                   (parse (fourth l)))]
            [(if)
             (ifC (parse (second l))
                  (parse (third l))
                  (parse (fourth l)))]

          
            [(true)  (boolC #t)]
            [(false) (boolC #f)]

            [(object)
             (let* ([raw-fields  (s-exp->list (second l))]
                    [raw-methods (s-exp->list (third  l))]  
                    [fields
                     (map (lambda (f-wrapper)
                            (let* ([f     (s-exp->list f-wrapper)]
                                   [fname (s-exp->symbol (first f))]
                                   [fexpr (parse (second f))])
                              (pair fname fexpr)))
                          raw-fields)]
                    [methods
                     (map (lambda (m-wrapper)
                            (let* ([m   (s-exp->list m-wrapper)]
                                   [name (s-exp->symbol (first m))]
                                   [arglist (s-exp->list (second m))]
                                   [arg-syms (map s-exp->symbol arglist)]
                                   [body (parse (third m))])
                              (method-decl name arg-syms body)))
                          raw-methods)]
                    )
               (objectC (none) fields methods))]



            [(begin)
             (beginC (map parse (rest l)))]




         
            [(object-del)
             (let* ([raw-delegate (second l)]
                    [raw-fields   (s-exp->list (third  l))]
                    [raw-methods  (s-exp->list (fourth l))]
                    [fields
                     (map (lambda (f)
                            (let* ([fl     (s-exp->list f)]
                                   [fname  (s-exp->symbol (first fl))]
                                   [fexpr  (second fl)])
                              (pair fname (parse fexpr))))
                          raw-fields)]
                    [methods
                     (map (lambda (m)
                            (let* ([ml      (s-exp->list m)]
                                   [name    (s-exp->symbol (first ml))]
                                   [arglist (s-exp->list (second ml))]
                                   [arg-syms (map s-exp->symbol arglist)]
                                   [body    (parse (third ml))])
                              (method-decl name arg-syms body)))
                          raw-methods)])
               (objectC (some (parse raw-delegate)) fields methods))]

          
            [(msg)
             (let* ([o-expr   (parse (first (rest l)))]
                    [m-name   (s-exp->symbol (first (rest (rest l))))]
                    [arg-sexps (rest (rest (rest l)))]
                    [args     (map parse arg-sexps)])
               (msgC o-expr m-name args))]

          
            [(get-field)
             (get-fieldC (s-exp->symbol (second l)))]

           
            [(set-field!)
             (set-field!C (s-exp->symbol (second l))
                          (parse (third l)))]

         
            [else
             (appC (parse (first l))
                   (parse (second l)))]
            )]

       
         [else
          (appC (parse (first l))
                (parse (second l)))]))]

    [else
     (error 'parse "bad syntax")]))


(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define-type Field
  [field (name : symbol) (loc : Location)])






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

(define-type Result
  [res (v : Value) (s : Store)])

(define new-loc
  (let ([counter (box 0)])
    (lambda () 
      (let ([l (unbox counter)])
        (begin (set-box! counter (+ 1 l))
               l)))))
     
(define (eval-env (env : Env) (sto : Store) (e : Expr)) : Result
  (type-case Expr e
    [numC (n) (res (numV n) sto)]
    [boolC (b) (res (boolV b) sto)]  
    [lambdaC (x e) (res (closV env x e) sto)]
    [plusC (e1 e2)
           (type-case Result (eval-env env sto e1)
             [res (v1 sto-1)
                  (type-case Result (eval-env env sto-1 e2)
                    [res (v2 sto-2)
                         (res (numV (+ (numV-n v1) (numV-n v2)))
                              sto-2)])])]
    [equal?C (e1 e2)
             (type-case Result (eval-env env sto e1)
               [res (v1 sto-1)
                    (type-case Result (eval-env env sto-1 e2)
                      [res (v2 sto-2)
                           (cond
                             [(and (numV? v1) (numV? v2))
                              (res (boolV (= (numV-n v1) (numV-n v2))) sto-2)]

                             [(and (boolV? v1) (boolV? v2))
                              (res (boolV (eq? (boolV-b v1) (boolV-b v2))) sto-2)]

                             [else
                              (error 'equal?C "Comparison on non-base values")])])])]

    [timesC (e1 e2)
            (type-case Result (eval-env env sto e1)
              [res (v1 sto-1)
                   (type-case Result (eval-env env sto-1 e2)
                     [res (v2 sto-2)
                          (res (numV (* (numV-n v1) (numV-n v2)))
                               sto-2)])])]
    [letC (x e1 e2) (eval-env env sto (appC (lambdaC x e2) e1))]
    [appC (e1 e2)
          (type-case Result (eval-env env sto e1)
            [res (v1 sto-1)
                 (type-case Result (eval-env env sto-1 e2)
                   [res (v2 sto-2)
                        (eval-env
                         (extend-env (bind (closV-x v1) v2) (closV-env v1))
                         sto-2
                         (closV-e v1))])])]
    [ifC (guard e1 e2)
         (type-case Result (eval-env env sto guard)
           [res (v-guard sto-1)
                (if (boolV-b v-guard)
                    (eval-env env sto-1 e1)
                    (eval-env env sto-1 e2))])]

            
    [idC (x) (res (lookup x env) sto)]
    
            
    [beginC (exprs)
            (letrec ([eval-seq
                      (lambda (es st)
                        (cond
                          [(empty? es)
                           (error 'beginC "begin with no expressions")]
                          [(empty? (rest es))
                           (eval-env env st (first es))]
                          [else
                           (type-case Result (eval-env env st (first es))
                             [res (_ st-next)
                                  (eval-seq (rest es) st-next)])]))])
              (eval-seq exprs sto))]

    
    [objectC (delegate fields methods)

            
             (let* ([deleg-result
                     (if (none? delegate)
                         (pair (none) sto)
                         (let* ([d-expr (some-v delegate)]
                                [res-deleg (eval-env env sto d-expr)])
                           (type-case Result res-deleg
                             [res (dv sto-deleg)
                                  (pair (some dv) sto-deleg)])))]
                    [v-delegate       (fst deleg-result)]  
                    [sto-after-deleg  (snd deleg-result)]) 

             
               (letrec ([eval-fields
                         (lambda (fs st)
                           (if (empty? fs)
                               (pair empty st)
                               (let* ([fld    (first fs)]         
                                      [name   (fst fld)]          
                                      [expr   (snd fld)]  
                                      [res-f  (eval-env env st expr)])
                                 (type-case Result res-f
                                   [res (v st-next)
                                        (let* ([loc       (new-loc)]
                                               [st-with   (override-store (cell loc v) st-next)]
                                               [rest-pair (eval-fields (rest fs) st-with)]
                                               [rest-flds (fst rest-pair)]
                                               [st-final  (snd rest-pair)])
                                          (pair (cons (field name loc) rest-flds)
                                                st-final))]))))])

               
                 (let* ([fs+store  (eval-fields fields sto-after-deleg)]
                        [field-list (fst fs+store)]  
                        [sto-final  (snd fs+store)]) 
                   (res (objV field-list methods v-delegate)
                        sto-final))))]




    [msgC (o-expr m args)
          (type-case Result (eval-env env sto o-expr)
            [res (v-obj sto-1)
                 (if (not (objV? v-obj))
                     (error 'msgC "msgC: target is not an object")

                  
                     (letrec (

                             
                              [eval-args
                               (lambda ((es  : (listof Expr))
                                        (st  : Store)
                                        (acc : (listof Value)))
                                 (if (empty? es)
                                     (pair (reverse acc) st)
                                     (type-case Result (eval-env env st (first es))
                                       [res (v st-next)
                                            (eval-args (rest es)
                                                       st-next
                                                       (cons v acc))])))]

                            
                              [find-method-in
                               (lambda (obj)
                                 (letrec ([search
                                           (lambda (ms)
                                             (cond
                                               [(empty? ms) (none)]
                                               [(symbol=? (method-decl-name (first ms)) m)
                                                (some (first ms))]
                                               [else (search (rest ms))]))])
                                   (search (objV-methods obj))))]

                           
                              [find-method
                               (lambda (obj)
                                 (let ([found (find-method-in obj)])
                                   (if (none? found)
                                       (let ([del (objV-delegate obj)])
                                         (if (none? del)
                                             (error 'msgC "method not found in object or delegate")
                                             (let ([dobj (some-v del)])
                                               (if (not (objV? dobj))
                                                   (error 'msgC "delegate is not an object")
                                                   (find-method dobj)))))
                                       (some-v found))))]

                           
                              [bind-args
                               (lambda (names vals env0)
                                 (if (empty? names)
                                     env0
                                     (bind-args (rest names)
                                                (rest vals)
                                                (extend-env
                                                 (bind (first names)
                                                       (first vals))
                                                 env0))))]

                              )
                       (let* ([args-pair (eval-args args sto-1 (list))]
                              [arg-vals  (fst args-pair)]
                              [sto-args  (snd args-pair)]

                            
                              [md      (find-method v-obj)]
                              [names   (method-decl-args md)]
                              [self-name   (first names)]
                              [other-names (rest names)]

                           
                              [env-self  (extend-env (bind self-name v-obj) env)]
                              [env-final (bind-args other-names arg-vals env-self)]

                           
                              [res-body (eval-env env-final sto-args
                                                  (method-decl-body md))])

                      
                         res-body)))])]



    [get-fieldC (name)
                (let ([self (lookup 'self env)])
                  (if (not (objV? self))
                      (error 'get-fieldC "self is not an object")
                      (letrec ([search-fields
                               
                                (lambda (fs)
                                  (cond
                                    [(empty? fs) (none)]
                                    [(symbol=? (field-name (first fs)) name)
                                     (some (first fs))]
                                    [else
                                     (search-fields (rest fs))]))]
                               [lookup-field
                              
                                (lambda (obj)
                                  (let ([found (search-fields (objV-fields obj))])
                                    (if (some? found)
                                        (let* ([fld (some-v found)]
                                               [loc (field-loc fld)])
                                          (fetch loc sto))
                                        (let ([del (objV-delegate obj)])
                                          (if (none? del)
                                              (error 'get-fieldC "field not found")
                                              (let ([dobj (some-v del)])
                                                (if (not (objV? dobj))
                                                    (error 'get-fieldC "delegate not an object")
                                                    (lookup-field dobj))))))))])
                        (res (lookup-field self) sto))))]


    [set-field!C (name e)
                 (type-case Result (eval-env env sto e)
                   [res (v-new sto1)
                        (let ([self (lookup 'self env)])
                          (if (not (objV? self))
                              (error 'set-field!C "self is not an object")

                              (letrec ([search-fields
                                       
                                        (lambda (obj)
                                          (letrec ([search
                                                    (lambda (fs)
                                                      (cond
                                                        [(empty? fs)
                                                         (let ([del (objV-delegate obj)])
                                                           (if (none? del)
                                                               (error 'set-field!C "field not found")
                                                               (let ([dobj (some-v del)])
                                                                 (if (not (objV? dobj))
                                                                     (error 'set-field!C "delegate not an object")
                                                                     (search-fields dobj)))))]

                                                        [(symbol=? (field-name (first fs)) name)
                                                         (field-loc (first fs))]

                                                        [else
                                                         (search (rest fs))]))])
                                            (search (objV-fields obj))))])

                                (let* ([loc    (search-fields self)]
                                       [sto2   (override-store (cell loc v-new) sto1)])
                                
                                  (res self sto2)))))])]


    )
  )

(define fix
  (parse
   '(lambda f
      (let b (box 0)
        (let frec (lambda x ((f (unbox b)) x))
          (begin
            (set-box! b frec)
            frec))))))

(define (eval (e : Expr))
  (eval-env empty-env empty-store e))


(define (eval-base (e : Expr)) : BaseValue
  (let ([r (eval-env empty-env empty-store e)])
    (type-case Result r
      [res (v sto)
           (value->base v)])))


(define (value->base (v : Value)) : BaseValue
  (type-case Value v
    [numV (n)   (numBV n)]
    [boolV (b)  (boolBV b)]
    [else (error 'eval-base "expected a base value")]))

