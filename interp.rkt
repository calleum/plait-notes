#lang plait
;: This is a toy language, built out while reading PLAI, which uses plait as
;; the host language. It is derived from scheme.

; Interp is the second coming of learning.rkt - it may have less rigorous comments
; but more functionality, firstly handling local bindings.

; The `Exp` definition is the same, only extended to include
; the new types in the AST.
(define-type Exp
  [numE (n : Number)]
  [plusE (left : Exp) (right : Exp)]
  [varE (name : Symbol)]
  ; Functions
  ; In this language, functions are going to be allowed to be nested.
  ; We can piggyback using let1 to assign a lambda to a variable in this language.
  ; So, we need to define a way to *create* the function, and a way to *use* it.
  ; This is sometimes referred to as *introduction* and *elimination*:
  ; *introduction*
  [lamE (var : Symbol) (body : Exp)]
  ; *elimination*
  [appE (fun : Exp) (arg : Exp)]
  [let1E (var : Symbol)
         (value : Exp)
         (body : Exp)])

;; Next, we need to write a parser:
; First, we need a conditional to check what kind of s-exp we were given:
(define (parse s)
  (cond
    [(s-exp-number? s)
     ; If it's a numeric s-exp, then we need to extract the number and pass it
     ; to the `num` constructor:
     (numE (s-exp->number s))]
    ; Otherwise, we need to extract the list and check whether the first thing in the list is an addition
    ; symbol.
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (if (symbol=? '+
                     (s-exp->symbol (first l)))
           ; This models the expected expression with a recurrence
           (plusE (parse (second l))
                  (parse (third l)))
           ; If the first thing in the list is not an addition symbol
           ; then we signal an error:
           (error 'parse "list not an addition")))]))

; Here, we introduce the `Value` datatype to represent the types of
; answers our evaluator can produce:
(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)]
  [funV (var : Symbol) (body : Exp)])

(define-type-alias Env (Hashof Symbol Value))
(define mt-env (hash empty)) ;; "empty environment"

;; Defining an add operator:
(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))


; This environment `Env` will now be supplied to the interpreter, to use in
; place of substitution.
(interp : (Exp Env -> Value))
(define (interp e nv)
  (type-case Exp e
    [(numE n) (numV n)]
    [(varE s) (lookup s nv)]
    [(plusE l r) (add (interp l nv) (interp r nv))]
    [(lamE v b) (funV v b)]
    [(appE f a) (let ([fv (interp f nv)]
                      [av (interp a nv)])
                  (type-case Value fv
                    [(funV v b) (interp b (extend nv v av))]
                    [else (error 'app "didn't get a function")]))]
    [(let1E var val body)
     ; note that here, let is used from the host language to define let1 in this
     ; language.
     (let ([new-env (extend nv
                            var
                            (interp val nv))])
       (interp body new-env))]))

;; Consider what other cases we should search for,
; Addition is defined to take exactly two sub-expressions. We should provide tests for
; zero, one, two, three, four ... n expressions.

;; Let's write some tests for the calc evaluator
#| (test/exn (interp (plusE (numE 4) (boolE #false))) "RHS") |#
#| (test-interp : (Exp Exp -> ())) |#
(define (test-interp t r)
  (test (interp t mt-env) r))

(test-interp (numE 1) (numV 1))
(test-interp (numE 1) (numV 1))
(test-interp (numE 2.3) (numV 2.3))
(test-interp (plusE (numE 1) (numE 2)) (numV 3))
(test-interp (plusE (plusE (numE 1) (numE 2))
                   (numE 3))
      (numV 6))
(test-interp (plusE (numE 1)
                   (plusE (numE 2) (numE 3)))
      (numV 6))
(test-interp (plusE (numE 1)
                   (plusE (plusE (numE 2)
                                 (numE 3))
                          (numE 4)))
      (numV 10))


; Now when a variable is encountered, we attempt to look it up in the current
; environment, and either succeed or return an empty/optional if it is not found.
(define (lookup (s : Symbol) (n : Env))
  (type-case (Optionof Value) (hash-ref n s)
    [(none) (error s "not bound")]
    [(some v) v]))
; From here, we can tackle `let1`, allowing us to bind to local scopes, etc.
; We have to:
; * evaluate the vody of the expression, in
; * an environment that has been extended, with
; * the new name
; * bound to its value
(extend : (Env Symbol Value -> Env))
(define (extend old-env new-name value)
  (hash-set old-env new-name value))


;; Because we are using `parse` to produce output to be used
; as input for some function `calc`, we can compose the two into a test helper function.
;; Here, combine the `parse` and `calc` functions to
; `run` an  integration test with the plait syntax directly
;; e.g. use `{+ 1 2} instead of (calc (plusE (numE 1) (numE 2)))
(run : (S-Exp -> Value))
(define (run s)
  (interp (parse s) mt-env))

;; Extending from the `run` function above, we can create a macro to allow
; passing in a `Number` directly for the expected output:
(define (test-num t e)
  (test t (numV e)))
; Rather than manually type the AST node:
(test (run `1) (numV 1))
; provide the expected number: 
(test-num (run `1) 1)

(test-num (run `1) 1)
(test-num (run `2.3) 2.3)
(test-num (run `{+ 1 2}) 3)
(test-num (run `{+ {+ 1 2} 3}) 6)
(test-num (run `{+ 1 {+ 2 3}}) 6)
(test-num (run `{+ 1 {+ {+ 2 3} 4}}) 10)

