#lang plait
;: This is a toy language, built out while reading PLAI, which uses plait as
;; the host language. It is derived from scheme.

;; As the language defined here becomes harder to keep track of, it is easier to
; define it in BNF:
;
; <expr> ::= <num>
;          | {+ <expr> <expr>}
;          | {let1 {<var> <expr>} <expr>}
;          | <var>
;
; Note that in the used implementation of plait, braces, square brackets and
; parentheses are treated the same. To differentiate the toy language and the host
; language, we are going to use curly braces, but it doesn't really matter.
;
; We are going to use *static scoping* s.t. a variable's binding is only
; determined by it's position in the source program, and not the order of execution.
; Using dynamic scoping would allow the resulting output of a function such as...
; ------------------------
; {let1 {x 1}
;    {+ {if {random}
;            4
;            {let1 {x 2} x}}
;         x}}
; ------------------------
; ...to either return a value or an unbound-variable error, depending on the
; truthiness of the {random} input.

;; Implementation 
; We are defining a new type, `Exp`
(define-type  Exp
  ; There are three ways of making an `Exp`:
  ; Through the constructor `num`:
  ; -- A `num` takes one argument
  ; -- That argument must be an actual number
  [numE (n : Number)]
  ; Through the constructor `bool`:
  ; -- A `book` takes one arguments
  ; -- That argument must be a boolean value
  [boolE (b : Boolean)]
  ; Through the constructor `plus`:
  ; -- A `plus` takes two arguments
  ; -- Both arguments must be `Exp`s
  [plusE (left : Exp) (right : Exp)]
  ; Through the constructor `cnd`:
  ; -- A `cnd` takes three arguments
  ; -- All three are `Exp`s
  [cndE (test : Exp) (then : Exp) (else : Exp)])
; this allows us to create an AST, where the program:
;     1 + 2
; will be represented as:
;     (plusE (numE 1) (numE 2))
; In short, ASTs are tree structured data that *represent programs in programs*.

; Here, we introduce the `Value` datatype to represent the types of
; answers our evaluator can produce:
(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)])

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
;; Of course, we need to write some tests to check the logic:
;
; This will stop the output of all tests being printed...
(print-only-errors #true)
(test (parse `1) (numE 1))
(test (parse `2.3) (numE 2.3))
(test (parse `{+ 1 2}) (plusE (numE 1) (numE 2)))
(test (parse `{+ 1
                 {+ {+ 2 3}
                    4}})
      (plusE (numE 1)
             (plusE (plusE (numE 2)
                           (numE 3))
                    (numE 4))))
; To write negative test cases, we can use `test/exn`:
(test/exn (parse `{1 + 2}) "")
; The second parameter of the test above is the expected error message of the test result ->
; 
; So, why is it marked as a passing test?: 
; -> The parameter must be a substring of the returned error message,
; where the empty string "" is a substring of any error message aka expect *an* error.

;; Consider what other cases we should search for,
; Addition is defined to take exactly two sub-expressions. We should provide tests for
; zero, one, two, three, four ... n expressions.

;; Because we are using `parse` to produce output to be used
; as input for some function `calc`, we can compose the two into a test helper function.

;; Defining an add operator:
(define (add v1 v2)
  (type-case Value v1
    [(numV n1)
     (type-case Value v2
       [(numV n2) (numV (+ n1 n2))]
       [else (error '+ "expects RHS to be a number")])]
    [else (error '+ "expects LHS to be a number")]))

;; Defining a boolean-decision operator:
;
(define (boolean-decision v)
  (type-case Value v
    [(boolV b) b]
    [else (error 'if "expects conditional to evaluate to a boolean")]))


;; Defining an evaluator:
;
(calc : (Exp -> Value))

(define (calc e)
  ; Given some `Exp` we will want to take it apart using `type-case`,
  ; which tells us there are multiple options, each with additional metadata:
  (type-case Exp e
    ; if the whole expression is already a number, return the value type
    ; for a number from the expression type.
    [(numE n) (numV n)]
    ; if the whole expression is a boolean, return the value type
    ; for a boolean from the expression type.
    [(boolE b) (boolV b)]
    ; if there are two expressions, we have to add the two sides,
    ; first converting them from an expression into a number.
    [(plusE l r) (add (calc l) (calc r))]
    ; if there are three expressions, we first need to recursively evaulate
    [(cndE c t e) (if (boolean-decision (calc c))
                      (calc t)
                      (calc e))]))

;; Let's write some tests for the calc evaluator
(test/exn (calc (plusE (numE 4) (boolE #false))) "RHS")
(test (calc (numE 1)) (numV 1))
(test (calc (numE 2.3)) (numV 2.3))
(test (calc (plusE (numE 1) (numE 2))) (numV 3))
(test (calc (plusE (plusE (numE 1) (numE 2))
                   (numE 3)))
      (numV 6))
(test (calc (plusE (numE 1)
                   (plusE (numE 2) (numE 3))))
      (numV 6))
(test (calc (plusE (numE 1)
                   (plusE (plusE (numE 2)
                                 (numE 3))
                          (numE 4))))
      (numV 10))

; Test with decimals
; (test (calc (plusE (numE 0.1) (numE 0.2))) 0.3)

;; Here, combine the `parse` and `calc` functions to
; `run` an  integration test with the plait syntax directly
;; e.g. use `{+ 1 2} instead of (calc (plusE (numE 1) (numE 2)))
(run : (S-Exp -> Value))
(define (run s)
  (calc (parse s)))

;; Extending from the combine function above, we can create a macro to allow
; passing in a `Number` directly for the expected output.
(define (testNum t e)
  (test t (numV e)))
; Rather than manually type the AST node:
(test (run `1) (numV 1))
; provide the expected number: 
(testNum (run `1) 1)

(testNum (run `1) 1)
(testNum (run `2.3) 2.3)
(testNum (run `{+ 1 2}) 3)
(testNum (run `{+ {+ 1 2} 3}) 6)
(testNum (run `{+ 1 {+ 2 3}}) 6)
(testNum (run `{+ 1 {+ {+ 2 3} 4}}) 10)
