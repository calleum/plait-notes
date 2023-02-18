#lang plait
; We are defining a new type, `Exp`
(define-type  Exp
  ; There are two ways of making an `Exp`:
  ; through the constructor `num`:
  ; -- A `num` takes on argument
  ; -- That argument must be an actual number
  [num (n : Number)] 
  ; The other way is through the constructor `plus`:
  ; -- A `plus` takes two arguments
  ; -- Both arguments must be `Exp`s
  [plus (left : Exp) (right : Exp)])
; this allows us to create an AST, where the program:
;     1 + 2
; will be represented as:
;     (plus (num 1) (num 2))
; In short, ASTs are tree structured data that *represent programs in programs*.
;

;; Next, we need to write a parser:
; First, we need a conditional to check what kind of s-exp we were given:
(define (parse s)
  (cond
    [(s-exp-number? s)
     ; If it's a numeric s-exp, then we need to extract the number and pass it to the `num` constructor:
     (num (s-exp->number s))]
    ; Otherwise, we need to extract the list and check whether the first thing in the list is an addition
    ; symbol. 
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (if (symbol=? '+
                     (s-exp->symbol (first l)))
           ; This models the expected expression with a recurrence
           (plus (parse (second l))
                 (parse (third l)))
           ; If the first thing in the list is not an addition symbol
           ; then we signal an error:
           (error 'parse "list not an addition")))]))
;; Of course, we need to write some tests to check the logic:
;
; This will stop the output of all tests being printed...
(print-only-errors #true)
(test (parse `1) (num 1))
(test (parse `2.3) (num 2.3))
(test (parse `{+ 1 2}) (plus (num 1) (num 2)))
(test (parse `{+ 1
                 {+ {+ 2 3}
                    4}})
      (plus (num 1)
            (plus (plus (num 2)
                        (num 3))
                  (num 4))))
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

;; Defining an evaluator:
;
(calc : (Exp -> Number))

(define (calc e)
  ; Given some `Exp` we will want to take it apart using `type-case`,
  ; which tells us there are two options, each with additional metadata:
  (type-case Exp e
    ; if the whole expressions is already a number, just return it:
    [(num n) n]
    ; else we have to add the two sides, first converting them
    ; from an expression into a number.
    [(plus l r) (+ (calc l) (calc r))]))

;; Let's write some tests for the calc evaluator

(test (calc (num 1)) 1)
(test (calc (num 2.3)) 2.3)
(test (calc (plus (num 1) (num 2))) 3)
(test (calc (plus (plus (num 1) (num 2))
                  (num 3)))
      6)
(test (calc (plus (num 1)
                  (plus (num 2) (num 3))))
      6)
(test (calc (plus (num 1)
                  (plus (plus (num 2)
                              (num 3))
                        (num 4))))
      10)

;; Here, combine the `parse` and `calc` functions to
; `run` an  integration test:
(run : (S-Exp -> Number))

(define (run s)
  (calc (parse s)))

(test (run `1) 1)
(test (run `2.3) 2.3)
(test (run `{+ 1 2}) 3)
(test (run `{+ {+ 1 2} 3}) 6)
(test (run `{+ 1 {+ 2 3}}) 6)
(test (run `{+ 1 {+ {+ 2 3} 4}}) 10)
