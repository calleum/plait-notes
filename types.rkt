#lang racket
(require [only-in plait test print-only-errors])
;; TYPES

(define-type BinOp [plus])
(define-type Expr [binE (operator : BinOp) (left : Expr) (right : Expr)] [numE (value : Number)])

(calc : (Expr -> Number))
(define (calc e)
  (type-case Expr e [(binE o l r) (type-case BinOp o [(plus) (+ (calc l) (calc r))])] [(numE v) v]))

(test (calc (binE (plus) (numE 5) (numE 6))) 11)

;; What happens with a type checker?
(bad-tc : (Expr -> Boolean))
(define (bad-tc e)
  (type-case Expr e [(binE o l r) (type-case BinOp o [(plus) (and (tc l) (tc r))])] [(numE v) #true]))
(test (tc (binE (plus) (numE 5) (numE 6))) #true)

;; The typechecker above will return true when given a number. Since there is no path to return
;; false, all programs type checked by this typechecker will be type correct, even when they
;; shouldn't be.

;; One way to think about it, the type checker must be a calculator for all defined types, that is,
;; it closely resembles our evaluator, but over the set of types rather than terms.

;; define the type that we  expect the typechecker to produce
(define-type Type [numT] [strT])

;; Expect the typechecker to return a type in the host language
(tc : (Expr -> Type))
(define (tc e)
  (type-case Expr
             e
             [(binE o l r)
              (type-case BinOp
                         o
                         [(plus)
                          (if (and (numT? (tc l)) (numT? (tc r)))
                              (numT)
                              (error 'tc "arguments supplied to plus are not both numbers"))]
                         [(++)
                          (if (and (strT? (tc l)) (strT? (tc r)))
                              (strT)
                              (error 'tc "arguments supplied to plus are not both strings"))])]
             [(numE v) (numT)]
             [(strE v) (strT)]))
(test (tc (binE (plus) (numE 5) (numE 6))) (numT))
(test (tc (binE (++) (strE "hello") (strE "world"))) (strT))
(test/exn (tc (binE (++) (numE 5) (numE 6))) "strings")
(test/exn (tc (binE (plus) (strE "hello") (strE "world"))) "numbers")

;; The typechecker follows the same implementation model as our interpreter:
;; algebraic datatype to represent to AST, and structural recursion to process it.
;; In PLAI - this schema is called 'SImPl'

;; A typechecker, unlike an interpreter, operates with weak values: note, for instance, how the
;; numE case ignores the actual numeric values.
;; Sometimes referred to as primitives, in plai the basic types are referred to as axioms.

;; implementing typechecks for conditional branching, for an expression with the parts (C T E)
;; C must always evaluate to some boolean or truthy/falsy value, but there is a choice to be made for
;; T and E -> these can be the unit type, a sum type/union of the types of T and E, or they can
;; resolve to the same type. Personal preference withstanding, the conditional type would
;; have T and E resolve to the same type.

;; FUNCTION TYPE
;; The function type is special here, because we need a constructor for the type that provides
;; the types that the function will consume and produce. In this language, the arrow constructor is
;; used ( ??? -> ??? ) to construct the function type.
;; Just like in the interpreter, when typing a lambda, an environment is going to be passed in with
;; lambda itself, resulting in the type checker syntax:
;; Γ |- e : T -> "the environment Gamma proves that `e` has a type of `T`"
;; This means that for we can use the environment to get the type of a variable v:
;; Γ |- v : Γ(v)

;; Typing Recusion
;; Each application of the function type above consumes an arrow (->) each use. In order to supply
;; a recursive construct for the language, it is typically done by adding a recursive function
;; construct with a custom type.
