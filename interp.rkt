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
  [let1E (var : Symbol)
         (value : Exp)
         (body : Exp)])

; Here, we introduce the `Value` datatype to represent the types of
; answers our evaluator can produce:
(define-type Value
  [numV (the-number : Number)]
  [boolV (the-boolean : Boolean)])

(define-type-alias Env (Hashof Symbol Value))
(define mt-env (hash empty)) ;; "empty environment"

; This environment `Env` will now be supplied to the interpreter, to use in
; place of substitution.
; (interp : (Exp Env -> Value))
(define (interp e nv)
  (type-case Exp e
    [(numE n) n]
    [(varE s) (lookup s nv)]
    [(plusE l r) (+ (interp l nv) (interp r nv))]
    [(let1E var val body)
     ; note that here, let is used from the host language to define let1 in this
     ; language.
     (let ([new-env (extend nv
                            var
                            (interp val nv))])
       (interp body new-env))]))

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
