#lang racket
(require [only-in plait test print-only-errors])

;; OBJECTS
;; Two main choices presented; the object model can be implemented as:
;; 1. a core part of the language
;; 2. through syntactic sugar

;; An object is...
;; a value, that maps names to things, either other values, or methods
;;

;; define an object `o` that takes in either `add1` or `sub1`, and in
;; each case returns a function that either increments or decrements:
(define o
  (lambda (m)
    (case m
      [(add1) (lambda (x) (+ x 1))]
      [(sub1) (lambda (x) (- x 1))])))

(test ((o 'add1) 5) 6)

;; Observe from above: basic objects are a generalisation of a lambda
;; to have multiple points to call against the lambda.
;; From this, a `lambda` is an object which only has _one_ entrypoint
;; and as such, we don't need to disambiguate which entry point to call.

;; Although, it's not a good experience to use the above syntax to call 
;; a 'method' within the object, so we can define a function to apply the 
;; call against the object, removing one paren:
(define (msg o m . a)
  (apply (o m) a))
;; Notice the '.' - this is Racket's `variable-arity`?? syntax
;; `. a` means bind the remaining 0-* arguments to a list named `a`.
(test (msg o 'add1 5) 6)
