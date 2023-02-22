#lang racket

;; Racket is a truthy/falsy language; any `if` takes any non-false value to be true.
;; To modify Racket to use strict if statements, we can define a macro:
(define-syntax strict-if
  (syntax-rules ()
    [(strict-if C T E)
     (if (boolean? C)
         (if C T E)
         (error 'strict-if "expected a boolean"))]))

;; It may be tempting to use a function here instead, to wrap the inner if expression,
;; but doing that will result in the parameters being eagerly expanded/evaluated.
;; An example of this is defined below:
(define (eager-strict-if C T E)
  (if (boolean? C)
      (if C T E)
      (error 'eager-strict-if "expected a boolean")))

;; To see the difference, run the following:
;; (strict-if true 1 (/ 1 0)) ;; return 1
;; (eager-strict-if true 1 (/ 1 0)) ;; error - /: division by zero

;; This is an example where eager evaluation is seen clearly. An example of where this
;; would be unwanted other than above, would be if there is a heavy computation on each
;; side of the conditional branch, and both needed to be computed when the conditional
;; is evaluated. Creating macros allows the language to be programmed, where we manipulate
;; the program source using the written macros, which is expanded at compile time.

;; Throughout plait, new pieces of syntax are often referred to as *constructs*, and in
;; Lisp/Scheme/Racket communities, they are often referred to as *special forms*.

;; defining a macro to allow the syntax:
;;
;; (cal:let1 (x 3) (+ x x))
(define-syntax cal:let1
  (syntax-rules ()
    [(cal:let1 (var val) body)
     ((lambda (var) body) val)
     ]))
;; When broken down, all cal:let1 does is *bind* a name to a value, then immediately 
;; evaluate that value in an environment extended by its name. 
;; There is already something from interp.rkt that does that -> a function!
;; test: (cal:let1 (x 3) (+ x x)) ; expect 6
;;
;; This is a pattern; an anonymous function that is used right away, and it is called
;; *left-left-lambda*, due to the two left parentheses before the lambda call.
;;
