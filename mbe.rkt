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

;; Binding 1..* locals, it would be nice to be able to fluently denote an arbitrarily
;; long sequence with syntax such as:
(define-syntax cal:let2
  (syntax-rules ()
    [(cal:let2 ([var val] ...) body)
     ((lambda (var ...) body) val ...)]))

;; This says, cal:let2 is followed by any number of the pair var-val, followed by a
;; body. Turn that into a lambda with all the vars expanded out into formal arguments,
;; and use the argument body as body.
;; usage:
(cal:let2 ([x 3] [y 4]) (+ x y))

;; Further exploring the ellipses, take the example of a pattern matching conditional:
;; It can  also be defined here that the construct can be extended to match any
;; number of patterns, and falling through through the bottom will result in an error.
(define-syntax cal:cond
  (syntax-rules ()
    [(cal:cond) (error 'cal:cond "could not find a matching arm to expand")]
    [(cal:cond [q0 a0] [q1 a1] ...)
     (if q0
         a0
         (cal:cond [q1 a1] ...))]))

(define (sign n)
  (cal:cond
   [(< n 0) "negative"]
   [(= n 0) "zero"]
   [(> n 0) "positive"]))

;; Hygiene
;; Macros can expand to appear as though they are capturing inner variables
;; in outer scope. Since variables in macros are bound to an environment,
;; the macros themselves are hygienic. Take the following example:
;; Defining a piece of syntax, to only proceed if the condition is *not*
;; true:
(define-syntax unless
  (syntax-rules ()
    ;; There is another new syntax here, using `_` means to match any syntax
    ;; item, from https://docs.racket-lang.org/reference/stx-patterns.html
    [(_ cond body ...)
     (if (not cond)
         (begin
           body
           ...)
         (void))]))
;; The case where the above macro seems ambiguous is as follows -
;; using the macro with a let expression that seems to capture
;; the named variables within the macro:
(let ([not (λ (v) v)])
  (unless false
    (println 1)
    (println 2)))
;; Running this shows that the macro is hyginic, and the outer env
;; and that in the macro are expanded without leaking the namespace, etc.
;; This is unlike the C pre-processor

;; Macro definition footguns:
;; When defining a macro to act like a two-armed ternary operator, which
;; evaluates the left arm, if it is truthy, returns it, otherwise returns the
;; right arm:
;; It would be natural to attempt to define the macro like this below, but the 
;; issue is in the case where `e1` has some side effect that we want to only 
;; run once.
(define-syntax cal:bad-or
  (syntax-rules ()
    [(_ e1 e2)
     (if e1
         true
         e2)]))
;; With this macro, the following would print "hello" twice, which is not what we 
;; want:
(cal:bad-or (print "hello") "not found")
;; In order to fix this, define the macro such that the side effect is suppressed 
;; when the macro evaluates its truthiness, then returned when that is confirmed
(define-syntax cal:or
  (syntax-rules ()
    [(_ e1 e2)
     (let ([v e1])
       (if v v e2))]))
;; See that the correct output is shown when we invoke the following: 
(cal:or (print "hello") "not found")
