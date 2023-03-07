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

;; Because the object is defined by the language as syntax sugaring,
;; there is no mapping from the target back to the implementation in
;; the orginal form, before desugaring.
;; Because of this, some static analysis tools must implement a
;; functionality such as this to map the expanded form being analysed
;; back to the source code.

;; CONSTRUCTORS
;; In this language, a constructor is a function that is bound to an
;; object - and invoked at that object's construction time.
(define (o-constr x)
  (lambda (m)
    (case m
      [(addX) (lambda (y) (+ x y))])))
;; using the constructor as such:
(test (msg (o-constr 5) 'addX 3) 8)
(test (msg (o-constr 2) 'addX 3) 5)

;; CLASSES
;; In order to encapsulate state, we can use variables in the desugared
;; language to mutate the shared state of an object
(define (mk-o-state count)
  (lambda (m)
    (case m
      [(inc) (lambda () (set! count (+ count 1)))]
      [(dec) (lambda () (set! count (- count 1)))]
      [(get) (lambda () count)])))
;; using the shared state/mutation
(test (let ([o (mk-o-state 5)])
        (begin
          (msg o 'inc)
          (msg o 'inc)
          (msg o 'dec)
          (msg o 'get)))
      6)
;; when we have multiple objects, mutating one doesn't affect another:
(test (let ([o1 (mk-o-state 3)] [o2 (mk-o-state 3)])
        (begin
          (msg o1 'inc)
          (msg o1 'inc)
          (+ (msg o1 'get) (msg o2 'get))))
      (+ 5 3))

;; PRIVATE MEMBERS
;; data hiding is a common feature in object oriented languages,
;; and should be straight forward to implement here using lexical scoping
;; to ensure that attempts to access count from outside of the object
;; would fail.
(define (mk-o-state/priv init)
  (let ([count init])
    (lambda (m)
      (case m
        [(inc) (lambda () (set! count (+ count 1)))]
        [(dec) (lambda () (set! count (- count 1)))]
        [(get) (lambda () count)]))))

;; STATIC MEMBERS
;; Static memebers are shared between all instances of an object:
(define mk-o-static
  (let ([counter 0])
    (lambda (amount)
      (begin
        (set! counter (+ 1 counter))
        (lambda (m)
          [(inc) (lambda (n) (set! amount (+ amount n)))]
          [(dec) (lambda (n) (set! amount (- amount n)))]
          [(get) (lambda () amount)]
          [(count) (lambda () counter)])))))
;; use the static initialisation and ensure that the global count is
;; incremented
(test (let ([o (mk-o-static 1000)]) (msg o 'count)) 1)
(test (let ([o (mk-o-static 0)]) (msg o 'count)) 2)

;; note: this construct is tied to the object... we should be able to
;; access a static member, since this is owned by the _class_ and not
;; by the object itself.

;; Self reference
;; it is expected that members within an object can directly reference
;; one another, and a method has a way of reaching into it's
;; enclosing object and accessing the methods and state within it.
;; This is usually a reference to itself called `this` or `self`.

;; Self reference using mutation
(define o-self!
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda m
              (case m
                [(first) (lambda (x) (msg self 'second (+ x 1)))]
                [(second) (lambda (x) (+ x 1))])))
      self)))
;; testing the usage of self refrence:
(test (msg o-self! 'first 5) 7)

;; Alternatively, self refrence can be implemented without mutation,
;; where we send the object itself as a parameter of the function/
;; message passing
(define o-self-no!
  (lambda (m)
    (case m
      [(first) (lambda (self x) (msg/self self 'second (+ x 1)))]
      [(second) (lambda (self x) (+ x 2))])))
;; we now have to modify the method invocation on the object to allow
;; passing in an object reference as a parameter
(define (msg/self o m . a)
  (apply (o m) o a))

(test (msg/self o-self-no! 'first 5) 7)

;; DYNAMIC DISPATCH
;; The process of deciding which polymorphic operation is used at
;; runtime - a key characteristic of object oriented systems. Allows a
;; caller to invoke a method without knowing or deciding which object
;; will handle the invocation.

;; Modelling this, there doesn't need to be any type-case as we can use the constructs that have
;; been introduced so far into the language.
;; Tree Objects
;; TO1
(define (mt)
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(sum) (lambda () 0)])))
      self)))
;; TO2
(define (node v l r)
  (let ([self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(sum) (lambda () (+ v (msg l 'sum) (msg r 'sum)))])))
      self)))
;; using the above tree objects, create a tree
(define a-tree (node 10 (node 5 (mt) (mt)) (node 15 (node 6 (mt) (mt)) (mt))))
;; test dynamic dispatch on the above constructs
(test (msg a-tree 'sum) (+ 10 5 15 6))

;; OTHER
;; Member Names - is the set of member names statically fixed, or dynamically varying? Is the
;; member being access at a point statically fixed
;; Reviewing these variables leads to 4 cells in a 2x2 table:
;;
;;    fixed set of members: Name is static, Name is computed
;; variable set of members: Name is static, Name is computed
;;
;; from this, we can see that fixed/static is what is used in java, fixed/computed is java, using
;; reflection, and variable/computed is what is seen in python, javascript in the syntax
;; `object["member"]`.

;; INHERITANCE
;; To extend/inherit from a parent class, using the desugaring model we have been, we need to
;; create two objects, and track calls to each of them. If we need to call a method from the parent,
;; then we use dynamic dispatch to call to the parent's member?
;; TO1-sized
(define (node/size parent-maker v l r)
  (let ([parent-object (parent-maker v l r)] [self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(size) (lambda () (+ 1 (msg l 'size) (msg r 'size)))]
                [else (parent-object m)])))
      self)))
;; TO2-sized
(define (mt/size parent-maker)
  (let ([parent-object (parent-maker)] [self 'dummy])
    (begin
      (set! self
            (lambda (m)
              (case m
                [(size) (lambda () 0)]
                [else (parent-object m)])))
      self)))
;; With this pattern, the constructor has to explicitly pass in the parent object on each invocation
;; of the downstream constructor
(define a-tree/size
  (node/size node
             10
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 15 (node/size node 6 (mt/size mt) (mt/size mt)) (mt/size mt))))

;; MIXINS
;; We can use mixins to write syntax such as
;; mixin M extends I1 implements I2 { ... }
;; where I1 and I2 are interfaces. Mixins can be thought of in this context as a class that has
;; been turned into a function over parent classes:
;; M :: I2 -> I2

;; TRAITS are a generalisation of mixins that says that instead of extending a single mixin, we
;;  can extend a set of them.
