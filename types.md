# How to implement dependent types in 80 lines of code

Taken from [^htidti80lc]

What are dependent types? 
* in most general purpose programming languages, we have types that are dependent on types - generic
types.
* in _dependently typed_ langurages, we have types dependent on _terms_ 
* this allows programmers to perform term computation at compile time.

example:
consider what should happen when the type checker sees some argument x applied to f -- it should check
that the type of x can be applied to f, that is, it is computationally equivalent (referred to as 
"beta-convertible" in the literature). This involves reducing these two types to their beta normal 
forms and comparing them for alpha equivalence.
Two terms are alpha-equivalent if they are syntactically equal up to renaming of bound variables; 
e.g., `\x . \y . x` (the so-called "K combinator") is alpha-equivalent to `\a . \b . a`.
Additional checks may apply to make type checking more permissive; this includes eta conversion -- 
when `\x . f x` and `f` are considered equal.

topics:
* calculus of constructions: [^coc]
* Barendregt Lambda Cube
* De Brujin Indices
* Higher-Order Abstract Syntax (HOAS)

[^htidti80lc]: https://gist.github.com/Hirrolot/27e6b02a051df333811a23b97c375196
[^coc]: https://www.amazon.com/Type-Theory-Formal-Proof-Introduction/dp/110703650X
[^dbji]: https://en.wikipedia.org/wiki/De_Bruijn_index
