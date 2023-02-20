Some definitions for the implementation:

Core idea is the that program's syntax is represented as abstract
syntax using a (mutually) recursive algebraic datatype, and we then
write a similar (mutually) recursive program to process it.

* interpreter processes values
* compiler processes programs 
* type-checker produces judgements about the type-correctness

There are some places where we would have to duplicate effort in the
implementation of a language - take the example of for and while loops 
in C: 

```C
int sum = 0;
int i;
for(i = 0; i < 10; i++) {
    sum += i;
}
```
This is functionally the same as: 

```C
int sum = 0;
int i = 0;
while (i < 10) {
    sum += i;
    i++;
}
```

These two constructs need to be implemented to be used in a language,
presenting duplicated effort. In reality we can choose to implement a
core language including a loop, which is the **desugared** version of
the loop constructs above. This limits the pain to implement and
maintain the language, but allows the convenience and quality of life of
having both loop constructs in the language's vocabulary.

