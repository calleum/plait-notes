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

Interesting... Javascript
[IIFE](https://developer.mozilla.org/en-US/docs/Glossary/IIFE): 
*Immediately Invoked Function Expression* - 
Another case where an anonymous function is Immediately invoked when 
defined, and can be defined as: 
```javascript
() => {
  // some initiation code
  let firstVariable;
  let secondVariable;
})();
```
This *could* be useful when needing to run initialisation functions or
defining variables or functions when you do not want to pollute global
scope.

#### Java upcasting
if there is an inheriting class Pt3, extending Pt2, Pt3 can be cast to Pt2 and the values in that
object can be seen...
```java
class Main {
    public static void main(String[] args) {
        Pt3 p3345 = new Pt3(3, 4, 5);
        Pt3 p3678 = new Pt3(6, 7, 8);
        // Print the Pt3 Object member values
		System.out.println(p3345.x);
        System.out.println(p3678.x);
		// Cast to Pt2 and print those values 
		System.out.println(((Pt2)p3345).x);
        System.out.println(((Pt2)p3678).x);
	}
}

```

### Javascript wat: 
[wat talk](https://www.destroyallsoftware.com/talks/wat)
This reminds me of the video 
[javascript is weird (extreme edition)](https://www.youtube.com/watch?v=sRWE5tnaxlI)
