lisp2js
=======

Translate Lisp code into JavaScript.

[Demo](https://tyfkda.github.io/lisp2js/)

## Feature
* Compiled JS code run on JS directly (not VM)
* Compiled code can easily access JS, and vice versa
* Self hosting compiler
* Lisp-1 (namespace for function and variable is same)
* Old style macro (not hygienic)

### No
* Continuation
* Tail call optimization
* Argument count check (because of JavaScript nature)

Currently, many features and syntaxes are Scheme-ish, but will be replaced into CommonLisp/Arc style.

## Mapping
Lisp code is compiled into JS, in a rule below:

| Lisp       | JavaScript      |
|------------|-----------------|
| `()`       | `false`         |
| Pair       | `Cons` object   |
| symbol     | `Symbol` object |
| string     | String          |
| lambda     | Function        |
| hash table | Object          |


## History
* v0.1
  * Basic self hosting compiler
