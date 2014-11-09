lisp2js
=======

Translate Lisp code into JavaScript.

## Feature
* Compiled JS code is run on JS directly (not VM)
* Compiled code can easily access JS, and vice versa
* Self compiler
* Lisp-1 (namespace for function and variable is same)
* Old style macro (not hygienic)

### No
* Continuation
* Tail call optimization

Currently, many features and syntaxes are Scheme-ish, but will be replaced into CommonLisp/Arc style.

## Mapping
Lisp code is compiled into JS, in a rule below:

| Lisp       | JavaScript      |
|------------|-----------------|
| `()`       | `null`          |
| `#f`       | `null`          |
| `#t`       | `true`          |
| Pair       | `Cons` object   |
| symbol     | `Symbol` object |
| string     | String          |
| lambda     | Function        |
| hash table | Object          |


## History
* v0.1
  * Basic self hosting compiler
