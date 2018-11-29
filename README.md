lisp2js
=======

[![Build Status](https://travis-ci.org/tyfkda/lisp2js.svg?branch=master)](https://travis-ci.org/tyfkda/lisp2js)

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
* Tail call optimization (depends on JS environment)
* Argument count check (because of JavaScript nature)
* Multiple values


## Mapping
Lisp code is compiled into JS, in a rule below:

| Lisp       | JavaScript      |
|------------|-----------------|
| `()`       | `false`         |
| nil        | `false`         |
| t          | `true`          |
| Pair       | `Cons` object   |
| symbol     | `Symbol` object |
| string     | String          |
| lambda     | Function        |
| hash table | Object          |
| vector     | Array           |


## How to build

You need to install Node.js and npm.

* Run `npm install`
* When you modify source code, run `npm run build` to update `lisp2js.js`
* To update minified code, run `npm run release`


## History
* v0.1
  * Basic self hosting compiler
