(function() {
  'use strict';

  var LISP = require('../lisp2js');

  function print(value) {
    console.log(value);
  }

  function equals(x, y) {
    if (LISP['eq?'](x, y))
      return true;
    if (x instanceof LISP.Cons && y instanceof LISP.Cons &&
        equals(LISP.car(x), LISP.car(y)) &&
        equals(LISP.cdr(x), LISP.cdr(y)))
      return true;
    if (x instanceof Array && y instanceof Array)
      return x.toString() === y.toString();
    if (x instanceof RegExp && y instanceof RegExp)
      return x.toString() === y.toString();
    return false;
  }

  function evalExpression(expression) {
    var stream = new LISP.StrStream(expression);
    var result;
    for (;;) {
      var s = LISP.read(stream);
      if (s == null)
        return result;
      result = LISP.eval(s);
    }
  }

  function test(title, expected, expression) {
    process.stdout.write('Testing ' + title + '... ');
    var result = evalExpression(expression);
    if (equals(expected, result)) {
      print('ok');
      return;
    }

    console.error("\x1b[1;31m[ERROR]\x1b[0;39m");
    console.error("  expected " + expected + ' : actual ' + result);
    process.exit(1);
  }

  function fail(title, expression) {
    process.stdout.write('Testing ' + title + '... ');
    try {
      var result = evalExpression(expression);
      console.error("\x1b[1;31m[ERROR]\x1b[0;39m");
      console.error('  Failure expected, but succeeded! [' + result + ']');
      process.exit(1);
    } catch (e) {
      print('ok (' + e.toString() + ')');
      return;
    }
  }

  test('integer', 123, '123');
  test('symbol', LISP.intern('abc'), '(quote abc)');
  test('string', 'abc', '"abc"');
  test('vector', [1, 2, 3], '#(1 2 3)');
  test('t', true, 't');
  test('nil', false, 'nil');
  test('quote', 123, '(quote 123)');
  test('quote', LISP.list(1, false, 3), '(quote (1 () 3))');
  test('cons', LISP.cons(1, 2), '(cons 1 2)');
  test('if-true', 2, '(if 1 2 3)');
  test('if-false', 3, '(if nil 2 3)');
  test('if-false2', false, '(if nil 2)');
  test('set!', 123, '(begin (def xyz nil) (set! xyz 123) xyz)');
  test('lambda', 2222, '((^(x) (+ x x)) 1111)');
  test('nested-lambda', 3, '(((^(x) (^(y) (+ x y))) 1) 2)');
  test('lambda-rest', LISP.list(1, 2, 3), '((^(x &rest y) (cons x y)) 1 2 3)');
  test('lambda-rest2', LISP.list(1), '((^(x &rest y) (cons x y)) 1)');
  test('def', 123, '(begin (def x 123) x)');
  test('new', [], '(new Array)');
  test('+', 6, '(+ 1 2 3)');

  // Vector.
  test('vector', [1, 'foo', LISP.list(2, LISP.intern('bar'))], "(vector 1 \"foo\" '(2 bar))");
  test('vector-length', 3, '(vector-length #(1 2 3))');
  test('vector-ref', 2, '(vector-ref #(1 2 3) 1)');

  // Macros.
  test('defmacro', false, ("(defmacro nil! (x) (list 'def x 'nil))" +
                           "(nil! xyz)" +
                           "xyz"));

  // Field reference.
  test('refer-field', 123, ("(def h (make-hash-table))" +
                            "(set! h.x 123)" +
                            "h.x"));

  // Fail cases
  fail('invalid-apply', '(1 2 3)');

  print("\x1b[1;32mTEST ALL SUCCEEDED!\x1b[0;39m");
})();
