(function() {
  'use strict';

  var LISP = require('../src/runtime/runtime');

  function print(value) {
    console.log(value);
  }

  function equals(x, y) {
    if (LISP['eq?'](x, y))
      return true;
    if (LISP['pair?'](x) && LISP['pair?'](y) &&
        equals(LISP.car(x), LISP.car(y)) &&
        equals(LISP.cdr(x), LISP.cdr(y)))
      return true;
    if (x instanceof Array && y instanceof Array)
      return x.toString() === y.toString();
    if (x instanceof RegExp && y instanceof RegExp)
      return x.toString() === y.toString();
    return false;
  }

  function test(title, expected, expression) {
    process.stdout.write('Testing ' + title + '... ');
    var result = LISP['read-from-string'](expression);
    if (equals(expected, result)) {
      print('ok');
      return;
    }

    console.error("\x1b[1;31m[ERROR]\x1b[0;39m");
    console.error("  expected " + expected + ' : actual ' + result);
    process.exit(1);
  }

  function fail(title, exception, code) {
    process.stdout.write('Testing ' + title + '... ');
    var errorMessage;
    try {
      var result = LISP['read-from-string'](code);
      errorMessage = 'Failure expected, but succeeded: result=' + result;
    } catch (exc) {
      if (exc instanceof exception) {
        print('ok');
        return;
      }
      errorMessage = 'Unexpected exception: expected ' + exception + ' : actual ' + exc;
    }

    console.error("\x1b[1;31m[ERROR]\x1b[0;39m");
    console.error('  ' + errorMessage);
    process.exit(1);
  }

  test('integer', 123, '123');
  test('float', 1.23, '1.23');
  test('positive number', 123, '+123');
  test('negative number', -123, '-123');
  test('nil', LISP.nil, '()');
  test('nil2', LISP.nil, 'nil');
  test('t', LISP.t, 't');
  test('single element list', LISP.cons(1, LISP.nil), '(1)');
  test('multiple elements list', LISP.cons(1, LISP.cons(2, LISP.cons(3, LISP.nil))), '(1 2 3)');
  test('dotted pair', LISP.cons(1, LISP.cons(2, 3)), '(1 2 . 3)');
  test('dotted pair2', LISP.list(1, LISP.cons(2, 3), 4), '(1 (2 . 3) 4)');

  test('line comment', 123, ';comment\n123');
  test('block comment', 123, '#| commenct\n|# 123');
  test('symbol', LISP.intern('symbol'), 'symbol');
  test('number like symbol', LISP.intern('1+'), '1+');
  test('non ascii character symbol', LISP.intern('Ｓｙｍｂｏｌ'), 'Ｓｙｍｂｏｌ');
  test('quote', LISP.list(LISP.intern('quote'), LISP.intern('abc')), "'abc");
  test('string', 'abc', '"abc"');
  test('complex string', 'foo bar\nbaz', '"foo bar\nbaz"');
  test('double quote in string', '"', '"\\""');
  test('escape character', '\x1b', '"\x1b"');
  test('keyword', LISP['make-keyword']('keyword'), ':keyword');

  test('quasiquote', LISP.list(LISP.intern('quasiquote'), LISP.intern('abc')), "`abc");
  test('unquote', LISP.list(LISP.intern('unquote'), LISP.intern('abc')), ",abc");
  test('unquote-splicing', LISP.list(LISP.intern('unquote-splicing'), LISP.intern('abc')), ",@abc");

  test('vector', [1, 2, 3], "#(1 2 3)");

  test('regexp', /abc/, "#/abc/");

  fail('no close paren', LISP.NoCloseParenException, '(1 2 3');

  print("\x1b[1;32mTEST ALL SUCCEEDED!\x1b[0;39m");
})();
