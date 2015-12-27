(() => {
  'use strict'

  const LISP = require('../lisp2js')

  const print = (value) => {
    console.log(value)
  }

  const equals = (x, y) => {
    if (LISP['eq?'](x, y))
      return true
    if (LISP['pair?'](x) && LISP['pair?'](y) &&
        equals(LISP.car(x), LISP.car(y)) &&
        equals(LISP.cdr(x), LISP.cdr(y)))
      return true
    if (x instanceof Array && y instanceof Array)
      return x.toString() === y.toString()
    if (x instanceof RegExp && y instanceof RegExp)
      return x.toString() === y.toString()
    return false
  }

  const evalExpression = (expression) => {
    const stream = new LISP.StrStream(expression)
    let result
    for (;;) {
      const s = LISP.read(stream)
      if (s == null)
        return result
      result = LISP.eval(s)
    }
  }

  const test = (title, expected, expression) => {
    process.stdout.write('Testing ' + title + '... ')
    const result = evalExpression(expression)
    if (equals(expected, result)) {
      print('ok')
      return
    }

    console.error('\x1b[1;31m[ERROR]\x1b[0;39m')
    console.error('  expected ' + expected + ' : actual ' + result)
    process.exit(1)
  }

  const fail = (title, expression) => {
    process.stdout.write('Testing ' + title + '... ')
    try {
      const result = evalExpression(expression)
      console.error('\x1b[1;31m[ERROR]\x1b[0;39m')
      console.error('  Failure expected, but succeeded! [' + result + ']')
      process.exit(1)
    } catch (e) {
      print('ok (' + e.toString() + ')')
      return
    }
  }

  test('integer', 123, '123')
  test('symbol', LISP.intern('abc'), '(quote abc)')
  test('string', 'abc', '"abc"')
  test('vector', [1, 2, 3], '#(1 2 3)')
  test('t', true, 't')
  test('nil', false, 'nil')
  test('quote', 123, '(quote 123)')
  test('quote', LISP.list(1, false, 3), '(quote (1 () 3))')
  test('cons', LISP.cons(1, 2), '(cons 1 2)')
  test('if-true', 2, '(if 1 2 3)')
  test('if-false', 3, '(if nil 2 3)')
  test('if-false2', false, '(if nil 2)')
  test('set!', 123, '(do (def xyz nil) (set! xyz 123) xyz)')
  test('lambda', 2222, '((lambda (x) (+ x x)) 1111)')
  test('nested-lambda', 3, '(((lambda (x) (lambda (y) (+ x y))) 1) 2)')
  test('lambda-rest', LISP.list(1, 2, 3), '((lambda (x &rest y) (cons x y)) 1 2 3)')
  test('lambda-rest2', LISP.list(1), '((lambda (x &rest y) (cons x y)) 1)')
  test('def', 123, '(do (def x 123) x)')
  test('new', [], '(new Array)')
  test('+', 6, '(+ 1 2 3)')

  // Vector.
  test('vector', [1, 'foo', LISP.list(2, LISP.intern('bar'))], '(vector 1 "foo" \'(2 bar))')
  test('vector-length', 3, '(vector-length #(1 2 3))')
  test('vector-ref', 2, '(vector-ref #(1 2 3) 1)')

  // Macros.
  test('defmacro', false, ('(defmacro nil! (x) (list \'def x \'nil))' +
                           '(nil! xyz)' +
                           'xyz'))

  // Field reference.
  test('refer-field', 123, ('(def h (make-hash-table))' +
                            '(set! h.x 123)' +
                            'h.x'))

  // Fail cases
  fail('invalid-apply', '(1 2 3)')
  fail('invalid-param', '(lambda (nil t 1) 2)')

  print('\x1b[1;32mTEST ALL SUCCEEDED!\x1b[0;39m')
})()
