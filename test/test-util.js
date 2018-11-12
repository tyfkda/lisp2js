module.exports = (() => {
  'use strict'

  const {LISP} = require('../src/runtime/jslisp')

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

  const runTest = (converter, proc) => {
    let successCount = 0
    let errorCount = 0

    const test = (title, expected, expression) => {
      process.stdout.write('Testing ' + title + '... ')
      const result = converter(expression)
      if (equals(expected, result)) {
        print('ok')
        ++successCount
        return
      }

      console.error('\x1b[1;31m[ERROR]\n  expected ' + expected +
                    ' : actual ' + result + '\x1b[0;39m')
      ++errorCount
    }

    const fail = (title, exception, code) => {
      process.stdout.write('Testing ' + title + '... ')
      let err
      try {
        const result = converter(code)
        err = 'Failure expected, but succeeded: result=' + result
      } catch (exc) {
        if (exception == null || exc instanceof exception) {
          print('ok')
          ++successCount
          return
        }
        err = 'Unexpected exception: expected ' + exception + ' : actual ' + exc
      }

      console.error('\x1b[1;31m[ERROR]\n  ' + err + '\x1b[0;39m')
      ++errorCount
    }

    proc(test, fail)

    if (errorCount > 0) {
      console.error('\x1b[1;31m' + errorCount + '/' +
                    (successCount + errorCount) + ' tests failed\x1b[0;39m')
      return process.exit(1)
    }
    print('\x1b[1;32mALL ' + successCount + ' TESTS SUCCEEDED!\x1b[0;39m')
  }

  return {runTest}
})()
