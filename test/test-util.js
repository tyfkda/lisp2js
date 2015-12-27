module.exports = (() => {
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

  const runTest = (converter, proc) => {
    const test = (title, expected, expression) => {
      process.stdout.write('Testing ' + title + '... ')
      let result = converter(expression)
      if (equals(expected, result)) {
        print('ok')
        return
      }

      console.error('\x1b[1;31m[ERROR]\x1b[0;39m')
      console.error('  expected ' + expected + ' : actual ' + result)
      process.exit(1)
    }

    const fail = (title, exception, code) => {
      process.stdout.write('Testing ' + title + '... ')
      let errorMessage
      try {
        const result = converter(code)
        errorMessage = 'Failure expected, but succeeded: result=' + result
      } catch (exc) {
        if (exception == null || exc instanceof exception) {
          print('ok')
          return
        }
        errorMessage = 'Unexpected exception: expected ' + exception + ' : actual ' + exc
      }

      console.error('\x1b[1;31m[ERROR]\x1b[0;39m')
      console.error('  ' + errorMessage)
      process.exit(1)
    }

    proc(test, fail)

    print('\x1b[1;32mTEST ALL SUCCEEDED!\x1b[0;39m')
  }

  return {runTest}
})()
