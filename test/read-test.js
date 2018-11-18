(() => {
  'use strict'

  const {LISP} = require('../src/runtime/jslisp')
  const U = require('./test-util')

  U.runTest(LISP['read-from-string'], (test, fail) => {
    test('integer', 123, '123')
    test('float', 1.23, '1.23')
    test('positive number', 123, '+123')
    test('negative number', -123, '-123')
    test('nil', LISP.nil, '()')
    test('nil2', LISP.nil, 'nil')
    test('t', LISP.t, 't')
    test('single element list', LISP.cons(1, LISP.nil), '(1)')
    test('multiple elements list',
         LISP.cons(1, LISP.cons(2, LISP.cons(3, LISP.nil))), '(1 2 3)')
    test('dotted pair', LISP.cons(1, LISP.cons(2, 3)), '(1 2 . 3)')
    test('dotted pair2', LISP.list(1, LISP.cons(2, 3), 4), '(1 (2 . 3) 4)')

    test('line comment', 123, ';comment\n123')
    test('block comment', 123, '#| commenct\n|# 123')
    test('symbol', LISP.intern('symbol'), 'symbol')
    test('number like symbol', LISP.intern('1+'), '1+')
    test('non ascii character symbol', LISP.intern('Ｓｙｍｂｏｌ'),
         'Ｓｙｍｂｏｌ')
    test('quote', LISP.list(LISP.intern('quote'), LISP.intern('abc')), '\'abc')
    test('string', 'abc', '"abc"')
    test('complex string', 'foo bar\nbaz', '"foo bar\nbaz"')
    test('double quote in string', '"', '"\\""')
    test('escape character', '\x1b', '"\x1b"')
    test('keyword', LISP['make-keyword']('keyword'), ':keyword')

    test('quasiquote',
         LISP.list(LISP.intern('quasiquote'), LISP.intern('abc')),
         '`abc')
    test('unquote', LISP.list(LISP.intern('unquote'), LISP.intern('abc')),
         ',abc')
    test('unquote-splicing',
         LISP.list(LISP.intern('unquote-splicing'), LISP.intern('abc')),
         ',@abc')

    test('vector', [1, 2, 3], '#(1 2 3)')
    test('quote in vector',
         [LISP.list(LISP.intern('quote'), LISP.intern('a'))], '#(\'a)')

    test('regexp', /abc/, '#/abc/')

    test('sharp-dot', 3, '#.(+ 1 2)')

    fail('no close paren', null /*LISP.NoCloseParenException*/, '(1 2 3')
    fail('no close paren2', null /*LISP.NoCloseParenException*/, '(1 2 .')
    fail('no value after dot', null /*LISP.UnexpectedCharacterException*/, '(1 2 .)')
    fail('no close quote', null /*LISP.NoCloseQuoteException*/, '"foo')
    fail('no close paren for vector', null /*LISP.NoCloseParenException*/, '#(1 2 3')
  })
})()
