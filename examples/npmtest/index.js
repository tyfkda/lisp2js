#!/usr/bin/env node

(() => {
  'use strict'

  const LISP = require('lisp2js/lisp2js.min')

  const result = LISP.eval(LISP['read-from-string']('(+ 1 2 3)'))
  console.log(result + '\n')
})()
