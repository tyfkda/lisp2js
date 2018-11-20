'use strict'

const {LISP} = require('./runtime.js')

const basic = require('../../gen/basic.js')
const backquote = require('../../gen/backquote.js')
const parser = require('../../gen/parser.js')
const compiler = require('../../gen/compiler.js')
const version = require('../../gen/version.js')

for (let f of [basic, backquote, parser, compiler, version])
  f(LISP)

if (typeof module !== 'undefined')
  module.exports = LISP
