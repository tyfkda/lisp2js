'use strict'

import {LISP} from './runtime.js'

import basic from '../../gen/basic.js'
import backquote from '../../gen/backquote.js'
import parser from '../../gen/parser.js'
import compiler from '../../gen/compiler.js'

for (let f of [basic, backquote, parser, compiler])
  f(LISP)

window.LISP = LISP
