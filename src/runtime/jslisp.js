'use strict'

if (typeof __non_webpack_require__ === 'undefined')
  global.__non_webpack_require__ = require  // To hack for non-webpacked

const fs = __non_webpack_require__('fs')
const readline = __non_webpack_require__('readline')
const tty = __non_webpack_require__('tty')

const {LISP} = require('./lisp2js.js')

const runtimeNode = require('./runtime_node.js')
runtimeNode(LISP)

// Run stream.
const runStream = (stream, compile) => {
  let result
  for (;;) {
    const s = LISP.read(stream)
    if (s === LISP.nil)
      return result

    if (compile) {
      result = LISP.compile(s)
      LISP.print(`  ${result};\n`)
    } else {
      result = LISP.eval(s)
    }
  }
}

// Run codes.
const runCodes = (codes, compile = false) => runStream(LISP['make-string-input-stream'](codes), compile)

const dumpException = (e) => {
  if (e instanceof LISP.UnexpectedCharacterException) {
    console.error(`Unexpected character: ${e.char.toString()}`)
  } else if (e instanceof LISP.NoCloseParenException) {
    console.error('No close paren')
  } else if (e instanceof LISP.NoCloseQuoteException) {
    console.error('No close quote')
  } else {
    console.error(e)
  }
}

// Read-Eval-Print loop.
const repl = () => {
  const rl = readline.createInterface(process.stdin, process.stdout)

  process.stdin.resume()
  process.stdin.setEncoding('utf8')

  const prompt = '> '
  const prompt2 = '. '
  const inputs = []

  rl.setPrompt(prompt)
  rl.prompt()
  rl.on('line', (line) => {
    inputs.push(line)
    try {
      const code = inputs.join('\n')
      if (code.match(/^\s*$/)) {
        inputs.length = 0
      } else {
        const result = runCodes(code)
        // Otherwise input should be consumed.
        inputs.length = 0
        console.log(LISP['x->string'](result, 10))
      }
      rl.setPrompt(prompt)
      rl.prompt()
    } catch (e) {
      if (e instanceof LISP.NoCloseParenException) {
        // In REPL, if NoCloseParenException occurs,
        // a user keep typing so inputs should be kept.
        rl.setPrompt(prompt2)
      } else {
        dumpException(e)
        inputs.length = 0
        rl.setPrompt(prompt)
      }
      rl.prompt()
    }
  }).on('close', () => {
    if (inputs.length > 0) {
      console.error('Input not terminated: [' + inputs + ']')
      process.exit(1)
      return
    }
    process.exit(0)
  })
}
const replNoPrompt = () => {
  process.stdin.resume()
  process.stdin.setEncoding('utf8')
  try {
    return LISP.load(LISP['*stdin*'])
  } catch (e) {
    dumpException(e)
    process.exit(1)
  }
}

// Run script.

// Main.
function main() {
  let index = 2
  let compileOnly = false
  for (; index < process.argv.length; ++index) {
    const option = process.argv[index]
    if (option == '-c') {
      compileOnly = true
      continue
    }
    break
  }

  if (index >= process.argv.length) {  // No input file name: read from stdin.
    if (tty.isatty(0))
      return repl()
    if (!compileOnly)
      return replNoPrompt()

    process.stdin.resume()
    process.stdin.setEncoding('utf8')
    try {
      return runStream(LISP['*stdin*'], compileOnly)
    } catch (e) {
      dumpException(e)
      process.exit(1)
    }
  }

  // Process command line argument as a script file.
  try {
    LISP['*argv*'] = LISP['vector->list'](process.argv.slice(index + 1))
    let status = 0
    if (compileOnly) {
      console.log('module.export = function(LISP) {')
      console.log('  \'use strict\'')
      let text = fs.readFileSync(process.argv[index], 'utf-8')
      const matchShebang = text.match(/^#!(.*)\n/)
      if (matchShebang)
        text = text.slice(matchShebang[0].length)
      runCodes(text, compileOnly)
      console.log('}')
    } else {
      LISP.load(process.argv[index])
      if ('main' in LISP)
        status = LISP.main(LISP.cons(process.argv[index], LISP['*argv*']))
    }
    if (status)
      process.exit(status)
  } catch (e) {
    dumpException(e)
    process.exit(1)
  }
}

const objs = {
  LISP,
  main,
}

if (typeof module !== 'undefined')  // for $/jslisp (development)
  module.exports = objs

if (typeof __module !== 'undefined') {  // for dist/jslisp (Webpack-ed)
  __module.exports = objs

  if (typeof __non_webpack_require__ !== 'undefined' &&
      __non_webpack_require__.main === __module) {
    main()
  }
}