'use strict'

const LISP = (typeof window !== 'undefined') ? (  // Running on a browser
  window.LISP = window.LISP || require('./lisp2js.js')
) : require('./lisp2js.js')

if (typeof __non_webpack_require__ === 'undefined') {  // Not webpacked: Directly imported.
  global.__non_webpack_require__ = require  // To hack for non-webpacked
}

// Run codes.
function runCodes(streamOrText, compile = false) {
  const stream = (typeof streamOrText === 'string') ? LISP['make-string-input-stream'](streamOrText) : streamOrText

  if (!compile)
    return LISP.load(stream)

  let result = LISP.nil
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

function runCodesWithErrorHandling(stream, compile) {
  // Redirect
  try {
    return runCodes(stream, compile)
  } catch (e) {
    dumpException(e)
    process.exit(1)
  }
}

function dumpException(e) {
  console.error(e.toString())
}

// Main.
function main(argv) {
  const fs = __non_webpack_require__('fs')
  const readline = __non_webpack_require__('readline')
  const tty = __non_webpack_require__('tty')

  const getOpts = require('get-options')

  const runtimeNode = require('./runtime_node.js')
  runtimeNode(LISP)

  // Read-Eval-Print loop.
  const repl = () => {
    const rl = readline.createInterface(process.stdin, process.stdout)

    const kNormalPrompt = '> '
    const kContinuePrompt = '. '
    const inputs = []

    rl.setPrompt(kNormalPrompt)
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
        rl.setPrompt(kNormalPrompt)
        rl.prompt()
      } catch (e) {
        if (e instanceof LISP.NoCloseParenException) {
          // In REPL, if NoCloseParenException occurs,
          // a user keep typing so inputs should be kept.
          rl.setPrompt(kContinuePrompt)
        } else {
          dumpException(e)
          inputs.length = 0
          rl.setPrompt(kNormalPrompt)
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

  // ================================================
  // Start

  const result = getOpts(argv, {
    '-c':            '',  // Compile only.
    '-e':            '<expressions>',  // Evaluate.
    '-h, --help':    '',  // Show help.
    '-v, --version': '',  // Show version.
  })

  const compileOnly = result.options.c

  if (result.options.v) {
    console.log(`JsLisp: version ${LISP['*version*']}`)
    return
  }
  if (result.options.h) {
    console.log('Usage:\n' +
                '\t-h, --help     Show help\n' +
                '\t-v, --version  Show version\n' +
                '\t-c             Compile only\n' +
                '\t-e             Evaluate argument as an expression\n')
    return
  }

  if (result.options.e != null)
    return runCodesWithErrorHandling(result.options.e, compileOnly)

  if (result.argv.length === 0) {  // No input file name: read from stdin.
    process.stdin.resume()
    process.stdin.setEncoding('utf8')

    if (tty.isatty(0))
      return repl()

    // Redirect
    return runCodesWithErrorHandling(LISP['*stdin*'], compileOnly)
  }

  // Process command line argument as a script file.
  try {
    LISP['*argv*'] = LISP['vector->list'](result.argv.slice(1))
    let status = 0
    if (compileOnly) {
      console.log('module.export = function(LISP) {')
      console.log('  \'use strict\';')
      let text = fs.readFileSync(result.argv[0], 'utf-8')
      const matchShebang = text.match(/^#!(.*)\n/)
      if (matchShebang)
        text = text.slice(matchShebang[0].length)
      runCodes(text, compileOnly)
      console.log('}')
    } else {
      LISP.load(result.argv[0])
      if ('main' in LISP)
        status = LISP.main(LISP.cons(result.argv[0], LISP['*argv*']))
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

  if (__non_webpack_require__.main === __module) {  // not imported.
    main(process.argv.slice(2))
  }
}

if (typeof window !== 'undefined') {
  const scriptTags = document.getElementsByTagName('script')
  const myScriptTag = scriptTags[scriptTags.length - 1]

  try {
    runCodes(myScriptTag.text)
  } catch (e) {
    dumpException(e)
  }
}
