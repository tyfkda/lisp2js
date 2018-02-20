((createLisp, installEval, installAux) => {
  'use strict'

  const g = ((typeof window !== 'undefined') ? window :
             (typeof global !== 'undefined') ? global : {})

  const LISP = createLisp(g)
  installEval(LISP)
  installAux(LISP)

  if (typeof module !== 'undefined')
    module.exports = LISP
  else
    g.LISP = LISP

  // Running on browser: Execute inner text.
  if (typeof document != 'undefined') {
    const getMyCode = () => {
      const currentScript = document.currentScript || (() => {
        const nodeList = document.getElementsByTagName('script')
        return nodeList.item(nodeList.length - 1)
      })()
      return currentScript.text
    }

    // Run Lisp codes.
    const runCodes = (codes) => {
      const stream = new LISP.StrStream(codes)
      for (;;) {
        const s = LISP.read(stream)
        if (s === undefined)
          break
        LISP.eval(s)
      }
    }

    runCodes(getMyCode())
  }
})((global) => {
  'use strict'

  const LISP = {}

  // Convert JS array into Lisp list.
  const arrayToList = (array) => {
    let result = LISP.nil
    for (let i = array.length; --i >= 0;)
      result = LISP.cons(array[i], result)
    return result
  }

  const jsBoolToS = x => x ? LISP.t : LISP.nil

  const makeString = (x, inspect) => {
    if (x === LISP.nil)
      return 'nil'
    if (x === LISP.t)
      return 't'
    if (typeof x == 'string')
      return inspect ? inspectString(x) : x
    if (x instanceof Array)
      return `#(${x.map(v => makeString(v, inspect)).join(' ')})`
    if (x == null)  // null or undefined
      return '' + x
    return x.toString(inspect)
  }

  LISP.nil = false
  LISP.t = true

  LISP.isTrue = (x) => {
    return x !== LISP.nil && x != null  // !(false || null || undefined)
  }

  LISP._getRestArgs = (args, start) => {
    return arrayToList(Array.prototype.slice.call(args, start))
  }
  LISP._output = (typeof process !== 'undefined'
                  ? (str) => {  // for node.js.
                    process.stdout.write(str)
                  } : (str) => {  // for browser.
                    console.log(str)
                  })

  {
    const macroTable = {}
    LISP['register-macro'] = (name, func) => {
      macroTable[name] = func
      return name
    }
    LISP['macroexpand-1'] = (s) => {
      if (!LISP['pair?'](s) || !(s.car in macroTable))
        return s
      const macrofn = macroTable[s.car]
      return LISP.apply(macrofn, s.cdr)
    }
  }

  LISP.error = function() {
    throw Array.prototype.slice.call(arguments).join(', ')
  }

  LISP.new = function(klass) {
    return new (Function.prototype.bind.apply(klass, arguments))
  }

  // Base class.
  class SObject {
  }

  // Symbol.
  class Symbol extends SObject {
    constructor(name) {
      super()
      this.name = name
    }

    static getTypeName() {
      return 'symbol'
    }

    toString() {
      return this.name
    }
  }

  LISP['symbol->string'] = x => x.name

  {
    const symbolTable = {}  // key(string) => Symbol object
    LISP.intern = (name) => {
      if (name in symbolTable)
        return symbolTable[name]
      return symbolTable[name] = new Symbol(name)
    }
  }
  {
    let index = 0
    LISP.gensym = () => {
      return LISP.intern(`__${++index}`)
    }
  }

  class Keyword extends SObject {
    constructor(name) {
      super()
      this.name = name
    }

    static getTypeName() {
      return 'keyword'
    }

    toString(inspect) {
      return inspect ? `:${this.name}` : this.name
    }
  }

  {
    const keywordTable = {}  // key(string) => Keyword object
    LISP['make-keyword'] = (name) => {
      if (name in keywordTable)
        return keywordTable[name]
      return keywordTable[name] = new Keyword(name)
    }
  }
  LISP['keyword->string'] = x => x.name

  LISP.type = (x) => {
    let type
    if (x === LISP.nil || x === LISP.t)
      type = 'bool'
    else {
      type = typeof x
      if (type === 'object') {
        if (x instanceof Array)
          type = 'vector'
        if (x instanceof RegExp)
          type = 'regexp'
        else if (x instanceof SObject)
          type = x.constructor.getTypeName()
      }
    }
    return LISP.intern(type)
  }

  LISP['eq?'] = (x, y) => jsBoolToS(x === y)

  // Cons cell.
  class Cons extends SObject {
    constructor(car, cdr, lineNo, path) {
      super()
      this.car = car
      this.cdr = cdr

      if (lineNo != null) {
        this.lineNo = lineNo
        this.path = path
      }
    }

    static getTypeName() {
      return 'pair'
    }

    toString(inspect) {
      const abbrev = Cons.canAbbrev(this)
      if (abbrev)
        return `${abbrev}${makeString(this.cdr.car, inspect)}`

      const ss = []
      let separator = '('
      let p
      for (p = this; p instanceof Cons; p = p.cdr) {
        ss.push(separator)
        ss.push(makeString(p.car, inspect))
        separator = ' '
      }
      if (p !== LISP.nil) {
        ss.push(' . ')
        ss.push(makeString(p, inspect))
      }
      ss.push(')')
      return ss.join('')
    }

    toArray() {
      const result = []
      for (let p = this; p instanceof Cons; p = p.cdr)
        result.push(p.car)
      return result
    }

    static canAbbrev(s) {
      const kAbbrevTable = {
        quote: '\'',
        quasiquote: '`',
        unquote: ',',
        'unquote-splicing': ',@',
      }
      return (s.car instanceof Symbol &&
              s.car.name in kAbbrevTable &&
              s.cdr instanceof Cons &&
              LISP['eq?'](s.cdr.cdr, LISP.nil)) ? kAbbrevTable[s.car.name] : false
    }
  }

  LISP.cons = (car, cdr) => new Cons(car, cdr)
  LISP.car = (s) => {
    if (s instanceof Cons)
      return s.car
    return s
  }
  LISP.cdr = (s) => {
    if (s instanceof Cons)
      return s.cdr
    return LISP.nil
  }
  LISP['set-car!'] = (s, x) => s.car = x
  LISP['set-cdr!'] = (s, x) => s.cdr = x

  LISP.list = function() {
    let result = LISP.nil
    for (let i = arguments.length; --i >= 0;)
      result = LISP.cons(arguments[i], result)
    return result
  }
  LISP['reverse!'] = (x) => {
    let rev = LISP.nil
    for (let ls = x; LISP['pair?'](ls);) {
      const d = ls.cdr
      ls.cdr = rev
      rev = ls
      ls = d
    }
    return rev
  }

  LISP['number->string'] = (x, n) => x.toString(n)
  LISP['+'] = function() {
    if (arguments.length == 0)
      return 0
    let result = arguments[0]
    for (let i = 1; i < arguments.length; ++i)
      result += arguments[i]
    return result
  }
  LISP['*'] = function() {
    if (arguments.length == 0)
      return 1
    let result = arguments[0]
    for (let i = 1; i < arguments.length; ++i)
      result *= arguments[i]
    return result
  }
  LISP['-'] = function() {
    if (arguments.length == 0)
      return 0
    let result = arguments[0]
    if (arguments.length == 1)
      return -result
    for (let i = 1; i < arguments.length; ++i)
      result -= arguments[i]
    return result
  }
  LISP['/'] = function() {
    if (arguments.length == 0)
      return 1
    let result = arguments[0]
    if (arguments.length == 1)
      return 1.0 / result
    for (let i = 1; i < arguments.length; ++i)
      result /= arguments[i]
    return result
  }
  LISP['%'] = function() {
    if (arguments.length == 0)
      return 0
    let result = arguments[0]
    if (arguments.length == 1)
      return result
    for (let i = 1; i < arguments.length; ++i)
      result %= arguments[i]
    return result
  }
  LISP['<'] = function() {
    if (arguments.length > 0) {
      let value = arguments[0]
      for (let i = 1; i < arguments.length; ++i) {
        const target = arguments[i]
        if (!(value < target))
          return LISP.nil
        value = target
      }
    }
    return LISP.t
  }
  LISP['>'] = function() {
    if (arguments.length > 0) {
      let value = arguments[0]
      for (let i = 1; i < arguments.length; ++i) {
        const target = arguments[i]
        if (!(value > target))
          return LISP.nil
        value = target
      }
    }
    return LISP.t
  }
  LISP['<='] = function() {
    if (arguments.length > 0) {
      let value = arguments[0]
      for (let i = 1; i < arguments.length; ++i) {
        const target = arguments[i]
        if (!(value <= target))
          return LISP.nil
        value = target
      }
    }
    return LISP.t
  }
  LISP['>='] = function() {
    if (arguments.length > 0) {
      let value = arguments[0]
      for (let i = 1; i < arguments.length; ++i) {
        const target = arguments[i]
        if (!(value >= target))
          return LISP.nil
        value = target
      }
    }
    return LISP.t
  }

  // String.
  LISP['string=?'] = (x, y) => jsBoolToS(x === y)
  LISP['string-append'] = function() {
    return Array.prototype.slice.call(arguments).join('')
  }
  LISP['string-join'] = (list, separator) => {
    if (list === LISP.nil)
      return ''
    return list.toArray().join(separator)
  }
  LISP['string-length'] = str => str.length
  LISP['string-ref'] = (str, index) => str[index]
  LISP.substring = (str, start, end) => str.slice(start, end)
  LISP['string-scan'] = (str, item) => {
    const index = str.indexOf(item)
    return index >= 0 ? index : LISP.nil
  }

  LISP['char->integer'] = (char, index) => char.charCodeAt(index)

  const kEscapeCharTable = {'\\': '\\\\', '\t': '\\t', '\n': '\\n', '"': '\\"'}
  const inspectString = (str) => {
    const f = (m) => {
      if (m in kEscapeCharTable)
        return kEscapeCharTable[m]
      return `\\x${('0' + m.charCodeAt(0).toString(16)).slice(-2)}`
    }
    return `"${str.replace(/[\x00-\x1f\"\\]/g, f)}"`
  }

  LISP['x->string'] = makeString
  LISP.print = (x, stream) => {
    const s = makeString(x)
    if (stream)
      stream.write(s)
    else
      LISP._output(s)
    return x
  }
  LISP.puts = (x) => {
    LISP._output(makeString(x))
    if (typeof process !== 'undefined')
      LISP._output('\n')
    return x
  }
  LISP.write = (x) => {
    LISP._output(makeString(x, 10))  // 10 means true, and it is used as radix.
    return x
  }

  LISP.apply = function(fn, ...params) {
    if (params.length > 0) {
      // Last argument for `apply` is expected as list (or nil).
      const last = params.pop()
      if (last !== LISP.nil)
        params = params.concat(last.toArray())
    }
    return fn.apply(null, params)
  }
  LISP.JS = global

  class HashTable extends SObject {
    static getTypeName() {
      return 'table'
    }

    toString() {
      let contents = ''
      for (const k in this) {
        if (!(this.hasOwnProperty(k)))
          continue
        if (contents.length > 0)
          contents += ', '
        contents += `${k}:${this[k]}`
      }
      return `#table<${contents}>`
    }
  }
  LISP.HashTable = HashTable

  // Hash table.
  LISP['make-hash-table'] = () => new HashTable()
  LISP['hash-table-exists?'] = (hash, x) => x in hash ? LISP.t : LISP.nil
  LISP['hash-table-get'] = function(hash, x, valueForNonExist = LISP.nil) {
    return (x in hash) ? hash[x] : valueForNonExist
  }
  LISP['hash-table-put!'] = (hash, x, value) => hash[x] = value

  // Vector.
  LISP.vector = function() {
    return Array.prototype.slice.call(arguments)
  }
  LISP['make-vector'] = (count, value) => {
    if (value === undefined)
      value = LISP.nil
    const vector = new Array(count)
    for (let i = 0; i < count; ++i)
      vector[i] = value
    return vector
  }
  LISP['vector-length'] = vector => vector.length
  LISP['vector-ref'] = (vector, index) => vector[index]
  LISP['vector-set!'] = (vector, index, value) => vector[index] = value

  // Regexp.
  LISP.rxmatch = (re, str) => jsBoolToS(re.exec(str))
  LISP['regexp-replace-all'] = (re, str, fn) => {
    if (!re.global) {
      const s = re.toString()
      const i = s.lastIndexOf('/')
      re = new RegExp(s.slice(1, i), `${s.slice(i + 1)}g`)
    }
    return str.replace(re, (match) => {
      return fn(() => {  // TODO: handle arguments.
        return match
      })
    })
  }
  LISP['regexp->string'] = (x) => {
    const s = x.toString()
    return s.slice(1, s.length - 1)
  }

  // Stream.
  class Stream extends SObject {
    constructor() {
      super()
      this.str = ''
      this.lineNo = 0
    }

    close() {}
    peek() {
      const result = this.fetch()
      if (result == null)
        return result
      return this.str[0]
    }
    getc() {
      const c = this.peek()
      if (c == null)
        return c
      this.str = this.str.slice(1)
      return c
    }
    ungetc(c) {
      if (this.str)
        this.str = c + this.str
      else
        this.str = c
    }
    match(regexp, keep) {
      const result = this.fetch()
      if (result == null)
        return result

      const m = this.str.match(regexp)
      if (m && !keep)
        this.str = RegExp.rightContext
      return m
    }
    eof() {
      return this.str == null
    }
    getLine() {
      const result = this.str || this.readLine()
      this.str = ''
      return result
    }
    fetch() {
      if (this.str == null)
        return null

      if (this.str === '') {
        if ((this.str = this.readLine()) == null)
          return undefined
        ++this.lineNo
      }
      return this.str
    }
  }

  class StrStream extends Stream {
    constructor(str) {
      super()
      this.str = str
      this.lineNo = 1
    }

    readLine() {
      return null
    }
  }
  LISP.StrStream = StrStream

  // Reader.
  LISP.NoCloseParenException = function() {}
  LISP.NoCloseQuoteException = function() {}

  const kDelimitors = '\\s(){}\\[\\]\'`,;#"'
  const kReSingleDot = RegExp(`^\\.(?=[${kDelimitors}])`)
  const kReSymbolOrNumber = RegExp(`^([^${kDelimitors}]+)`)
  const kReadUnescapeTable = {
    't': '\t',
    'n': '\n',
  }

  const readTable = {}

  class Reader {
    static read(stream) {
      do {
        if (stream.eof())
          return null
      } while (stream.match(/^(\s+|$)/))

      const c = stream.peek()
      if (c in readTable)
        return readTable[c](stream, stream.getc())

      let m
      if (stream.match(/^;[^\n]*\n?/))  // Line comment.
        return Reader.read(stream)
      if (m = stream.match(/^"/))  // string.
        return Reader.readString(stream)
      if (stream.match(/^#\(/))  // vector.
        return Reader.readVector(stream)
      if (m = stream.match(/^#\/([^\/]*)\//))  // regexp
        return new RegExp(m[1])  // TODO: Implement properly.
      if (stream.match(/^#\|(.|[\n\r])*?\|#/))  // Block comment.
        return Reader.read(stream)
      if (stream.match(kReSingleDot, true))  // Single dot.
        return undefined
      if (m = stream.match(kReSymbolOrNumber))  // Symbol or number.
        return Reader.readSymbolOrNumber(m[1])
      return undefined
    }

    static readSymbolOrNumber(str) {
      if (str === 'nil')
        return LISP.nil
      if (str === 't')
        return LISP.t
      if (str[0] === ':')
        return LISP['make-keyword'](str.slice(1))
      if (str.match(/^([+\-]?[0-9]+(\.[0-9]*)?)$/))  // Number.
        return parseFloat(str)
      return LISP.intern(str)
    }

    static readList(stream) {
      let result = LISP.nil
      for (;;) {
        const x = Reader.read(stream)
        if (x == null)
          break
        result = new Cons(x, result, stream.lineNo, stream.path)
      }

      if (stream.match(/^\s*\)/))  // Close paren.
        return LISP['reverse!'](result)
      if (stream.match(kReSingleDot)) {  // Dot.
        const last = Reader.read(stream)
        if (last != null) {
          if (stream.match(/^\s*\)/)) {  // Close paren.
            const reversed = LISP['reverse!'](result)
            result.cdr = last
            return reversed
          }
        }
      }
      // Error
      throw new LISP.NoCloseParenException()
    }

    static readString(stream) {
      let m
      if (m = stream.match(/^((\\.|[^"\\])*)"/))
        return Reader.unescape(m[1])
      // Error
      throw new LISP.NoCloseQuoteException()
    }

    static readVector(stream) {
      const result = []
      for (;;) {
        const x = Reader.read(stream)
        if (x == null)
          break
        result.push(x)
      }

      if (stream.match(/^\s*\)/))  // Close paren.
        return result
      // Error
      throw new LISP.NoCloseParenException()
    }

    static unescape(str) {
      return str.replace(/\\(x([0-9a-fA-F]{2})|(.))/g, (_1, _2, hex, c) => {
        if (hex)
          return String.fromCharCode(parseInt(hex, 16))
        if (c in kReadUnescapeTable)
          return kReadUnescapeTable[c]
        return c
      })
    }

    static setMacroCharacter(c, fn) {
      readTable[c] = fn
    }
  }

  LISP['set-macro-character'] = Reader.setMacroCharacter

  Reader.setMacroCharacter('(', (stream, _c) =>  // Left paren '('.
                           Reader.readList(stream))

  LISP.read = stream => Reader.read(stream || LISP['*stdin*'])

  LISP['read-from-string'] = str => Reader.read(new StrStream(str))

  LISP['read-line'] = (stream) => {
    return (stream || LISP['*stdin*']).getLine()
  }

  LISP['read-char'] = (stream = LISP['*stdin*']) => {
    return stream.getc()
  }

  LISP['unread-char'] = (c, stream = LISP['*stdin*']) => {
    stream.ungetc(c)
    return LISP.nil
  }

  // For node JS.
  if (typeof process !== 'undefined') {
    const fs = require('fs')

    const BUFFER_SIZE = 4096
    const buffer = new Buffer(BUFFER_SIZE)
    class FileStream extends Stream {
      constructor(fd, path) {
        super()
        this.fd = fd
        this.path = path
        this.lines = []
        this.index = 0
      }

      close() {
        if (this.fd == null)
          return
        fs.closeSync(this.fd)
        this.fd = null
        this.lines.length = this.index = 0
        this.str = null
        this.chomped = false
      }
      readLine() {
        for (;;) {
          let left = ''
          if (this.index < this.lines.length) {
            if (this.index < this.lines.length - 1 || !this.chomped)
              return this.lines[this.index++]
            if (this.chomped)
              left = this.lines[this.index]
          }

          if (this.fd == null)
            return LISP.nil
          const n = fs.readSync(this.fd, buffer, 0, BUFFER_SIZE)
          if (n <= 0)
            return null
          let string = left + buffer.slice(0, n).toString()
          this.chomped = false
          if (string.length > 0) {
            if (string[string.length - 1] != '\n')
              this.chomped = true
            else
              // Remove last '\n' to avoid last empty line.
              string = string.slice(0, string.length - 1)
          }
          this.lines = string.split('\n')
          this.index = 0
        }
      }
      write(s) {
        fs.write(this.fd, s)
      }
    }
    LISP.FileStream = FileStream

    LISP['*stdin*'] = new FileStream(process.stdin.fd, '*stdin*')
    LISP['*stdout*'] = new FileStream(process.stdout.fd, '*stdout*')
    LISP['*stderr*'] = new FileStream(process.stderr.fd, '*stderr*')

    LISP.open = (path, flag) => {
      try {
        const fd = fs.openSync(path, flag || 'r')
        return new LISP.FileStream(fd, path)
      } catch (e) {
        return LISP.nil
      }
    }

    LISP.close = (stream) => {
      stream.close()
      return stream
    }

    LISP.load = (fileSpec) => {
      let stream
      if (typeof fileSpec == 'string') {
        stream = LISP.open(fileSpec)
        if (!stream)
          return LISP.error(`Cannot open [${fileName}]`)
      } else if (fileSpec instanceof Stream)
        stream = fileSpec
      else
        return LISP.error(`Illegal fileSpec: ${fileSpec}`)

      if (stream.match(/^#!/, true))
        stream.getLine()  // Skip Shebang.

      let result
      for (;;) {
        const s = LISP.read(stream)
        if (s == null)
          break
        result = LISP.eval(s)
      }
      if (stream !== fileSpec)
        LISP.close(stream)
      return result
    }

    // System
    LISP.exit = (code) => process.exit(code)

    LISP.jsrequire = require
  }

  return LISP
}, (LISP) => {
  // Using eval JS function prevent uglify to mangle local variable names,
  // so put such code here.
  LISP.eval = (exp) => eval(LISP.compile(exp))
}, (/*eslint no-unused-vars: 0*/LISP) => {
  /*==== EMBED COMPILED CODE HERE ====*/
})
