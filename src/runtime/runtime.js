'use strict'

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

    while (this.str === '') {
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

const inspectString = (str) => {
  const kEscapeCharTable = {'\\': '\\\\', '\t': '\\t', '\n': '\\n', '"': '\\"'}
  const f = (m) => {
    if (m in kEscapeCharTable)
      return kEscapeCharTable[m]
    return `\\x${('0' + m.charCodeAt(0).toString(16)).slice(-2)}`
  }
  return `"${str.replace(/[\x00-\x1f\"\\]/g, f)}"`
}

const makeString = (x, inspect) => {
  if (x === LISP.nil)
    return 'nil'
  if (x === LISP.t)
    return 't'

  switch (typeof x) {
  case 'string':
    return inspect ? inspectString(x) : x
  case 'function':
    return x.name ? `#<function ${x.name}>` : `#<function>`
  default:
    if (x instanceof Array)
      return `#(${x.map(v => makeString(v, inspect)).join(' ')})`
    if (x == null)  // null or undefined
      return '' + x
    return x.toString(inspect)
  }
}

const LISP = ((createLisp, installEval) => {
  'use strict'

  const g = ((typeof window !== 'undefined') ? window :
             (typeof global !== 'undefined') ? global : {})

  const LISP = createLisp(g)
  installEval(LISP)

  return LISP
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
    LISP['register-macro'] = function register$2dmacro(name, func) {
      macroTable[name] = func
      return name
    }
    LISP['macroexpand-1'] = function macroexpand$2d1(s) {
      if (!LISP['pair?'](s) || !(s.car in macroTable))
        return s
      const macrofn = macroTable[s.car]
      return LISP.apply(macrofn, s.cdr)
    }
  }

  LISP.error = function error() {
    throw new Error(Array.prototype.slice.call(arguments).join(', '))
  }

  LISP.new = function _new(klass) {
    return new (Function.prototype.bind.apply(klass, arguments))
  }

  LISP['symbol->string'] = function symbol$2d$3estring(x) { return x.name }

  {
    const symbolTable = {}  // key(string) => Symbol object
    LISP.intern = function intern(name) {
      if (name in symbolTable)
        return symbolTable[name]
      return symbolTable[name] = new Symbol(name)
    }
  }
  {
    let index = 0
    LISP.gensym = function gensym() {
      return LISP.intern(`__${++index}`)
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
  LISP['keyword->string'] = function keyword$2d$3estring(x) { return x.name }

  LISP.type = function type(x) {
    let t
    if (x === LISP.nil || x === LISP.t)
      t = 'bool'
    else {
      t = typeof x
      if (t === 'object') {
        if (x instanceof Array)
          t = 'vector'
        if (x instanceof RegExp)
          t = 'regexp'
        else if (x instanceof SObject)
          t = x.constructor.getTypeName()
      }
    }
    return LISP.intern(t)
  }

  LISP['eq?'] = function eq$3f(x, y) { return jsBoolToS(x === y) }

  LISP.cons = function cons(car, cdr) { return new Cons(car, cdr) }
  LISP.car = function car(s) {
    if (s instanceof Cons)
      return s.car
    return s
  }
  LISP.cdr = function cdr(s) {
    if (s instanceof Cons)
      return s.cdr
    return LISP.nil
  }
  LISP['set-car!'] = function set$2dcar$21(s, x) { return s.car = x }
  LISP['set-cdr!'] = function set$2dcdr$21(s, x) { return s.cdr = x }

  LISP.list = function list() {
    let result = LISP.nil
    for (let i = arguments.length; --i >= 0;)
      result = LISP.cons(arguments[i], result)
    return result
  }
  LISP['reverse!'] = function reverse$21(x) {
    let rev = LISP.nil
    for (let ls = x; LISP['pair?'](ls);) {
      const d = ls.cdr
      ls.cdr = rev
      rev = ls
      ls = d
    }
    return rev
  }

  LISP['number->string'] = function number$2d$3estring(x, n) { return x.toString(n) }
  LISP['+'] = function $2b() {
    if (arguments.length === 0)
      return 0
    let result = arguments[0]
    for (let i = 1; i < arguments.length; ++i)
      result += arguments[i]
    return result
  }
  LISP['*'] = function $2a() {
    if (arguments.length === 0)
      return 1
    let result = arguments[0]
    for (let i = 1; i < arguments.length; ++i)
      result *= arguments[i]
    return result
  }
  LISP['-'] = function $2d() {
    if (arguments.length === 0)
      return 0
    let result = arguments[0]
    if (arguments.length === 1)
      return -result
    for (let i = 1; i < arguments.length; ++i)
      result -= arguments[i]
    return result
  }
  LISP['/'] = function $2f() {
    if (arguments.length === 0)
      return 1
    let result = arguments[0]
    if (arguments.length === 1)
      return 1.0 / result
    for (let i = 1; i < arguments.length; ++i)
      result /= arguments[i]
    return result
  }
  LISP['%'] = function $25() {
    if (arguments.length === 0)
      return 0
    let result = arguments[0]
    if (arguments.length === 1)
      return result
    for (let i = 1; i < arguments.length; ++i)
      result %= arguments[i]
    return result
  }
  LISP['<'] = function $3c() {
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
  LISP['>'] = function $3e() {
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
  LISP['<='] = function $3c$3d() {
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
  LISP['>='] = function $3e$3d() {
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
  LISP['string=?'] = function string$3d$3f(x, y) { return jsBoolToS(x === y) }
  LISP['string-append'] = function string_append() {
    return Array.prototype.slice.call(arguments).join('')
  }
  LISP['string-join'] = function string$2djoin(list, separator) {
    if (list === LISP.nil)
      return ''
    return list.toArray().join(separator)
  }
  LISP['string-length'] = function string$2dlength(str) { return str.length }
  LISP['string-ref'] = function string$2dref(str, index) { return str[index] }
  LISP.substring = function substring(str, start, end) { return str.slice(start, end) }
  LISP['string-scan'] = function string$2dscan(str, item) {
    const index = str.indexOf(item)
    return index >= 0 ? index : LISP.nil
  }
  LISP['string->number'] = function string$2d$3enumber(str, radix) {
    return radix ? parseInt(str, radix) : parseFloat(str)
  }

  LISP['char->number'] = function char$2d$3enumber(char, index = 0) { return char.charCodeAt(index) }
  LISP['number->char'] = function number$2d$3echar(num) { return String.fromCharCode(num) }

  LISP['x->string'] = makeString
  LISP.print = function print(x, stream) {
    const s = makeString(x)
    if (stream)
      stream.write(s)
    else
      LISP._output(s)
    return x
  }
  LISP.puts = function puts(x) {
    LISP._output(makeString(x))
    if (typeof process !== 'undefined')
      LISP._output('\n')
    return x
  }
  LISP.write = function write(x) {
    LISP._output(makeString(x, 10))  // 10 means true, and it is used as radix.
    return x
  }

  LISP.apply = function apply(fn, ...params) {
    if (params.length > 0) {
      // Last argument for `apply` is expected as list (or nil).
      const last = params.pop()
      if (last !== LISP.nil)
        params = params.concat(last.toArray())
    }
    return fn.apply(null, params)
  }
  LISP.JS = global

  // Hash table.
  LISP['make-hash-table'] = function make$2dhash$2dtable() { return new HashTable() }
  LISP['hash-table-exists?'] = function hash$2dtable$2dexists$3f(hash, x) { return x in hash ? LISP.t : LISP.nil }
  LISP['hash-table-get'] = function hash$2dtable$2dget(hash, x, valueForNonExist = LISP.nil) {
    return (x in hash) ? hash[x] : valueForNonExist
  }
  LISP['hash-table-put!'] = function hash$2dtable$2dput$21(hash, x, value) { return hash[x] = value }

  // Vector.
  LISP.vector = function vector() {
    return Array.prototype.slice.call(arguments)
  }
  LISP['make-vector'] = function make$2dvector(count, value) {
    if (value === undefined)
      value = LISP.nil
    const vector = new Array(count)
    for (let i = 0; i < count; ++i)
      vector[i] = value
    return vector
  }
  LISP['vector-length'] = function vector$2dlength(vector) { return vector.length }
  LISP['vector-ref'] = function vector$2dref(vector, index) { return vector[index] }
  LISP['vector-set!'] = function vector$2dset$21(vector, index, value) { return vector[index] = value }

  // Regexp.
  LISP.regexp = function regexp(str) { return new RegExp(str) }
  LISP.rxmatch = function rxmatch(re, str) { return jsBoolToS(re.exec(str)) }
  LISP['regexp-replace-all'] = function regexp$2dreplace$2dall(re, str, fn) {
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
  LISP['regexp->string'] = function regexp$2d$3estring(x) {
    const s = x.toString()
    return s.slice(1, s.length - 1)
  }

  LISP['make-string-input-stream'] = function make$2dstring$2dinput$2dstream(str, start, end) {
    if (typeof start !== 'undefined' || typeof end !== 'undefined')
      str = str.slice(start | 0, end | str.length)
    return new StrStream(str)
  }

  // Reader.
  LISP.NoCloseParenException = function() {}
  LISP.NoCloseQuoteException = function() {}
  LISP.UnexpectedCharacterException = function(char) {
    this.char = char
  }

  const kDelimitors = '\\s(){}\\[\\]\'`,;#"'
  const kReSingleDot = new RegExp(`^\\.(?=([${kDelimitors}]|$))`)
  const kReSymbolOrNumber = new RegExp(`^([^.${kDelimitors}][^${kDelimitors}]*)`)

  const readTable = {
    dispatchTable: {},
  }

  const dispatchMacroCharacter = function(stream, c) {
    const table = readTable.dispatchTable[c]
    const subc = stream.peek()
    if (subc in table)
      return table[subc](stream, c, stream.getc())
    // Throw
    throw new Error(`No dispatch macro character: ${c}${subc}`)
  }

  class Reader {
    static skipWhitespaces(stream) {
      do {
        if (stream.eof())
          return false
      } while (stream.match(/^(\s+|$)/))
      return true
    }

    static read(stream) {
      if (!Reader.skipWhitespaces(stream))
        return null

      const c = stream.peek()
      if (c == null)
        return null
      if (c in readTable)
        return readTable[c](stream, stream.getc())

      let m
      if (stream.match(kReSingleDot, true))  // Single dot.
        return undefined
      if (m = stream.match(kReSymbolOrNumber))  // Symbol or number.
        return Reader.readSymbolOrNumber(m[1])

      throw new LISP.UnexpectedCharacterException(stream.peek())
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
        Reader.skipWhitespaces(stream)
        let c = stream.peek()
        if (c === ')') {  // Close paren.
          stream.getc()
          return LISP['reverse!'](result)
        }
        if (stream.match(kReSingleDot)) {  // Dot.
          const last = Reader.read(stream)
          if (last != null) {
            if (stream.match(/^\s*\)/)) {  // Close paren.
              const reversed = LISP['reverse!'](result)
              result.cdr = last
              return reversed
            }
          }
          break
        }

        const x = Reader.read(stream)
        if (x == null)
          break
        result = new Cons(x, result, stream.lineNo, stream.path)
      }

      // Error
      throw new LISP.NoCloseParenException()
    }

    static setMacroCharacter(c, fn) {
      readTable[c] = fn
    }

    static setDispatchMacroCharacter(c, subc, fn) {
      if (!(c in readTable.dispatchTable)) {
        readTable.dispatchTable[c] = {}
      }
      readTable.dispatchTable[c][subc] = fn
      readTable[c] = dispatchMacroCharacter
    }
  }

  LISP['set-macro-character'] = Reader.setMacroCharacter
  LISP['set-dispatch-macro-character'] = Reader.setDispatchMacroCharacter

  Reader.setMacroCharacter('(', (stream, _c) =>  // Left paren '('.
                           Reader.readList(stream))

  LISP.read = function read(stream = LISP['*stdin*'], err, eofval) {
    const result = Reader.read(stream)
    if (result != null)
      return result
    if (LISP.isTrue(err)) {
      throw new Error('EOF')
    }
    return eofval != null ? eofval : LISP.nil
  }

  LISP['read-from-string'] = function read$2dfrom$2dstring(str) { return Reader.read(new StrStream(str)) }

  LISP['read-line'] = function read$2dline(stream = LISP['*stdin*']) {
    let line = stream.getLine()
    if (line == null)
      return LISP.nil
    // chomp
    if (line.length > 0 && line[line.length - 1] === '\n')
      line = line.slice(0, line.length - 1)
    return line
  }

  LISP['read-char'] = function read$2dchar(stream = LISP['*stdin*']) {
    let c = stream.getc()
    return c != null ? c : LISP.nil
  }

  LISP['unread-char'] = function unread$2dchar(c, stream = LISP['*stdin*']) {
    stream.ungetc(c)
    return LISP.nil
  }

  return LISP
}, (LISP) => {
  // Using eval JS function prevent uglify to mangle local variable names,
  // so put such code here.
  LISP.eval = function _eval(exp) { return eval(LISP.compile(exp)) }
})

module.exports = {
  LISP,
  Stream,
}
