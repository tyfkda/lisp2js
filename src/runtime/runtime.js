((createLisp, installEval) => {
  'use strict'

  var g = ((typeof window !== 'undefined') ? window :
           (typeof GLOBAL !== 'undefined') ? GLOBAL : {})

  let LISP = createLisp(g)
  installEval(LISP)

  if (typeof module !== 'undefined')
    module.exports = LISP
  else
    g.LISP = LISP
})((global) => {
  'use strict'

  let LISP = {}

  // Convert JS array into Lisp list.
  let arrayToList = (array) => {
    let result = LISP.nil
    for (let i = array.length; --i >= 0; )
      result = LISP.cons(array[i], result)
    return result
  }

  let jsBoolToS = x => x ? LISP.t : LISP.nil
  let arguments2Array = (args, start) => {
    let len = args.length - start
    if (len <= 0)
      return []
    let array = new Array(len)
    for (let i = 0; i < len; ++i)
      array[i] = args[i + start]
    return array
  }

  let makeString = (x, inspect) => {
    if (x === LISP.nil)
      return 'nil'
    if (x === LISP.t)
      return 't'
    if (typeof x == 'string')
      return inspect ? inspectString(x) : x
    if (x instanceof Array)
      return '#(' + x.map(v => makeString(v, inspect)).join(' ') + ')'
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
  LISP._output = (typeof(process) !== 'undefined'
                  ? (str) => {  // for node.js.
                    process.stdout.write(str)
                  } : (str) => {  // for browser.
                    console.log(str)
                  })

  let macroTable = {}
  LISP['register-macro'] = (name, func) => {
    macroTable[name] = func
    return name
  }
  LISP['macroexpand-1'] = (s) => {
    if (!LISP['pair?'](s) || !(s.car in macroTable))
      return s
    let macrofn = macroTable[s.car]
    return LISP.apply(macrofn, s.cdr)
  }

  LISP.error = function() {
    throw arguments2Array(arguments, 0).join(', ')
  }

  // Symbol.
  let Symbol = function(name) {
    this.name = name
  }
  Symbol.prototype = {
    toString: function() {
      return this.name
    },
  }

  LISP['symbol->string'] = x => x.name

  LISP.intern = (() => {
    const symbolTable = {}  // key(string) => Symbol object
    return (name) => {
      if (name in symbolTable)
        return symbolTable[name]
      return symbolTable[name] = new Symbol(name)
    }
  })()
  LISP.gensym = (() => {
    let index = 0
    return () => {
      return LISP.intern('__' + (++index))
    }
  })()
  LISP['symbol?'] = x => jsBoolToS(x instanceof Symbol)

  let Keyword = function(name) {
    this.name = name
  }
  Keyword.prototype = {
    toString: function(inspect) {
      return inspect ? ':' + this.name : this.name
    },
  }
  LISP['make-keyword'] = (() => {
    const keywordTable = {}  // key(string) => Keyword object
    return (name) => {
      if (name in keywordTable)
        return keywordTable[name]
      return keywordTable[name] = new Keyword(name)
    }
  })()
  LISP['keyword?'] = x => jsBoolToS(x instanceof Keyword)
  LISP['keyword->string'] = x => x.name

  LISP.type = (x) => {
    let type
    if (x === LISP.nil || x === LISP.t)
      type = 'bool'
    else {
      type = typeof x
      if (type === 'object') {
        if (x instanceof Symbol)
          type = 'symbol'
        else if (x instanceof Keyword)
          type = 'keyword'
        else if (x instanceof Cons)
          type = 'pair'
        else if (x instanceof Array)
          type = 'vector'
        else if (x instanceof LISP.HashTable)
          type = 'table'
      }
    }
    return LISP.intern(type)
  }

  LISP['eq?'] = (x, y) => jsBoolToS(x === y)

  // Cons cell.
  let Cons = function(car, cdr, lineNo, path) {
    this.car = car
    this.cdr = cdr

    if (lineNo != null) {
      this.lineNo = lineNo
      this.path = path
    }
  }

  Cons.prototype = {
    toString: (() => {
      let abbrevTable = { quote: "'", quasiquote: '`', unquote: ',', 'unquote-splicing': ',@' }
      return function(inspect) {
        if (this.car instanceof Symbol &&  // (symbol? car)
            this.cdr instanceof Cons &&    // (pair? cdr)
            this.cdr.cdr &&                // (null? (cdr cdr))
            this.car.name in abbrevTable) {
          return abbrevTable[this.car.name] + makeString(this.cdr.car, inspect)
        }

        let ss = []
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
    })(),
    toArray: function() {
      let result = []
      for (let p = this; p instanceof Cons; p = p.cdr)
        result.push(p.car)
      return result
    },
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
  LISP['set-car!'] = (s, x) => (s.car = x)
  LISP['set-cdr!'] = (s, x) => (s.cdr = x)

  LISP['pair?'] = x => jsBoolToS(x instanceof Cons)
  LISP.list = function() {
    let result = LISP.nil
    for (let i = arguments.length; --i >= 0; )
      result = LISP.cons(arguments[i], result)
    return result
  }
  LISP['reverse!'] = (x) => {
    let rev = LISP.nil
    for (let ls = x; LISP['pair?'](ls); ) {
      let d = ls.cdr
      ls.cdr = rev
      rev = ls
      ls = d
    }
    return rev
  }

  LISP['number?'] = x => jsBoolToS(typeof x === 'number')
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
        let target = arguments[i]
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
        let target = arguments[i]
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
        let target = arguments[i]
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
        let target = arguments[i]
        if (!(value >= target))
          return LISP.nil
        value = target
      }
    }
    return LISP.t
  }

  // String.
  LISP['string?'] = x => jsBoolToS(typeof x === 'string')
  LISP['string=?'] = (x, y) => jsBoolToS(x === y)
  LISP['string-append'] = function() {
    return arguments2Array(arguments, 0).join('')
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
    let index = str.indexOf(item)
    return index >= 0 ? index : LISP.nil
  }

  LISP['char->integer'] = (char, index) => char.charCodeAt(index)

  let kEscapeCharTable = { '\\': '\\\\', '\t': '\\t', '\n': '\\n', '"': '\\"' }
  let inspectString = (str) => {
    let f = (m) => {
      if (m in kEscapeCharTable)
        return kEscapeCharTable[m]
      return '\\x' + ('0' + m.charCodeAt(0).toString(16)).slice(-2)
    }
    return '"' + str.replace(/[\x00-\x1f"\\]/g, f) + '"'
  }

  LISP['x->string'] = makeString
  LISP.print = (x) => {
    LISP._output(makeString(x))
    return x
  }
  LISP.puts = (x) => {
    LISP._output(makeString(x))
    if (typeof(process) !== 'undefined')
      LISP._output('\n')
    return x
  }
  LISP.write = (x) => {
    LISP._output(makeString(x, 10))  // 10 means true, and it is used as radix.
    return x
  }

  LISP.apply = function(fn) {
    let params = []
    if (arguments.length > 1) {
      for (let i = 1; i < arguments.length - 1; ++i)
        params.push(arguments[i])
      // Last argument for `apply` is expected as list (or nil).
      let last = arguments[arguments.length - 1]
      if (last !== LISP.nil)
        params = params.concat(last.toArray())
    }
    return fn.apply(null, params)
  }
  LISP.JS = global

  LISP.HashTable = function() {}
  LISP.HashTable.prototype = {
    toString: function() {
      let contents = ''
      for (let k in this) {
        if (!(this.hasOwnProperty(k)))
          continue
        if (contents.length > 0)
          contents += ', '
        contents += k + ':' + this[k]
      }
      return '#table<' + contents + '>'
    },
  }

  // Hash table.
  LISP['make-hash-table'] = () => new LISP.HashTable()
  LISP['hash-table?'] = x => x instanceof LISP.HashTable
  LISP['hash-table-exists?'] = (hash, x) => x in hash ? LISP.t : LISP.nil
  LISP['hash-table-get'] = function(hash, x) {
    if (x in hash)
      return hash[x]
    return (arguments.length >= 3) ? arguments[3 - 1] : LISP.nil
  }
  LISP['hash-table-put!'] = (hash, x, value) => hash[x] = value

  // Vector.
  LISP.vector = function() {
    return arguments2Array(arguments, 0)
  }
  LISP['make-vector'] = (count, value) => {
    if (value === undefined)
      value = LISP.nil
    let vector = new Array(count)
    for (let i = 0; i < count; ++i)
      vector[i] = value
    return vector
  }
  LISP['vector?'] = x => jsBoolToS(x instanceof Array)
  LISP['vector-length'] = vector => vector.length
  LISP['vector-ref'] = (vector, index) => vector[index]
  LISP['vector-set!'] = (vector, index, value) => vector[index] = value

  // Regexp.
  LISP['regexp?'] = x => jsBoolToS(x instanceof RegExp)
  LISP.rxmatch = (re, str) => jsBoolToS(re.exec(str))
  LISP['regexp-replace-all'] = (re, str, fn) => {
    if (!re.global) {
      let s = re.toString()
      let i = s.lastIndexOf('/')
      re = new RegExp(s.slice(1, i), s.slice(i + 1) + 'g')
    }
    return str.replace(re, (match) => {
      return fn(() => {  // TODO: handle arguments.
        return match
      })
    })
  }
  LISP['regexp->string'] = (x) => {
    let s = x.toString()
    return s.slice(1, s.length - 1)
  }

  // Stream.
  let Stream = function() {
    this.str = ''
    this.lineNo = 0
  }
  Stream.prototype = {
    close: function() {},
    peek: function() {
      let result = this.fetch()
      if (result == null)
        return result
      return this.str[0]
    },
    getc: function() {
      let c = this.peek()
      if (c == null)
        return c
      this.str = this.str.slice(1)
      return c
    },
    match: function(regexp, keep) {
      let result = this.fetch()
      if (result == null)
        return result

      let m = this.str.match(regexp)
      if (m && !keep)
        this.str = RegExp.rightContext
      return m
    },
    eof: function() {
      return this.str == null
    },
    getLine: function() {
      let result = this.str || this.readLine()
      this.str = ''
      return result
    },
    fetch: function() {
      if (this.str == null)
        return null

      if (this.str === '') {
        if ((this.str = this.readLine()) == null)
          return undefined
        ++this.lineNo
      }
      return this.str
    },
  }

  let StrStream = function(str) {
    Stream.call(this)
    this.str = str
    this.lineNo = 1
  }
  StrStream.prototype = Object.create(Stream.prototype)
  StrStream.prototype.readLine = function() {
    return null
  }
  LISP.StrStream = StrStream

  // Reader.
  LISP.NoCloseParenException = function() {}

  let kDelimitors = '\\s(){}\\[\\]\'`,;#"'
  let kReSingleDot = RegExp('^\\.(?=[' + kDelimitors + '])')
  let kReSymbolOrNumber = RegExp('^([^' + kDelimitors + ']+)')
  let kReadUnescapeTable = {
    't': '\t',
    'n': '\n',
  }

  let readTable = {}

  let Reader = {
    read: (stream) => {
      do {
        if (stream.eof())
          return null
      } while (stream.match(/^\s+/))

      let c = stream.peek()
      if (c in readTable)
        return readTable[c](stream, stream.getc())

      let m
      if (stream.match(/^\(/))  // Left paren '('.
        return Reader.readList(stream)
      if (stream.match(/^;[^\n]*\n?/))  // Line comment.
        return Reader.read(stream)
      if (m = stream.match(/^"((\\.|[^"\\])*)"/))  // string.
        return Reader.unescape(m[1])
      if (stream.match(/^#\(/))  // vector.
        return Reader.readVector(stream)
      if (m = stream.match(/^#\/([^\/]*)\//))  // regexp TODO: Implement properly.
        return new RegExp(m[1])
      if (stream.match(/^#\|(.|[\n\r])*?\|#/))  // Block comment.
        return Reader.read(stream)
      if (stream.match(kReSingleDot, true))  // Single dot.
        return undefined
      if (m = stream.match(kReSymbolOrNumber))  // Symbol or number.
        return Reader.readSymbolOrNumber(m[1])
      return undefined
    },

    readSymbolOrNumber: (str) => {
      if (str === 'nil')
        return LISP.nil
      if (str === 't')
        return LISP.t
      if (str[0] === ':')
        return LISP['make-keyword'](str.slice(1))
      if (str.match(/^([+\-]?[0-9]+(\.[0-9]*)?)$/))  // Number.
        return parseFloat(str)
      return LISP.intern(str)
    },

    readList: (stream) => {
      let result = LISP.nil
      for (;;) {
        let x = Reader.read(stream)
        if (x != null) {
          result = new Cons(x, result, stream.lineNo, stream.path)
          continue
        }

        if (stream.match(/^\s*\)/)) {  // Close paren.
          return LISP['reverse!'](result)
        }
        if (stream.match(kReSingleDot)) {  // Dot.
          let last = Reader.read(stream)
          if (last != null) {
            if (stream.match(/^\s*\)/)) {  // Close paren.
              let reversed = LISP['reverse!'](result)
              result.cdr = last
              return reversed
            }
          }
        }
        // Error
        throw new LISP.NoCloseParenException()
      }
    },

    readVector: (stream) => {
      let result = []
      for (;;) {
        let x = Reader.read(stream)
        if (x !== undefined) {
          result.push(x)
          continue
        }

        if (stream.match(/^\s*\)/)) {  // Close paren.
          return result
        }
        // Error
        throw new LISP.NoCloseParenException()
      }
    },

    unescape: (str) => {
      return str.replace(/\\(x([0-9a-fA-F]{2})|(.))/g, (_1, _2, hex, c) => {
        if (hex)
          return String.fromCharCode(parseInt(hex, 16))
        if (c in kReadUnescapeTable)
          return kReadUnescapeTable[c]
        return c
      })
    },
  }

  LISP['set-macro-character'] = (c, fn) => {
    readTable[c] = fn
  }

  LISP['set-macro-character']("'", (stream, c) => {
    return LISP.list(LISP.intern('quote'), Reader.read(stream))
  })
  LISP['set-macro-character']('`', (stream, c) => {
    return LISP.list(LISP.intern('quasiquote'), Reader.read(stream))
  })
  LISP['set-macro-character'](',', (stream, c) => {
    let c2 = stream.peek()
    let keyword = 'unquote'
    if (c2 == '@') {
      keyword = 'unquote-splicing'
      stream.getc()
    }
    return LISP.list(LISP.intern(keyword), Reader.read(stream))
  })

  LISP.read = stream => Reader.read(stream || LISP['*stdin*'])

  LISP['read-from-string'] = str => Reader.read(new StrStream(str))

  LISP['read-line'] = (stream) => {
    return (stream || LISP['*stdin*']).getLine()
  }


  // For node JS.
  if (typeof process !== 'undefined') {
    const fs = require('fs')

    LISP.FileStream = (() => {
      const BUFFER_SIZE = 4096
      let buffer = new Buffer(BUFFER_SIZE)
      let FileStream = function(fd, path) {
        Stream.call(this)
        this.fd = fd
        this.path = path
        this.lines = []
        this.index = 0
      }
      FileStream.prototype = Object.create(Stream.prototype)
      FileStream.prototype.close = function() {
        if (this.fd == null)
          return
        fs.closeSync(this.fd)
        this.fd = null
        this.lines.length = this.index = 0
        this.str = null
        this.chomped = false
      }
      FileStream.prototype.readLine = function() {
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
          let n = fs.readSync(this.fd, buffer, 0, BUFFER_SIZE)
          if (n <= 0)
            return null
          let string = left + buffer.slice(0, n).toString()
          this.chomped = false
          if (string.length > 0) {
            if (string[string.length - 1] != '\n')
              this.chomped = true
            else
              string = string.slice(0, string.length - 1)  // Remove last '\n' to avoid last empty line.
          }
          this.lines = string.split('\n')
          this.index = 0
        }
      }
      return FileStream
    })()

    LISP['*stdin*'] = new LISP.FileStream(process.stdin.fd, '*stdin*')
    LISP['*stdout*'] = new LISP.FileStream(process.stdout.fd, '*stdout*')
    LISP['*stderr*'] = new LISP.FileStream(process.stderr.fd, '*stderr*')

    LISP.open = (path, flag) => {
      try {
        let fd = fs.openSync(path, flag || 'r')
        return new LISP.FileStream(fd, path)
      } catch (e) {
        return LISP.nil
      }
    }

    LISP.close = (stream) => {
      stream.close()
      return stream
    }

    LISP.load = (fileName) => {
      let stream = LISP.open(fileName)
      if (!stream) {
        return LISP.error('Cannot open [' + fileName + ']')
      }

      if (stream.match(/^#!/, true))
        stream.getLine()  // Skip Shebang.

      let result
      for (;;) {
        let s = LISP.read(stream)
        if (s == null)
          break
        result = LISP.eval(s)
      }
      LISP.close(stream)
      return result
    }

    // System
    LISP.exit = (code) => process.exit(code)

    LISP.jsrequire = require
  }

  /*==== EMBED COMPILED CODE HERE ====*/

  return LISP
}, (LISP) => {
  // Using eval JS function prevent uglify to mangle local variable names,
  // so put such code here.
  LISP.eval = (exp) => eval(LISP.compile(exp))
})
