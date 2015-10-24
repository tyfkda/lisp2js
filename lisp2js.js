(function(scope, val) {
  'use strict';
  if (typeof module !== 'undefined')
    module.exports = val;
  else
    scope.LISP = val;
})(this, (function() {
  'use strict';

  var LISP = {};

  // Convert JS array into Lisp list.
  var arrayToList = function(array) {
    var result = LISP.nil;
    for (var i = array.length; --i >= 0; )
      result = LISP.cons(array[i], result);
    return result;
  };

  function jsBoolToS(x)  { return x ? LISP.t : LISP.nil; }
  function arguments2Array(args, start) {
    var len = args.length - start;
    if (len <= 0)
      return [];
    var array = new Array(len);
    for (var i = 0; i < len; ++i)
      array[i] = args[i + start];
    return array;
  };

  function makeString(x, inspect) {
    if (x === LISP.nil)
      return 'nil';
    if (x === LISP.t)
      return 't';
    if (typeof x == 'string')
      return inspect ? inspectString(x) : x;
    if (x instanceof Array)
      return '#(' + x.map(function(v) { return makeString(v, inspect) }).join(' ') + ')';
    if (x == null)  // null or undefined
      return '' + x;
    return x.toString(inspect);
  };

  LISP.nil = false;
  LISP.t = true;

  LISP.isTrue = function(x) {
    return x !== LISP.nil && x != null;  // !(false || null || undefined)
  };

  LISP._getRestArgs = function(args, start) {
    return arrayToList(Array.prototype.slice.call(args, start));
  };
  LISP._output = (typeof(process) !== 'undefined'
                  ? function(str) {  // for node.js.
                    process.stdout.write(str);
                  } : function(str) {  // for browser.
                    console.log(str);
                  });

  LISP['*macro-table*'] = {};
  LISP['register-macro'] = function(name, func) {
    LISP['*macro-table*'][name] = func;
    return name;
  };
  LISP['macroexpand-1'] = function(s) {
    if (!LISP['pair?'](s) || !(s.car in LISP['*macro-table*']))
      return s;
    var macrofn = LISP['*macro-table*'][s.car];
    return LISP.apply(macrofn, s.cdr);
  };

  LISP.eval = function(exp) {
    return eval(LISP.compile(exp));
  };

  LISP.error = function() {
    throw arguments2Array(arguments, 0).join(', ');
  };

  // Symbol.
  var Symbol = function(name) {
    this.name = name;
  };
  Symbol.prototype = {
    toString: function() {
      return this.name;
    },
  };

  LISP['symbol->string'] = function(x) {
    return x.name;
  };

  LISP.intern = (function() {
    var symbolTable = {};  // key(string) => Symbol object
    return function(name) {
      if (name in symbolTable)
        return symbolTable[name];
      return symbolTable[name] = new Symbol(name);
    };
  })();
  LISP.gensym = (function() {
    var index = 0;
    return function() {
      return LISP.intern('__' + (++index));
    };
  })();
  LISP['symbol?'] = function(x) {
    return jsBoolToS(x instanceof Symbol);
  };

  var Keyword = function(name) {
    this.name = name;
  };
  Keyword.prototype = {
    toString: function(inspect) {
      return inspect ? ':' + this.name : this.name;
    },
  };
  LISP['make-keyword'] = (function() {
    var keywordTable = {};  // key(string) => Keyword object
    return function(name) {
      if (name in keywordTable)
        return keywordTable[name];
      return keywordTable[name] = new Keyword(name);
    };
  })();
  LISP['keyword?'] = function(x) {
    return jsBoolToS(x instanceof Keyword);
  };
  LISP['keyword->string'] = function(x) {
    return x.name;
  };

  LISP.type = function(x) {
    var type;
    if (x === LISP.nil || x === LISP.t)
      type = 'bool';
    else {
      var type = typeof x;
      if (type === 'object') {
        if (x instanceof Symbol)
          type = 'symbol';
        else if (x instanceof Keyword)
          type = 'keyword';
        else if (x instanceof LISP.Cons)
          type = 'pair';
        else if (x instanceof Array)
          type = 'vector';
        else if (x instanceof LISP.HashTable)
          type = 'table';
      }
    }
    return LISP.intern(type);
  };

  LISP['eq?'] = function(x, y) {
    return jsBoolToS(x === y);
  };

  // Cons cell.
  LISP.Cons = function(car, cdr, lineNo, path) {
    this.car = car;
    this.cdr = cdr;

    if (lineNo != null) {
      this.lineNo = lineNo;
      this.path = path;
    }
  };

  LISP.Cons.prototype = {
    toString: (function() {
      var abbrevTable = { quote: "'", quasiquote: '`', unquote: ',', 'unquote-splicing': ',@' };
      return function(inspect) {
        if (LISP['symbol?'](this.car) && LISP['pair?'](this.cdr) && LISP['null?'](this.cdr.cdr) &&
            this.car.name in abbrevTable) {
          return abbrevTable[this.car.name] + makeString(this.cdr.car, inspect);
        }

        var ss = [];
        var separator = '(';
        var p;
        for (p = this; p instanceof LISP.Cons; p = p.cdr) {
          ss.push(separator);
          ss.push(makeString(p.car, inspect));
          separator = ' ';
        }
        if (p !== LISP.nil) {
          ss.push(' . ');
          ss.push(makeString(p, inspect));
        }
        ss.push(')');
        return ss.join('');
      };
    })(),
    toArray: function() {
      var result = [];
      for (var p = this; p instanceof LISP.Cons; p = p.cdr)
        result.push(p.car);
      return result;
    },
  };

  LISP.cons = function(car, cdr) {
    return new LISP.Cons(car, cdr);
  };
  LISP.car = function(s) {
    if (s instanceof LISP.Cons)
      return s.car;
    return s;
  };
  LISP.cdr = function(s) {
    if (s instanceof LISP.Cons)
      return s.cdr;
    return LISP.nil;
  };
  LISP['set-car!'] = function(s, x) {
    return (s.car = x);
  };
  LISP['set-cdr!'] = function(s, x) {
    return (s.cdr = x);
  };

  LISP['pair?'] = function(x) {
    return jsBoolToS(x instanceof LISP.Cons);
  };
  LISP.list = function() {
    var result = LISP.nil;
    for (var i = arguments.length; --i >= 0; )
      result = LISP.cons(arguments[i], result);
    return result;
  };
  LISP['reverse!'] = function(x) {
    var rev = LISP.nil;
    for (var ls = x; LISP['pair?'](ls); ) {
      var d = ls.cdr;
      ls.cdr = rev;
      rev = ls;
      ls = d;
    }
    return rev;
  };

  LISP['number?'] = function(x) {
    return jsBoolToS(typeof x === 'number');
  };
  LISP['number->string'] = function(x, n) {
    return x.toString(n);
  };
  LISP['+'] = function() {
    if (arguments.length == 0)
      return 0;
    var result = arguments[0];
    for (var i = 1; i < arguments.length; ++i)
      result += arguments[i];
    return result;
  };
  LISP['*'] = function() {
    if (arguments.length == 0)
      return 1;
    var result = arguments[0];
    for (var i = 1; i < arguments.length; ++i)
      result *= arguments[i];
    return result;
  };
  LISP['-'] = function() {
    if (arguments.length == 0)
      return 0;
    var result = arguments[0];
    if (arguments.length == 1)
      return -result;
    for (var i = 1; i < arguments.length; ++i)
      result -= arguments[i];
    return result;
  };
  LISP['/'] = function() {
    if (arguments.length == 0)
      return 1;
    var result = arguments[0];
    if (arguments.length == 1)
      return 1.0 / result;
    for (var i = 1; i < arguments.length; ++i)
      result /= arguments[i];
    return result;
  };
  LISP['%'] = function() {
    if (arguments.length == 0)
      return 0;
    var result = arguments[0];
    if (arguments.length == 1)
      return result;
    for (var i = 1; i < arguments.length; ++i)
      result %= arguments[i];
    return result;
  };
  LISP['<'] = function() {
    if (arguments.length > 0) {
      var value = arguments[0];
      for (var i = 1; i < arguments.length; ++i) {
        var target = arguments[i];
        if (!(value < target))
          return LISP.nil;
        value = target;
      }
    }
    return LISP.t;
  };
  LISP['>'] = function() {
    if (arguments.length > 0) {
      var value = arguments[0];
      for (var i = 1; i < arguments.length; ++i) {
        var target = arguments[i];
        if (!(value > target))
          return LISP.nil;
        value = target;
      }
    }
    return LISP.t;
  };
  LISP['<='] = function() {
    if (arguments.length > 0) {
      var value = arguments[0];
      for (var i = 1; i < arguments.length; ++i) {
        var target = arguments[i];
        if (!(value <= target))
          return LISP.nil;
        value = target;
      }
    }
    return LISP.t;
  };
  LISP['>='] = function() {
    if (arguments.length > 0) {
      var value = arguments[0];
      for (var i = 1; i < arguments.length; ++i) {
        var target = arguments[i];
        if (!(value >= target))
          return LISP.nil;
        value = target;
      }
    }
    return LISP.t;
  };

  // String.
  LISP['string?'] = function(x) {
    return jsBoolToS(typeof x === 'string');
  };
  LISP['string=?'] = function(x, y) {
    return jsBoolToS(x === y);
  };
  LISP['string-append'] = function() {
    return arguments2Array(arguments, 0).join('');
  };
  LISP['string-join'] = function(list, separator) {
    if (list === LISP.nil)
      return '';
    return list.toArray().join(separator);
  };
  LISP['string-length'] = function(str) {
    return str.length;
  };
  LISP['string-ref'] = function(str, index) {
    return str[index];
  };
  LISP.substring = function(str, start, end) {
    return str.slice(start, end);
  };
  LISP['string-scan'] = function(str, item) {
    var index = str.indexOf(item);
    return index >= 0 ? index : LISP.nil;
  };

  LISP['char->integer'] = function(char, index) {
    return char.charCodeAt(index);
  };

  var kEscapeCharTable = { '\\': '\\\\', '\t': '\\t', '\n': '\\n', '"': '\\"' };
  function inspectString(str) {
    return '"' + str.replace(/[\\\t\n"]/g, function(m) { return kEscapeCharTable[m]; }) + '"';
  };

  LISP['x->string'] = makeString;
  LISP.print = function(x) {
    LISP._output(makeString(x));
    return x;
  };
  LISP.puts = function(x) {
    LISP._output(makeString(x));
    if (typeof(process) !== 'undefined')
      LISP._output('\n');
    return x;
  };
  LISP.write = function(x) {
    LISP._output(makeString(x, 10));  // 10 means true, and it is used as radix.
    return x;
  };

  LISP.apply = function(fn) {
    var params = [];
    if (arguments.length > 1) {
      for (var i = 1; i < arguments.length - 1; ++i)
        params.push(arguments[i]);
      // Last argument for `apply` is expected as list (or nil).
      var last = arguments[arguments.length - 1];
      if (last !== LISP.nil)
        params = params.concat(last.toArray());
    }
    return fn.apply(null, params);
  };
  LISP.JS = ((typeof window !== 'undefined') ? window :
             (typeof GLOBAL !== 'undefined') ? GLOBAL : {}),

  LISP.HashTable = function() {};
  LISP.HashTable.prototype = {
    toString: function() {
      var contents = '';
      for (var k in this) {
        if (!(this.hasOwnProperty(k)))
          continue;
        if (contents.length > 0)
          contents += ', ';
        contents += k + ':' + this[k];
      }
      return '#table<' + contents + '>';
    },
  };

  // Hash table.
  LISP['make-hash-table'] = function() {
    return new LISP.HashTable();
  };
  LISP['hash-table?'] = function(x) {
    return x instanceof LISP.HashTable;
  };
  LISP['hash-table-exists?'] = function(hash, x) {
    return x in hash ? LISP.t : LISP.nil;
  };
  LISP['hash-table-get'] = function(hash, x) {
    if (x in hash)
      return hash[x];
    return (arguments.length >= 3) ? arguments[3 - 1] : LISP.nil;
  };
  LISP['hash-table-put!'] = function(hash, x, value) {
    return hash[x] = value;
  };

  // Vector.
  LISP.vector = function() {
    return arguments2Array(arguments, 0);
  };
  LISP['make-vector'] = function(count, value) {
    if (value === undefined)
      value = LISP.nil;
    var vector = new Array(count);
    for (var i = 0; i < count; ++i)
      vector[i] = value;
    return vector;
  };
  LISP['vector?'] = function(x) {
    return jsBoolToS(x instanceof Array);
  };
  LISP['vector-length'] = function(vector) {
    return vector.length;
  };
  LISP['vector-ref'] = function(vector, index) {
    return vector[index];
  };
  LISP['vector-set!'] = function(vector, index, value) {
    return vector[index] = value;
  };

  // Regexp.
  LISP['regexp?'] = function(x) {
    return jsBoolToS(x instanceof RegExp);
  };
  LISP.rxmatch = function(re, str) {
    return jsBoolToS(re.exec(str));
  };
  LISP['regexp-replace-all'] = function(re, str, fn) {
    if (!re.global)
      re = eval(re.toString() + 'g')
    return str.replace(re, function (match) {
      return fn(function() {  // TODO: handle arguments.
        return match;
      });
    });
  };
  LISP['regexp->string'] = function(x) {
    var s = x.toString();
    return s.slice(1, s.length - 1);
  };

  // Stream.
  var Stream = function() {
    this.str = '';
    this.lineNo = 0;
  };
  Stream.prototype = {
    close: function() {},
    peek: function() {
      var result = this.fetch();
      if (result == null)
        return result;
      return this.str[0];
    },
    getc: function() {
      var c = this.peek();
      if (c == null)
        return c;
      this.str = this.str.slice(1);
      return c;
    },
    match: function(regexp, keep) {
      var result = this.fetch();
      if (result == null)
        return result;

      var m = this.str.match(regexp);
      if (m && !keep)
        this.str = RegExp.rightContext;
      return m;
    },
    eof: function() {
      return this.str == null;
    },
    getLine: function() {
      var result = this.str || this.readLine();
      this.str = null;
      return result;
    },
    fetch: function() {
      if (this.str == null)
        return null;

      if (this.str === '') {
        if ((this.str = this.readLine()) == null)
          return undefined;
        ++this.lineNo;
      }
      return this.str;
    },
  };

  var StrStream = function(str) {
    Stream.call(this);
    this.str = str;
    this.lineNo = 1;
  };
  StrStream.prototype = Object.create(Stream.prototype);
  StrStream.prototype.readLine = function() {
    return null;
  };
  LISP.StrStream = StrStream;

  // Reader.
  LISP.NoCloseParenException = function() {};

  var kDelimitors = '\\s(){}\\[\\]\'`,;#"';
  var kReSingleDot = RegExp('^\\.(?=[' + kDelimitors + '])');
  var kReSymbolOrNumber = RegExp('^([^' + kDelimitors + ']+)');

  var readTable = {};

  var Reader = {
    read: function(stream) {
      do {
        if (stream.eof())
          return null;
      } while (stream.match(/^\s+/))

      var c = stream.peek();
      if (c in readTable)
        return readTable[c](stream, stream.getc());

      var m;
      if (stream.match(/^\(/))  // Left paren '('.
        return Reader.readList(stream);
      if (stream.match(/^;[^\n]*\n?/))  // Line comment.
        return Reader.read(stream);
      if (m = stream.match(/^"((\\.|[^"\\])*)"/))  // string.
        return Reader.unescape(m[1]);
      if (stream.match(/^#\(/))  // vector.
        return Reader.readVector(stream);
      if (m = stream.match(/^#\/([^\/]*)\//))  // regexp TODO: Implement properly.
        return new RegExp(m[1]);
      if (stream.match(/^#\|(.|[\n\r])*?\|#/))  // Block comment.
        return Reader.read(stream);
      if (stream.match(kReSingleDot, true))  // Single dot.
        return undefined;
      if (m = stream.match(kReSymbolOrNumber))  // Symbol or number.
        return Reader.readSymbolOrNumber(m[1]);
      return undefined;
    },

    readSymbolOrNumber: function(str) {
      if (str === 'nil')
        return LISP.nil;
      if (str === 't')
        return LISP.t;
      if (str[0] === ':')
        return LISP['make-keyword'](str.slice(1));
      if (str.match(/^([+\-]?[0-9]+(\.[0-9]*)?)$/))  // Number.
        return parseFloat(str);
      return LISP.intern(str);
    },

    readList: function(stream) {
      var result = LISP.nil;
      for (;;) {
        var x = Reader.read(stream);
        if (x != null) {
          result = new LISP.Cons(x, result, stream.lineNo, stream.path);
          continue;
        }

        if (stream.match(/^\s*\)/)) {  // Close paren.
          return LISP['reverse!'](result);
        }
        if (stream.match(kReSingleDot)) {  // Dot.
          var last = Reader.read(stream);
          if (last != null) {
            if (stream.match(/^\s*\)/)) {  // Close paren.
              var reversed = LISP['reverse!'](result);
              result.cdr = last;
              return reversed;
            }
          }
        }
        // Error
        throw new LISP.NoCloseParenException();
      }
    },

    readVector: function(stream) {
      var result = [];
      for (;;) {
        var x = Reader.read(stream);
        if (x !== undefined) {
          result.push(x);
          continue;
        }

        if (stream.match(/^\s*\)/)) {  // Close paren.
          return result;
        }
        // Error
        throw new LISP.NoCloseParenException();
      }
    },

    unescape: function(str) {
      return str.replace(/(\\x[0-9a-fA-F]{2})/g, function(match) {
        return String.fromCharCode(parseInt(match, 16));
      }).replace(/\\./g, function(match) {
        switch (match[1]) {
        case 't':  return '\t';
        case 'n':  return '\n';
        default:  return match[1];
        }
      });
    },
  };

  LISP['set-macro-character'] = function(c, fn) {
    readTable[c] = fn;
  };

  LISP['set-macro-character']("'", function(stream, c) {
    return LISP.list(LISP.intern('quote'), Reader.read(stream));
  });
  LISP['set-macro-character']('`', function(stream, c) {
    return LISP.list(LISP.intern('quasiquote'), Reader.read(stream));
  });
  LISP['set-macro-character'](',', function(stream, c) {
    var c = stream.peek();
    var keyword = 'unquote';
    if (c == '@') {
      keyword = 'unquote-splicing';
      stream.getc();
    }
    return LISP.list(LISP.intern(keyword), Reader.read(stream));
  });

  LISP.read = function(stream) {
    return Reader.read(stream || LISP['*stdin*']);
  };

  LISP['read-from-string'] = function(str) {
    return Reader.read(new StrStream(str));
  };

  LISP['read-line'] = function(stream) {
    return (stream || LISP['*stdin*']).getLine();
  };


  // For node JS.
  if (typeof process !== 'undefined') {
    var fs = require('fs');

    LISP.FileStream = (function() {
      var BUFFER_SIZE = 4096;
      var buffer = new Buffer(BUFFER_SIZE);
      var FileStream = function(fd, path) {
        Stream.call(this);
        this.fd = fd;
        this.path = path;
        this.lines = [];
        this.index = 0;
      };
      FileStream.prototype = Object.create(Stream.prototype);
      FileStream.prototype.close = function() {
        if (this.fd == null)
          return;
        fs.closeSync(this.fd);
        this.fd = null;
        this.lines.length = this.index = 0;
        this.str = null;
        this.chomped = false;
      };
      FileStream.prototype.readLine = function() {
        for (;;) {
          var left = '';
          if (this.index < this.lines.length) {
            if (this.index < this.lines.length - 1 || !this.chomped)
              return this.lines[this.index++];
            if (this.chomped)
              left = this.lines[this.index];
          }

          if (this.fd == null)
            return LISP.nil;
          var n = fs.readSync(this.fd, buffer, 0, BUFFER_SIZE);
          if (n <= 0)
            return null;
          var string = left + buffer.slice(0, n).toString();
          this.chomped = false;
          if (string.length > 0) {
            if (string[string.length - 1] != '\n')
              this.chomped = true;
            else
              string = string.slice(0, string.length - 1);  // Remove last '\n' to avoid last empty line.
          }
          this.lines = string.split('\n');
          this.index = 0;
        }
      };
      return FileStream;
    })();

    LISP['*stdin*'] = new LISP.FileStream(process.stdin.fd, '*stdin*');
    LISP['*stdout*'] = new LISP.FileStream(process.stdout.fd, '*stdout*');
    LISP['*stderr*'] = new LISP.FileStream(process.stderr.fd, '*stderr*');

    LISP.open = function(path, flag) {
      try {
        var fd = fs.openSync(path, flag || 'r');
        return new LISP.FileStream(fd, path);
      } catch (e) {
        return LISP.nil;
      }
    };

    LISP.close = function(stream) {
      stream.close();
      return stream;
    };

    LISP.load = function(fileName) {
      var stream = LISP.open(fileName);
      if (!stream) {
        return LISP.error('Cannot open [' + fileName + ']');
      }

      var result;
      for (;;) {
        var s = LISP.read(stream);
        if (s == null)
          break;
        result = LISP.eval(s);
      }
      LISP.close(stream);
      return result;
    };

    // System
    LISP.exit = function(code) {
      process.exit(code);
    };

    LISP.jsrequire = require;
  }

  /*==== EMBED COMPILED CODE HERE ====*/
LISP["register-macro"](LISP.intern("defmacro"), (function(name, params){var body = LISP._getRestArgs(arguments, 2); return (LISP.list(LISP.intern("register-macro"), LISP.list(LISP.intern("quote"), name), LISP["list*"](LISP.intern("lambda"), params, body)));}));
LISP["register-macro"](LISP.intern("defun"), (function(name, params){var body = LISP._getRestArgs(arguments, 2); return (LISP.list(LISP.intern("def"), name, LISP["list*"](LISP.intern("lambda"), params, body)));}));
LISP["register-macro"](LISP.intern("let"), (function(pairs){var body = LISP._getRestArgs(arguments, 1); return ((LISP.isTrue(LISP["symbol?"](pairs)) ? ((function() { var __2 = LISP.list(LISP.nil); return (function(name, pairs, body){return (LISP["list*"](LISP.list(LISP.intern("lambda"), LISP.list(name), LISP.list(LISP.intern("set!"), name, LISP["list*"](LISP.intern("lambda"), LISP.map(LISP.car, pairs), body)), LISP["list*"](name, LISP.map(LISP.cadr, pairs))), __2));}); })()(pairs, LISP.car(body), LISP.cdr(body))) : (LISP["list*"](LISP["list*"](LISP.intern("lambda"), LISP.map(LISP.car, pairs), body), LISP.map(LISP.cadr, pairs)))));}));
LISP["register-macro"](LISP.intern("let1"), (function(name, value){var body = LISP._getRestArgs(arguments, 2); return (LISP["list*"](LISP.intern("let"), LISP.list(LISP.list(name, value)), body));}));
LISP["register-macro"](LISP.intern("let*"), (function(pairs){var body = LISP._getRestArgs(arguments, 1); return ((LISP.isTrue(LISP["null?"](pairs)) ? (LISP["list*"](LISP.intern("do"), body)) : (LISP.list(LISP.intern("let1"), LISP.caar(pairs), LISP.cadar(pairs), LISP["list*"](LISP.intern("let*"), LISP.cdr(pairs), body)))));}));
LISP["register-macro"](LISP.intern("when"), (function(pred){var body = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("if"), pred, LISP["list*"](LISP.intern("do"), body)));}));
LISP["register-macro"](LISP.intern("unless"), (function(pred){var body = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("if"), pred, LISP.nil, LISP["list*"](LISP.intern("do"), body)));}));
LISP["register-macro"](LISP.intern("cond"), (function(){var clauses = LISP._getRestArgs(arguments, 0); return ((LISP.isTrue(LISP["null?"](clauses)) ? (LISP.nil) : ((function(clause, rest){return ((LISP.isTrue(LISP["eq?"](LISP.car(clause), LISP.t)) ? (LISP["list*"](LISP.intern("do"), LISP.cdr(clause))) : ((LISP.isTrue(LISP["null?"](LISP.cdr(clause))) ? ((function(g){return (LISP.list(LISP.intern("let"), LISP.list(LISP.list(g, LISP.car(clause))), LISP.list(LISP.intern("if"), g, g, LISP["list*"](LISP.intern("cond"), rest))));})(LISP.gensym())) : ((LISP.isTrue(LISP["eq?"](LISP.cadr(clause), LISP.intern("=>"))) ? ((function(g){return (LISP.list(LISP.intern("let"), LISP.list(LISP.list(g, LISP.car(clause))), LISP.list(LISP.intern("if"), g, LISP.list(LISP.caddr(clause), g), LISP["list*"](LISP.intern("cond"), rest))));})(LISP.gensym())) : (LISP.list(LISP.intern("if"), LISP.car(clause), LISP["list*"](LISP.intern("do"), LISP.cdr(clause)), LISP["list*"](LISP.intern("cond"), rest)))))))));})(LISP.car(clauses), LISP.cdr(clauses)))));}));
LISP["register-macro"](LISP.intern("case"), (function(x){var clauses = LISP._getRestArgs(arguments, 1); return ((function(value){return (LISP.list(LISP.intern("let1"), value, x, LISP["list*"](LISP.intern("cond"), LISP.map((function(clause){return ((LISP.isTrue(LISP["eq?"](LISP.car(clause), LISP.t)) ? (clause) : ((LISP.isTrue(LISP["null?"](LISP.cdar(clause))) ? (LISP["list*"](LISP.list(LISP.intern("eq?"), value, LISP.list(LISP.intern("quote"), LISP.caar(clause))), LISP.cdr(clause))) : (LISP["list*"](LISP.list(LISP.intern("member"), value, LISP.list(LISP.intern("quote"), LISP.car(clause))), LISP.cdr(clause)))))));}), clauses))));})(LISP.gensym()));}));
LISP["register-macro"](LISP.intern("and"), (function() { var __3 = LISP.list(LISP.nil); return (function(){var args = LISP._getRestArgs(arguments, 0); return ((LISP.isTrue(LISP["null?"](args)) ? (LISP.t) : ((LISP.isTrue(LISP["null?"](LISP.cdr(args))) ? (LISP.car(args)) : (LISP["list*"](LISP.intern("if"), LISP.car(args), LISP["list*"](LISP.intern("and"), LISP.cdr(args)), __3))))));}); })());
LISP["register-macro"](LISP.intern("or"), (function(){var args = LISP._getRestArgs(arguments, 0); return ((LISP.isTrue(LISP["null?"](LISP.cdr(args))) ? (LISP.car(args)) : ((function(g){return (LISP.list(LISP.intern("let1"), g, LISP.car(args), LISP.list(LISP.intern("if"), g, g, LISP["list*"](LISP.intern("or"), LISP.cdr(args)))));})(LISP.gensym()))));}));
LISP["register-macro"](LISP.intern("do"), (function(){var body = LISP._getRestArgs(arguments, 0); return ((LISP.isTrue(LISP["null?"](body)) ? (LISP.nil) : ((LISP.isTrue(LISP["null?"](LISP.cdr(body))) ? (LISP.car(body)) : (LISP["list*"](LISP.intern("let"), LISP.nil, body))))));}));
LISP["register-macro"](LISP.intern("aif"), (function(expr, thn){var els = LISP._getRestArgs(arguments, 2); return (LISP.list(LISP.intern("let1"), LISP.intern("it"), expr, LISP["list*"](LISP.intern("if"), LISP.intern("it"), thn, els)));}));
LISP["register-macro"](LISP.intern("awhen"), (function(expr){var body = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("aif"), expr, LISP["list*"](LISP.intern("do"), body)));}));
LISP["register-macro"](LISP.intern("awhile"), (function(expr){var body = LISP._getRestArgs(arguments, 1); return ((function(loop){return (LISP.list(LISP.intern("let"), loop, LISP.nil, LISP.list(LISP.intern("let1"), LISP.intern("it"), expr, LISP["list*"](LISP.intern("when"), LISP.intern("it"), LISP.append(body, LISP.list(LISP.list(loop)))))));})(LISP.gensym()));}));
LISP["null?"] = (function(x){return (LISP["eq?"](x, LISP.nil));});
LISP.not = (function(x){return (LISP["eq?"](x, LISP.nil));});
LISP.caar = (function(x){return (LISP.car(LISP.car(x)));});
LISP.cadr = (function(x){return (LISP.car(LISP.cdr(x)));});
LISP.cdar = (function(x){return (LISP.cdr(LISP.car(x)));});
LISP.cddr = (function(x){return (LISP.cdr(LISP.cdr(x)));});
LISP.cadar = (function(x){return (LISP.cadr(LISP.car(x)));});
LISP.caddr = (function(x){return (LISP.car(LISP.cddr(x)));});
LISP.cdddr = (function(x){return (LISP.cdr(LISP.cddr(x)));});
LISP["equal?"] = (function(x, y){return ((LISP.isTrue(LISP["eq?"](x, y)) ? (LISP.t) : ((function(xtype){return ((LISP.isTrue(LISP["eq?"](xtype, LISP.type(y))) ? ((function(__4){return ((LISP.isTrue(LISP["eq?"](__4, LISP.intern("pair"))) ? ((LISP.isTrue(LISP["equal?"](LISP.car(x), LISP.car(y))) ? (LISP["equal?"](LISP.cdr(x), LISP.cdr(y))) : (LISP.nil))) : ((LISP.isTrue(LISP["eq?"](__4, LISP.intern("vector"))) ? ((function(n){return ((LISP.isTrue(LISP["eq?"](n, LISP["vector-length"](y))) ? ((function(loop){return (loop = (function(i){return ((function(__5){return ((LISP.isTrue(__5) ? (__5) : ((LISP.isTrue(LISP["equal?"](LISP["vector-ref"](x, i), LISP["vector-ref"](y, i))) ? (loop((i + 1))) : (LISP.nil)))));})(LISP[">="](i, n)));}), loop(0));})(LISP.nil)) : (LISP.nil)));})(LISP["vector-length"](x))) : (LISP.nil)))));})(xtype)) : (LISP.nil)));})(LISP.type(x)))));});
LISP.length = (function(ls){return ((function(loop){return (loop = (function(ls, acc){return ((LISP.isTrue(LISP["pair?"](ls)) ? (loop(LISP.cdr(ls), (acc + 1))) : (acc)));}), loop(ls, 0));})(LISP.nil));});
LISP["last-pair"] = (function(ls){return ((LISP.isTrue(LISP["pair?"](LISP.cdr(ls))) ? (LISP["last-pair"](LISP.cdr(ls))) : (ls)));});
LISP.member = (function(x, ls){return ((LISP.isTrue(LISP["null?"](ls)) ? (LISP.nil) : ((LISP.isTrue(LISP["eq?"](x, LISP.car(ls))) ? (ls) : (LISP.member(x, LISP.cdr(ls)))))));});
LISP.assoc = (function(x, ls){return ((LISP.isTrue(LISP["null?"](ls)) ? (LISP.nil) : ((LISP.isTrue(LISP["eq?"](x, LISP.caar(ls))) ? (LISP.car(ls)) : (LISP.assoc(x, LISP.cdr(ls)))))));});
LISP.acons = (function(key, datum, alist){return (LISP.cons(LISP.cons(key, datum), alist));});
LISP.map = (function(f, ls){return ((LISP.isTrue(LISP["null?"](ls)) ? (LISP.nil) : (LISP.cons(f(LISP.car(ls)), LISP.map(f, LISP.cdr(ls))))));});
LISP.append = (function(ls){var rest = LISP._getRestArgs(arguments, 1); return ((LISP.isTrue(LISP["null?"](rest)) ? (ls) : ((LISP.isTrue(LISP["null?"](ls)) ? (LISP.apply(LISP.append, rest)) : (LISP.cons(LISP.car(ls), LISP.apply(LISP.append, LISP.cdr(ls), rest)))))));});
LISP.reverse = (function(ls){return ((function(loop){return (loop = (function(ls, acc){return ((LISP.isTrue(LISP["pair?"](ls)) ? (loop(LISP.cdr(ls), LISP.cons(LISP.car(ls), acc))) : (acc)));}), loop(ls, LISP.nil));})(LISP.nil));});
LISP["list*"] = (function(){var args = LISP._getRestArgs(arguments, 0); return ((LISP.isTrue(LISP["null?"](args)) ? (LISP.nil) : ((LISP.isTrue(LISP["null?"](LISP.cdr(args))) ? (LISP.car(args)) : ((function(loop){return (loop = (function(p, q){return ((LISP.isTrue(LISP["null?"](LISP.cdr(q))) ? ((function(){return (LISP["set-cdr!"](p, LISP.car(q)), args);})()) : (loop(q, LISP.cdr(q)))));}), loop(args, LISP.cdr(args)));})(LISP.nil))))));});
LISP["last-pair"] = (function(ls){return ((LISP.isTrue(LISP["pair?"](LISP.cdr(ls))) ? (LISP["last-pair"](LISP.cdr(ls))) : (ls)));});
LISP["proper-list?"] = (function(ls){return ((LISP.isTrue(LISP["pair?"](ls)) ? (LISP["null?"](LISP.cdr(LISP["last-pair"](ls)))) : (LISP.nil)));});
LISP["dotted->proper"] = (function(ls){return ((LISP.isTrue(LISP["pair?"](ls)) ? ((LISP.isTrue(LISP["proper-list?"](ls)) ? (ls) : ((function(dot, rev){return ((function(dup){return (LISP["set-cdr!"](rev, LISP.list(dot)), dup);})(LISP["reverse!"](rev)));})(LISP.cdr(LISP["last-pair"](ls)), LISP.reverse(ls))))) : (LISP.list(ls))));});
LISP["vector-map"] = (function(proc, vect){return ((function(len){return ((function(new$2dvect){return ((function(loop){return (loop = (function(i){return ((LISP.isTrue(LISP[">="](i, len)) ? (new$2dvect) : ((function(){return (LISP["vector-set!"](new$2dvect, i, proc(LISP["vector-ref"](vect, i))), loop((i + 1)));})())));}), loop(0));})(LISP.nil));})(LISP["make-vector"](len)));})(LISP["vector-length"](vect)));});
LISP["vector->list"] = (function(vect){return ((function(n){return ((LISP.isTrue(LISP["<="](n, 0)) ? (LISP.nil) : ((function(loop){return (loop = (function(i, acc){return ((LISP.isTrue(LISP["<"](i, 0)) ? (acc) : (loop((i - 1), LISP.cons(LISP["vector-ref"](vect, i), acc)))));}), loop((n - 1), LISP.nil));})(LISP.nil))));})(LISP["vector-length"](vect)));});
LISP["position-if"] = (function(pred, seq){return ((function(loop){return (loop = (function(p, i){return ((LISP.isTrue(p) ? ((LISP.isTrue(pred(LISP.car(p))) ? (i) : (loop(LISP.cdr(p), (i + 1))))) : (LISP.nil)));}), loop(seq, 0));})(LISP.nil));});
LISP.take = (function(n, ls){return ((function(loop){return (loop = (function(n, ls, acc){return ((LISP.isTrue((function(__6){return ((LISP.isTrue(__6) ? (__6) : (LISP["null?"](ls))));})(LISP["<="](n, 0))) ? (LISP["reverse!"](acc)) : (loop((n - 1), LISP.cdr(ls), LISP.cons(LISP.car(ls), acc)))));}), loop(n, ls, LISP.nil));})(LISP.nil));});
LISP.drop = (function(n, ls){return ((LISP.isTrue((function(__7){return ((LISP.isTrue(__7) ? (__7) : (LISP["null?"](ls))));})(LISP["<="](n, 0))) ? (ls) : (LISP.drop((n - 1), LISP.cdr(ls)))));});
LISP.elt = (function(n, ls){return (LISP.car(LISP.drop(n, ls)));});
LISP["remove-if"] = (function(test, seq){return ((function(loop){return (loop = (function(seq, acc){return ((LISP.isTrue(LISP["null?"](seq)) ? (LISP["reverse!"](acc)) : (loop(LISP.cdr(seq), (LISP.isTrue(test(LISP.car(seq))) ? (acc) : (LISP.cons(LISP.car(seq), acc)))))));}), loop(seq, LISP.nil));})(LISP.nil));});
LISP["register-macro"](LISP.intern("dotimes"), (function(params){var body = LISP._getRestArgs(arguments, 1); return ((function() { var __8 = LISP.list(0), __9 = LISP.list(1); return (function(i, limit, loop){return (LISP.list(LISP.intern("let1"), limit, LISP.cadr(params), LISP.list(LISP.intern("let"), loop, LISP.list(LISP["list*"](i, __8)), LISP.list(LISP.intern("if"), LISP.list(LISP.intern("<"), i, limit), LISP["list*"](LISP.intern("do"), LISP.append(body, LISP.list(LISP.list(loop, LISP["list*"](LISP.intern("+"), i, __9))))), LISP.caddr(params)))));}); })()(LISP.car(params), LISP.gensym(), LISP.gensym()));}));
LISP["register-macro"](LISP.intern("dolist"), (function(pair){var body = LISP._getRestArgs(arguments, 1); return ((function(i, loop, ls){return (LISP.list(LISP.intern("let"), loop, LISP.list(LISP.list(ls, LISP.cadr(pair))), LISP.list(LISP.intern("let1"), i, LISP.list(LISP.intern("car"), ls), LISP["list*"](LISP.intern("when"), i, LISP.append(body, LISP.list(LISP.list(loop, LISP.list(LISP.intern("cdr"), ls))))))));})(LISP.car(pair), LISP.gensym(), LISP.gensym()));}));
LISP["register-macro"](LISP.intern("labels"), (function(lss){var body = LISP._getRestArgs(arguments, 1); return (LISP["list*"](LISP.intern("let"), LISP.map((function(ls){return (LISP.car(ls));}), lss), LISP.append(LISP.map((function(ls){return (LISP.list(LISP.intern("set!"), LISP.car(ls), LISP["list*"](LISP.intern("lambda"), LISP.cdr(ls))));}), lss), body)));}));
LISP.nreconc = (function(ls, tail){return ((function(top){return (LISP["set-cdr!"](ls, tail), top);})(LISP["reverse!"](ls)));});
LISP.any = (function(f, ls){return ((LISP.isTrue(LISP["null?"](ls)) ? (LISP.nil) : ((LISP.isTrue(f(LISP.car(ls))) ? (LISP.t) : (LISP.any(f, LISP.cdr(ls)))))));});
LISP.every = (function(f, ls){return ((LISP.isTrue(LISP["null?"](ls)) ? (LISP.t) : ((LISP.isTrue(f(LISP.car(ls))) ? (LISP.every(f, LISP.cdr(ls))) : (LISP.nil)))));});
LISP["*bq-clobberable*"] = LISP.gensym();
LISP["*bq-quote-nil*"] = LISP.list(LISP.intern("quote"), LISP.nil);
LISP["register-macro"](LISP.intern("quasiquote"), (function(x){return (LISP["bq-completely-process"](x));}));
LISP["bq-completely-process"] = (function(x){return (LISP["bq-simplify"](LISP["bq-process"](x)));});
LISP["bq-process"] = (function(x){return ((LISP.isTrue(LISP.not(LISP["pair?"](x))) ? (LISP.list(LISP.intern("quote"), x)) : ((LISP.isTrue(LISP["eq?"](LISP.car(x), LISP.intern("quasiquote"))) ? (LISP["bq-process"](LISP["bq-completely-process"](LISP.cadr(x)))) : ((LISP.isTrue(LISP["eq?"](LISP.car(x), LISP.intern("unquote"))) ? (LISP.cadr(x)) : ((LISP.isTrue(LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))) ? (LISP.error(",@~S after `", LISP.cadr(x))) : ((LISP.isTrue(LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))) ? (LISP.error(",.~S after `", LISP.cadr(x))) : ((function(loop){return (loop = (function(p, q){return ((LISP.isTrue(LISP.not(LISP["pair?"](p))) ? (LISP.cons(LISP.intern("append"), LISP.nreconc(q, LISP.list(LISP.list(LISP.intern("quote"), p))))) : ((LISP.isTrue(LISP["eq?"](LISP.car(p), LISP.intern("unquote"))) ? ((function(){return ((LISP.isTrue(LISP["null?"](LISP.cddr(p))) ? (LISP.nil) : (LISP.error("Malformed ,~S", p))), LISP.cons(LISP.intern("append"), LISP.nreconc(q, LISP.list(LISP.cadr(p)))));})()) : ((function(){return ((LISP.isTrue(LISP["eq?"](LISP.car(p), LISP.intern("unquote-splicing"))) ? (LISP.error("Dotted ,@~S", p)) : (LISP.nil)), (LISP.isTrue(LISP["eq?"](LISP.car(p), LISP.intern("unquote-dot"))) ? (LISP.error("Dotted ,.~S", p)) : (LISP.nil)), loop(LISP.cdr(p), LISP.cons(LISP.bracket(LISP.car(p)), q)));})())))));}), loop(x, LISP.nil));})(LISP.nil))))))))))));});
LISP.bracket = (function(x){return ((LISP.isTrue(LISP.not(LISP["pair?"](x))) ? (LISP.list(LISP.intern("list"), LISP["bq-process"](x))) : ((LISP.isTrue(LISP["eq?"](LISP.car(x), LISP.intern("unquote"))) ? (LISP.list(LISP.intern("list"), LISP.cadr(x))) : ((LISP.isTrue(LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))) ? (LISP.cadr(x)) : ((LISP.isTrue(LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))) ? (LISP.list(LISP["*bq-clobberable*"], LISP.cadr(x))) : (LISP.list(LISP.intern("list"), LISP["bq-process"](x)))))))))));});
LISP.maptree = (function(fn, x){return ((LISP.isTrue(LISP.not(LISP["pair?"](x))) ? (fn(x)) : ((function(a, d){return ((LISP.isTrue((LISP.isTrue(LISP["equal?"](a, LISP.car(x))) ? (LISP["equal?"](d, LISP.cdr(x))) : (LISP.nil))) ? (x) : (LISP.cons(a, d))));})(fn(LISP.car(x)), LISP.maptree(fn, LISP.cdr(x))))));});
LISP["bq-splicing-frob"] = (function(x){return ((LISP.isTrue(LISP["pair?"](x)) ? ((function(__2){return ((LISP.isTrue(__2) ? (__2) : (LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot")))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing")))) : (LISP.nil)));});
LISP["bq-frob"] = (function(x){return ((LISP.isTrue(LISP["pair?"](x)) ? ((function(__3){return ((LISP.isTrue(__3) ? (__3) : ((function(__4){return ((LISP.isTrue(__4) ? (__4) : (LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot")))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote")))) : (LISP.nil)));});
LISP["bq-simplify"] = (function(x){return ((LISP.isTrue(LISP["pair?"](x)) ? ((function(x){return ((LISP.isTrue(LISP.not(LISP["eq?"](LISP.car(x), LISP.intern("append")))) ? (x) : (LISP["bq-simplify-args"](x))));})((LISP.isTrue(LISP["eq?"](LISP.car(x), LISP.intern("quote"))) ? (x) : (LISP.maptree(LISP["bq-simplify"], x))))) : (x)));});
LISP["bq-simplify-args"] = (function(x){return ((function(loop){return (loop = (function(args, result){return ((LISP.isTrue(LISP.not(LISP["null?"](args))) ? (loop(LISP.cdr(args), (LISP.isTrue(LISP.not(LISP["pair?"](LISP.car(args)))) ? (LISP["bq-attach-append"](LISP.intern("append"), LISP.car(args), result)) : ((LISP.isTrue((LISP.isTrue(LISP["eq?"](LISP.caar(args), LISP.intern("list"))) ? (LISP.not(LISP.any(LISP["bq-splicing-frob"], LISP.cdar(args)))) : (LISP.nil))) ? (LISP["bq-attach-conses"](LISP.cdar(args), result)) : ((LISP.isTrue((LISP.isTrue(LISP["eq?"](LISP.caar(args), LISP.intern("list*"))) ? (LISP.not(LISP.any(LISP["bq-splicing-frob"], LISP.cdar(args)))) : (LISP.nil))) ? (LISP["bq-attach-conses"](LISP.reverse(LISP.cdr(LISP.reverse(LISP.cdar(args)))), LISP["bq-attach-append"](LISP.intern("append"), LISP.car(LISP.last(LISP.car(args))), result))) : ((LISP.isTrue((LISP.isTrue(LISP["eq?"](LISP.caar(args), LISP.intern("quote"))) ? ((LISP.isTrue(LISP["pair?"](LISP.cadar(args))) ? ((LISP.isTrue(LISP.not(LISP["bq-frob"](LISP.cadar(args)))) ? (LISP.not(LISP.cddar(args))) : (LISP.nil))) : (LISP.nil))) : (LISP.nil))) ? (LISP["bq-attach-conses"](LISP.list(LISP.list(LISP.intern("quote"), LISP.caadar(args))), result)) : ((LISP.isTrue(LISP["eq?"](LISP.caar(args), LISP["*bq-clobberable*"])) ? (LISP["bq-attach-append"](LISP.intern("append!"), LISP.cadar(args), result)) : (LISP["bq-attach-append"](LISP.intern("append"), LISP.car(args), result))))))))))))) : (result)));}), loop(LISP.reverse(LISP.cdr(x)), LISP.nil));})(LISP.nil));});
LISP["null-or-quoted"] = (function(x){return ((function(__5){return ((LISP.isTrue(__5) ? (__5) : ((LISP.isTrue(LISP["pair?"](x)) ? (LISP["eq?"](LISP.car(x), LISP.intern("quote"))) : (LISP.nil)))));})(LISP["null?"](x)));});
LISP["bq-attach-append"] = (function(op, item, result){return ((LISP.isTrue((LISP.isTrue(LISP["null-or-quoted"](item)) ? (LISP["null-or-quoted"](result)) : (LISP.nil))) ? (LISP.list(LISP.intern("quote"), LISP.append(LISP.cadr(item), LISP.cadr(result)))) : ((LISP.isTrue((function(__6){return ((LISP.isTrue(__6) ? (__6) : (LISP["equal?"](result, LISP["*bq-quote-nil*"]))));})(LISP["null?"](result))) ? ((LISP.isTrue(LISP["bq-splicing-frob"](item)) ? (LISP.list(op, item)) : (item))) : ((LISP.isTrue((LISP.isTrue(LISP["pair?"](result)) ? (LISP["eq?"](LISP.car(result), op)) : (LISP.nil))) ? (LISP["list*"](LISP.car(result), item, LISP.cdr(result))) : (LISP.list(op, item, result))))))));});
LISP["bq-attach-conses"] = (function(items, result){return ((LISP.isTrue((LISP.isTrue(LISP.every(LISP["null-or-quoted"], items)) ? (LISP["null-or-quoted"](result)) : (LISP.nil))) ? (LISP.list(LISP.intern("quote"), LISP.append(LISP.map(LISP.cadr, items), LISP.cadr(result)))) : ((LISP.isTrue((function(__7){return ((LISP.isTrue(__7) ? (__7) : (LISP["equal?"](result, LISP["*bq-quote-nil*"]))));})(LISP["null?"](result))) ? (LISP.cons(LISP.intern("list"), items)) : ((LISP.isTrue((LISP.isTrue(LISP["pair?"](result)) ? ((function(__8){return ((LISP.isTrue(__8) ? (__8) : (LISP["eq?"](LISP.car(result), LISP.intern("list*")))));})(LISP["eq?"](LISP.car(result), LISP.intern("list")))) : (LISP.nil))) ? (LISP.cons(LISP.car(result), LISP.append(items, LISP.cdr(result)))) : (LISP.cons(LISP.intern("list*"), LISP.append(items, LISP.list(result))))))))));});
LISP.macroexpand = (function(exp){return ((function(expanded){return ((LISP.isTrue(LISP["equal?"](expanded, exp)) ? (exp) : (LISP.macroexpand(expanded))));})(LISP["macroexpand-1"](exp)));});
LISP["create-scope"] = (function(parent$2dscope, params){return (LISP.vector(LISP["remove-if"]((function() { var __2 = LISP.list(LISP.intern("&rest"), LISP.intern("&body")); return (function(x){return (LISP.member(x, __2));}); })(), params), LISP.nil, parent$2dscope));});
LISP["scope-param"] = (function(scope){return (LISP["vector-ref"](scope, 0));});
LISP["scope-outer"] = (function(scope){return (LISP["vector-ref"](scope, 2));});
LISP["scope-add-var"] = (function(scope, val){return ((function(x){return (LISP["vector-set!"](scope, 1, LISP.cons(LISP.cons(x, val), LISP["vector-ref"](scope, 1))), LISP["vector-set!"](scope, 0, LISP.cons(x, LISP["vector-ref"](scope, 0))), x);})(LISP.gensym()));});
LISP["scope-get-var"] = (function(scope){return (LISP["vector-ref"](scope, 1));});
LISP["scope-var?"] = (function(scope, x){return ((LISP.isTrue(LISP["null?"](scope)) ? (LISP.nil) : ((LISP.isTrue(LISP.member(x, LISP["scope-param"](scope))) ? (LISP.t) : (LISP["scope-var?"](LISP["scope-outer"](scope), x))))));});
LISP["local-var?"] = (function(scope, sym){return ((LISP.isTrue(LISP["symbol?"](sym)) ? (LISP["scope-var?"](scope, LISP["get-receiver"](sym))) : (LISP.nil)));});
LISP["traverse-args"] = (function(args, scope){return (LISP.map((function(x){return (LISP["traverse*"](x, scope));}), args));});
LISP["register-macro"](LISP.intern("record"), (function(args, param){var body = LISP._getRestArgs(arguments, 2); return (LISP.list(LISP.intern("apply"), LISP["list*"](LISP.intern("lambda"), param, body), args));}));
LISP["register-macro"](LISP.intern("record-case"), (function(x){var clauses = LISP._getRestArgs(arguments, 1); return ((function(value){return (LISP.list(LISP.intern("let1"), value, x, LISP["list*"](LISP.intern("case"), LISP.list(LISP.intern("car"), value), LISP.map((function(clause){return ((LISP.isTrue(LISP["eq?"](LISP.car(clause), LISP.t)) ? (clause) : ((function(key){return (LISP.list(LISP.list(key), LISP["list*"](LISP.intern("record"), LISP.list(LISP.intern("cdr"), value), LISP.cdar(clause), LISP.cdr(clause))));})(LISP.caar(clause)))));}), clauses))));})(LISP.gensym()));}));
LISP["traverse-quoted-value"] = (function(x){return ((LISP.isTrue(LISP["pair?"](x)) ? (LISP.vector(LISP["make-keyword"]("FUNCALL"), LISP.vector(LISP["make-keyword"]("REF"), (LISP.isTrue(LISP["proper-list?"](x)) ? (LISP.intern("list")) : (LISP.intern("list*")))), LISP.map(LISP["traverse-quoted-value"], LISP["dotted->proper"](x)))) : (LISP.vector(LISP["make-keyword"]("CONST"), x))));});
LISP["confirm-valid-params"] = (function(params){return ((LISP.isTrue(params) ? ((LISP.isTrue(LISP["symbol?"](LISP.car(params))) ? (LISP["confirm-valid-params"](LISP.cdr(params))) : (LISP["compile-error"]("function parameter must be symbol, but", LISP.car(params))))) : (LISP.nil)));});
LISP["traverse-list"] = (function(s, scope){return ((function(__3){return ((function(__4){return ((LISP.isTrue(LISP["eq?"](__4, LISP.intern("quote"))) ? (LISP.apply((function(x){return ((LISP.isTrue(LISP["pair?"](x)) ? (LISP.vector(LISP["make-keyword"]("REF"), LISP["scope-add-var"](scope, LISP["traverse-quoted-value"](x)))) : (LISP.vector(LISP["make-keyword"]("CONST"), x))));}), LISP.cdr(__3))) : ((LISP.isTrue(LISP["eq?"](__4, LISP.intern("if"))) ? (LISP.apply((function(p, thn){var els = LISP._getRestArgs(arguments, 2); return (LISP.vector(LISP["make-keyword"]("IF"), LISP["traverse*"](p, scope), LISP["traverse*"](thn, scope), (LISP.isTrue(LISP["null?"](els)) ? (LISP.nil) : (LISP["traverse*"](LISP.car(els), scope)))));}), LISP.cdr(__3))) : ((LISP.isTrue(LISP["eq?"](__4, LISP.intern("set!"))) ? (LISP.apply((function(x, v){return (LISP.vector(LISP["make-keyword"]("SET!"), LISP["traverse*"](x, scope), LISP["traverse*"](v, scope)));}), LISP.cdr(__3))) : ((LISP.isTrue(LISP["eq?"](__4, LISP.intern("lambda"))) ? (LISP.apply((function(params){var body = LISP._getRestArgs(arguments, 1); return ((function(){return (LISP["confirm-valid-params"](params), (function(new$2dscope){return (LISP.vector(LISP["make-keyword"]("LAMBDA"), new$2dscope, params, LISP["traverse-args"](body, new$2dscope)));})(LISP["create-scope"](scope, params)));})());}), LISP.cdr(__3))) : ((LISP.isTrue(LISP["eq?"](__4, LISP.intern("def"))) ? (LISP.apply((function(name, value){return (LISP.vector(LISP["make-keyword"]("DEF"), LISP["traverse*"](name, scope), LISP["traverse*"](value, scope)));}), LISP.cdr(__3))) : ((LISP.isTrue(LISP["eq?"](__4, LISP.intern("new"))) ? (LISP.apply((function(klass){var args = LISP._getRestArgs(arguments, 1); return (LISP.vector(LISP["make-keyword"]("NEW"), klass, LISP["traverse-args"](args, LISP["new-scope"])));}), LISP.cdr(__3))) : (LISP.vector(LISP["make-keyword"]("FUNCALL"), LISP["traverse*"](LISP.car(s), scope), LISP["traverse-args"](LISP.cdr(s), scope)))))))))))))));})(LISP.car(__3)));})(s));});
LISP["traverse*"] = (function(s, scope){return ((LISP.isTrue(LISP["pair?"](s)) ? ((LISP.isTrue(LISP["local-var?"](scope, LISP.car(s))) ? (LISP["traverse-list"](s, scope)) : ((function(expanded){return ((LISP.isTrue(LISP["pair?"](expanded)) ? (LISP["traverse-list"](expanded, scope)) : (LISP["traverse*"](expanded, scope))));})(LISP.macroexpand(s))))) : ((LISP.isTrue(LISP["symbol?"](s)) ? (LISP.vector(LISP["make-keyword"]("REF"), s)) : (LISP.vector(LISP["make-keyword"]("CONST"), s))))));});
LISP["get-receiver"] = (function(sym){return ((function(s){return ((function(it){return ((LISP.isTrue(it) ? (LISP.intern(LISP.substring(s, 0, it))) : (sym)));})(LISP["string-scan"](s, ".")));})(LISP["symbol->string"](sym)));});
LISP["expand-args"] = (function(args, scope){return (LISP["string-join"](LISP.map((function(x){return (LISP["compile*"](x, scope));}), args), ", "));});
LISP["expand-body"] = (function(body, scope){return ((LISP.isTrue(LISP["null?"](body)) ? ("LISP.nil") : (LISP["expand-args"](body, scope))));});
(function(table){return (LISP["hash-table-put!"](table, "\\", "\\\\"), LISP["hash-table-put!"](table, "\t", "\\t"), LISP["hash-table-put!"](table, "\n", "\\n"), LISP["hash-table-put!"](table, "\"", "\\\""), LISP["escape-char"] = (function(c){return ((function(__5){return ((LISP.isTrue(__5) ? (__5) : (c)));})(LISP["hash-table-get"](table, c)));}));})(LISP["make-hash-table"]());
LISP["escape-string"] = (function(s){return (LISP["regexp-replace-all"](/[\\\t\n"]/, s, (function(m){return (LISP["escape-char"](m()));})));});
LISP["escape-sym-char"] = (function(c){return (LISP["string-append"]("$", LISP["integer->hex-string"](LISP["char->integer"](c), "00")));});
LISP["integer->hex-string"] = (function(x, padding){return ((function(s){return ((function(sl){return ((function(pl){return (LISP.substring(s, (sl - pl), sl));})(LISP["string-length"](padding)));})(LISP["string-length"](s)));})(LISP["string-append"](padding, LISP["number->string"](x, 16))));});
LISP["escape-symbol"] = (function(sym){return (LISP["regexp-replace-all"](/[^0-9A-Za-z_.]/, LISP["symbol->string"](sym), (function(m){return (LISP["escape-sym-char"](LISP["string-ref"](m(), 0)));})));});
LISP["compile-symbol"] = (function(sym, scope){return ((LISP.isTrue(LISP["local-var?"](scope, sym)) ? (LISP["escape-symbol"](sym)) : ((function(s){return ((LISP.isTrue(LISP.rxmatch(/^[0-9A-Za-z_.]*$/, s)) ? (LISP["string-append"]("LISP.", s)) : (LISP["string-append"]("LISP[\"", LISP["escape-string"](s), "\"]"))));})(LISP["symbol->string"](sym)))));});
LISP["compile-keyword"] = (function(keyword){return (LISP["string-append"]("LISP[\"make-keyword\"](\"", LISP["escape-string"](LISP["keyword->string"](keyword)), "\")"));});
LISP["compile-vector"] = (function(vect, scope){return (LISP["string-append"]("[", (function(v){return (v.join(", "));})(LISP["vector-map"]((function(x){return (LISP["compile-quote"](x, scope));}), vect)), "]"));});
LISP["compile-regexp"] = (function(regex){return (LISP["string-append"]("/", LISP["regexp->string"](regex), "/"));});
LISP["compile-literal"] = (function(s, scope){return ((LISP.isTrue(LISP["number?"](s)) ? (LISP["number->string"](s)) : ((LISP.isTrue(LISP["symbol?"](s)) ? (LISP["compile-symbol"](s, scope)) : ((LISP.isTrue(LISP["keyword?"](s)) ? (LISP["compile-keyword"](s)) : ((LISP.isTrue(LISP["string?"](s)) ? (LISP["x->string"](s, LISP.t)) : ((LISP.isTrue(LISP["vector?"](s)) ? (LISP["compile-vector"](s, scope)) : ((LISP.isTrue(LISP["regexp?"](s)) ? (LISP["compile-regexp"](s)) : ((LISP.isTrue(LISP["null?"](s)) ? ("LISP.nil") : ((LISP.isTrue(LISP["eq?"](s, LISP.t)) ? ("LISP.t") : (LISP.error(LISP["string-append"]("compile-literal: [", s, "]")))))))))))))))))));});
LISP["unary-op?"] = (function() { var __6 = LISP.list(LISP.intern("+"), LISP.intern("-"), LISP.intern("!"), LISP.intern("~")); return (function(sym){return (LISP.member(sym, __6));}); })();
LISP["compile-unary-op"] = (function(fn, arg, scope){return (LISP["string-append"]("(", LISP["symbol->string"](fn), LISP["compile*"](arg, scope), ")"));});
LISP["binop?"] = (function() { var __7 = LISP.list(LISP.intern("+"), LISP.intern("-"), LISP.intern("*"), LISP.intern("/"), LISP.intern("%")); return (function(sym){return (LISP.member(sym, __7));}); })();
LISP["compile-binop"] = (function(fn, args, scope){return (LISP["string-append"]("(", LISP["string-join"](LISP.map((function(x){return (LISP["compile*"](x, scope));}), args), LISP["string-append"](" ", LISP["symbol->string"](fn), " ")), ")"));});
LISP["do-compile-funcall"] = (function(fn, args, scope){return (LISP["string-append"](LISP["compile*"](fn, scope), "(", LISP["expand-args"](args, scope), ")"));});
LISP["compile-funcall"] = (function(fn, args, scope){return ((LISP.isTrue((LISP.isTrue(LISP["eq?"](LISP["vector-ref"](fn, 0), LISP["make-keyword"]("REF"))) ? ((LISP.isTrue(LISP.not(LISP["local-var?"](scope, LISP["vector-ref"](fn, 1)))) ? (LISP.not(LISP["null?"](args))) : (LISP.nil))) : (LISP.nil))) ? ((function(fnsym){return ((LISP.isTrue((LISP.isTrue(LISP["binop?"](fnsym)) ? (LISP.not(LISP["null?"](LISP.cdr(args)))) : (LISP.nil))) ? (LISP["compile-binop"](fnsym, args, scope)) : ((LISP.isTrue((LISP.isTrue(LISP["unary-op?"](fnsym)) ? (LISP["null?"](LISP.cdr(args))) : (LISP.nil))) ? (LISP["compile-unary-op"](fnsym, LISP.car(args), scope)) : (LISP["do-compile-funcall"](fn, args, scope))))));})(LISP["vector-ref"](fn, 1))) : (LISP["do-compile-funcall"](fn, args, scope))));});
LISP["compile-quote"] = (function(x, scope){return ((LISP.isTrue(LISP["pair?"](x)) ? (LISP["compile*"](LISP.list(LISP.intern("cons"), LISP.list(LISP.intern("quote"), LISP.car(x)), LISP.list(LISP.intern("quote"), LISP.cdr(x))), scope)) : ((LISP.isTrue(LISP["symbol?"](x)) ? (LISP["string-append"]("LISP.intern(\"", LISP["escape-string"](LISP["symbol->string"](x)), "\")")) : ((LISP.isTrue(LISP["keyword?"](x)) ? (LISP["compile-keyword"](x)) : (LISP["compile-literal"](x, scope))))))));});
LISP["compile-if"] = (function(pred$2dnode, then$2dnode, else$2dnode, scope){return (LISP["string-append"]("(LISP.isTrue(", LISP["compile*"](pred$2dnode, scope), ") ? (", LISP["compile*"](then$2dnode, scope), ") : (", (LISP.isTrue(else$2dnode) ? (LISP["compile*"](else$2dnode, scope)) : ("LISP.nil")), "))"));});
LISP["compile-set!"] = (function(sym, val, scope){return (LISP["string-append"](LISP["compile*"](sym, scope), " = ", LISP["compile*"](val, scope)));});
LISP["compile-lambda"] = (function(params, bodies, base$2dscope, extended$2dscope){return ((LISP.isTrue((function(__8){return ((LISP.isTrue(__8) ? (__8) : (LISP["pair?"](params))));})(LISP["null?"](params))) ? (LISP.nil) : (LISP.error("function parameters must be a list"))), (function(rest$2dpos){return ((function(proper$2dparams, rest){return (LISP["string-append"]("(function(", LISP["string-join"](LISP.map((function(x){return (LISP["escape-symbol"](x));}), proper$2dparams), ", "), "){", (LISP.isTrue(LISP["null?"](rest)) ? ("") : (LISP["string-append"]("var ", LISP["escape-symbol"](rest), " = LISP._getRestArgs(arguments, ", LISP["number->string"](LISP.length(proper$2dparams)), "); "))), "return (", LISP["expand-body"](bodies, extended$2dscope), ");})"));})((LISP.isTrue(rest$2dpos) ? (LISP.take(rest$2dpos, params)) : (params)), (LISP.isTrue(rest$2dpos) ? (LISP.elt((rest$2dpos + 1), params)) : (LISP.nil))));})(LISP["position-if"]((function() { var __9 = LISP.list(LISP.intern("&rest"), LISP.intern("&body")); return (function(sym){return (LISP.member(sym, __9));}); })(), params)));});
LISP["compile-def"] = (function(name, value, scope){return (LISP["string-append"](LISP["compile*"](name, scope), " = ", LISP["compile*"](value, scope)));});
LISP["compile-new"] = (function(class$2dname, args, scope){return (LISP["string-append"]("new ", LISP["symbol->string"](class$2dname), "(", LISP["expand-args"](args, scope), ")"));});
LISP["compile-new-scope"] = (function(scope, compiled$2dbody){return ((function(it){return ((LISP.isTrue(it) ? (LISP["string-append"]("(function() { var ", LISP["string-join"](LISP.map((function(x){return (LISP["string-append"](LISP["escape-symbol"](LISP.car(x)), " = ", LISP["compile*"](LISP.cdr(x), scope)));}), LISP.reverse(it)), ", "), "; return ", compiled$2dbody, "; })()")) : (compiled$2dbody)));})(LISP["scope-get-var"](scope)));});
LISP["compile*"] = (function(s, scope){return ((function(__10){return ((LISP.isTrue(LISP["eq?"](__10, LISP["make-keyword"]("CONST"))) ? (LISP["compile-quote"](LISP["vector-ref"](s, 1), scope)) : ((LISP.isTrue(LISP["eq?"](__10, LISP["make-keyword"]("REF"))) ? (LISP["compile-symbol"](LISP["vector-ref"](s, 1), scope)) : ((LISP.isTrue(LISP["eq?"](__10, LISP["make-keyword"]("IF"))) ? ((function(p, thn, els){return (LISP["compile-if"](p, thn, els, scope));})(LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), LISP["vector-ref"](s, 3))) : ((LISP.isTrue(LISP["eq?"](__10, LISP["make-keyword"]("FUNCALL"))) ? (LISP["compile-funcall"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), scope)) : ((LISP.isTrue(LISP["eq?"](__10, LISP["make-keyword"]("SET!"))) ? (LISP["compile-set!"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), scope)) : ((LISP.isTrue(LISP["eq?"](__10, LISP["make-keyword"]("LAMBDA"))) ? ((function(extended$2dscope, params, body){return (LISP["compile-new-scope"](extended$2dscope, LISP["compile-lambda"](params, body, scope, extended$2dscope)));})(LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), LISP["vector-ref"](s, 3))) : ((LISP.isTrue(LISP["eq?"](__10, LISP["make-keyword"]("DEF"))) ? (LISP["compile-def"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), scope)) : ((LISP.isTrue(LISP["eq?"](__10, LISP["make-keyword"]("NEW"))) ? (LISP["compile-new"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), scope)) : (LISP["string-append"]("???", s, "???"))))))))))))))))));})(LISP["vector-ref"](s, 0)));});
LISP["compile-error"] = (function(){var args = LISP._getRestArgs(arguments, 0); return (LISP.error(args));});
LISP.compile = (function(s){return ((function(top$2dscope){return ((function(tree){return (LISP["compile-new-scope"](top$2dscope, LISP["compile*"](tree, top$2dscope)));})(LISP["traverse*"](s, top$2dscope)));})(LISP["create-scope"](LISP.nil, LISP.nil)));});

  return LISP;
})());
