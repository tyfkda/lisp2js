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

  return LISP;
})());
