(function(LISP) {
  'use strict';

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

  LISP.nil = false;
  LISP.t = true;

  LISP._getRestArgs = function(args, start) {
    return arrayToList(Array.prototype.slice.call(args, start));
  };
  LISP._output = (typeof(process) !== 'undefined'
                 ? function(str) {  // for node.js.
                   process.stdout.write(str);
                 } : function(str) {  // for browser.
                   console.log(str);
                 });

  LISP["*macro-table*"] = {};
  LISP["register-macro"] = function(name, func) {
    LISP['*macro-table*'][name] = func;
    return name;
  };
  LISP["do-compile-defmacro"] = function(name, exp) {
    var compiled = LISP.compile(exp);
    return ("LISP['register-macro'](LISP.intern(\"" +
            LISP['escape-string'](LISP['symbol->string'](name)) +
            "\"), " +
            compiled +
            ")");
  };
  LISP["macroexpand-1"] = function(s) {
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
  LISP.Symbol = function(name) {
    this.name = name;
  };
  LISP.Symbol.prototype = {
    toString: function() {
      return this.name;
    },
  };

  LISP["symbol->string"] = function(x) {
    return x.name;
  };
  var __gensymIndex = 0;
  LISP.gensym = function() {
    return LISP.intern("__" + (++__gensymIndex));
  };

  LISP.$$symbolTable = {};  // key(string) => Symbol object
  LISP.intern = function(name) {
    if (name in LISP.$$symbolTable)
      return LISP.$$symbolTable[name];
    return LISP.$$symbolTable[name] = new LISP.Symbol(name);
  };
  LISP["symbol?"] = function(x) {
    return jsBoolToS(x instanceof LISP.Symbol);
  };
  LISP.type = function(x) {
    var type = typeof x;
    if (type === 'object') {
      if (x instanceof LISP.Symbol)
        type = 'symbol';
      else if (x instanceof LISP.Cons)
        type = 'pair';
      else if (x instanceof Array)
        type = 'vector';
      else if (x instanceof LISP.HashTable)
        type = 'table';
    }
    return LISP.intern(type);
  };

  LISP["eq?"] = function(x, y) {
    return jsBoolToS(x === y);
  };

  // Cons cell.
  LISP.Cons = function(car, cdr) {
    this.car = car;
    this.cdr = cdr;
  };

  LISP.Cons.prototype = {
    toString: (function() {
      var abbrevTable = { quote: "'", quasiquote: '`', unquote: ',', "unquote-splicing": ',@' };
      return function(inspect) {
        if (LISP['symbol?'](this.car) && LISP['pair?'](this.cdr) && LISP['null?'](this.cdr.cdr) &&
            this.car.name in abbrevTable) {
          return abbrevTable[this.car.name] + LISP.makeString(this.cdr.car, inspect);
        }

        var ss = [];
        var separator = "(";
        var p;
        for (p = this; p instanceof LISP.Cons; p = p.cdr) {
          ss.push(separator);
          ss.push(LISP.makeString(p.car, inspect));
          separator = " ";
        }
        if (p !== LISP.nil) {
          ss.push(" . ");
          ss.push(LISP.makeString(p, inspect));
        }
        ss.push(")");
        return ss.join("");
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
    if (s === LISP.nil)
      return s;
    return s.car;
  };
  LISP.cdr = function(s) {
    if (s === LISP.nil)
      return s;
    return s.cdr;
  };
  LISP["set-car!"] = function(s, x) {
    return (s.car = x);
  };
  LISP["set-cdr!"] = function(s, x) {
    return (s.cdr = x);
  };

  LISP["pair?"] = function(x) {
    return jsBoolToS(x instanceof LISP.Cons);
  };
  LISP.list = function() {
    var result = LISP.nil;
    for (var i = arguments.length; --i >= 0; )
      result = LISP.cons(arguments[i], result);
    return result;
  };
  LISP["reverse!"] = function(x) {
    var rev = LISP.nil;
    for (var ls = x; LISP['pair?'](ls, LISP.nil); ) {
      var d = ls.cdr;
      ls.cdr = rev;
      rev = ls;
      ls = d;
    }
    return rev;
  };

  LISP["number?"] = function(x) {
    return jsBoolToS(typeof x === 'number');
  };
  LISP["number->string"] = function(x, n) {
    return x.toString(n);
  };
  LISP["+"] = function() {
    if (arguments.length == 0)
      return 0;
    var result = arguments[0];
    for (var i = 1; i < arguments.length; ++i)
      result += arguments[i];
    return result;
  };
  LISP["*"] = function() {
    if (arguments.length == 0)
      return 1;
    var result = arguments[0];
    for (var i = 1; i < arguments.length; ++i)
      result *= arguments[i];
    return result;
  };
  LISP["-"] = function() {
    if (arguments.length == 0)
      return 0;
    var result = arguments[0];
    if (arguments.length == 1)
      return -result;
    for (var i = 1; i < arguments.length; ++i)
      result -= arguments[i];
    return result;
  };
  LISP["/"] = function() {
    if (arguments.length == 0)
      return 1;
    var result = arguments[0];
    if (arguments.length == 1)
      return 1.0 / result;
    for (var i = 1; i < arguments.length; ++i)
      result /= arguments[i];
    return result;
  };
  LISP["%"] = function() {
    if (arguments.length == 0)
      return 0;
    var result = arguments[0];
    if (arguments.length == 1)
      return result;
    for (var i = 1; i < arguments.length; ++i)
      result %= arguments[i];
    return result;
  };
  LISP["<"] = function() {
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
  LISP[">"] = function() {
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
  LISP["<="] = function() {
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
  LISP[">="] = function() {
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
  LISP["string?"] = function(x) {
    return jsBoolToS(typeof x === 'string');
  };
  LISP["string=?"] = function(x, y) {
    return jsBoolToS(x === y);
  };
  LISP["string-append"] = function() {
    return arguments2Array(arguments, 0).join('');
  };
  LISP["string-join"] = function(list, separator) {
    if (list === LISP.nil)
      return '';
    return list.toArray().join(separator);
  };
  LISP["string-length"] = function(str) {
    return str.length;
  };
  LISP["string-ref"] = function(str, index) {
    return str[index];
  };
  LISP.substring = function(str, start, end) {
    return str.slice(start, end);
  };
  LISP["string-scan"] = function(str, item) {
    var index = str.indexOf(item);
    return index >= 0 ? index : LISP.nil;
  };

  LISP["char->integer"] = function(char, index) {
    return char.charCodeAt(index);
  };

  LISP._escapeCharTable = { '\\': '\\\\', '\t': '\\t', '\n': '\\n' };
  LISP._inspectString = function(str) {
    return '"' + str.replace(/[\\\t\n"]/g, function(m) { return LISP._escapeCharTable[m]; }) + '"';
  };

  LISP.makeString = function(x, inspect) {
    if (x === LISP.nil)
      return 'nil';
    if (x === LISP.t)
      return 't';
    if (typeof x == 'string')
      return inspect ? LISP._inspectString(x) : x;
    if (x instanceof Array)
      return '#(' + x.map(function(v) { return LISP.makeString(v, inspect) }).join(' ') + ')';
    if (x === undefined || x === null)
      return '' + x;
    return x.toString(inspect);
  };
  LISP.print = function(x) {
    LISP._output(LISP.makeString(x));
    return x;
  };
  LISP.puts = function(x) {
    LISP._output(LISP.makeString(x));
    if (typeof(process) !== 'undefined')
      LISP._output('\n');
    return x;
  };
  LISP.write = function(x) {
    LISP._output(LISP.makeString(x, 10));  // 10 means true, and it is used as radix.
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

  // Hash table.
  LISP["make-hash-table"] = function() {
    return new LISP.HashTable();
  };
  LISP["hash-table?"] = function(x) {
    return x instanceof LISP.HashTable;
  };
  LISP["hash-table-exists?"] = function(hash, x) {
    return x in hash ? LISP.t : LISP.nil;
  };
  LISP["hash-table-get"] = function(hash, x) {
    if (x in hash)
      return hash[x];
    return (arguments.length >= 3) ? arguments[3 - 1] : LISP.nil;
  };
  LISP["hash-table-put!"] = function(hash, x, value) {
    return hash[x] = value;
  };

  // Vector.
  LISP.vector = function() {
    return arguments2Array(arguments, 0);
  };
  LISP["make-vector"] = function(count, value) {
    if (value === undefined)
      value = LISP.nil;
    var vector = new Array(count);
    for (var i = 0; i < count; ++i)
      vector[i] = value;
    return vector;
  };
  LISP["vector?"] = function(x) {
    return jsBoolToS(x instanceof Array);
  };
  LISP["vector-length"] = function(vector) {
    return vector.length;
  };
  LISP["vector-ref"] = function(vector, index) {
    return vector[index];
  };
  LISP["vector-set!"] = function(vector, index, value) {
    return vector[index] = value;
  };

  // Regexp.
  LISP["regexp?"] = function(x) {
    return jsBoolToS(x instanceof RegExp);
  };
  LISP.rxmatch = function(re, str) {
    return jsBoolToS(re.exec(str));
  };
  LISP["regexp-replace-all"] = function(re, str, fn) {
    if (!re.global)
      re = eval(re.toString() + 'g')
    return str.replace(re, function (match) {
      return fn(function() {  // TODO: handle arguments.
        return match;
      });
    });
  };
  LISP["regexp->string"] = function(x) {
    var s = x.toString();
    return s.slice(1, s.length - 1);
  };


  // System
  LISP.exit = function(code) {
    process.exit(code);
  };


  // Reader.
  LISP.Reader = function(str) {
    this.str = str;
  };

  LISP.NoCloseParenException = function() {};

  LISP.Reader.prototype = {
    read: function() {
      var m;
      if (m = this.str.match(/^\s*\(/))  // Left paren '('.
        return this.proceed(), this.readList(RegExp.rightContext);
      if (m = this.str.match(/^\s*;[^\n]*\n?/))  // Line comment.
        return this.proceed(), this.read();
      if (m = this.str.match(/^\s*'/))  // quote.
        return this.proceed(), this.readQuote();
      if (m = this.str.match(/^\s*"((\\.|[^"\\])*)"/))  // string.
        return this.proceed(), this.unescape(m[1]);
      if (m = this.str.match(/^\s*`/))  // quasiquote.
        return this.proceed(), this.readQuasiQuote();
      if (m = this.str.match(/^\s*,(@?)/))  // unquote or unquote-splicing.
        return this.proceed(), this.readUnquote(m[1]);
      if (m = this.str.match(/^\s*#\(/))  // vector.
        return this.proceed(), this.readVector();
      if (m = this.str.match(/^\s*#\/([^\/]*)\//))  // regexp TODO: Implement properly.
        return this.proceed(), new RegExp(m[1]);
      if (m = this.str.match(/^\s*#\|(.|[\n\r])*?\|#/))  // Block comment.
        return this.proceed(), this.read();
      if (m = this.str.match(/^\s*([^\s(){}\[\]'`,;#]+)/))  // Symbol or number.
        return this.readSymbolOrNumber(m[1]);
      return undefined;
    },

    proceed: function() {
      this.str = RegExp.rightContext;
    },

    readSymbolOrNumber: function(str) {
      if (str === '.')  // Refuse single dot.
        return undefined;

      this.proceed();
      if (str.match(/^([+\-]?[0-9]+(\.[0-9]*)?)$/))  // Number.
        return parseFloat(str);
      return LISP.intern(str);
    },

    readList: function() {
      var result = LISP.nil;
      var m;
      for (;;) {
        var x = this.read();
        if (x !== undefined) {
          result = LISP.cons(x, result);
          continue;
        }

        if (m = this.str.match(/^\s*\)/)) {  // Close paren.
          this.proceed();
          return LISP['reverse!'](result);
        }
        if (m = this.str.match(/^\s*\.\s/)) {  // Dot.
          this.proceed();
          var last = this.read();
          if (last !== undefined) {
            if (m = this.str.match(/^\s*\)/)) {  // Close paren.
              this.proceed();
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

    readVector: function() {
      var result = [];
      var m;
      for (;;) {
        var x = this.read();
        if (x !== undefined) {
          result.push(x);
          continue;
        }

        if (m = this.str.match(/^\s*\)/)) {  // Close paren.
          this.proceed();
          return result;
        }
        // Error
        throw new LISP.NoCloseParenException();
      }
    },

    readQuote: function() {
      return LISP.list(LISP.intern('quote'), this.read());
    },

    readQuasiQuote: function() {
      return LISP.list(LISP.intern('quasiquote'), this.read());
    },

    readUnquote: function(splicing) {
      var keyword = splicing === '@' ? 'unquote-splicing' : 'unquote';
      return LISP.list(LISP.intern(keyword), this.read());
    },

    unescape: function(str) {
      return str.replace(/(\\x[0-9a-fA-F]{2})/g, function(match) {
        return eval('"' + match + '"');
      }).replace(/\\./g, function(match) {
        switch (match[1]) {
        case 't':  return '\t';
        case 'n':  return '\n';
        default:  return match[1];
        }
      });
    },
  };

  LISP["read-from-string"] = function(str) {
    var reader = new LISP.Reader(str);
    return reader.read();
  };

  /*==== EMBED COMPILED CODE HERE ====*/
})(typeof exports !== 'undefined' ? exports : (this.LISP = {}));
