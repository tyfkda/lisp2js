LISP = (function() {
  'use strict';

  // Convert JS array into Lisp list.
  var arrayToList = function(array) {
    var result = LISP.nil;
    for (var i = array.length; --i >= 0; )
      result = LISP.cons(array[i], result);
    return result;
  };

  var LISP = {
    nil: false,
    t: true,

    _jsBoolToS: function(x)  { return x ? LISP.t : LISP.nil; },
    _getRestArgs: function(args, start) {
      return arrayToList(Array.prototype.slice.call(args, start));
    },
    _output: (typeof(process) !== 'undefined'
              ? function(str) {  // for node.js.
                process.stdout.write(str);
              } : function(str) {  // for browser.
                console.log(str);
              }),
    _arguments2Array: function(args, start) {
      var len = args.length - start;
      if (len <= 0)
        return [];
      var array = new Array(len);
      for (var i = 0; i < len; ++i)
        array[i] = args[i + start];
      return array;
    },

    "*macro-table*": {},
    "register-macro": function(name, func) {
      LISP['*macro-table*'][name] = func;
      return name;
    },
    "do-compile-defmacro": function(name, exp) {
      var compiled = LISP.compile(exp);
      return ("LISP['register-macro'](LISP.intern(\"" +
              LISP['escape-string'](LISP['symbol->string'](name)) +
              "\"), " +
              compiled +
              ")");
    },
    "macroexpand-1": function(s) {
      if (!LISP['pair?'](s) || !(s.car in LISP['*macro-table*']))
        return s;
      var macrofn = LISP['*macro-table*'][s.car];
      return LISP.apply(macrofn, s.cdr);
    },

    jseval: function(str) {
      return eval(str);
    },
    eval: function(exp) {
      return eval(LISP.compile(exp));
    },

    error: function() {
      throw LISP._arguments2Array(arguments, 0).join(', ');
    },

    Symbol: function(name) {
      this.name = name;
    },
    "symbol->string": function(x) {
      return x.name;
    },
    __gensymIndex: 0,
    gensym: function() {
      return LISP.intern("__" + (++LISP.__gensymIndex));
    },

    $$symbolTable: {},  // key(string) => Symbol object
    intern: function(name) {
      if (name in LISP.$$symbolTable)
        return LISP.$$symbolTable[name];
      return LISP.$$symbolTable[name] = new LISP.Symbol(name);
    },
    "symbol?": function(x) {
      return LISP._jsBoolToS(x instanceof LISP.Symbol);
    },

    "eq?": function(x, y) {
      return LISP._jsBoolToS(x === y);
    },

    Cons: function(car, cdr) {
      this.car = car;
      this.cdr = cdr;
    },

    cons: function(car, cdr) {
      return new LISP.Cons(car, cdr);
    },
    car: function(s) {
      if (s === LISP.nil)
        return s;
      return s.car;
    },
    cdr: function(s) {
      if (s === LISP.nil)
        return s;
      return s.cdr;
    },
    "set-car!": function(s, x) {
      return (s.car = x);
    },
    "set-cdr!": function(s, x) {
      return (s.cdr = x);
    },

    "pair?": function(x) {
      return LISP._jsBoolToS(x instanceof LISP.Cons);
    },
    list: function() {
      var result = LISP.nil;
      for (var i = arguments.length; --i >= 0; )
        result = LISP.cons(arguments[i], result);
      return result;
    },
    "reverse!": function(x) {
      var rev = LISP.nil;
      for (var ls = x; LISP['pair?'](ls, LISP.nil); ) {
        var d = ls.cdr;
        ls.cdr = rev;
        rev = ls;
        ls = d;
      }
      return rev;
    },

    "number?": function(x) {
      return LISP._jsBoolToS(typeof x === 'number');
    },
    "number->string": function(x, n) {
      return x.toString(n);
    },
    "+": function() {
      if (arguments.length == 0)
        return 0;
      var result = arguments[0];
      for (var i = 1; i < arguments.length; ++i)
        result += arguments[i];
      return result;
    },
    "*": function() {
      if (arguments.length == 0)
        return 1;
      var result = arguments[0];
      for (var i = 1; i < arguments.length; ++i)
        result *= arguments[i];
      return result;
    },
    "-": function() {
      if (arguments.length == 0)
        return 0;
      var result = arguments[0];
      if (arguments.length == 1)
        return -result;
      for (var i = 1; i < arguments.length; ++i)
        result -= arguments[i];
      return result;
    },
    "/": function() {
      if (arguments.length == 0)
        return 1;
      var result = arguments[0];
      if (arguments.length == 1)
        return 1.0 / result;
      for (var i = 1; i < arguments.length; ++i)
        result /= arguments[i];
      return result;
    },
    "%": function() {
      if (arguments.length == 0)
        return 0;
      var result = arguments[0];
      if (arguments.length == 1)
        return result;
      for (var i = 1; i < arguments.length; ++i)
        result %= arguments[i];
      return result;
    },
    "<": function() {
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
    },
    ">": function() {
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
    },
    "<=": function() {
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
    },
    ">=": function() {
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
    },

    // String.
    "string?": function(x) {
      return LISP._jsBoolToS(typeof x === 'string');
    },
    "string=?": function(x, y) {
      return LISP._jsBoolToS(x === y);
    },
    "string-append": function() {
      return LISP._arguments2Array(arguments, 0).join('');
    },
    "string-join": function(list, separator) {
      if (list === LISP.nil)
        return '';
      return list.toArray().join(separator);
    },
    "string-length": function(str) {
      return str.length;
    },
    "string-ref": function(str, index) {
      return str[index];
    },
    substring: function(str, start, end) {
      return str.slice(start, end);
    },
    "string-scan": function(str, item) {
      var index = str.indexOf(item);
      return index >= 0 ? index : LISP.nil;
    },

    "char->integer": function(char, index) {
      return char.charCodeAt(index);
    },

    _escapeCharTable: { '\\': '\\\\', '\t': '\\t', '\n': '\\n' },
    _inspectString: function(str) {
      return '"' + str.replace(/[\\\t\n"]/g, function(m) { return LISP._escapeCharTable[m]; }) + '"';
    },

    makeString: function(x, inspect) {
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
    },
    print: function(x) {
      LISP._output(LISP.makeString(x));
      return x;
    },
    puts: function(x) {
      LISP._output(LISP.makeString(x));
      if (typeof(process) !== 'undefined')
        LISP._output('\n');
      return x;
    },
    write: function(x) {
      LISP._output(LISP.makeString(x, 10));  // 10 means true, and it is used as radix.
      return x;
    },

    apply: function(fn) {
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
    },
    JS: ((typeof window !== 'undefined') ? window :
         (typeof GLOBAL !== 'undefined') ? GLOBAL : {}),

    // Hash table.
    "make-hash-table": function() {
      return {};
    },
    "hash-table-exists?": function(hash, x) {
      return x in hash ? LISP.t : LISP.nil;
    },
    "hash-table-get": function(hash, x) {
      if (x in hash)
        return hash[x];
      return (arguments.length >= 3) ? arguments[3 - 1] : LISP.nil;
    },
    "hash-table-put!": function(hash, x, value) {
      return hash[x] = value;
    },

    // Vector.
    vector: function() {
      return LISP._arguments2Array(arguments, 0);
    },
    "make-vector": function(count, value) {
      if (value === undefined)
        value = LISP.nil;
      var vector = new Array(count);
      for (var i = 0; i < count; ++i)
        vector[i] = value;
      return vector;
    },
    "vector?": function(x) {
      return LISP._jsBoolToS(x instanceof Array);
    },
    "vector-length": function(vector) {
      return vector.length;
    },
    "vector-ref": function(vector, index) {
      return vector[index];
    },
    "vector-set!": function(vector, index, value) {
      return vector[index] = value;
    },

    // Regexp.
    "regexp?": function(x) {
      return LISP._jsBoolToS(x instanceof RegExp);
    },
    rxmatch: function(re, str) {
      return LISP._jsBoolToS(re.exec(str));
    },
    "regexp-replace-all": function(re, str, fn) {
      if (!re.global)
        re = eval(re.toString() + 'g')
      return str.replace(re, function (match) {
        return fn(function() {  // TODO: handle arguments.
          return match;
        });
      });
    },
    "regexp->string": function(x) {
      var s = x.toString();
      return s.slice(1, s.length - 1);
    },
  };

  LISP.Symbol.prototype = {
    toString: function() {
      return this.name;
    },
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
LISP['register-macro'](LISP.intern("let"), (function(pairs){var body = LISP._getRestArgs(arguments, 1); return (((LISP["symbol?"](pairs)) !== LISP.nil ? ((function() { var __2 = LISP.list(LISP.intern("nil")); return (function(name, pairs, body){return (LISP["list*"](LISP.list(LISP.intern("lambda"), LISP.list(name), LISP.list(LISP.intern("set!"), name, LISP["list*"](LISP.intern("lambda"), LISP.map(LISP.car, pairs), body)), LISP["list*"](name, LISP.map(LISP.cadr, pairs))), __2));}); })()(pairs, LISP.car(body), LISP.cdr(body))) : (LISP["list*"](LISP["list*"](LISP.intern("lambda"), LISP.map(LISP.car, pairs), body), LISP.map(LISP.cadr, pairs)))));}));
LISP['register-macro'](LISP.intern("let1"), (function(name, value){var body = LISP._getRestArgs(arguments, 2); return (LISP.list(LISP["list*"](LISP.intern("lambda"), LISP.list(name), body), value));}));
LISP['register-macro'](LISP.intern("let*"), (function(pairs){var body = LISP._getRestArgs(arguments, 1); return (((LISP["null?"](pairs)) !== LISP.nil ? (LISP["list*"](LISP.intern("begin"), body)) : (LISP.list(LISP.intern("let1"), LISP.caar(pairs), LISP.cadar(pairs), LISP["list*"](LISP.intern("let*"), LISP.cdr(pairs), body)))));}));
LISP['register-macro'](LISP.intern("when"), (function(pred){var body = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("if"), pred, LISP["list*"](LISP.intern("begin"), body)));}));
LISP['register-macro'](LISP.intern("unless"), (function(pred){var body = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("if"), pred, LISP.intern("nil"), LISP["list*"](LISP.intern("begin"), body)));}));
LISP['register-macro'](LISP.intern("cond"), (function(){var clauses = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](clauses)) !== LISP.nil ? (LISP.nil) : ((function(clause, rest){return (((LISP["eq?"](LISP.car(clause), LISP.intern("else"))) !== LISP.nil ? (LISP["list*"](LISP.intern("begin"), LISP.cdr(clause))) : (((LISP["null?"](LISP.cdr(clause))) !== LISP.nil ? ((function(g){return (LISP.list(LISP.intern("let"), LISP.list(LISP.list(g, LISP.car(clause))), LISP.list(LISP.intern("if"), g, g, LISP["list*"](LISP.intern("cond"), rest))));})(LISP.gensym())) : (((LISP["eq?"](LISP.cadr(clause), LISP.intern("=>"))) !== LISP.nil ? ((function(g){return (LISP.list(LISP.intern("let"), LISP.list(LISP.list(g, LISP.car(clause))), LISP.list(LISP.intern("if"), g, LISP.list(LISP.caddr(clause), g), LISP["list*"](LISP.intern("cond"), rest))));})(LISP.gensym())) : (LISP.list(LISP.intern("if"), LISP.car(clause), LISP["list*"](LISP.intern("begin"), LISP.cdr(clause)), LISP["list*"](LISP.intern("cond"), rest)))))))));})(LISP.car(clauses), LISP.cdr(clauses)))));}));
LISP['register-macro'](LISP.intern("case"), (function(x){var clauses = LISP._getRestArgs(arguments, 1); return ((function(value){return (LISP.list(LISP.intern("let1"), value, x, LISP["list*"](LISP.intern("cond"), LISP.map((function(clause){return (((LISP["eq?"](LISP.car(clause), LISP.intern("else"))) !== LISP.nil ? (clause) : (((LISP["null?"](LISP.cdar(clause))) !== LISP.nil ? (LISP["list*"](LISP.list(LISP.intern("eq?"), value, LISP.list(LISP.intern("quote"), LISP.caar(clause))), LISP.cdr(clause))) : (LISP["list*"](LISP.list(LISP.intern("member"), value, LISP.list(LISP.intern("quote"), LISP.car(clause))), LISP.cdr(clause)))))));}), clauses))));})(LISP.gensym()));}));
LISP['register-macro'](LISP.intern("and"), (function() { var __3 = LISP.list(LISP.intern("nil")); return (function(){var args = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](args)) !== LISP.nil ? (LISP.intern("t")) : (((LISP["null?"](LISP.cdr(args))) !== LISP.nil ? (LISP.car(args)) : (LISP["list*"](LISP.intern("if"), LISP.car(args), LISP["list*"](LISP.intern("and"), LISP.cdr(args)), __3))))));}); })());
LISP['register-macro'](LISP.intern("or"), (function(){var args = LISP._getRestArgs(arguments, 0); return (((LISP.not(LISP["null?"](args))) !== LISP.nil ? ((function(g){return (LISP.list(LISP.intern("let1"), g, LISP.car(args), LISP.list(LISP.intern("if"), g, g, LISP["list*"](LISP.intern("or"), LISP.cdr(args)))));})(LISP.gensym())) : (LISP.nil)));}));
LISP['register-macro'](LISP.intern("begin"), (function(){var body = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](body)) !== LISP.nil ? (LISP.nil) : (((LISP["null?"](LISP.cdr(body))) !== LISP.nil ? (LISP.car(body)) : (LISP["list*"](LISP.intern("let"), LISP.nil, body))))));}));
LISP['register-macro'](LISP.intern("aif"), (function(expr){var rest = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("let1"), LISP.intern("it"), expr, LISP["list*"](LISP.intern("if"), LISP.intern("it"), rest)));}));
LISP["null?"] = (function(x){return (LISP["eq?"](x, LISP.nil));});
LISP.not = (function(x){return (LISP["eq?"](x, LISP.nil));});
LISP.caar = (function(x){return (LISP.car(LISP.car(x)));});
LISP.cadr = (function(x){return (LISP.car(LISP.cdr(x)));});
LISP.cdar = (function(x){return (LISP.cdr(LISP.car(x)));});
LISP.cddr = (function(x){return (LISP.cdr(LISP.cdr(x)));});
LISP.cadar = (function(x){return (LISP.cadr(LISP.car(x)));});
LISP.caddr = (function(x){return (LISP.car(LISP.cddr(x)));});
LISP.cdddr = (function(x){return (LISP.cdr(LISP.cddr(x)));});
LISP["equal?"] = (function(x, y){return (((LISP["eq?"](x, y)) !== LISP.nil ? (LISP.t) : (((LISP["pair?"](x)) !== LISP.nil ? (((LISP["pair?"](y)) !== LISP.nil ? (((LISP["equal?"](LISP.car(x), LISP.car(y))) !== LISP.nil ? (LISP["equal?"](LISP.cdr(x), LISP.cdr(y))) : (LISP.nil))) : (LISP.nil))) : (LISP.nil)))));});
LISP.length = (function(ls){return ((function(loop){return (loop = (function(ls, acc){return (((LISP["pair?"](ls)) !== LISP.nil ? (loop(LISP.cdr(ls), (acc + 1))) : (acc)));}), loop(ls, 0));})(LISP.nil));});
LISP["last-pair"] = (function(ls){return (((LISP["pair?"](LISP.cdr(ls))) !== LISP.nil ? (LISP["last-pair"](LISP.cdr(ls))) : (ls)));});
LISP["proper-list?"] = (function(ls){return (((LISP["pair?"](ls)) !== LISP.nil ? (LISP["null?"](LISP.cdr(LISP["last-pair"](ls)))) : (LISP.nil)));});
LISP.member = (function(x, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (((LISP["eq?"](x, LISP.car(ls))) !== LISP.nil ? (ls) : (LISP.member(x, LISP.cdr(ls)))))));});
LISP.assoc = (function(x, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (((LISP["eq?"](x, LISP.caar(ls))) !== LISP.nil ? (LISP.car(ls)) : (LISP.assoc(x, LISP.cdr(ls)))))));});
LISP.map = (function(f, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (LISP.cons(f(LISP.car(ls)), LISP.map(f, LISP.cdr(ls))))));});
LISP.append = (function(ls){var rest = LISP._getRestArgs(arguments, 1); return (((LISP["null?"](rest)) !== LISP.nil ? (ls) : (((LISP["null?"](ls)) !== LISP.nil ? (LISP.apply(LISP.append, rest)) : (LISP.cons(LISP.car(ls), LISP.apply(LISP.append, LISP.cdr(ls), rest)))))));});
LISP.reverse = (function(ls){return ((function(loop){return (loop = (function(ls, acc){return (((LISP["pair?"](ls)) !== LISP.nil ? (loop(LISP.cdr(ls), LISP.cons(LISP.car(ls), acc))) : (acc)));}), loop(ls, LISP.nil));})(LISP.nil));});
LISP["list*"] = (function(){var args = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](args)) !== LISP.nil ? (LISP.nil) : (((LISP["null?"](LISP.cdr(args))) !== LISP.nil ? (LISP.car(args)) : ((function(loop){return (loop = (function(p, q){return (((LISP["null?"](LISP.cdr(q))) !== LISP.nil ? ((function(){return (LISP["set-cdr!"](p, LISP.car(q)), args);})()) : (loop(q, LISP.cdr(q)))));}), loop(args, LISP.cdr(args)));})(LISP.nil))))));});
LISP["last-pair"] = (function(ls){return (((LISP["pair?"](LISP.cdr(ls))) !== LISP.nil ? (LISP["last-pair"](LISP.cdr(ls))) : (ls)));});
LISP["proper-list?"] = (function(ls){return (LISP["null?"](LISP.cdr(LISP["last-pair"](ls))));});
LISP["dotted->proper"] = (function(ls){return (((LISP["pair?"](ls)) !== LISP.nil ? (((LISP["proper-list?"](ls)) !== LISP.nil ? (ls) : ((function(dot, rev){return ((function(dup){return (LISP["set-cdr!"](rev, LISP.list(dot)), dup);})(LISP["reverse!"](rev)));})(LISP.cdr(LISP["last-pair"](ls)), LISP.reverse(ls))))) : (LISP.list(ls))));});
LISP["vector-map"] = (function(proc, vect){return ((function(len){return ((function(new$2dvect){return ((function(loop){return (loop = (function(i){return (((LISP[">="](i, len)) !== LISP.nil ? (new$2dvect) : ((function(){return (LISP["vector-set!"](new$2dvect, i, proc(LISP["vector-ref"](vect, i))), loop((i + 1)));})())));}), loop(0));})(LISP.nil));})(LISP["make-vector"](len)));})(LISP["vector-length"](vect)));});
LISP.nreconc = (function(ls, tail){return ((function(top){return (LISP["set-cdr!"](ls, tail), top);})(LISP["reverse!"](ls)));});
LISP.any = (function(f, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (((f(LISP.car(ls))) !== LISP.nil ? (LISP.t) : (LISP.any(f, LISP.cdr(ls)))))));});
LISP.every = (function(f, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.t) : (((f(LISP.car(ls))) !== LISP.nil ? (LISP.every(f, LISP.cdr(ls))) : (LISP.nil)))));});
LISP["*bq-clobberable*"] = LISP.gensym();
LISP["*bq-quote-nil*"] = LISP.list(LISP.intern("quote"), LISP.nil);
LISP['register-macro'](LISP.intern("quasiquote"), (function(x){return (LISP["bq-completely-process"](x));}));
LISP["bq-completely-process"] = (function(x){return (LISP["bq-simplify"](LISP["bq-process"](x)));});
LISP["bq-process"] = (function(x){return (((LISP.not(LISP["pair?"](x))) !== LISP.nil ? (LISP.list(LISP.intern("quote"), x)) : (((LISP["eq?"](LISP.car(x), LISP.intern("quasiquote"))) !== LISP.nil ? (LISP["bq-process"](LISP["bq-completely-process"](LISP.cadr(x)))) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote"))) !== LISP.nil ? (LISP.cadr(x)) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))) !== LISP.nil ? (LISP.error(",@~S after `", LISP.cadr(x))) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))) !== LISP.nil ? (LISP.error(",.~S after `", LISP.cadr(x))) : ((function(loop){return (loop = (function(p, q){return (((LISP.not(LISP["pair?"](p))) !== LISP.nil ? (LISP.cons(LISP.intern("append"), LISP.nreconc(q, LISP.list(LISP.list(LISP.intern("quote"), p))))) : (((LISP["eq?"](LISP.car(p), LISP.intern("unquote"))) !== LISP.nil ? ((function(){return (((LISP["null?"](LISP.cddr(p))) !== LISP.nil ? (LISP.nil) : (LISP.error("Malformed ,~S", p))), LISP.cons(LISP.intern("append"), LISP.nreconc(q, LISP.list(LISP.cadr(p)))));})()) : ((function(){return (((LISP["eq?"](LISP.car(p), LISP.intern("unquote-splicing"))) !== LISP.nil ? (LISP.error("Dotted ,@~S", p)) : (LISP.nil)), ((LISP["eq?"](LISP.car(p), LISP.intern("unquote-dot"))) !== LISP.nil ? (LISP.error("Dotted ,.~S", p)) : (LISP.nil)), loop(LISP.cdr(p), LISP.cons(LISP.bracket(LISP.car(p)), q)));})())))));}), loop(x, LISP.nil));})(LISP.nil))))))))))));});
LISP.bracket = (function(x){return (((LISP.not(LISP["pair?"](x))) !== LISP.nil ? (LISP.list(LISP.intern("list"), LISP["bq-process"](x))) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote"))) !== LISP.nil ? (LISP.list(LISP.intern("list"), LISP.cadr(x))) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))) !== LISP.nil ? (LISP.cadr(x)) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))) !== LISP.nil ? (LISP.list(LISP["*bq-clobberable*"], LISP.cadr(x))) : (LISP.list(LISP.intern("list"), LISP["bq-process"](x)))))))))));});
LISP.maptree = (function(fn, x){return (((LISP.not(LISP["pair?"](x))) !== LISP.nil ? (fn(x)) : ((function(a, d){return (((((LISP["equal?"](a, LISP.car(x))) !== LISP.nil ? (LISP["equal?"](d, LISP.cdr(x))) : (LISP.nil))) !== LISP.nil ? (x) : (LISP.cons(a, d))));})(fn(LISP.car(x)), LISP.maptree(fn, LISP.cdr(x))))));});
LISP["bq-splicing-frob"] = (function(x){return (((LISP["pair?"](x)) !== LISP.nil ? ((function(__4){return (((__4) !== LISP.nil ? (__4) : ((function(__5){return (((__5) !== LISP.nil ? (__5) : (LISP.nil)));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing")))) : (LISP.nil)));});
LISP["bq-frob"] = (function(x){return (((LISP["pair?"](x)) !== LISP.nil ? ((function(__6){return (((__6) !== LISP.nil ? (__6) : ((function(__7){return (((__7) !== LISP.nil ? (__7) : ((function(__8){return (((__8) !== LISP.nil ? (__8) : (LISP.nil)));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote")))) : (LISP.nil)));});
LISP["bq-simplify"] = (function(x){return (((LISP["pair?"](x)) !== LISP.nil ? ((function(x){return (((LISP.not(LISP["eq?"](LISP.car(x), LISP.intern("append")))) !== LISP.nil ? (x) : (LISP["bq-simplify-args"](x))));})(((LISP["eq?"](LISP.car(x), LISP.intern("quote"))) !== LISP.nil ? (x) : (LISP.maptree(LISP["bq-simplify"], x))))) : (x)));});
LISP["bq-simplify-args"] = (function(x){return ((function(loop){return (loop = (function(args, result){return (((LISP.not(LISP["null?"](args))) !== LISP.nil ? (loop(LISP.cdr(args), ((LISP.not(LISP["pair?"](LISP.car(args)))) !== LISP.nil ? (LISP["bq-attach-append"](LISP.intern("append"), LISP.car(args), result)) : (((((LISP["eq?"](LISP.caar(args), LISP.intern("list"))) !== LISP.nil ? (LISP.not(LISP.any(LISP["bq-splicing-frob"], LISP.cdar(args)))) : (LISP.nil))) !== LISP.nil ? (LISP["bq-attach-conses"](LISP.cdar(args), result)) : (((((LISP["eq?"](LISP.caar(args), LISP.intern("list*"))) !== LISP.nil ? (LISP.not(LISP.any(LISP["bq-splicing-frob"], LISP.cdar(args)))) : (LISP.nil))) !== LISP.nil ? (LISP["bq-attach-conses"](LISP.reverse(LISP.cdr(LISP.reverse(LISP.cdar(args)))), LISP["bq-attach-append"](LISP.intern("append"), LISP.car(LISP.last(LISP.car(args))), result))) : (((((LISP["eq?"](LISP.caar(args), LISP.intern("quote"))) !== LISP.nil ? (((LISP["pair?"](LISP.cadar(args))) !== LISP.nil ? (((LISP.not(LISP["bq-frob"](LISP.cadar(args)))) !== LISP.nil ? (LISP.not(LISP.cddar(args))) : (LISP.nil))) : (LISP.nil))) : (LISP.nil))) !== LISP.nil ? (LISP["bq-attach-conses"](LISP.list(LISP.list(LISP.intern("quote"), LISP.caadar(args))), result)) : (((LISP["eq?"](LISP.caar(args), LISP["*bq-clobberable*"])) !== LISP.nil ? (LISP["bq-attach-append"](LISP.intern("append!"), LISP.cadar(args), result)) : (LISP["bq-attach-append"](LISP.intern("append"), LISP.car(args), result))))))))))))) : (result)));}), loop(LISP.reverse(LISP.cdr(x)), LISP.nil));})(LISP.nil));});
LISP["null-or-quoted"] = (function(x){return ((function(__9){return (((__9) !== LISP.nil ? (__9) : ((function(__10){return (((__10) !== LISP.nil ? (__10) : (LISP.nil)));})(((LISP["pair?"](x)) !== LISP.nil ? (LISP["eq?"](LISP.car(x), LISP.intern("quote"))) : (LISP.nil))))));})(LISP["null?"](x)));});
LISP["bq-attach-append"] = (function(op, item, result){return (((((LISP["null-or-quoted"](item)) !== LISP.nil ? (LISP["null-or-quoted"](result)) : (LISP.nil))) !== LISP.nil ? (LISP.list(LISP.intern("quote"), LISP.append(LISP.cadr(item), LISP.cadr(result)))) : ((((function(__11){return (((__11) !== LISP.nil ? (__11) : ((function(__12){return (((__12) !== LISP.nil ? (__12) : (LISP.nil)));})(LISP["equal?"](result, LISP["*bq-quote-nil*"])))));})(LISP["null?"](result))) !== LISP.nil ? (((LISP["bq-splicing-frob"](item)) !== LISP.nil ? (LISP.list(op, item)) : (item))) : (((((LISP["pair?"](result)) !== LISP.nil ? (LISP["eq?"](LISP.car(result), op)) : (LISP.nil))) !== LISP.nil ? (LISP["list*"](LISP.car(result), item, LISP.cdr(result))) : (LISP.list(op, item, result))))))));});
LISP["bq-attach-conses"] = (function(items, result){return (((((LISP.every(LISP["null-or-quoted"], items)) !== LISP.nil ? (LISP["null-or-quoted"](result)) : (LISP.nil))) !== LISP.nil ? (LISP.list(LISP.intern("quote"), LISP.append(LISP.map(LISP.cadr, items), LISP.cadr(result)))) : ((((function(__13){return (((__13) !== LISP.nil ? (__13) : ((function(__14){return (((__14) !== LISP.nil ? (__14) : (LISP.nil)));})(LISP["equal?"](result, LISP["*bq-quote-nil*"])))));})(LISP["null?"](result))) !== LISP.nil ? (LISP.cons(LISP.intern("list"), items)) : (((((LISP["pair?"](result)) !== LISP.nil ? ((function(__15){return (((__15) !== LISP.nil ? (__15) : ((function(__16){return (((__16) !== LISP.nil ? (__16) : (LISP.nil)));})(LISP["eq?"](LISP.car(result), LISP.intern("list*"))))));})(LISP["eq?"](LISP.car(result), LISP.intern("list")))) : (LISP.nil))) !== LISP.nil ? (LISP.cons(LISP.car(result), LISP.append(items, LISP.cdr(result)))) : (LISP.cons(LISP.intern("list*"), LISP.append(items, LISP.list(result))))))))));});
LISP["new-scope"] = (function(parent$2dscope, params){return (LISP.vector(LISP["dotted->proper"](params), LISP.nil, parent$2dscope));});
LISP["scope-param"] = (function(scope){return (LISP["vector-ref"](scope, 0));});
LISP["scope-outer"] = (function(scope){return (LISP["vector-ref"](scope, 2));});
LISP["scope-add-var"] = (function(scope, val){return ((function(x){return (LISP["vector-set!"](scope, 1, LISP.cons(LISP.cons(x, val), LISP["vector-ref"](scope, 1))), LISP["vector-set!"](scope, 0, LISP.cons(x, LISP["vector-ref"](scope, 0))), x);})(LISP.gensym()));});
LISP["scope-get-var"] = (function(scope){return (LISP["vector-ref"](scope, 1));});
LISP["scope-var?"] = (function(scope, x){return (((LISP["null?"](scope)) !== LISP.nil ? (LISP.nil) : (((LISP.member(x, LISP["scope-param"](scope))) !== LISP.nil ? (LISP.t) : (LISP["scope-var?"](LISP["scope-outer"](scope), x))))));});
LISP["traverse-args"] = (function(args, scope){return (LISP.map((function(x){return (LISP["traverse*"](x, scope));}), args));});
LISP['register-macro'](LISP.intern("record"), (function(args, param){var body = LISP._getRestArgs(arguments, 2); return (LISP.list(LISP.intern("apply"), LISP["list*"](LISP.intern("lambda"), param, body), args));}));
LISP['register-macro'](LISP.intern("record-case"), (function(x){var clauses = LISP._getRestArgs(arguments, 1); return ((function(value){return (LISP.list(LISP.intern("let1"), value, x, LISP["list*"](LISP.intern("case"), LISP.list(LISP.intern("car"), value), LISP.map((function(clause){return (((LISP["eq?"](LISP.car(clause), LISP.intern("else"))) !== LISP.nil ? (clause) : ((function(key){return (LISP.list(LISP.list(key), LISP["list*"](LISP.intern("record"), LISP.list(LISP.intern("cdr"), value), LISP.cdar(clause), LISP.cdr(clause))));})(LISP.caar(clause)))));}), clauses))));})(LISP.gensym()));}));
LISP["traverse-quoted-value"] = (function(x){return (((LISP["pair?"](x)) !== LISP.nil ? (LISP.vector(LISP.intern(":FUNCALL"), LISP.vector(LISP.intern(":REF"), ((LISP["proper-list?"](x)) !== LISP.nil ? (LISP.intern("list")) : (LISP.intern("list*")))), LISP.map(LISP["traverse-quoted-value"], LISP["dotted->proper"](x)))) : (LISP.vector(LISP.intern(":CONST"), x))));});
LISP["traverse-list"] = (function(s, scope){return ((function(__17){return ((function(__18){return (((LISP["eq?"](__18, LISP.intern("quote"))) !== LISP.nil ? (LISP.apply((function(x){return (((LISP["pair?"](x)) !== LISP.nil ? (LISP.vector(LISP.intern(":REF"), LISP["scope-add-var"](scope, LISP["traverse-quoted-value"](x)))) : (LISP.vector(LISP.intern(":CONST"), x))));}), LISP.cdr(__17))) : (((LISP["eq?"](__18, LISP.intern("if"))) !== LISP.nil ? (LISP.apply((function(p, thn){var els = LISP._getRestArgs(arguments, 2); return (LISP.vector(LISP.intern(":IF"), LISP["traverse*"](p, scope), LISP["traverse*"](thn, scope), ((LISP["null?"](els)) !== LISP.nil ? (LISP.nil) : (LISP["traverse*"](LISP.car(els), scope)))));}), LISP.cdr(__17))) : (((LISP["eq?"](__18, LISP.intern("set!"))) !== LISP.nil ? (LISP.apply((function(x, v){return (LISP.vector(LISP.intern(":SET!"), LISP["traverse*"](x, scope), LISP["traverse*"](v, scope)));}), LISP.cdr(__17))) : (((LISP["eq?"](__18, LISP.intern("lambda"))) !== LISP.nil ? (LISP.apply((function(params){var body = LISP._getRestArgs(arguments, 1); return ((function(new$2dscope){return (LISP.vector(LISP.intern(":LAMBDA"), new$2dscope, params, LISP["traverse-args"](body, new$2dscope)));})(LISP["new-scope"](scope, params)));}), LISP.cdr(__17))) : (((LISP["eq?"](__18, LISP.intern("define"))) !== LISP.nil ? (LISP.apply((function(name, value){var rest = LISP._getRestArgs(arguments, 2); return (((LISP["pair?"](name)) !== LISP.nil ? (LISP["traverse*"](LISP.list(LISP.intern("define"), LISP.car(name), LISP["list*"](LISP.intern("lambda"), LISP.cdr(name), value, rest)), scope)) : (LISP.vector(LISP.intern(":DEFINE"), LISP["traverse*"](name, scope), LISP["traverse*"](value, scope)))));}), LISP.cdr(__17))) : (((LISP["eq?"](__18, LISP.intern("define-macro"))) !== LISP.nil ? (LISP.apply((function(name$2dparams){var body = LISP._getRestArgs(arguments, 1); return ((function(name, params){return (LISP.vector(LISP.intern(":DEFMACRO"), name, LISP["list*"](LISP.intern("lambda"), params, body)));})(LISP.car(name$2dparams), LISP.cdr(name$2dparams)));}), LISP.cdr(__17))) : (((LISP["eq?"](__18, LISP.intern("new"))) !== LISP.nil ? (LISP.apply((function(klass){var args = LISP._getRestArgs(arguments, 1); return (LISP.vector(LISP.intern(":NEW"), klass, LISP["traverse-args"](args, LISP["new-scope"])));}), LISP.cdr(__17))) : (LISP.vector(LISP.intern(":FUNCALL"), LISP["traverse*"](LISP.car(s), scope), LISP["traverse-args"](LISP.cdr(s), scope)))))))))))))))));})(LISP.car(__17)));})(s));});
LISP["traverse*"] = (function(s, scope){return (((LISP["pair?"](s)) !== LISP.nil ? ((function(expanded){return (((LISP["pair?"](expanded)) !== LISP.nil ? (LISP["traverse-list"](expanded, scope)) : (LISP["traverse*"](expanded, scope))));})(LISP.macroexpand(s))) : (((LISP["symbol?"](s)) !== LISP.nil ? (LISP.vector(LISP.intern(":REF"), s)) : (LISP.vector(LISP.intern(":CONST"), s))))));});
LISP["get-receiver"] = (function(sym){return ((function(s){return ((function(it){return (((it) !== LISP.nil ? (LISP.intern(LISP.substring(s, 0, it))) : (sym)));})(LISP["string-scan"](s, ".")));})(LISP["symbol->string"](sym)));});
LISP["local-var?"] = (function(sym, scope){return (LISP["scope-var?"](scope, LISP["get-receiver"](sym)));});
LISP["expand-args"] = (function(args, scope){return (LISP["string-join"](LISP.map((function(x){return (LISP["compile*"](x, scope));}), args), ", "));});
LISP["expand-body"] = (function(body, scope){return (((LISP["null?"](body)) !== LISP.nil ? ("LISP.nil") : (LISP["expand-args"](body, scope))));});
LISP["escape-char"] = (function(c){return (((LISP["string=?"](c, "\\")) !== LISP.nil ? ("\\\\") : (((LISP["string=?"](c, "\t")) !== LISP.nil ? ("\\t") : (((LISP["string=?"](c, "\n")) !== LISP.nil ? ("\\n") : (((LISP["string=?"](c, "\"")) !== LISP.nil ? ("\\\"") : (c)))))))));});
LISP["escape-string"] = (function(s){return (LISP["regexp-replace-all"](/[\\\t\n"]/, s, (function(m){return (LISP["escape-char"](m()));})));});
LISP["escape-symbol"] = (function(sym){return (LISP["escape-sym-char"] = (function(c){return (LISP["string-append"]("$", LISP["integer->hex-string"](LISP["char->integer"](c), "00")));}), LISP["integer->hex-string"] = (function(x, padding){return ((function(s){return ((function(sl){return ((function(pl){return (LISP.substring(s, (sl - pl), sl));})(LISP["string-length"](padding)));})(LISP["string-length"](s)));})(LISP["string-append"](padding, LISP["number->string"](x, 16))));}), LISP["regexp-replace-all"](/[^0-9A-Za-z_.]/, LISP["symbol->string"](sym), (function(m){return (LISP["escape-sym-char"](LISP["string-ref"](m(), 0)));})));});
LISP["compile-symbol"] = (function(sym, scope){return (((LISP["local-var?"](sym, scope)) !== LISP.nil ? (LISP["escape-symbol"](sym)) : ((function(s){return (((LISP.rxmatch(/^[0-9A-Za-z_.]*$/, s)) !== LISP.nil ? (LISP["string-append"]("LISP.", s)) : (LISP["string-append"]("LISP[\"", LISP["escape-string"](s), "\"]"))));})(LISP["symbol->string"](sym)))));});
LISP["compile-string"] = (function(str){return (LISP["string-append"]("\"", LISP["escape-string"](str), "\""));});
LISP["compile-vector"] = (function(vect, scope){return (LISP["string-append"]("[", (function(v){return (v.join(", "));})(LISP["vector-map"]((function(x){return (LISP["compile-quote"](x, scope));}), vect)), "]"));});
LISP["compile-regexp"] = (function(regex){return (LISP["string-append"]("/", LISP["regexp->string"](regex), "/"));});
LISP["compile-literal"] = (function(s, scope){return (((LISP["number?"](s)) !== LISP.nil ? (LISP["number->string"](s)) : (((LISP["symbol?"](s)) !== LISP.nil ? (LISP["compile-symbol"](s, scope)) : (((LISP["string?"](s)) !== LISP.nil ? (LISP["compile-string"](s)) : (((LISP["vector?"](s)) !== LISP.nil ? (LISP["compile-vector"](s, scope)) : (((LISP["regexp?"](s)) !== LISP.nil ? (LISP["compile-regexp"](s)) : (((LISP["null?"](s)) !== LISP.nil ? ("LISP.nil") : (LISP.error(LISP["string-append"]("compile-literal: [", s, "]")))))))))))))));});
LISP["unary-op?"] = (function() { var __19 = LISP.list(LISP.intern("+"), LISP.intern("-")); return (function(sym){return (LISP.member(sym, __19));}); })();
LISP["compile-unary-op"] = (function(fn, arg, scope){return (LISP["string-append"]("(", LISP["symbol->string"](fn), LISP["compile*"](arg, scope), ")"));});
LISP["binop?"] = (function() { var __20 = LISP.list(LISP.intern("+"), LISP.intern("-"), LISP.intern("*"), LISP.intern("/"), LISP.intern("%")); return (function(sym){return (LISP.member(sym, __20));}); })();
LISP["compile-binop"] = (function(fn, args, scope){return (LISP["string-append"]("(", LISP["string-join"](LISP.map((function(x){return (LISP["compile*"](x, scope));}), args), LISP["string-append"](" ", LISP["symbol->string"](fn), " ")), ")"));});
LISP["do-compile-funcall"] = (function(fn, args, scope){return (LISP["string-append"](LISP["compile*"](fn, scope), "(", LISP["expand-args"](args, scope), ")"));});
LISP["compile-funcall"] = (function(fn, args, scope){return (((((LISP["eq?"](LISP["vector-ref"](fn, 0), LISP.intern(":REF"))) !== LISP.nil ? (((LISP.not(LISP["local-var?"](LISP["vector-ref"](fn, 1), scope))) !== LISP.nil ? (LISP.not(LISP["null?"](args))) : (LISP.nil))) : (LISP.nil))) !== LISP.nil ? ((function(fnsym){return (((((LISP["binop?"](fnsym)) !== LISP.nil ? (LISP.not(LISP["null?"](LISP.cdr(args)))) : (LISP.nil))) !== LISP.nil ? (LISP["compile-binop"](fnsym, args, scope)) : (((((LISP["unary-op?"](fnsym)) !== LISP.nil ? (LISP["null?"](LISP.cdr(args))) : (LISP.nil))) !== LISP.nil ? (LISP["compile-unary-op"](fnsym, LISP.car(args), scope)) : (LISP["do-compile-funcall"](fn, args, scope))))));})(LISP["vector-ref"](fn, 1))) : (LISP["do-compile-funcall"](fn, args, scope))));});
LISP["compile-quote"] = (function(x, scope){return (((LISP["pair?"](x)) !== LISP.nil ? (LISP["compile*"](LISP.list(LISP.intern("cons"), LISP.list(LISP.intern("quote"), LISP.car(x)), LISP.list(LISP.intern("quote"), LISP.cdr(x))), scope)) : (((LISP["symbol?"](x)) !== LISP.nil ? (LISP["string-append"]("LISP.intern(\"", LISP["escape-string"](LISP["symbol->string"](x)), "\")")) : (LISP["compile-literal"](x, scope))))));});
LISP["compile-if"] = (function(pred$2dnode, then$2dnode, else$2dnode, scope){return (LISP["string-append"]("((", LISP["compile*"](pred$2dnode, scope), ") !== LISP.nil ? (", LISP["compile*"](then$2dnode, scope), ") : (", ((else$2dnode) !== LISP.nil ? (LISP["compile*"](else$2dnode, scope)) : ("LISP.nil")), "))"));});
LISP["compile-set!"] = (function(sym, val, scope){return (LISP["string-append"](LISP["compile*"](sym, scope), " = ", LISP["compile*"](val, scope)));});
LISP["compile-lambda"] = (function(params, bodies, base$2dscope, extended$2dscope){return ((function(proper$2dparams, rest){return (LISP["string-append"]("(function(", LISP["string-join"](LISP.map((function(x){return (LISP["escape-symbol"](x));}), proper$2dparams), ", "), "){", ((LISP["null?"](rest)) !== LISP.nil ? ("") : (LISP["string-append"]("var ", LISP["symbol->string"](rest), " = LISP._getRestArgs(arguments, ", LISP["number->string"](LISP.length(proper$2dparams)), "); "))), "return (", LISP["expand-body"](bodies, extended$2dscope), ");})"));})(((LISP["proper-list?"](params)) !== LISP.nil ? (params) : (LISP["reverse!"](LISP.reverse(params)))), ((LISP["pair?"](params)) !== LISP.nil ? (LISP.cdr(LISP["last-pair"](params))) : (params))));});
LISP["compile-define"] = (function(name, value, scope){return (LISP["string-append"](LISP["compile*"](name, scope), " = ", LISP["compile*"](value, scope)));});
LISP.macroexpand = (function(exp){return ((function(expanded){return (((LISP["equal?"](expanded, exp)) !== LISP.nil ? (exp) : (LISP.macroexpand(expanded))));})(LISP["macroexpand-1"](exp)));});
LISP["compile-new"] = (function(class$2dname, args, scope){return (LISP["string-append"]("new ", LISP["symbol->string"](class$2dname), "(", LISP["expand-args"](args, scope), ")"));});
LISP["compile-new-scope"] = (function(scope, compiled$2dbody){return ((function(it){return (((it) !== LISP.nil ? (LISP["string-append"]("(function() { var ", LISP["string-join"](LISP.map((function(x){return (LISP["string-append"](LISP["escape-symbol"](LISP.car(x)), " = ", LISP["compile*"](LISP.cdr(x), scope)));}), LISP.reverse(it)), ", "), "; return ", compiled$2dbody, "; })()")) : (compiled$2dbody)));})(LISP["scope-get-var"](scope)));});
LISP["compile*"] = (function(s, scope){return ((function(__21){return (((LISP["eq?"](__21, LISP.intern(":CONST"))) !== LISP.nil ? (LISP["compile-quote"](LISP["vector-ref"](s, 1), scope)) : (((LISP["eq?"](__21, LISP.intern(":REF"))) !== LISP.nil ? (LISP["compile-symbol"](LISP["vector-ref"](s, 1), scope)) : (((LISP["eq?"](__21, LISP.intern(":IF"))) !== LISP.nil ? ((function(p, thn, els){return (LISP["compile-if"](p, thn, els, scope));})(LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), LISP["vector-ref"](s, 3))) : (((LISP["eq?"](__21, LISP.intern(":FUNCALL"))) !== LISP.nil ? (LISP["compile-funcall"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), scope)) : (((LISP["eq?"](__21, LISP.intern(":SET!"))) !== LISP.nil ? (LISP["compile-set!"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), scope)) : (((LISP["eq?"](__21, LISP.intern(":LAMBDA"))) !== LISP.nil ? ((function(extended$2dscope, params, body){return (LISP["compile-new-scope"](extended$2dscope, LISP["compile-lambda"](params, body, scope, extended$2dscope)));})(LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), LISP["vector-ref"](s, 3))) : (((LISP["eq?"](__21, LISP.intern(":DEFINE"))) !== LISP.nil ? (LISP["compile-define"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), scope)) : (((LISP["eq?"](__21, LISP.intern(":DEFMACRO"))) !== LISP.nil ? (LISP["do-compile-defmacro"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2))) : (((LISP["eq?"](__21, LISP.intern(":NEW"))) !== LISP.nil ? (LISP["compile-new"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), scope)) : (LISP["string-append"]("???", s, "???"))))))))))))))))))));})(LISP["vector-ref"](s, 0)));});
LISP.compile = (function(s){return ((function(top$2dscope){return ((function(tree){return (LISP["compile-new-scope"](top$2dscope, LISP["compile*"](tree, top$2dscope)));})(LISP["traverse*"](s, top$2dscope)));})(LISP["new-scope"](LISP.nil, LISP.nil)));});


  return LISP;
})();

module.exports = LISP;