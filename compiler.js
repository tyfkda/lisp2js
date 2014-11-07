LISP = {
  nil: null,
  t: true,

  _jsBoolToS: function(x)  { return x ? LISP.t : LISP.nil;  },
  _getRestArgs: function(args, start) {
    return Array.prototype.slice.call(args, start).toList();
  },
  "gauche-version": function() { return LISP.nil; },

  jseval: function(str) {
    return eval(str);
  },
  eval: function(exp) {
    return eval(LISP.compile(exp));
  },
  "interaction-environment": function() { return LISP.nil; },

  Symbol: function(name) {
    this.name = name;
  },
  "symbol->string": function(x) {
    return x.toString();
  },
  __gensymIndex: 0,
  gensym: function() {
    return "__" + (++LISP.__gensymIndex);
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
    return s.car;
  },
  cdr: function(s) {
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
    for (var ls = x; !LISP['eq?'](ls, LISP.nil); ) {
      var d = ls.cdr;
      ls.cdr = rev;
      rev = ls;
      ls = d;
    }
    return rev;
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
  "number?": function(x) {
    return LISP._jsBoolToS(typeof x === 'number');
  },
  "number->string": function(x) {
    return x + '';
  },

  // String.
  "string?": function(x) {
    return LISP._jsBoolToS(typeof x === 'string');
  },
  "string-append": function() {
    var argumentsArray = [];
    argumentsArray = argumentsArray.concat.apply(argumentsArray, arguments);
    return argumentsArray.join('');
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

  "char->integer": function(char, index) {
    return char.charCodeAt(index);
  },

  makeString: function(x) {
    if (x === LISP.nil)
      return 'nil';
    if (x === LISP.t)
      return 't';
    if (typeof x == 'string')
      return x;
    return x.toString();
  },
  print: function(x) {
    console.log(LISP.makeString(x));
    return x;
  },

  apply: function(fn) {
    var params = [];
    if (arguments.length > 1) {
      for (var i = 1; i < arguments.length - 1; ++i)
        params.push(arguments[i]);
      params = params.concat(arguments[arguments.length - 1].toArray());
    }
    return fn.apply(null, params);
  },

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

  // Regexp.
  "regexp?": function(x) {
    return LISP._jsBoolToS(x instanceof RegExp);
  },
  rxmatch: function(re, str) {
    return re.exec(str);
  },
  "regexp-replace-all": function(re, str, fn) {
    if (!re.global)
      re = eval(re.toString() + 'g')
    return str.replace(re, function (match) {
      return fn(function() {  // TODO: handle arguments.
        return match[0];
      });
    });
  },
};

LISP.Symbol.prototype = {
  toString: function() {
    return this.name;
  },
};

LISP.Cons.prototype = {
  toString: function() {
    var ss = [];
    var separator = "(";
    var p;
    for (p = this; p instanceof LISP.Cons; p = p.cdr) {
      ss.push(separator);
      ss.push(LISP.makeString(p.car));
      separator = " ";
    }
    if (p !== LISP.nil) {
      ss.push(" . ");
      ss.push(LISP.makeString(p));
    }
    ss.push(")");
    return ss.join("");
  },
  toArray: function() {
    var result = [];
    for (var p = this; p instanceof LISP.Cons; p = p.cdr)
      result.push(p.car);
    return result;
  },
};

// Convert JS array into Lisp list.
Array.prototype.toList = function() {
  var result = LISP.nil;
  for (var i = this.length; --i >= 0; )
    result = LISP.cons(this[i], result);
  return result;
};

module.exports = LISP;
if (typeof LISP === 'undefined')
  LISP = {};

LISP.SReader = function(str) {
  this.str = str;
}

LISP.SReader.prototype = {
  read: function() {
    var m;
    if (m = this.str.match(/^\s*([0-9]+)/))  // Number.
      return this.proceed(), parseInt(m[1]);
    if (m = this.str.match(/^\s*([0-9A-Za-z_\-+*\/%!?~^&<>=]+)/))  // Symbol.
      return this.proceed(), LISP.intern(m[1]);
    if (m = this.str.match(/^\s*\(/))  // Left paren '('.
      return this.proceed(), this.readList(RegExp.rightContext);
    if (m = this.str.match(/^\s*;[^\n]*\n?/))  // Line comment.
      return this.proceed(), this.read();
    if (m = this.str.match(/^\s*'/))  // quote.
      return this.proceed(), this.readQuote();
    if (m = this.str.match(/^\s*"([^"]*)"/))  // string.
      return this.proceed(), m[1];
    if (m = this.str.match(/^\s*`/))  // quasiquote.
      return this.proceed(), this.readQuasiQuote();
    if (m = this.str.match(/^\s*,(@?)/))  // unquote or unquote-splicing.
      return this.proceed(), this.readUnquote(m[1]);
    return undefined;
  },

  proceed: function(value) {
    this.str = RegExp.rightContext;
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

      if (m = this.str.match(/^\s*\)/)) {
        this.proceed();
        return LISP['reverse!'](result);
      }
      // Error
      console.error('Read failed: ' + this.str);
      return process.exit(1);
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
};

LISP["read-from-string"] = function(str) {
  var reader = new LISP.SReader(str);
  return reader.read();
};

module.exports = LISP;
/*let*/;
/*let1*/;
/*let**/;
/*cond*/;
/*and*/;
LISP["null?"] = (function(x){return (LISP["eq?"](x, LISP.nil));});
LISP.not = (function(x){return (LISP["eq?"](x, LISP.nil));});
LISP.caar = (function(x){return (LISP.car(LISP.car(x)));});
LISP.cadr = (function(x){return (LISP.car(LISP.cdr(x)));});
LISP.cdar = (function(x){return (LISP.cdr(LISP.car(x)));});
LISP.cddr = (function(x){return (LISP.cdr(LISP.cdr(x)));});
LISP.caddr = (function(x){return (LISP.car(LISP.cddr(x)));});
LISP.cdddr = (function(x){return (LISP.cdr(LISP.cddr(x)));});
LISP["equal?"] = (function(x, y){return (((LISP["eq?"](x, y)) !== LISP.nil ? (LISP.t) : (((LISP["pair?"](x)) !== LISP.nil ? (((LISP["pair?"](y)) !== LISP.nil ? (((LISP["equal?"](LISP.car(x), LISP.car(y))) !== LISP.nil ? (LISP["equal?"](LISP.cdr(x), LISP.cdr(y))) : (LISP.nil))) : (LISP.nil))) : (LISP.nil)))));});
LISP.length = (function(ls){return ((function(loop){return (loop = (function(ls, acc){return (((LISP["pair?"](ls)) !== LISP.nil ? (loop(LISP.cdr(ls), LISP["+"](acc, 1))) : (acc)));}), loop(ls, 0));})(LISP.nil));});
LISP["last-pair"] = (function(ls){return (((LISP["pair?"](LISP.cdr(ls))) !== LISP.nil ? (LISP["last-pair"](LISP.cdr(ls))) : (ls)));});
LISP["proper-list?"] = (function(ls){return (((LISP["pair?"](ls)) !== LISP.nil ? (LISP["null?"](LISP.cdr(LISP["last-pair"](ls)))) : (LISP.nil)));});
LISP.member = (function(x, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (((LISP["eq?"](x, LISP.car(ls))) !== LISP.nil ? (ls) : (LISP.member(x, LISP.cdr(ls)))))));});
LISP.assoc = (function(x, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (((LISP["eq?"](x, LISP.caar(ls))) !== LISP.nil ? (LISP.car(ls)) : (LISP.assoc(x, LISP.cdr(ls)))))));});
LISP.map = (function(f, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (LISP.cons(f(LISP.car(ls)), LISP.map(f, LISP.cdr(ls))))));});
LISP.append = (function(ls){var rest = LISP._getRestArgs(arguments, 1); return (((LISP["null?"](rest)) !== LISP.nil ? (ls) : (((LISP["null?"](ls)) !== LISP.nil ? (LISP.apply(LISP.append, rest)) : (LISP.cons(LISP.car(ls), LISP.apply(LISP.append, LISP.cdr(ls), rest)))))));});
LISP.reverse = (function(ls){return ((function(loop){return (loop = (function(ls, acc){return (((LISP["null?"](ls)) !== LISP.nil ? (acc) : (loop(LISP.cdr(ls), LISP.cons(LISP.car(ls), acc)))));}), loop(ls, LISP.nil));})(LISP.nil));});
LISP["list*"] = (function(){var args = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](args)) !== LISP.nil ? (LISP.nil) : (((LISP["null?"](LISP.cdr(args))) !== LISP.nil ? (LISP.car(args)) : ((function(loop){return (loop = (function(p, q){return (((LISP["null?"](LISP.cdr(q))) !== LISP.nil ? ((LISP["set-cdr!"](p, LISP.car(q)), args)) : (loop(q, LISP.cdr(q)))));}), loop(args, LISP.cdr(args)));})(LISP.nil))))));});
LISP["*run-on-gosh*"] = LISP["gauche-version"]();
LISP["expand-args"] = (function(args, env){return (LISP["string-join"](LISP.map((function(x){return (LISP["compile*"](x, env));}), args), ", "));});
LISP["expand-body"] = (function(body, env){return (((LISP["null?"](body)) !== LISP.nil ? ("LISP.nil") : (LISP["expand-args"](body, env))));});
LISP["escape-char"] = (function(c){return (((LISP["string=?"](c, "\\")) !== LISP.nil ? ("\\\\") : (((LISP["string=?"](c, "\t")) !== LISP.nil ? ("\\t") : (((LISP["string=?"](c, "\n")) !== LISP.nil ? ("\\n") : (((LISP["string=?"](c, "\"")) !== LISP.nil ? ("\\\"") : (c)))))))));});
LISP["escape-string"] = (function(s){return (LISP["regexp-replace-all"](/[\u0009\u000a"\\]/, s, (function(m){return (LISP["escape-char"](m()));})));});
LISP["escape-symbol"] = (function(sym){return (LISP["escape-char"] = (function(c){return (LISP["string-append"]("$", LISP["integer->hex-string"](LISP["char->integer"](c), "00")));}), LISP["integer->hex-string"] = (function(x, padding){return ((function(s){return ((function(sl){return ((function(pl){return (LISP.substring(s, LISP["-"](sl, pl), sl));})(LISP["string-length"](padding)));})(LISP["string-length"](s)));})(LISP["string-append"](padding, LISP["number->string"](x, 16))));}), LISP["regexp-replace-all"](/[^0-9A-Z_a-z]/, LISP["symbol->string"](sym), (function(m){return (LISP["escape-char"](LISP["string-ref"](m(), 0)));})));});
LISP["compile-symbol"] = (function(sym, env){return (LISP["local-var?"] = (function(sym, env){return (LISP.member(sym, env));}), ((LISP["local-var?"](sym, env)) !== LISP.nil ? (LISP["escape-symbol"](sym)) : ((function(s){return (((LISP.rxmatch(/^[0-9A-Z_a-z]*$/, s)) !== LISP.nil ? (LISP["string-append"]("LISP.", s)) : (LISP["string-append"]("LISP[\"", LISP["escape-string"](s), "\"]"))));})(LISP["symbol->string"](sym)))));});
LISP["compile-string"] = (function(str){return (LISP["string-append"]("\"", LISP["escape-string"](str), "\""));});
LISP["compile-regexp"] = (function(regex){return (LISP["string-append"]("/", LISP["regexp->string"](regex), "/"));});
LISP["compile-literal"] = (function(s, env){return (((LISP["number?"](s)) !== LISP.nil ? (LISP["number->string"](s)) : (((LISP["symbol?"](s)) !== LISP.nil ? (LISP["compile-symbol"](s, env)) : (((LISP["string?"](s)) !== LISP.nil ? (LISP["compile-string"](s)) : (((LISP["regexp?"](s)) !== LISP.nil ? (LISP["compile-regexp"](s)) : (((LISP["null?"](s)) !== LISP.nil ? ("LISP.nil") : (((LISP["eq?"](s, LISP.t)) !== LISP.nil ? ("LISP.t") : (((LISP["eq?"](s, LISP.nil)) !== LISP.nil ? ("LISP.nil") : (LISP.error(LISP["string-append"]("compile-literal: [", s, "]")))))))))))))))));});
LISP["compile-funcall"] = (function(s, env){return ((function(fn, args){return (LISP["string-append"](LISP["compile*"](fn, env), "(", LISP["expand-args"](args, env), ")"));})(LISP.car(s), LISP.cdr(s)));});
LISP["compile-quote"] = (function(s, env){return ((function(x){return (((LISP["pair?"](x)) !== LISP.nil ? (LISP["compile*"](LISP.list(LISP.intern("cons"), LISP.list(LISP.intern("quote"), LISP.car(x)), LISP.list(LISP.intern("quote"), LISP.cdr(x))), env)) : (((LISP["symbol?"](x)) !== LISP.nil ? (LISP["string-append"]("LISP.intern(\"", LISP["escape-string"](LISP["symbol->string"](x)), "\")")) : (LISP["compile-literal"](x, env))))));})(LISP.car(s)));});
LISP["compile-if"] = (function(s, env){return ((function(p, then$2dnode, else$3f){return (LISP["string-append"]("((", LISP["compile*"](p, env), ") !== LISP.nil ? (", LISP["compile*"](then$2dnode, env), ") : (", ((LISP["null?"](else$3f)) !== LISP.nil ? ("LISP.nil") : (LISP["compile*"](LISP.car(else$3f), env))), "))"));})(LISP.car(s), LISP.cadr(s), LISP.cddr(s)));});
LISP["compile-set!"] = (function(s, env){return ((function(sym, val){return (LISP["string-append"](LISP["compile*"](sym, env), " = ", LISP["compile*"](val, env)));})(LISP.car(s), LISP.cadr(s)));});
LISP["compile-begin"] = (function(s, env){return (((LISP["null?"](s)) !== LISP.nil ? ("LISP.nil") : (((LISP["null?"](LISP.cdr(s))) !== LISP.nil ? (LISP["compile*"](LISP.car(s), env)) : (LISP["string-append"]("(", LISP["expand-body"](s, env), ")"))))));});
LISP["compile-lambda"] = (function(s, env){return (LISP["extend-env"] = (function(env, params){return (LISP.append(params, env));}), (function(raw$2dparams){return ((function(params){return ((function(rest){return ((function(bodies){return ((function(newenv){return (LISP["string-append"]("(function(", LISP["expand-args"](params, newenv), "){", ((LISP["null?"](rest)) !== LISP.nil ? ("") : (LISP["string-append"]("var ", LISP["symbol->string"](rest), " = LISP._getRestArgs(arguments, ", LISP["number->string"](LISP.length(params)), "); "))), "return (", LISP["expand-body"](bodies, newenv), ");})"));})(LISP["extend-env"](env, ((LISP["null?"](rest)) !== LISP.nil ? (params) : (LISP.append(LISP.list(rest), params))))));})(LISP.cdr(s)));})(((LISP["pair?"](raw$2dparams)) !== LISP.nil ? (LISP.cdr(LISP["last-pair"](raw$2dparams))) : (raw$2dparams))));})(((LISP["proper-list?"](raw$2dparams)) !== LISP.nil ? (raw$2dparams) : (LISP["reverse!"](LISP.reverse(raw$2dparams))))));})(LISP.car(s)));});
LISP["compile-define"] = (function(s, env){return ((function(name, body){return (((LISP["pair?"](name)) !== LISP.nil ? (LISP["compile-define"](LISP.list(LISP.car(name), LISP["list*"](LISP.intern("lambda"), LISP.cdr(name), body)), env)) : (LISP["string-append"](LISP["compile-symbol"](name, env), " = ", LISP["compile*"](LISP.car(body), env)))));})(LISP.car(s), LISP.cdr(s)));});
LISP["*macro-table*"] = LISP["make-hash-table"]();
LISP["register-macro"] = (function(name, func){return (LISP["hash-table-put!"](LISP["*macro-table*"], name, func));});
LISP["compile-defmacro"] = (function(s, env){return ((function(name, params, body){return ((function(exp){return (((LISP["*run-on-gosh*"]) !== LISP.nil ? ((LISP["hash-table-put!"](LISP["*macro-table*"], name, LISP.eval(exp, LISP["interaction-environment"]())), LISP["string-append"]("/*", LISP["symbol->string"](name), "*/"))) : ((function(compiled){return (LISP["register-macro"](name, LISP.jseval(compiled)), LISP["string-append"]("LISP['register-macro'](", compiled, ")"));})(LISP.compile(exp)))));})(LISP["list*"](LISP.intern("lambda"), params, body)));})(LISP.car(s), LISP.cadr(s), LISP.cddr(s)));});
LISP["macro?"] = (function(symbol){return (LISP["hash-table-exists?"](LISP["*macro-table*"], symbol));});
LISP["macroexpand-1"] = (function(s){return ((function(f){return (((f) !== LISP.nil ? (LISP.apply(f, LISP.cdr(s))) : (s)));})(((LISP["pair?"](s)) !== LISP.nil ? (LISP["hash-table-get"](LISP["*macro-table*"], LISP.car(s), LISP.nil)) : (LISP.nil))));});
LISP.macroexpand = (function(exp){return ((function(expanded){return (((LISP["equal?"](expanded, exp)) !== LISP.nil ? (exp) : (LISP.macroexpand(expanded))));})(LISP["macroexpand-1"](exp)));});
LISP["*special-forms*"] = LISP.list(LISP.cons(LISP.intern("quote"), LISP["compile-quote"]), LISP.cons(LISP.intern("if"), LISP["compile-if"]), LISP.cons(LISP.intern("begin"), LISP["compile-begin"]), LISP.cons(LISP.intern("set!"), LISP["compile-set!"]), LISP.cons(LISP.intern("lambda"), LISP["compile-lambda"]), LISP.cons(LISP.intern("define"), LISP["compile-define"]), LISP.cons(LISP.intern("define-macro"), LISP["compile-defmacro"]));
LISP["special-form?"] = (function(s){return ((function(G41){return (((G41) !== LISP.nil ? (LISP.cdr(G41)) : (LISP.nil)));})(LISP.assoc(LISP.car(s), LISP["*special-forms*"])));});
LISP["compile*"] = (function(s, env){return (((LISP["pair?"](s)) !== LISP.nil ? (((LISP["macro?"](LISP.car(s))) !== LISP.nil ? (LISP["compile*"](LISP.macroexpand(s), env)) : ((function(G42){return (((G42) !== LISP.nil ? ((function(fn){return (fn(LISP.cdr(s), env));})(G42)) : (LISP["compile-funcall"](LISP.macroexpand(s), env))));})(LISP["special-form?"](s))))) : (LISP["compile-literal"](s, env))));});
LISP.compile = (function(s){return (LISP["compile*"](s, LISP.nil));});
LISP.main = (function(args){return ((function(ss){return (LISP.dolist(LISP.s(ss), LISP.display(LISP.compile(LISP.s)), LISP.display(";\n")));})(LISP["port->sexp-list"](LISP["current-input-port"]())), 0);});
