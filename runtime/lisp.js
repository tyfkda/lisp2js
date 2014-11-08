LISP = {
  nil: null,
  t: true,

  _jsBoolToS: function(x)  { return x ? LISP.t : LISP.nil;  },
  _getRestArgs: function(args, start) {
    return Array.prototype.slice.call(args, start).toList();
  },
  _output: function(str) {
    console.log(str);
  },
  "*run-on-js*": true,  // LISP.t

  "*macro-table*": {},
  "register-macro": function(name, func) {
    LISP['*macro-table*'][name] = func;
  },

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

  // String.
  "string?": function(x) {
    return LISP._jsBoolToS(typeof x === 'string');
  },
  "string=?": function(x, y) {
    return LISP._jsBoolToS(x === y);
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
    return x.toString(inspect);
  },
  print: function(x) {
    LISP._output(LISP.makeString(x));
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
  toString: function(inspect) {
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
