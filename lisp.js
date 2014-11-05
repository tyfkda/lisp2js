var LISP = {
  nil: null,
  t: true,

  _jsBoolToS: function(x)  { return x ? LISP.t : LISP.nil;  },
  _getRestArgs: function(args, start) {
    return Array.prototype.slice.call(args, start).toList();
  },

  Symbol: function(name) {
    this.name = name;
  },
  "symbol->string": function(x) {
    return x.toString();
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

  "pair?": function(x) {
    return LISP._jsBoolToS(x instanceof LISP.Cons);
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
  "number?": function(x) {
    return LISP._jsBoolToS(typeof x === 'number');
  },
  "number->string": function(x) {
    return x + '';
  },

  // String.
  "string-append": function() {
    var argumentsArray = [];
    argumentsArray = argumentsArray.concat.apply(argumentsArray, arguments);
    return argumentsArray.join('');
  },
  "string-join": function(list, separator) {
    return list.toArray().join(separator);
  },

  print: function(x) {
    switch (x) {
    case LISP.nil:
      console.log("nil");
      break;
    case LISP.t:
      console.log("t");
      break;
    default:
      if (typeof x == 'string')
        console.log(x);
      else
        console.log(x.toString());
      break;
    }
    return x;
  },

  apply: function(fn, params) {
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

  // Regexp.
  rxmatch: function(re, str) {
    return re.exec(str);
  },
  "regexp-replace-all": function(re, str, fn) {
    if (!re.global)
      re = eval(re.toString() + 'g')
    return str.replace(re, fn);
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
      ss.push(p.car.toString());
      separator = " ";
    }
    if (p !== LISP.nil) {
      ss.push(" . ");
      ss.push(p.toString());
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
