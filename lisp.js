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

  list: function() {
    var result = LISP.nil;
    for (var i = arguments.length; --i >= 0; )
      result = LISP.cons(arguments[i], result);
    return result;
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
        console.log('"' + x + '"');
      else
        console.log(x.toString());
      break;
    }
    return x;
  },

  // Hash table.
  "make-hash-table": function() {
    return {};
  },
  "hash-table-exists?": function(hash, x) {
    return x in hash ? LISP.t : LISP.nil;
  },
  "hash-table-get": function(hash, x) {
    return hash[x];
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
};

// Convert JS array into Lisp list.
Array.prototype.toList = function() {
  var result = LISP.nil;
  for (var i = this.length; --i >= 0; )
    result = LISP.cons(this[i], result);
  return result;
};
