LISP = {
  nil: null,
  t: true,

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

  // +
  _2b: function() {
    if (arguments.length == 0)
      return 0;
    var result = arguments[0];
    for (var i = 1; i < arguments.length; ++i)
      result += arguments[i];
    return result;
  },
  // *
  _2a: function() {
    if (arguments.length == 0)
      return 1;
    var result = arguments[0];
    for (var i = 1; i < arguments.length; ++i)
      result *= arguments[i];
    return result;
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
      console.log(x.toString());
      break;
    }
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
