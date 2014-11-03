LISP = {
  nil: false,

  Cons: function(car, cdr) {
    this.car = car;
    this.cdr = cdr;
  },

  cons: function(car, cdr) {
    return new LISP.Cons(car, cdr);
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

var s = LISP.cons(1, LISP.cons(2, 3));
console.log(s.toString());
