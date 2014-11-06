if (typeof LISP === 'undefined')
  LISP = {};

function SReader(str) {
  this.str = str;
}

SReader.prototype.read = function() {
  var m;
  if (m = this.str.match(/^\s*([0-9]+)/))  // Number.
    return this.proceed(), parseInt(m[1]);
  if (m = this.str.match(/^\s*([0-9A-Za-z_\-+*/%!?~^&]+)/))  // Symbol.
    return this.proceed(), LISP.intern(m[1]);
  if (m = this.str.match(/^\s*\(/))  // Left paren '('.
    return this.proceed(), this.readList(RegExp.rightContext);
  if (m = this.str.match(/^\s*;[^\n]*\n?/))  // Line comment.
    return this.proceed(), this.read();
  return undefined;
};

SReader.prototype.proceed = function(value) {
  this.str = RegExp.rightContext;
};

SReader.prototype.readList = function() {
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
    return undefined;
  }
};

LISP["read-from-string"] = function(str) {
  var reader = new SReader(str);
  return reader.read();
};

module.exports = LISP;
